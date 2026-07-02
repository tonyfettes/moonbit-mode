;;; moonbit-ts-mode.el --- MoonBit tree-sitter major mode -*- lexical-binding: t; -*-

;; Author: MoonBit Contributors
;; URL: https://github.com/moonbit-community/moonbit-ts-mode
;; Keywords: languages, tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; MoonBit tree-sitter major mode with Eglot integration.
;;

;;; Code:

(require 'compile)
(require 'cl-lib)
(require 'eieio)
(require 'project)
(require 'treesit)

(defgroup moonbit nil
  "MoonBit language support."
  :group 'languages)

(defcustom moonbit-lsp-server-command '("moon" "lsp")
  "Command to start the MoonBit language server."
  :type '(repeat string)
  :group 'moonbit)

(defcustom moonbit-project-build-command "moon build"
  "Command used for building MoonBit projects."
  :type 'string
  :group 'moonbit)

(defcustom moonbit-project-check-command "moon check"
  "Command used for type checking MoonBit projects."
  :type 'string
  :group 'moonbit)

(defcustom moonbit-project-test-command "moon test"
  "Command used for testing MoonBit projects."
  :type 'string
  :group 'moonbit)

(defcustom moonbit-indent-offset 2
  "Number of spaces to use for each MoonBit indentation level."
  :type 'natnum
  :safe 'natnump
  :group 'moonbit)

(defvar project-compile-command nil
  "Project compile command used by some project integrations.")

(defvar project-test-command nil
  "Project test command used by some project integrations.")

(defvar eglot-server-programs)
(declare-function eglot--lsp-position-to-point "eglot")
(declare-function eglot--signal-textDocument/didChange "eglot")
(declare-function eglot--TextDocumentIdentifier "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function jsonrpc-async-request "jsonrpc")
(declare-function moonbit-eglot-server--eieio-childp "moonbit-ts-mode")

(defcustom moonbit-enable-compile-errors t
  "Whether to enable MoonBit compile error parsing in compilation buffers."
  :type 'boolean
  :group 'moonbit)

(defcustom moonbit-enable-semantic-tokens t
  "Whether to enable LSP semantic token highlighting when available."
  :type 'boolean
  :group 'moonbit)

(defcustom moonbit-semantic-tokens-refresh-delay 0.2
  "Idle delay before refreshing MoonBit semantic token highlighting."
  :type 'number
  :group 'moonbit)

(defvar moonbit-ts-mode--syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?/ ". 124b" table)
    (modify-syntax-entry ?* ". 23" table)
    (modify-syntax-entry ?\n "> b" table)
    (modify-syntax-entry ?\^m "> b" table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    table)
  "Syntax table for `moonbit-ts-mode'.")

(defconst moonbit-ts-mode--syntax-propertize-query
  '([(multiline_string_content)
     (multiline_interpolation_content)] @multiline-string-content)
  "Tree-sitter query for MoonBit multiline string contents.")

(defvar-local moonbit-ts-mode--compiled-syntax-propertize-query nil
  "Compiled `syntax-propertize' query for the current MoonBit grammar.")

(defun moonbit-ts-mode--syntax-propertize (start end)
  "Apply syntax properties to MoonBit strings between START and END.

In multiline string contents, slash characters must not retain syntax as
comment starters.  Interpolators are deliberately excluded because
comments in their embedded MoonBit expressions are real comments."
  (dolist (node
           (treesit-query-capture
            (moonbit-ts-mode--language)
            moonbit-ts-mode--compiled-syntax-propertize-query
            start end t))
    ;; A `multiline_interpolation_content' node containing an
    ;; `interpolator' has a named child; literal content nodes are leaves.
    (when (zerop (treesit-node-child-count node t))
      (let ((beg (max start (treesit-node-start node)))
            (limit (min end (treesit-node-end node))))
        (when (< beg limit)
          (save-excursion
            (goto-char beg)
            (while (search-forward "/" limit t)
              (put-text-property
               (1- (point)) (point) 'syntax-table
               (string-to-syntax ".")))))))))

(defun moonbit-ts-mode--multiline-string-line-p ()
  "Return non-nil if the current line is a multiline string fragment."
  (let* ((line-beg (line-beginning-position))
         (line-end (line-end-position))
         (pos (if (> line-end line-beg) (1- line-end) line-end))
         (node (treesit-node-at pos (moonbit-ts-mode--language)))
         (fragment
          (and node
               (treesit-parent-until
                node
                (lambda (candidate)
                  (member (treesit-node-type candidate)
                          '("multiline_string_fragment"
                            "multiline_interpolation_fragment")))
                t))))
    (and fragment
         (>= (treesit-node-start fragment) line-beg)
         (= (treesit-node-end fragment) line-end))))

(defun moonbit-ts-mode--comment-insert ()
  "Insert a comment without modifying a multiline string's value."
  (if (moonbit-ts-mode--multiline-string-line-p)
      (progn
        (back-to-indentation)
        (insert comment-start))
    (let ((comment-insert-comment-function nil))
      (comment-indent))))

(defface moonbit-semantic-token-async-face
  '((t (:slant italic)))
  "Face used for async MoonBit semantic tokens."
  :group 'moonbit)

(defface moonbit-semantic-token-error-face
  '((t (:underline t)))
  "Face used for error-raising MoonBit semantic tokens."
  :group 'moonbit)

(defconst moonbit--compile-error-regexp
  "\\[\\s-*\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)\\s-*\\]"
  "Regexp matching MoonBit compiler errors.")

(defconst moonbit--ts-font-lock-rules
  '((interpolator) @default

    (package_identifier) @font-lock-namespace-face

    (positional_parameter (lowercase_identifier) @font-lock-parameter-face)
    (labelled_parameter (label (lowercase_identifier)) @font-lock-parameter-face)
    (optional_parameter (optional_label (lowercase_identifier)) @font-lock-parameter-face)
    (optional_parameter_with_default (label (lowercase_identifier)) @font-lock-parameter-face)
    ((positional_parameter (lowercase_identifier) @font-lock-builtin-face)
     (:match "^self$" @font-lock-builtin-face))
    ((labelled_parameter (label (lowercase_identifier)) @font-lock-builtin-face)
     (:match "^self$" @font-lock-builtin-face))
    ((optional_parameter (optional_label (lowercase_identifier)) @font-lock-builtin-face)
     (:match "^self$" @font-lock-builtin-face))
    ((optional_parameter_with_default (label (lowercase_identifier)) @font-lock-builtin-face)
     (:match "^self$" @font-lock-builtin-face))

    (tuple_pattern (lowercase_identifier) @font-lock-variable-name-face)
    (constructor_pattern_argument (lowercase_identifier) @font-lock-variable-name-face)
    (constructor_pattern_argument "=" (lowercase_identifier) @font-lock-variable-name-face)
    (constructor_pattern_argument (label (lowercase_identifier) @font-lock-variable-name-face))
    (case_clause (lowercase_identifier) @font-lock-variable-name-face "=>")
    (matrix_case_clause (lowercase_identifier) @font-lock-variable-name-face "=>")
    (let_expression (lowercase_identifier) @font-lock-variable-name-face)

    (qualified_identifier (lowercase_identifier) @font-lock-variable-name-face)
    ((qualified_identifier (lowercase_identifier) @font-lock-builtin-face)
     (:match "^self$" @font-lock-builtin-face))
    (qualified_identifier (dot_lowercase_identifier) @font-lock-variable-name-face)

    (value_definition (lowercase_identifier) @font-lock-variable-name-face)
    (let_mut_expression (lowercase_identifier) @font-lock-variable-name-face)
    (for_in_expression "for" (lowercase_identifier) @font-lock-variable-name-face "in")
    (for_binder (lowercase_identifier) @font-lock-variable-name-face)
    (package_statement_identifier) @font-lock-variable-name-face
    (package_assignment_statement
     name: (package_statement_identifier) @font-lock-variable-name-face)

    (enum_constructor) @font-lock-constructor-face
    (constructor_expression (uppercase_identifier) @font-lock-constructor-face)
    (constructor_expression (dot_uppercase_identifier) @font-lock-constructor-face)

    (const_definition (uppercase_identifier) @font-lock-constant-face)
    ((constructor_expression (uppercase_identifier) @font-lock-constant-face)
     (:match "^[A-Z][A-Z_]+$" @font-lock-constant-face))
    ((constructor_expression (dot_uppercase_identifier) @font-lock-constant-face)
     (:match "^\\.[A-Z][A-Z_]+$" @font-lock-constant-face))

    (type_identifier) @font-lock-type-face
    (qualified_type_identifier) @font-lock-type-face

    (enum_definition (identifier) @font-lock-type-face)
    (struct_definition (identifier) @font-lock-type-face)
    (tuple_struct_definition (identifier) @font-lock-type-face)
    (type_definition (identifier) @font-lock-type-face)
    (trait_definition (identifier) @font-lock-type-face)
    (type_alias_targets (identifier) @font-lock-type-face)
    (type_alias_targets (dot_identifier) @font-lock-type-face)
    (type_alias_target (identifier) @font-lock-type-face)
    (error_type_definition (identifier) @font-lock-type-face)
    (trait_alias_targets (identifier) @font-lock-type-face)
    (trait_alias_targets (dot_identifier) @font-lock-type-face)
    (trait_alias_target (identifier) @font-lock-type-face)

    ((qualified_type_identifier) @font-lock-builtin-face
     (:match "^(?:Unit|Bool|Byte|Int16|UInt16|Int|UInt|Int64|UInt64|Float|Double|FixedArray|Array|Bytes|String|Error|Self)$"
             @font-lock-builtin-face))
    ((qualified_type_identifier) @font-lock-builtin-face
     (:match "^(?:Eq|Compare|Hash|Show|Default|ToJson|FromJson)$"
             @font-lock-builtin-face))

    (struct_field_declaration (lowercase_identifier) @font-lock-property-name-face)
    (struct_expression (labeled_expression (lowercase_identifier) @font-lock-property-name-face))
    (struct_expression (labeled_expression_pun (lowercase_identifier) @font-lock-property-name-face))
    (struct_expression (labeled_expression (lowercase_identifier) @font-lock-property-name-face))
    (struct_expression (labeled_expression_pun (lowercase_identifier) @font-lock-property-name-face))
    (struct_field_expression (labeled_expression (lowercase_identifier) @font-lock-property-name-face))
    (struct_field_expression (labeled_expression_pun (lowercase_identifier) @font-lock-property-name-face))
    (struct_field_expression (labeled_expression (lowercase_identifier) @font-lock-property-name-face))
    (struct_field_expression (labeled_expression_pun (lowercase_identifier) @font-lock-property-name-face))
    (struct_pattern (struct_field_pattern (labeled_pattern (lowercase_identifier) @font-lock-property-name-face)))
    (struct_pattern (struct_field_pattern (labeled_pattern_pun (lowercase_identifier) @font-lock-property-name-face)))
    (access_expression (accessor (dot_identifier) @font-lock-property-name-face))
    (constructor_pattern_argument (lowercase_identifier) @font-lock-property-name-face "=")
    (apply_expression
     (constructor_expression)
     (arguments (argument (labelled_argument (lowercase_identifier) @font-lock-property-name-face "="))))

    (attribute) @font-lock-attribute-face
    ((attribute) @font-lock-preprocessor-face
     (:match "^#coverage\\.skip$" @font-lock-preprocessor-face))
    ((attribute) @font-lock-preprocessor-face
     (:match "^#deprecated\\(.*\\)" @font-lock-preprocessor-face))

    (apply_expression (qualified_identifier (lowercase_identifier) @font-lock-function-call-face))
    (apply_expression (qualified_identifier (dot_lowercase_identifier) @font-lock-function-call-face))
    (package_apply_statement
     name: (package_statement_identifier) @font-lock-function-call-face)

    (method_expression (lowercase_identifier) @font-lock-function-call-face)
    (dot_apply_expression (dot_identifier) @font-lock-function-call-face)
    (dot_dot_apply_expression (dot_dot_identifier) @font-lock-function-call-face)

    (function_definition (function_identifier (lowercase_identifier) @font-lock-function-name-face))
    (struct_constructor_declaration (lowercase_identifier) @font-lock-function-name-face)
    (function_alias_targets (lowercase_identifier) @font-lock-function-name-face)
    (function_alias_targets (dot_lowercase_identifier) @font-lock-function-name-face)
    (function_alias_targets (dot_lowercase_identifier) @font-lock-function-name-face)
    (function_alias_target (lowercase_identifier) @font-lock-function-name-face)
    (trait_method_declaration (function_identifier) @font-lock-function-name-face)
    (impl_definition (function_identifier) @font-lock-function-name-face)

    (function_definition
     (function_identifier
      (type_name (qualified_type_identifier)) (lowercase_identifier) @font-lock-function-name-face))

    (loop_label) @font-lock-label-face
    ("continue" (label) @font-lock-label-face)
    ("break" (label) @font-lock-label-face)
    (package_argument
     label: (package_statement_identifier) @font-lock-label-face)
    (package_argument
     label: (string_literal) @font-lock-label-face)
    (package_map_entry
     key: (string_literal) @font-lock-label-face)
    (where_clause_field (lowercase_identifier) @font-lock-label-face)

    ["+" "-" "*" "/" "%"
     "<<" ">>" "|" "&" "^"
     "=" "+=" "-=" "*=" "/=" "%="
     "<" ">" ">=" "<=" "==" "!="
     "&&" "||"
     "|>"
     "=>" "->"
     "!" "!!" "?"] @font-lock-operator-face

    [(mutability) "mut"] @font-lock-keyword-face

    ["struct" "enum" "type" "trait" "typealias" "traitalias" "suberror"]
    @font-lock-keyword-face

    ["pub" "priv" "readonly" "all" "open" "extern"]
    @font-lock-keyword-face

    ["guard" "let" "letrec" "and" "const"
     "with" "as" "is" "lexmatch?" "using" "where" "longest" "nobreak"]
    @font-lock-keyword-face

    "derive" @font-lock-keyword-face

    ["package" "import"] @font-lock-keyword-face

    ["fn" "test" "impl" "fnalias"] @font-lock-keyword-face
    "return" @font-lock-keyword-face
    ["while" "loop" "for" "break" "continue" "in"] @font-lock-keyword-face

    ["if" "else" "match"] @font-lock-keyword-face

    "async" @font-lock-keyword-face

    ["try" "raise" "catch"] @font-lock-keyword-face

    ["noraise"] @font-lock-keyword-face

    ((lowercase_identifier) @font-lock-keyword-face
     (:match "^(?:import|using|where|proof_assert|proof_let|defer|lexmatch|recur|nobreak)$" @font-lock-keyword-face))

    ((lowercase_identifier) @font-lock-keyword-face
     (:match "^except$" @font-lock-keyword-face))

    [";" ","] @font-lock-delimiter-face
    ":" @font-lock-delimiter-face
    "::" @font-lock-delimiter-face
    "." @font-lock-delimiter-face
    ".." @font-lock-delimiter-face

    (array_sub_pattern "..") @font-lock-operator-face
    (dot_dot_apply_expression
     (dot_dot_identifier ".." @font-lock-delimiter-face))

    ["..<" "..=" "..<=" "..>" "..>="] @font-lock-operator-face

    ["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face

    (string_interpolation) @font-lock-string-face
    (string_literal) @font-lock-string-face
    (multiline_string_literal) @font-lock-string-face
    (escape_sequence) @font-lock-escape-face

    (interpolator
     "\\{" @font-lock-escape-face
     "}" @font-lock-escape-face)

    (integer_literal) @font-lock-number-face
    (float_literal) @font-lock-number-face
    (boolean_literal) @font-lock-constant-face
    (char_literal) @font-lock-string-face

    (comment) @font-lock-comment-face
    (block_comment) @font-lock-comment-face

    (ERROR) @font-lock-warning-face)
  "Tree-sitter font-lock rules for MoonBit.")

(defconst moonbit-mbtp--ts-font-lock-rules
  '((predicate_definition name: (lowercase_identifier) @font-lock-function-name-face)
    (logic_function_definition name: (lowercase_identifier) @font-lock-function-name-face)
    (logic_function_definition receiver: (uppercase_identifier) @font-lock-type-face)
    (lemma_definition name: (lowercase_identifier) @font-lock-function-name-face)

    (mbtp_parameter name: (lowercase_identifier) @font-lock-parameter-face)
    (mbtp_where_field name: (lowercase_identifier) @font-lock-label-face)
    (mbtp_quantified_term binder: (lowercase_identifier) @font-lock-parameter-face)
    (mbtp_arrow_parameter (lowercase_identifier) @font-lock-parameter-face)
    (mbtp_parameter_decl (lowercase_identifier) @font-lock-parameter-face)

    (mbtp_identifier_expression
     (mbtp_value_path (identifier (lowercase_identifier)) @font-lock-variable-name-face))
    (mbtp_identifier_expression
     (mbtp_value_path (identifier (uppercase_identifier)) @font-lock-constant-face))
    (mbtp_pattern_constructor (uppercase_identifier) @font-lock-constructor-face)
    (mbtp_pattern_constructor (dot_uppercase_identifier) @font-lock-constructor-face)
    (mbtp_type_path (identifier) @font-lock-type-face)

    ["predicate" "lemma" "where" "proof_assert" "fn"] @font-lock-keyword-face
    ["if" "else" "match"] @font-lock-keyword-face
    ["pub"] @font-lock-keyword-face

    ["∀" "∃" "→" "=>" "->" "!"
     "+" "-" "*" "/" "%" "<" "<=" ">" ">="
     "<<" ">>" "==" "!=" "&&" "||" "&" "^" "|"] @font-lock-operator-face

    ["(" ")" "{" "}" "[" "]"] @font-lock-bracket-face
    ["," ":" "::" ";"] @font-lock-delimiter-face

    (string_literal) @font-lock-string-face
    (integer_literal) @font-lock-number-face
    (float_literal) @font-lock-number-face
    (double_literal) @font-lock-number-face
    (boolean_literal) @font-lock-constant-face
    (char_literal) @font-lock-string-face
    (comment) @font-lock-comment-face
    (block_comment) @font-lock-comment-face
    (ERROR) @font-lock-warning-face)
  "Tree-sitter font-lock rules for MoonBit predicate files.")

(defvar moonbit-ts-mode--font-lock-settings nil)

(defconst moonbit-ts-mode--indent-definition-parent-regexp
  (regexp-opt
   '("const_definition"
     "enum_definition"
     "error_type_definition"
     "function_alias_definition"
     "function_definition"
     "impl_declaration"
     "impl_definition"
     "struct_constructor_declaration"
     "struct_definition"
     "test_definition"
     "trait_alias_definition"
     "trait_definition"
     "tuple_struct_definition"
     "type_alias_definition"
     "type_definition"
     "value_definition"))
  "Regexp matching MoonBit definition nodes whose bodies are indented.")

(defconst moonbit-ts-mode--indent-block-parent-regexp
  (regexp-opt
   '("block_expression"
     "else_clause"
     "guard_else_expression"
     "nobreak_clause"
     "noraise_clause"
     "try_catch_clause"
     "try_else_clause"
     "try_expression"
     "where_clause"))
  "Regexp matching MoonBit block-like nodes whose contents are indented.")

(defconst moonbit-ts-mode--indent-branch-parent-regexp
  (regexp-opt
   '("case_clause"
     "guard_expression"
     "guard_let_expression"
     "if_expression"
     "lexmatch_case_clause"
     "lexmatch_expression"
     "match_expression"
     "matrix_case_clause"
     "try_catch_expression"))
  "Regexp matching MoonBit branch-like nodes whose clauses are indented.")

(defconst moonbit-ts-mode--indent-list-parent-regexp
  (regexp-opt
   '("argument"
     "arguments"
     "constructor_parameter"
     "constructor_pattern"
     "constructor_pattern_argument"
     "derive_directive"
     "enum_constructor"
     "enum_constructor_payload"
     "import_declaration"
     "labelled_argument"
     "labelled_parameter"
     "optional_argument"
     "optional_parameter"
     "optional_parameter_with_default"
     "package_argument"
     "parameter"
     "parameters"
     "positional_parameter"
     "trait_method_parameter"
     "type_arguments"
     "type_parameters"
     "using_targets"))
  "Regexp matching MoonBit list-like nodes whose items are indented.")

(defconst moonbit-ts-mode--indent-field-parent-regexp
  (regexp-opt
   '("derive_item"
     "import_item"
     "map_element_expression"
     "map_element_pattern"
     "package_import_for_clause"
     "package_map_entry"
     "struct_field_declaration"
     "struct_field_expression"
     "struct_field_pattern"
     "super_trait_declaration"
     "trait_method_declaration"
     "where_clause_field"))
  "Regexp matching MoonBit field-like nodes whose continuations are indented.")

(defconst moonbit-ts-mode--indent-compound-parent-regexp
  (regexp-opt
   '("array_expression"
     "array_pattern"
     "array_sub_pattern"
     "bytes_literal"
     "list_comprehension_expression"
     "map_expression"
     "map_pattern"
     "package_array_expression"
     "package_map_expression"
     "struct_expression"
     "struct_pattern"
     "tuple_expression"
     "tuple_pattern"
     "tuple_type"))
  "Regexp matching MoonBit compound literal nodes whose items are indented.")

(defconst moonbit-ts-mode--indent-expression-parent-regexp
  (regexp-opt
   '("access_expression"
     "and_expression"
     "anonymous_lambda_expression"
     "anonymous_matrix_lambda_expression"
     "append_expression"
     "apply_expression"
     "array_access_expression"
     "arrow_function_expression"
     "as_expression"
     "assign_expression"
     "attribute_expression"
     "binary_expression"
     "constraint_expression"
     "constructor_expression"
     "defer_expression"
     "dot_apply_expression"
     "dot_dot_apply_expression"
     "for_expression"
     "for_in_expression"
     "is_expression"
     "labeled_expression"
     "let_expression"
     "let_mut_expression"
     "letrec_expression"
     "loop_expression"
     "method_expression"
     "named_lambda_expression"
     "named_matrix_expression"
     "package_apply_statement"
     "package_assignment_statement"
     "package_expression"
     "package_statement"
     "parenthesized_expression"
     "pipe_arrow_function_expression"
     "proof_assert_expression"
     "proof_let_expression"
     "raise_expression"
     "range_expression"
     "regex_match_expression"
     "regex_match_rhs"
     "return_expression"
     "unary_expression"
     "while_expression"))
  "Regexp matching MoonBit expression nodes whose continuations are indented.")

(defconst moonbit-mbtp--indent-definition-parent-regexp
  (regexp-opt
   '("lemma_definition"
     "logic_function_definition"
     "predicate_definition"))
  "Regexp matching MoonBit predicate definition nodes whose bodies are indented.")

(defconst moonbit-mbtp--indent-block-parent-regexp
  (regexp-opt
   '("lemma_block_expression"
     "mbtp_logic_block_expression"
     "mbtp_predicate_body"
     "mbtp_where_clause"))
  "Regexp matching MoonBit predicate block nodes whose contents are indented.")

(defconst moonbit-mbtp--indent-branch-parent-regexp
  (regexp-opt
   '("lemma_if_expression"
     "lemma_match_case"
     "lemma_match_expression"
     "mbtp_match_case"
     "mbtp_match_expression"))
  "Regexp matching MoonBit predicate branch nodes whose clauses are indented.")

(defconst moonbit-mbtp--indent-list-parent-regexp
  (regexp-opt
   '("mbtp_arrow_parameter"
     "mbtp_parameter"
     "mbtp_parameter_decl"
     "mbtp_parameter_list"))
  "Regexp matching MoonBit predicate list nodes whose items are indented.")

(defconst moonbit-mbtp--indent-expression-parent-regexp
  (regexp-opt
   '("attribute_expression"
     "lemma_nonsequence_expression"
     "lemma_proof_assert_expression"
     "mbtp_anonymous_function_expression"
     "mbtp_apply_expression"
     "mbtp_apply_expression_no_match"
     "mbtp_array_access_expression"
     "mbtp_array_access_expression_no_match"
     "mbtp_arrow_function_expression"
     "mbtp_binary_expression"
     "mbtp_binary_expression_no_match"
     "mbtp_declaration"
     "mbtp_dot_apply_expression"
     "mbtp_dot_apply_expression_no_match"
     "mbtp_expression"
     "mbtp_expression_no_match"
     "mbtp_field_expression"
     "mbtp_field_expression_no_match"
     "mbtp_lambda_expression"
     "mbtp_parenthesized_expression"
     "mbtp_parenthesized_expression_no_match"
     "mbtp_quantified_term"
     "mbtp_tuple_expression"
     "mbtp_tuple_expression_no_match"
     "mbtp_tuple_type"
     "mbtp_unary_expression"
     "mbtp_unary_expression_no_match"
     "mbtp_where_field"))
  "Regexp matching predicate expression nodes whose continuations are indented.")

(defun moonbit-ts-mode--indent-after-attributes-p (node parent _bol)
  "Check whether NODE is the declaration header after attributes in PARENT."
  (when (and node parent)
    (let ((first-child (treesit-node-child parent 0)))
      (and first-child
           (equal (treesit-node-type first-child) "attributes")
           (let ((next-sibling (treesit-node-next-sibling first-child)))
             (and next-sibling
                  (treesit-node-eq node next-sibling)))))))

(defun moonbit-ts-mode--language (&optional file-name)
  "Return the tree-sitter language for FILE-NAME or the current buffer."
  (pcase (file-name-extension (or file-name buffer-file-name ""))
    ("mbtp" 'moonbit_mbtp)
    (_ 'moonbit)))

(defun moonbit-ts-mode--font-lock-rules (&optional language)
  "Return tree-sitter font-lock rules for LANGUAGE."
  (pcase (or language (moonbit-ts-mode--language))
    ('moonbit_mbtp
     (if (boundp 'moonbit-mbtp--ts-font-lock-rules)
         moonbit-mbtp--ts-font-lock-rules
       moonbit--ts-font-lock-rules))
    (_ moonbit--ts-font-lock-rules)))

(defun moonbit-ts-mode--font-lock-settings (&optional language)
  "Return font-lock settings for `moonbit-ts-mode'."
  (let* ((language (or language (moonbit-ts-mode--language)))
         (cached-settings (alist-get language moonbit-ts-mode--font-lock-settings)))
    (or cached-settings
        (setf (alist-get language moonbit-ts-mode--font-lock-settings)
              (treesit-font-lock-rules
               :language language
               :feature 'default
               (moonbit-ts-mode--font-lock-rules language))))))

(defconst moonbit-ts-mode--treesit-indent-rules
  `((moonbit
     ((parent-is "source_file") column-0 0)
     ((parent-is "structure") column-0 0)
     (moonbit-ts-mode--indent-after-attributes-p parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((node-is "^\\(?:catch\\|try_catch_clause\\)$") parent-bol 0)
     ((parent-is "multiline_string_literal") parent-bol 0)
     ((parent-is "block_comment") parent-bol 1)
     ((parent-is ,moonbit-ts-mode--indent-definition-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-block-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-branch-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-list-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-field-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-compound-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-expression-parent-regexp)
      parent-bol moonbit-indent-offset))
    (moonbit_mbtp
     ((parent-is "source_file") column-0 0)
     ((parent-is "structure") column-0 0)
     (moonbit-ts-mode--indent-after-attributes-p parent-bol 0)
     ((node-is "}") parent-bol 0)
     ((node-is ")") parent-bol 0)
     ((node-is "]") parent-bol 0)
     ((node-is "else") parent-bol 0)
     ((parent-is "multiline_string_literal") parent-bol 0)
     ((parent-is "block_comment") parent-bol 1)
     ((parent-is ,moonbit-mbtp--indent-definition-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-mbtp--indent-block-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-mbtp--indent-branch-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is ,moonbit-mbtp--indent-list-parent-regexp)
      parent-bol moonbit-indent-offset)
     ((parent-is "^lemma_sequence_expression$") parent-bol 0)
     ((parent-is ,moonbit-mbtp--indent-expression-parent-regexp)
      parent-bol moonbit-indent-offset)))
  "Tree-sitter indentation rules for MoonBit buffers.")

(defun moonbit-ts-mode--indent-rules (&optional language)
  "Return tree-sitter indentation rules for LANGUAGE."
  (let ((language (or language (moonbit-ts-mode--language))))
    (list (assq language moonbit-ts-mode--treesit-indent-rules))))

(defun moonbit--treesit-ready-p (&optional language)
  "Return non-nil if tree-sitter is ready for LANGUAGE."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p (or language (moonbit-ts-mode--language)))))

(defun moonbit--project-root ()
  "Return the current project root, if any."
  (let ((proj (project-current nil)))
    (when proj
      (project-root proj))))

(defun moonbit--compile (command)
  "Run COMMAND with compilation in the project root when available."
  (let ((default-directory (or (moonbit--project-root) default-directory)))
    (compile command)))

(defun moonbit-build ()
  "Build the current MoonBit project."
  (interactive)
  (moonbit--compile moonbit-project-build-command))

(defun moonbit-check ()
  "Type-check the current MoonBit project."
  (interactive)
  (moonbit--compile moonbit-project-check-command))

(defun moonbit-test ()
  "Test the current MoonBit project."
  (interactive)
  (moonbit--compile moonbit-project-test-command))

(defun moonbit--setup-project-commands ()
  "Set project commands for MoonBit buffers."
  (setq-local compile-command moonbit-project-build-command)
  (setq-local project-compile-command moonbit-project-build-command)
  (setq-local project-test-command moonbit-project-test-command))

(defun moonbit--maybe-enable-eglot ()
  "Enable Eglot for MoonBit buffers when available."
  (when (fboundp 'eglot-ensure)
    (eglot-ensure)))

(defvar-local moonbit--semantic-token-overlays nil)
(defvar-local moonbit--semantic-tokens-refresh-timer nil)
(defvar-local moonbit--semantic-tokens-request-id 0)

(define-minor-mode moonbit-semantic-tokens-mode
  "Highlight semantic tokens reported by the MoonBit language server."
  :init-value nil
  :lighter nil
  (if moonbit-semantic-tokens-mode
      (progn
        (add-hook 'after-change-functions
                  #'moonbit--semantic-tokens-after-change nil t)
        (add-hook 'after-save-hook #'moonbit-semantic-tokens-refresh nil t)
        (moonbit--semantic-tokens-schedule-refresh 0))
    (remove-hook 'after-change-functions
                 #'moonbit--semantic-tokens-after-change t)
    (remove-hook 'after-save-hook #'moonbit-semantic-tokens-refresh t)
    (moonbit--semantic-tokens-cancel-refresh)
    (moonbit--semantic-tokens-clear)))

(defun moonbit--semantic-tokens-clear ()
  "Clear MoonBit semantic token overlays in the current buffer."
  (mapc #'delete-overlay moonbit--semantic-token-overlays)
  (setq moonbit--semantic-token-overlays nil))

(defun moonbit--semantic-tokens-cancel-refresh ()
  "Cancel any pending MoonBit semantic token refresh."
  (when (timerp moonbit--semantic-tokens-refresh-timer)
    (cancel-timer moonbit--semantic-tokens-refresh-timer))
  (setq moonbit--semantic-tokens-refresh-timer nil))

(defun moonbit--semantic-tokens-schedule-refresh (&optional delay)
  "Refresh MoonBit semantic tokens after DELAY seconds of idleness."
  (moonbit--semantic-tokens-cancel-refresh)
  (setq moonbit--semantic-tokens-refresh-timer
        (run-with-idle-timer
         (or delay moonbit-semantic-tokens-refresh-delay) nil
         (lambda (buffer)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq moonbit--semantic-tokens-refresh-timer nil)
               (moonbit-semantic-tokens-refresh))))
         (current-buffer))))

(defun moonbit--semantic-tokens-after-change (&rest _)
  "Schedule a MoonBit semantic token refresh after a buffer change."
  (moonbit--semantic-tokens-schedule-refresh))

(defun moonbit--semantic-tokens-seq-elt (seq index)
  "Return element INDEX from JSON array SEQ."
  (if (vectorp seq)
      (aref seq index)
    (nth index seq)))

(defun moonbit--semantic-tokens-seq-length (seq)
  "Return the length of JSON array SEQ."
  (if (vectorp seq)
      (length seq)
    (length seq)))

(defun moonbit--semantic-tokens-token-face (token-type token-modifiers)
  "Return the face list for TOKEN-TYPE and TOKEN-MODIFIERS."
  (let ((faces (pcase token-type
                 ("function_call" '(font-lock-function-call-face))
                 ("function_decl" '(font-lock-function-name-face))
                 (_ nil))))
    (when (member "async" token-modifiers)
      (push 'moonbit-semantic-token-async-face faces))
    (when (member "error" token-modifiers)
      (push 'moonbit-semantic-token-error-face faces))
    (nreverse faces)))

(defun moonbit--semantic-tokens-position-to-point (line character)
  "Return point for zero-based LINE and LSP CHARACTER."
  (eglot--lsp-position-to-point (list :line line :character character)))

(defun moonbit--semantic-tokens-end-point (line character length)
  "Return point at LINE, CHARACTER plus semantic token LENGTH."
  (eglot--lsp-position-to-point
   (list :line line :character (+ character length))))

(defun moonbit--semantic-tokens-decode-modifiers (legend mask)
  "Decode semantic token modifier MASK using LEGEND."
  (let ((modifiers (plist-get legend :tokenModifiers))
        result)
    (dotimes (index (moonbit--semantic-tokens-seq-length modifiers))
      (when (not (zerop (logand mask (ash 1 index))))
        (push (moonbit--semantic-tokens-seq-elt modifiers index) result)))
    (nreverse result)))

(defun moonbit--semantic-tokens-apply (result)
  "Apply semantic token RESULT from the MoonBit language server to the current buffer."
  (let* ((provider (eglot-server-capable :semanticTokensProvider))
         (legend (plist-get provider :legend))
         (types (plist-get legend :tokenTypes))
         (data (plist-get result :data))
         (line 0)
         (character 0)
         overlays)
    (when (and legend data)
      (save-excursion
        (save-restriction
          (widen)
          (cl-loop
           for index from 0 below (moonbit--semantic-tokens-seq-length data) by 5
           for delta-line = (moonbit--semantic-tokens-seq-elt data index)
           for delta-start = (moonbit--semantic-tokens-seq-elt data (+ index 1))
           for length = (moonbit--semantic-tokens-seq-elt data (+ index 2))
           for token-type-index = (moonbit--semantic-tokens-seq-elt data (+ index 3))
           for token-modifier-mask = (moonbit--semantic-tokens-seq-elt data (+ index 4))
           do
           (setq line (+ line delta-line))
           (setq character (if (zerop delta-line)
                               (+ character delta-start)
                             delta-start))
           (when (> length 0)
             (let* ((token-type (moonbit--semantic-tokens-seq-elt
                                 types token-type-index))
                    (token-modifiers
                     (moonbit--semantic-tokens-decode-modifiers
                      legend token-modifier-mask))
                    (face (moonbit--semantic-tokens-token-face
                           token-type token-modifiers)))
               (when face
                 (let* ((start (moonbit--semantic-tokens-position-to-point
                                line character))
                        (end (moonbit--semantic-tokens-end-point
                              line character length)))
                   (when (< start end)
                     (let ((overlay (make-overlay start end nil t nil)))
                       (overlay-put overlay 'face face)
                       (overlay-put overlay 'priority 20)
                       (push overlay overlays))))))))))
      (moonbit--semantic-tokens-clear)
      (setq moonbit--semantic-token-overlays (nreverse overlays)))))

(defun moonbit-semantic-tokens-refresh ()
  "Refresh MoonBit semantic token highlighting."
  (interactive)
  (when (and moonbit-semantic-tokens-mode
             (featurep 'eglot)
             (fboundp 'eglot-current-server)
             (eglot-current-server)
             (eglot-server-capable :semanticTokensProvider))
    (let ((server (eglot-current-server))
          (buffer (current-buffer))
          (tick (buffer-chars-modified-tick))
          (request-id (cl-incf moonbit--semantic-tokens-request-id)))
      (eglot--signal-textDocument/didChange)
      (jsonrpc-async-request
       server :textDocument/semanticTokens/full
       `(:textDocument ,(eglot--TextDocumentIdentifier))
       :deferred :moonbit-semantic-tokens
       :success-fn
       (lambda (result)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and moonbit-semantic-tokens-mode
                        (= request-id moonbit--semantic-tokens-request-id))
               (if (= tick (buffer-chars-modified-tick))
                   (moonbit--semantic-tokens-apply result)
                 (moonbit--semantic-tokens-schedule-refresh 0))))))))))

(defun moonbit--maybe-enable-semantic-tokens ()
  "Enable semantic tokens in MoonBit buffers managed by Eglot."
  (if (and moonbit-enable-semantic-tokens
           (derived-mode-p 'moonbit-ts-mode)
           (fboundp 'eglot-managed-p)
           (eglot-managed-p)
           (eglot-server-capable :semanticTokensProvider))
      (moonbit-semantic-tokens-mode 1)
    (when moonbit-semantic-tokens-mode
      (moonbit-semantic-tokens-mode -1))))

(defun moonbit--maybe-enable-compilation-errors ()
  "Enable MoonBit compilation error parsing if configured."
  (when moonbit-enable-compile-errors
    (add-to-list 'compilation-error-regexp-alist-alist
                 `(moonbit ,moonbit--compile-error-regexp 1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'moonbit)))

(defun moonbit--setup-common ()
  "Shared setup for MoonBit modes."
  (setq-local moonbit-ts-mode--compiled-syntax-propertize-query
              (treesit-query-compile
               (moonbit-ts-mode--language)
               moonbit-ts-mode--syntax-propertize-query))
  (setq-local syntax-propertize-function
              #'moonbit-ts-mode--syntax-propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "\\(?:///|\\|//+\\)\\s-*")
  (setq-local comment-use-syntax t)
  (setq-local comment-insert-comment-function
              #'moonbit-ts-mode--comment-insert)
  (setq-local indent-tabs-mode nil)
  (setq-local indent-line-function #'treesit-simple-indent)
  (moonbit--setup-project-commands)
  (moonbit--maybe-enable-compilation-errors)
  (moonbit--maybe-enable-eglot))

;;;###autoload
(define-derived-mode moonbit-ts-mode prog-mode "MoonBit[TS]"
  "Tree-sitter major mode for MoonBit."
  :syntax-table moonbit-ts-mode--syntax-table
  (let ((language (moonbit-ts-mode--language)))
    (unless (moonbit--treesit-ready-p language)
      (user-error "MoonBit tree-sitter grammar `%s` is unavailable" language))
    (treesit-parser-create language)
    (setq-local treesit-font-lock-settings (moonbit-ts-mode--font-lock-settings language))
    (setq-local treesit-font-lock-feature-list '((default)))
    (setq-local treesit-simple-indent-rules (moonbit-ts-mode--indent-rules language))
    (moonbit--setup-common)
    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbt\\'" . moonbit-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbti\\'" . moonbit-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbtp\\'" . moonbit-ts-mode))

(with-eval-after-load 'eglot
  (defclass moonbit-eglot-server (eglot-lsp-server) ()
    "Eglot server class for MoonBit.")

  (cl-defmethod eglot-client-capabilities ((_server moonbit-eglot-server))
    (let* ((capabilities (cl-call-next-method))
           (workspace (plist-get capabilities :workspace))
           (text-document (plist-get capabilities :textDocument)))
      (setf (plist-get workspace :semanticTokens) '(:refreshSupport t))
      (setf (plist-get text-document :semanticTokens)
            '(:dynamicRegistration :json-false
              :requests (:range :json-false :full (:delta :json-false))
              :tokenTypes ["namespace" "type" "class" "enum" "interface"
                           "struct" "typeParameter" "parameter" "variable"
                           "property" "enumMember" "event" "function"
                           "method" "macro" "keyword" "modifier" "comment"
                           "string" "number" "regexp" "operator" "decorator"
                           "function_call" "function_decl"]
              :tokenModifiers ["declaration" "definition" "readonly" "static"
                               "deprecated" "abstract" "async" "modification"
                               "documentation" "defaultLibrary" "error"]
              :formats ["relative"]
              :overlappingTokenSupport :json-false
              :multilineTokenSupport :json-false
              :serverCancelSupport t
              :augmentsSyntaxTokens t))
      capabilities))

  (add-hook 'eglot-managed-mode-hook #'moonbit--maybe-enable-semantic-tokens)
  (add-to-list 'eglot-server-programs
               `((moonbit-ts-mode :language-id "moonbit")
                 . (moonbit-eglot-server . ,moonbit-lsp-server-command))))

(provide 'moonbit-ts-mode)

;;; moonbit-ts-mode.el ends here
