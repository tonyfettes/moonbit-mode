;;; moonbit-ts-mode.el --- MoonBit tree-sitter major mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 MoonBit Contributors

;; Author: MoonBit Contributors
;; Assisted-by: Codex:GPT-5
;; URL: https://github.com/moonbit-community/moonbit-ts-mode
;; Keywords: languages, tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))

;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     https://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.

;;; Commentary:
;;
;; MoonBit tree-sitter major mode with Eglot integration.
;;

;;; Code:

(require 'compile)
(require 'cl-lib)
(require 'eglot)
(require 'eieio)
(require 'jsonrpc)
(require 'project)
(require 'seq)
(require 'treesit)

(defgroup moonbit-ts nil
  "MoonBit language support."
  :group 'languages)

(defcustom moonbit-ts-lsp-server-command '("moon" "lsp")
  "Command to start the MoonBit language server."
  :type '(repeat string)
  :group 'moonbit-ts)

(defcustom moonbit-ts-project-build-command "moon build"
  "Command used for building MoonBit projects."
  :type 'string
  :group 'moonbit-ts)

(defcustom moonbit-ts-project-check-command "moon check"
  "Command used for type checking MoonBit projects."
  :type 'string
  :group 'moonbit-ts)

(defcustom moonbit-ts-project-test-command "moon test"
  "Command used for testing MoonBit projects."
  :type 'string
  :group 'moonbit-ts)

(defcustom moonbit-ts-indent-offset 2
  "Number of spaces to use for each MoonBit indentation level."
  :type 'natnum
  :safe 'natnump
  :group 'moonbit-ts)

(defvar eglot-server-programs)
(declare-function eglot--lsp-position-to-point "eglot")
(declare-function eglot--signal-textDocument/didChange "eglot")
(declare-function eglot--TextDocumentIdentifier "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-managed-p "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function jsonrpc-async-request "jsonrpc")
(declare-function moonbit-ts-eglot-server--eieio-childp "moonbit-ts-mode")

(defcustom moonbit-ts-enable-compile-errors t
  "Whether to enable MoonBit compile error parsing in compilation buffers."
  :type 'boolean
  :group 'moonbit-ts)

(defcustom moonbit-ts-enable-semantic-tokens t
  "Whether to enable LSP semantic token highlighting when available."
  :type 'boolean
  :group 'moonbit-ts)

(defcustom moonbit-ts-semantic-tokens-refresh-delay 0.2
  "Idle delay before refreshing MoonBit semantic token highlighting."
  :type 'number
  :group 'moonbit-ts)

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

(defface moonbit-ts-semantic-token-async-face
  '((t (:slant italic)))
  "Face used for async MoonBit semantic tokens."
  :group 'moonbit-ts)

(defface moonbit-ts-semantic-token-error-face
  '((t (:underline t)))
  "Face used for error-raising MoonBit semantic tokens."
  :group 'moonbit-ts)

(defconst moonbit-ts--compile-error-regexp
  "\\[\\s-*\\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)\\s-*\\]"
  "Regexp matching MoonBit compiler errors.")

(defconst moonbit-ts--ts-font-lock-rules
  '((interpolator) @default

    (package_identifier) @font-lock-constant-face

    (positional_parameter (lowercase_identifier) @font-lock-variable-name-face)
    (labelled_parameter (label (lowercase_identifier)) @font-lock-variable-name-face)
    (optional_parameter (optional_label (lowercase_identifier)) @font-lock-variable-name-face)
    (optional_parameter_with_default (label (lowercase_identifier)) @font-lock-variable-name-face)
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

    (enum_constructor) @font-lock-type-face
    (constructor_expression (uppercase_identifier) @font-lock-type-face)
    (constructor_expression (dot_uppercase_identifier) @font-lock-type-face)

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
     (:match "^\\(?:Unit\\|Bool\\|Byte\\|Int16\\|UInt16\\|Int\\|UInt\\|Int64\\|UInt64\\|Float\\|Double\\|FixedArray\\|Array\\|Bytes\\|String\\|Error\\|Self\\)$"
             @font-lock-builtin-face))
    ((qualified_type_identifier) @font-lock-builtin-face
     (:match "^\\(?:Eq\\|Compare\\|Hash\\|Show\\|Default\\|ToJson\\|FromJson\\)$"
             @font-lock-builtin-face))

    (struct_field_declaration (lowercase_identifier) @font-lock-property-name-face)
    (struct_expression (labeled_expression (lowercase_identifier) @font-lock-property-name-face))
    (struct_expression (labeled_expression_pun (lowercase_identifier) @font-lock-property-name-face))
    (struct_field_expression (labeled_expression (lowercase_identifier) @font-lock-property-name-face))
    (struct_field_expression (labeled_expression_pun (lowercase_identifier) @font-lock-property-name-face))
    (struct_pattern (struct_field_pattern (labeled_pattern (lowercase_identifier) @font-lock-property-name-face)))
    (struct_pattern (struct_field_pattern (labeled_pattern_pun (lowercase_identifier) @font-lock-property-name-face)))
    (access_expression (accessor (dot_identifier) @font-lock-property-name-face))
    (constructor_pattern_argument (lowercase_identifier) @font-lock-property-name-face "=")
    (apply_expression
     (constructor_expression)
     (arguments (argument (labelled_argument (lowercase_identifier) @font-lock-property-name-face "="))))

    (attribute) @font-lock-preprocessor-face
    ((attribute) @font-lock-preprocessor-face
     (:match "^#coverage\\.skip$" @font-lock-preprocessor-face))
    ((attribute) @font-lock-preprocessor-face
     (:match "^#deprecated\\(?:([^)]*)\\)?$" @font-lock-preprocessor-face))

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
    (function_alias_target (lowercase_identifier) @font-lock-function-name-face)
    (trait_method_declaration (function_identifier) @font-lock-function-name-face)
    (impl_definition (function_identifier) @font-lock-function-name-face)

    (function_definition
     (function_identifier
      (type_name (qualified_type_identifier)) (lowercase_identifier) @font-lock-function-name-face))

    (loop_label) @font-lock-constant-face
    ("continue" (label) @font-lock-constant-face)
    ("break" (label) @font-lock-constant-face)
    (package_argument
     label: (package_statement_identifier) @font-lock-constant-face)
    (package_argument
     label: (string_literal) @font-lock-constant-face)
    (package_map_entry
     key: (string_literal) @font-lock-constant-face)
    (where_clause_field (lowercase_identifier) @font-lock-constant-face)

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

    (import_declaration "import" @font-lock-keyword-face)
    (using_declaration "using" @font-lock-keyword-face)
    (lexmatch_expression "lexmatch" @font-lock-keyword-face)
    (where_clause "where" @font-lock-keyword-face)
    (nobreak_clause "nobreak" @font-lock-keyword-face)
    (defer_expression "defer" @font-lock-keyword-face)
    (proof_assert_expression "proof_assert" @font-lock-keyword-face)
    (proof_let_expression "proof_let" @font-lock-keyword-face)

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
    (bytes_literal) @font-lock-string-face
    (multiline_string_literal) @font-lock-string-face
    (regex_literal) @font-lock-regexp-face
    (escape_sequence) @font-lock-escape-face

    (interpolator
     "\\{" @font-lock-escape-face
     "}" @font-lock-escape-face)

    (integer_literal) @font-lock-number-face
    (float_literal) @font-lock-number-face
    (double_literal) @font-lock-number-face
    (boolean_literal) @font-lock-constant-face
    (byte_literal) @font-lock-string-face
    (byte_escape_literal) @font-lock-string-face
    (char_literal) @font-lock-string-face

    (comment) @font-lock-comment-face
    (block_comment) @font-lock-comment-face

    (ERROR) @font-lock-warning-face)
  "Tree-sitter font-lock rules for MoonBit.")

(defconst moonbit-ts-mode--contextual-keyword-font-lock-rules
  '(((lowercase_identifier) @font-lock-keyword-face
     (:match "^\\(?:except\\|recur\\)$" @font-lock-keyword-face)))
  "High-priority font-lock rules for MoonBit contextual keywords.")

(defconst moonbit-ts-mbtp--ts-font-lock-rules
  '((predicate_definition name: (lowercase_identifier) @font-lock-function-name-face)
    (logic_function_definition name: (lowercase_identifier) @font-lock-function-name-face)
    (logic_function_definition receiver: (uppercase_identifier) @font-lock-type-face)
    (lemma_definition name: (lowercase_identifier) @font-lock-function-name-face)

    (mbtp_parameter name: (lowercase_identifier) @font-lock-variable-name-face)
    (mbtp_where_field name: (lowercase_identifier) @font-lock-constant-face)
    (mbtp_quantified_term binder: (lowercase_identifier) @font-lock-variable-name-face)
    (mbtp_arrow_parameter (lowercase_identifier) @font-lock-variable-name-face)
    (mbtp_parameter_decl (lowercase_identifier) @font-lock-variable-name-face)

    (mbtp_identifier_expression
     (mbtp_value_path (identifier (lowercase_identifier)) @font-lock-variable-name-face))
    (mbtp_identifier_expression
     (mbtp_value_path (identifier (uppercase_identifier)) @font-lock-constant-face))
    (mbtp_pattern_constructor (uppercase_identifier) @font-lock-type-face)
    (mbtp_pattern_constructor (dot_uppercase_identifier) @font-lock-type-face)
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
    (bytes_literal) @font-lock-string-face
    (multiline_string_literal) @font-lock-string-face
    (regex_literal) @font-lock-regexp-face
    (escape_sequence) @font-lock-escape-face
    (integer_literal) @font-lock-number-face
    (float_literal) @font-lock-number-face
    (double_literal) @font-lock-number-face
    (boolean_literal) @font-lock-constant-face
    (byte_literal) @font-lock-string-face
    (byte_escape_literal) @font-lock-string-face
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

(defconst moonbit-ts-mode--sexp-node-regexp
  (regexp-opt
   '("access_expression"
     "and_expression"
     "anonymous_lambda_expression"
     "anonymous_matrix_lambda_expression"
     "append_expression"
     "apply_expression"
     "argument"
     "arguments"
     "array_access_expression"
     "array_expression"
     "array_pattern"
     "array_sub_pattern"
     "arrow_function_expression"
     "as_expression"
     "assign_expression"
     "atomic_expression"
     "attribute_expression"
     "binary_expression"
     "block_expression"
     "boolean_literal"
     "break_expression"
     "byte_escape_literal"
     "byte_literal"
     "bytes_literal"
     "case_clause"
     "char_literal"
     "const_definition"
     "constraint_expression"
     "constructor_expression"
     "constructor_parameter"
     "constructor_pattern"
     "constructor_pattern_argument"
     "continue_expression"
     "defer_expression"
     "derive_item"
     "dot_apply_expression"
     "dot_dot_apply_expression"
     "double_literal"
     "else_clause"
     "enum_constructor"
     "enum_constructor_payload"
     "enum_definition"
     "error_type_definition"
     "float_literal"
     "for_expression"
     "for_in_expression"
     "forwarded_labelled_argument"
     "forwarded_optional_argument"
     "function_alias_definition"
     "function_definition"
     "guard_else_expression"
     "guard_expression"
     "guard_let_else_expression"
     "guard_let_expression"
     "identifier"
     "if_expression"
     "impl_declaration"
     "impl_definition"
     "import_declaration"
     "import_item"
     "integer_literal"
     "is_expression"
     "labeled_expression"
     "labeled_expression_pun"
     "labelled_argument"
     "labelled_parameter"
     "let_expression"
     "let_mut_expression"
     "letrec_expression"
     "lexmatch_case_clause"
     "lexmatch_expression"
     "lexmatch_pattern"
     "lexmatch_test_expression"
     "list_comprehension_expression"
     "literal"
     "loop_expression"
     "lowercase_identifier"
     "map_element_expression"
     "map_element_key"
     "map_element_pattern"
     "map_expression"
     "match_expression"
     "matrix_case_clause"
     "method_expression"
     "named_lambda_expression"
     "named_matrix_expression"
     "nobreak_clause"
     "noraise_clause"
     "optional_argument"
     "optional_parameter"
     "optional_parameter_with_default"
     "package_apply_statement"
     "package_argument"
     "package_array_expression"
     "package_assignment_statement"
     "package_expression"
     "package_import_for_clause"
     "package_map_entry"
     "package_map_expression"
     "package_statement"
     "package_statement_identifier"
     "parameter"
     "parameters"
     "parenthesized_expression"
     "pipe_arrow_function_expression"
     "positional_parameter"
     "proof_assert_expression"
     "proof_let_expression"
     "qualified_identifier"
     "qualified_type_identifier"
     "raise_expression"
     "range_expression"
     "regex_literal"
     "regex_match_binding"
     "regex_match_expression"
     "regex_match_rhs"
     "return_expression"
     "string_literal"
     "struct_constructor_declaration"
     "struct_definition"
     "struct_expression"
     "struct_field_declaration"
     "struct_field_expression"
     "struct_field_pattern"
     "super_trait_declaration"
     "test_definition"
     "trait_alias_definition"
     "trait_definition"
     "trait_method_declaration"
     "trait_method_parameter"
     "try_catch_clause"
     "try_catch_expression"
     "try_else_clause"
     "try_expression"
     "tuple_expression"
     "tuple_pattern"
     "tuple_struct_definition"
     "tuple_type"
     "type_alias_definition"
     "type_arguments"
     "type_definition"
     "type_identifier"
     "type_parameters"
     "unary_expression"
     "unit_expression"
     "uppercase_identifier"
     "using_declaration"
     "value_definition"
     "where_clause"
     "where_clause_field"
     "while_expression"))
  "Regexp matching MoonBit nodes used for tree-sitter sexp navigation.")

(defconst moonbit-ts-mode--text-node-regexp
  (regexp-opt
   '("block_comment"
     "comment"
     "multiline_string_literal"
     "regex_literal"
     "string_literal"))
  "Regexp matching MoonBit textual nodes.")

(defconst moonbit-ts-mbtp--indent-definition-parent-regexp
  (regexp-opt
   '("lemma_definition"
     "logic_function_definition"
     "predicate_definition"))
  "Regexp matching MoonBit predicate definition nodes whose bodies are indented.")

(defconst moonbit-ts-mbtp--indent-block-parent-regexp
  (regexp-opt
   '("lemma_block_expression"
     "mbtp_logic_block_expression"
     "mbtp_predicate_body"
     "mbtp_where_clause"))
  "Regexp matching MoonBit predicate block nodes whose contents are indented.")

(defconst moonbit-ts-mbtp--indent-branch-parent-regexp
  (regexp-opt
   '("lemma_if_expression"
     "lemma_match_case"
     "lemma_match_expression"
     "mbtp_match_case"
     "mbtp_match_expression"))
  "Regexp matching MoonBit predicate branch nodes whose clauses are indented.")

(defconst moonbit-ts-mbtp--indent-list-parent-regexp
  (regexp-opt
   '("mbtp_arrow_parameter"
     "mbtp_parameter"
     "mbtp_parameter_decl"
     "mbtp_parameter_list"))
  "Regexp matching MoonBit predicate list nodes whose items are indented.")

(defconst moonbit-ts-mbtp--indent-expression-parent-regexp
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

(defconst moonbit-ts-mbtp--sexp-node-regexp
  (regexp-opt
   '("attribute_expression"
     "boolean_literal"
     "bytes_literal"
     "char_literal"
     "double_literal"
     "float_literal"
     "identifier"
     "integer_literal"
     "lemma_block_expression"
     "lemma_definition"
     "lemma_if_expression"
     "lemma_match_case"
     "lemma_match_expression"
     "lemma_nonsequence_expression"
     "lemma_proof_assert_expression"
     "lemma_sequence_expression"
     "lemma_unit_expression"
     "logic_function_definition"
     "lowercase_identifier"
     "mbtp_anonymous_function_expression"
     "mbtp_apply_expression"
     "mbtp_apply_expression_no_match"
     "mbtp_array_access_expression"
     "mbtp_array_access_expression_no_match"
     "mbtp_arrow_function_expression"
     "mbtp_arrow_parameter"
     "mbtp_atomic_expression"
     "mbtp_atomic_expression_no_match"
     "mbtp_binary_expression"
     "mbtp_binary_expression_no_match"
     "mbtp_declaration"
     "mbtp_dot_apply_expression"
     "mbtp_dot_apply_expression_no_match"
     "mbtp_expression"
     "mbtp_expression_no_match"
     "mbtp_field_expression"
     "mbtp_field_expression_no_match"
     "mbtp_identifier_expression"
     "mbtp_lambda_expression"
     "mbtp_literal_expression"
     "mbtp_logic_block_expression"
     "mbtp_match_case"
     "mbtp_match_expression"
     "mbtp_method_expression"
     "mbtp_parameter"
     "mbtp_parameter_decl"
     "mbtp_parameter_list"
     "mbtp_parenthesized_expression"
     "mbtp_parenthesized_expression_no_match"
     "mbtp_pattern_constructor"
     "mbtp_predicate_body"
     "mbtp_quantified_term"
     "mbtp_tuple_expression"
     "mbtp_tuple_expression_no_match"
     "mbtp_tuple_type"
     "mbtp_unary_expression"
     "mbtp_unary_expression_no_match"
     "mbtp_where_clause"
     "mbtp_where_field"
     "predicate_definition"
     "string_literal"
     "uppercase_identifier"))
  "Regexp matching predicate nodes used for tree-sitter sexp navigation.")

(defconst moonbit-ts-mbtp--text-node-regexp
  (regexp-opt
   '("block_comment"
     "comment"
     "multiline_string_literal"
     "regex_literal"
     "string_literal"))
  "Regexp matching MoonBit predicate textual nodes.")

(defun moonbit-ts-mode--language (&optional file-name)
  "Return the tree-sitter language for FILE-NAME or the current buffer."
  (pcase (file-name-extension (or file-name buffer-file-name ""))
    ("mbtp" 'moonbit_mbtp)
    (_ 'moonbit)))

(defun moonbit-ts-mode--font-lock-rules (&optional language)
  "Return tree-sitter font-lock rules for LANGUAGE."
  (pcase (or language (moonbit-ts-mode--language))
    ('moonbit_mbtp
     (if (boundp 'moonbit-ts-mbtp--ts-font-lock-rules)
         moonbit-ts-mbtp--ts-font-lock-rules
       moonbit-ts--ts-font-lock-rules))
    (_ moonbit-ts--ts-font-lock-rules)))

(defun moonbit-ts-mode--font-lock-settings (&optional language)
  "Return font-lock settings for `moonbit-ts-mode' using LANGUAGE."
  (let* ((language (or language (moonbit-ts-mode--language)))
         (cached-settings (alist-get language moonbit-ts-mode--font-lock-settings)))
    (or cached-settings
        (setf (alist-get language moonbit-ts-mode--font-lock-settings)
              (treesit-font-lock-rules
               :language language
               :feature 'default
               (moonbit-ts-mode--font-lock-rules language)
               :language language
               :feature 'default
               :override t
               moonbit-ts-mode--contextual-keyword-font-lock-rules)))))

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
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-block-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-branch-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-list-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-field-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-compound-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mode--indent-expression-parent-regexp)
      parent-bol moonbit-ts-indent-offset))
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
     ((parent-is ,moonbit-ts-mbtp--indent-definition-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mbtp--indent-block-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mbtp--indent-branch-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is ,moonbit-ts-mbtp--indent-list-parent-regexp)
      parent-bol moonbit-ts-indent-offset)
     ((parent-is "^lemma_sequence_expression$") parent-bol 0)
     ((parent-is ,moonbit-ts-mbtp--indent-expression-parent-regexp)
      parent-bol moonbit-ts-indent-offset)))
  "Tree-sitter indentation rules for MoonBit buffers.")

(defun moonbit-ts-mode--indent-rules (&optional language)
  "Return tree-sitter indentation rules for LANGUAGE."
  (let ((language (or language (moonbit-ts-mode--language))))
    (list (assq language moonbit-ts-mode--treesit-indent-rules))))

(defconst moonbit-ts-mode--treesit-thing-settings
  `((moonbit
     (sexp ,moonbit-ts-mode--sexp-node-regexp)
     (text ,moonbit-ts-mode--text-node-regexp))
    (moonbit_mbtp
     (sexp ,moonbit-ts-mbtp--sexp-node-regexp)
     (text ,moonbit-ts-mbtp--text-node-regexp)))
  "Tree-sitter thing definitions for MoonBit navigation.")

(defun moonbit-ts-mode--thing-settings (&optional language)
  "Return tree-sitter thing settings for LANGUAGE."
  (let ((language (or language (moonbit-ts-mode--language))))
    (list (assq language moonbit-ts-mode--treesit-thing-settings))))

(defconst moonbit-ts-mode--treesit-fold-rules
  '((block_comment . treesit-fold-range-c-like-comment)
    (comment . treesit-fold-range-c-like-comment)
    (block_expression . treesit-fold-range-seq)
    (enum_definition . moonbit-ts-mode--treesit-fold-range-body)
    (error_type_definition . moonbit-ts-mode--treesit-fold-range-body)
    (for_expression . moonbit-ts-mode--treesit-fold-range-body)
    (for_in_expression . moonbit-ts-mode--treesit-fold-range-body)
    (function_definition . moonbit-ts-mode--treesit-fold-range-body)
    (if_expression . moonbit-ts-mode--treesit-fold-range-body)
    (impl_definition . moonbit-ts-mode--treesit-fold-range-body)
    (lexmatch_expression . moonbit-ts-mode--treesit-fold-range-body)
    (loop_expression . moonbit-ts-mode--treesit-fold-range-body)
    (match_expression . moonbit-ts-mode--treesit-fold-range-body)
    (struct_definition . moonbit-ts-mode--treesit-fold-range-body)
    (test_definition . moonbit-ts-mode--treesit-fold-range-body)
    (trait_definition . moonbit-ts-mode--treesit-fold-range-body)
    (try_catch_expression . moonbit-ts-mode--treesit-fold-range-body)
    (try_expression . moonbit-ts-mode--treesit-fold-range-body)
    (tuple_struct_definition . moonbit-ts-mode--treesit-fold-range-body)
    (type_definition . moonbit-ts-mode--treesit-fold-range-body)
    (while_expression . moonbit-ts-mode--treesit-fold-range-body)
    (lemma_definition . moonbit-ts-mode--treesit-fold-range-body)
    (lemma_block_expression . treesit-fold-range-seq)
    (lemma_if_expression . moonbit-ts-mode--treesit-fold-range-body)
    (lemma_match_expression . moonbit-ts-mode--treesit-fold-range-body)
    (logic_function_definition . moonbit-ts-mode--treesit-fold-range-body)
    (mbtp_logic_block_expression . treesit-fold-range-seq)
    (mbtp_match_expression . moonbit-ts-mode--treesit-fold-range-body)
    (mbtp_predicate_body . treesit-fold-range-seq)
    (predicate_definition . moonbit-ts-mode--treesit-fold-range-body))
  "Optional `treesit-fold' fold rules for MoonBit buffers.")

(declare-function treesit-fold-range-c-like-comment "treesit-fold")
(declare-function treesit-fold-range-markers "treesit-fold")
(declare-function treesit-fold-range-seq "treesit-fold")
(defvar treesit-fold-range-alist)

(defun moonbit-ts-mode--treesit-fold--add-offset (range offset)
  "Add `treesit-fold' OFFSET to RANGE."
  (pcase-let ((`(,beg . ,end) range)
              (`(,beg-offset . ,end-offset) (or offset '(0 . 0))))
    (cons (+ beg beg-offset) (+ end end-offset))))

(defconst moonbit-ts-mode--treesit-fold-body-node-types
  '("block_expression"
    "else_clause"
    "guard_else_expression"
    "lemma_block_expression"
    "mbtp_logic_block_expression"
    "mbtp_predicate_body"
    "nobreak_clause"
    "noraise_clause"
    "try_catch_clause"
    "try_else_clause")
  "MoonBit direct child node types that delimit foldable bodies.")

(defun moonbit-ts-mode--treesit-fold--own-delimited-range (node)
  "Return NODE's directly delimited range without an offset."
  (or (treesit-fold-range-markers node '(0 . 0) "{" "}")
      (treesit-fold-range-markers node '(0 . 0) "(" ")")))

(defun moonbit-ts-mode--treesit-fold--body-child-range (node)
  "Return NODE's delimited range, including nested body clauses."
  (let ((range (moonbit-ts-mode--treesit-fold--own-delimited-range node)))
    (dolist (child (treesit-node-children node t) range)
      (when (member (treesit-node-type child)
                    moonbit-ts-mode--treesit-fold-body-node-types)
        (let ((child-range
               (moonbit-ts-mode--treesit-fold--body-child-range child)))
          (when child-range
            (setq range
                  (if range
                      (cons (min (car range) (car child-range))
                            (max (cdr range) (cdr child-range)))
                    child-range))))))))

(defun moonbit-ts-mode--treesit-fold-range-body (node offset)
  "Return a `treesit-fold' body range for MoonBit NODE.

OFFSET is added to the final range."
  (when node
    (let* ((own-range
            (moonbit-ts-mode--treesit-fold--own-delimited-range node))
           (body-ranges
            (delq nil
                  (mapcar
                   (lambda (child)
                     (when (member
                            (treesit-node-type child)
                            moonbit-ts-mode--treesit-fold-body-node-types)
                       (moonbit-ts-mode--treesit-fold--body-child-range child)))
                   (treesit-node-children node t))))
           (range (or own-range
                      (when body-ranges
                        (cons (caar body-ranges)
                              (cdr (car (last body-ranges))))))))
      (when (and range (< (car range) (cdr range)))
        (moonbit-ts-mode--treesit-fold--add-offset range offset)))))

(defun moonbit-ts-mode--setup-treesit-fold ()
  "Register MoonBit rules in `treesit-fold-range-alist'."
  (setf (alist-get 'moonbit-ts-mode treesit-fold-range-alist)
        moonbit-ts-mode--treesit-fold-rules))

(defconst moonbit-ts-mode--definition-type-regexp
  (regexp-opt
   '("const_definition"
     "enum_definition"
     "error_type_definition"
     "function_alias_definition"
     "function_definition"
     "impl_declaration"
     "impl_definition"
     "lemma_definition"
     "logic_function_definition"
     "predicate_definition"
     "struct_constructor_declaration"
     "struct_definition"
     "test_definition"
     "trait_alias_definition"
     "trait_definition"
     "tuple_struct_definition"
     "type_alias_definition"
     "type_definition"
     "value_definition"))
  "Regexp matching MoonBit tree-sitter definition node types.")

(defconst moonbit-ts-mode--defun-type-regexp
  moonbit-ts-mode--definition-type-regexp
  "Regexp matching MoonBit tree-sitter defun node types.")

(defconst moonbit-ts-mode--outline-type-regexp
  moonbit-ts-mode--definition-type-regexp
  "Regexp matching MoonBit tree-sitter outline heading node types.")

(defconst moonbit-ts-mode--defun-name-node-regexp
  (regexp-opt
   '("function_identifier"
     "identifier"
     "lowercase_identifier"
     "package_statement_identifier"
     "qualified_identifier"
     "qualified_type_identifier"
     "string_literal"
     "type_identifier"
     "uppercase_identifier"))
  "Regexp matching MoonBit defun name node types.")

(defun moonbit-ts-mode--node-text (node)
  "Return NODE text without properties, or nil if NODE is nil."
  (when node
    (treesit-node-text node t)))

(defun moonbit-ts-mode--direct-child-of-type (node type)
  "Return NODE's first named direct child whose type is TYPE."
  (seq-find
   (lambda (child)
     (equal (treesit-node-type child) type))
   (treesit-node-children node t)))

(defun moonbit-ts-mode--defun-name (node)
  "Return the name for MoonBit defun NODE."
  (pcase (treesit-node-type node)
    ((or "function_definition" "impl_definition")
     (moonbit-ts-mode--node-text
      (moonbit-ts-mode--direct-child-of-type node "function_identifier")))
    ("test_definition"
     (moonbit-ts-mode--node-text
      (moonbit-ts-mode--direct-child-of-type node "string_literal")))
    ((or "type_alias_definition"
         "trait_alias_definition"
         "function_alias_definition")
     (let ((target-type
            (pcase (treesit-node-type node)
              ("type_alias_definition" "type_alias_targets")
              ("trait_alias_definition" "trait_alias_targets")
              (_ "function_alias_targets"))))
       (moonbit-ts-mode--node-text
        (moonbit-ts-mode--direct-child-of-type node target-type))))
    (_
     (or (moonbit-ts-mode--node-text
          (treesit-node-child-by-field-name node "name"))
         (moonbit-ts-mode--node-text
          (treesit-search-subtree
           node moonbit-ts-mode--defun-name-node-regexp nil nil 3))))))

(defun moonbit-ts--treesit-ready-p (&optional language)
  "Return non-nil if tree-sitter is ready for LANGUAGE."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p (or language (moonbit-ts-mode--language)))))

(defun moonbit-ts--project-root ()
  "Return the current project root, if any."
  (let ((proj (project-current nil)))
    (when proj
      (project-root proj))))

(define-compilation-mode moonbit-ts-compilation-mode "MoonBit Compilation"
  "Compilation mode for MoonBit commands."
  (when moonbit-ts-enable-compile-errors
    (setq-local compilation-error-regexp-alist-alist
                (cons `(moonbit-ts ,moonbit-ts--compile-error-regexp 1 2 3)
                      compilation-error-regexp-alist-alist))
    (setq-local compilation-error-regexp-alist
                (cons 'moonbit-ts compilation-error-regexp-alist))))

(defun moonbit-ts--compile (command)
  "Run COMMAND with compilation in the project root when available."
  (let ((default-directory (or (moonbit-ts--project-root) default-directory)))
    (compilation-start command #'moonbit-ts-compilation-mode)))

(defun moonbit-ts-build ()
  "Build the current MoonBit project."
  (interactive)
  (moonbit-ts--compile moonbit-ts-project-build-command))

(defun moonbit-ts-check ()
  "Type-check the current MoonBit project."
  (interactive)
  (moonbit-ts--compile moonbit-ts-project-check-command))

(defun moonbit-ts-test ()
  "Test the current MoonBit project."
  (interactive)
  (moonbit-ts--compile moonbit-ts-project-test-command))

(defun moonbit-ts--setup-project-commands ()
  "Set project commands for MoonBit buffers."
  (setq-local compile-command moonbit-ts-project-build-command))

(defun moonbit-ts--maybe-enable-eglot ()
  "Enable Eglot for MoonBit buffers when available."
  (when (fboundp 'eglot-ensure)
    (eglot-ensure)))

(defvar-local moonbit-ts--semantic-token-overlays nil
  "Semantic token overlays owned by the current MoonBit buffer.")
(defvar-local moonbit-ts--semantic-tokens-refresh-timer nil
  "Pending semantic token refresh timer for the current buffer.")
(defvar-local moonbit-ts--semantic-tokens-request-token nil
  "Opaque identity of the latest semantic token request.")

(define-minor-mode moonbit-ts-semantic-tokens-mode
  "Highlight semantic tokens reported by the MoonBit language server."
  :init-value nil
  :lighter nil
  (if moonbit-ts-semantic-tokens-mode
      (progn
        (add-hook 'after-change-functions
                  #'moonbit-ts--semantic-tokens-after-change nil t)
        (add-hook 'after-save-hook
                  #'moonbit-ts--semantic-tokens-after-save 90 t)
        (moonbit-ts--semantic-tokens-schedule-refresh 0))
    (remove-hook 'after-change-functions
                 #'moonbit-ts--semantic-tokens-after-change t)
    (remove-hook 'after-save-hook #'moonbit-ts--semantic-tokens-after-save t)
    (setq moonbit-ts--semantic-tokens-request-token (cons nil nil))
    (moonbit-ts--semantic-tokens-cancel-refresh)
    (moonbit-ts--semantic-tokens-clear)))

(defun moonbit-ts--semantic-tokens-set-overlays (overlays)
  "Replace current MoonBit semantic token overlays with OVERLAYS."
  (mapc #'delete-overlay moonbit-ts--semantic-token-overlays)
  (save-restriction
    (widen)
    (dolist (overlay (overlays-in (point-min) (point-max)))
      (when (and (overlay-get overlay 'moonbit-ts-semantic-token)
                 (not (memq overlay overlays)))
        (delete-overlay overlay))))
  (setq moonbit-ts--semantic-token-overlays overlays))

(defun moonbit-ts--semantic-tokens-clear ()
  "Clear MoonBit semantic token overlays in the current buffer."
  (moonbit-ts--semantic-tokens-set-overlays nil))

(defun moonbit-ts--semantic-tokens-cancel-refresh ()
  "Cancel any pending MoonBit semantic token refresh."
  (when (timerp moonbit-ts--semantic-tokens-refresh-timer)
    (cancel-timer moonbit-ts--semantic-tokens-refresh-timer))
  (setq moonbit-ts--semantic-tokens-refresh-timer nil))

(defun moonbit-ts--semantic-tokens-schedule-refresh (&optional delay)
  "Refresh MoonBit semantic tokens after DELAY seconds of idleness."
  (moonbit-ts--semantic-tokens-cancel-refresh)
  (setq moonbit-ts--semantic-tokens-refresh-timer
        (run-with-idle-timer
         (or delay moonbit-ts-semantic-tokens-refresh-delay) nil
         (lambda (buffer)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (setq moonbit-ts--semantic-tokens-refresh-timer nil)
               (moonbit-ts-semantic-tokens-refresh))))
         (current-buffer))))

(defun moonbit-ts--semantic-tokens-after-change (&rest _)
  "Schedule a MoonBit semantic token refresh after a buffer change."
  (moonbit-ts--semantic-tokens-schedule-refresh))

(defun moonbit-ts--semantic-tokens-after-save ()
  "Schedule a MoonBit semantic token refresh after saving."
  (moonbit-ts--semantic-tokens-schedule-refresh 0))

(defun moonbit-ts--semantic-tokens-seq-elt (seq index)
  "Return element INDEX from JSON array SEQ."
  (if (vectorp seq)
      (aref seq index)
    (nth index seq)))

(defun moonbit-ts--semantic-tokens-seq-length (seq)
  "Return the length of JSON array SEQ."
  (length seq))

(defun moonbit-ts--semantic-tokens-token-face (token-type token-modifiers)
  "Return the face list for TOKEN-TYPE and TOKEN-MODIFIERS."
  (let (faces)
    (when (member "async" token-modifiers)
      (push 'moonbit-ts-semantic-token-async-face faces))
    (when (member "error" token-modifiers)
      (push 'moonbit-ts-semantic-token-error-face faces))
    (and (member token-type '("function_call" "function_decl"))
         (nreverse faces))))

(defun moonbit-ts--semantic-tokens-position-to-point (line character)
  "Return point for zero-based LINE and LSP CHARACTER."
  (eglot--lsp-position-to-point (list :line line :character character)))

(defun moonbit-ts--semantic-tokens-end-point (line character length)
  "Return point at LINE, CHARACTER plus semantic token LENGTH."
  (eglot--lsp-position-to-point
   (list :line line :character (+ character length))))

(defun moonbit-ts--semantic-tokens-decode-modifiers (legend mask)
  "Decode semantic token modifier MASK using LEGEND."
  (let ((modifiers (plist-get legend :tokenModifiers))
        result)
    (dotimes (index (moonbit-ts--semantic-tokens-seq-length modifiers))
      (when (not (zerop (logand mask (ash 1 index))))
        (push (moonbit-ts--semantic-tokens-seq-elt modifiers index) result)))
    (nreverse result)))

(defun moonbit-ts--semantic-tokens-provider ()
  "Return the current server's full semantic token provider, if any."
  (let* ((provider (eglot-server-capable :semanticTokensProvider))
         (full (and (listp provider) (plist-member provider :full))))
    (when (and full
               (not (eq (cadr full) :json-false))
               (plist-get provider :legend))
      provider)))

(defun moonbit-ts--semantic-tokens-apply (result legend)
  "Apply semantic token RESULT decoded with LEGEND to the current buffer."
  (if (null result)
      (moonbit-ts--semantic-tokens-clear)
    (let* ((types (plist-get legend :tokenTypes))
           (data (plist-get result :data))
           (data-length (moonbit-ts--semantic-tokens-seq-length data))
           (type-count (moonbit-ts--semantic-tokens-seq-length types))
           (line 0)
           (character 0)
           specs)
      (unless (and (or (listp data) (vectorp data))
                   (or (listp types) (vectorp types)))
        (error "Invalid MoonBit semantic token response"))
      (unless (zerop (% data-length 5))
        (error "Invalid MoonBit semantic token data length: %d" data-length))
      (save-excursion
        (save-restriction
          (widen)
          (cl-loop
           for index from 0 below data-length by 5
           for delta-line = (moonbit-ts--semantic-tokens-seq-elt data index)
           for delta-start = (moonbit-ts--semantic-tokens-seq-elt data (+ index 1))
           for length = (moonbit-ts--semantic-tokens-seq-elt data (+ index 2))
           for token-type-index = (moonbit-ts--semantic-tokens-seq-elt data (+ index 3))
           for token-modifier-mask = (moonbit-ts--semantic-tokens-seq-elt data (+ index 4))
           do
           (unless (and (natnump delta-line)
                        (natnump delta-start)
                        (natnump length)
                        (natnump token-type-index)
                        (< token-type-index type-count)
                        (natnump token-modifier-mask))
             (error "Invalid MoonBit semantic token at data index %d" index))
           (setq line (+ line delta-line))
           (setq character (if (zerop delta-line)
                               (+ character delta-start)
                             delta-start))
           (when (> length 0)
             (let* ((token-type (moonbit-ts--semantic-tokens-seq-elt
                                 types token-type-index))
                    (token-modifiers
                     (moonbit-ts--semantic-tokens-decode-modifiers
                      legend token-modifier-mask))
                    (face (moonbit-ts--semantic-tokens-token-face
                           token-type token-modifiers)))
               (when face
                 (let ((start (moonbit-ts--semantic-tokens-position-to-point
                               line character))
                       (end (moonbit-ts--semantic-tokens-end-point
                             line character length)))
                   (when (< start end)
                     (push (list start end face) specs)))))))))
      (let (overlays complete)
        (unwind-protect
            (progn
              (dolist (spec (nreverse specs))
                (pcase-let ((`(,start ,end ,face) spec))
                  (let ((overlay (make-overlay start end nil t nil)))
                    (push overlay overlays)
                    (overlay-put overlay 'face face)
                    (overlay-put overlay 'priority 20)
                    (overlay-put overlay 'moonbit-ts-semantic-token t))))
              (setq complete t))
          (unless complete
            (mapc #'delete-overlay overlays)))
        (moonbit-ts--semantic-tokens-set-overlays (nreverse overlays))))))

(defun moonbit-ts--semantic-tokens-apply-safely (result legend)
  "Apply semantic token RESULT with LEGEND, warning on invalid data."
  (condition-case error-data
      (moonbit-ts--semantic-tokens-apply result legend)
    (error
     (display-warning
      'moonbit-ts
      (format "Ignoring invalid semantic tokens: %s"
              (error-message-string error-data))
      :warning))))

(defun moonbit-ts-semantic-tokens-refresh ()
  "Refresh MoonBit semantic token highlighting."
  (interactive)
  (when (and moonbit-ts-semantic-tokens-mode
             (featurep 'eglot)
             (fboundp 'eglot-current-server)
             (eglot-current-server))
    (when-let* ((server (eglot-current-server))
                (provider (moonbit-ts--semantic-tokens-provider))
                (legend (copy-tree (plist-get provider :legend)))
                (buffer (current-buffer))
                (tick (buffer-chars-modified-tick))
                (request-token (cons nil nil)))
      (setq moonbit-ts--semantic-tokens-request-token request-token)
      (eglot--signal-textDocument/didChange)
      (jsonrpc-async-request
       server :textDocument/semanticTokens/full
       `(:textDocument ,(eglot--TextDocumentIdentifier))
       :deferred :moonbit-ts-semantic-tokens
       :success-fn
       (lambda (result)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (when (and moonbit-ts-semantic-tokens-mode
                        (eq request-token
                            moonbit-ts--semantic-tokens-request-token)
                        (eglot-managed-p)
                        (eq server (eglot-current-server)))
               (if (= tick (buffer-chars-modified-tick))
                   (moonbit-ts--semantic-tokens-apply-safely result legend)
                 (moonbit-ts--semantic-tokens-schedule-refresh 0))))))))))

(defun moonbit-ts--maybe-enable-semantic-tokens ()
  "Enable semantic tokens in MoonBit buffers managed by Eglot."
  (if (and moonbit-ts-enable-semantic-tokens
           (derived-mode-p 'moonbit-ts-mode)
           (fboundp 'eglot-managed-p)
           (eglot-managed-p)
           (moonbit-ts--semantic-tokens-provider))
      (moonbit-ts-semantic-tokens-mode 1)
    (when moonbit-ts-semantic-tokens-mode
      (moonbit-ts-semantic-tokens-mode -1))))

(defun moonbit-ts--setup-common ()
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
  (setq-local treesit-outline-predicate moonbit-ts-mode--outline-type-regexp)
  (setq-local treesit-defun-type-regexp moonbit-ts-mode--defun-type-regexp)
  (setq-local treesit-defun-name-function #'moonbit-ts-mode--defun-name)
  (setq-local treesit-simple-imenu-settings
              `((nil ,moonbit-ts-mode--definition-type-regexp
                     nil moonbit-ts-mode--defun-name)))
  (setq-local treesit-thing-settings (moonbit-ts-mode--thing-settings))
  (moonbit-ts--setup-project-commands)
  (add-hook 'eglot-managed-mode-hook
            #'moonbit-ts--maybe-enable-semantic-tokens nil t)
  (moonbit-ts--maybe-enable-eglot))

;;;###autoload
(define-derived-mode moonbit-ts-mode prog-mode "MoonBit[TS]"
  "Tree-sitter major mode for MoonBit."
  :syntax-table moonbit-ts-mode--syntax-table
  (let ((language (moonbit-ts-mode--language)))
    (unless (moonbit-ts--treesit-ready-p language)
      (user-error "MoonBit tree-sitter grammar `%s` is unavailable" language))
    (treesit-parser-create language)
    (setq-local treesit-font-lock-settings (moonbit-ts-mode--font-lock-settings language))
    (setq-local treesit-font-lock-feature-list '((default)))
    (setq-local treesit-simple-indent-rules (moonbit-ts-mode--indent-rules language))
    (moonbit-ts--setup-common)
    (treesit-major-mode-setup)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbt\\'" . moonbit-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbti\\'" . moonbit-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbtp\\'" . moonbit-ts-mode))

(defclass moonbit-ts-eglot-server (eglot-lsp-server) ()
  "Eglot server class for MoonBit.")

(defun moonbit-ts--add-semantic-token-capabilities (capabilities)
  "Add MoonBit semantic token support to CAPABILITIES."
  (let* ((workspace (plist-get capabilities :workspace))
         (text-document (plist-get capabilities :textDocument)))
    (setf (plist-get capabilities :workspace)
          (plist-put workspace :semanticTokens
                     '(:refreshSupport :json-false)))
    (setf (plist-get capabilities :textDocument)
          (plist-put
           text-document :semanticTokens
           '(:dynamicRegistration :json-false
             :requests (:range :json-false :full (:delta :json-false))
             :tokenTypes ["function_call" "function_decl"]
             :tokenModifiers ["async" "error"]
             :formats ["relative"]
             :overlappingTokenSupport :json-false
             :multilineTokenSupport :json-false
             :serverCancelSupport :json-false
             :augmentsSyntaxTokens t)))
    capabilities))

(cl-defmethod eglot-client-capabilities ((_server moonbit-ts-eglot-server))
  "Add MoonBit semantic token support to Eglot client capabilities."
  (moonbit-ts--add-semantic-token-capabilities (cl-call-next-method)))

(defun moonbit-ts-mode--eglot-contact (_interactive _project)
  "Return the Eglot contact for MoonBit."
  `(moonbit-ts-eglot-server . ,moonbit-ts-lsp-server-command))

(add-to-list 'eglot-server-programs
             '((moonbit-ts-mode :language-id "moonbit")
               . moonbit-ts-mode--eglot-contact))

(when (require 'treesit-fold nil t)
  (moonbit-ts-mode--setup-treesit-fold))

(provide 'moonbit-ts-mode)

;;; moonbit-ts-mode.el ends here
