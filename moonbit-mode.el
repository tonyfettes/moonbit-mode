;;; moonbit-mode.el --- MoonBit major mode with tree-sitter -*- lexical-binding: t; -*-

;; Author: MoonBit Contributors
;; URL: https://github.com/tonyfettes/moonbit-mode
;; Keywords: languages, tree-sitter
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;;
;; MoonBit major mode with optional tree-sitter and Eglot integration.
;;

;;; Code:

(require 'compile)
(require 'project)
(require 'treesit)

(defgroup moonbit nil
  "MoonBit language support."
  :group 'languages)

(defcustom moonbit-lsp-server-command '("moonbit-lsp")
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

(defvar project-compile-command nil
  "Project compile command used by some project integrations.")

(defvar project-test-command nil
  "Project test command used by some project integrations.")

(defcustom moonbit-enable-compile-errors t
  "Whether to enable MoonBit compile error parsing in compilation buffers."
  :type 'boolean
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

    (method_expression (lowercase_identifier) @font-lock-function-call-face)
    (dot_apply_expression (dot_identifier) @font-lock-function-call-face)
    (dot_dot_apply_expression (dot_dot_identifier) @font-lock-function-call-face)

    (function_definition (function_identifier (lowercase_identifier) @font-lock-function-name-face))
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
     "with" "as" "is" "lexmatch?" "using" "longest"]
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
     (:match "^(?:import|using|defer|lexmatch|recur)$" @font-lock-keyword-face))

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

    (ERROR) @font-lock-warning-face)
  "Tree-sitter font-lock rules for MoonBit.")

(defvar moonbit-ts-mode--font-lock-settings nil)

(defun moonbit-ts-mode--font-lock-settings ()
  "Return font-lock settings for `moonbit-ts-mode'."
  (or moonbit-ts-mode--font-lock-settings
      (setq moonbit-ts-mode--font-lock-settings
            (treesit-font-lock-rules
             :language 'moonbit
             :feature 'default
             moonbit--ts-font-lock-rules))))

(defun moonbit--treesit-ready-p ()
  "Return non-nil if tree-sitter is ready for MoonBit."
  (and (fboundp 'treesit-available-p)
       (treesit-available-p)
       (treesit-language-available-p 'moonbit)))

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

(defun moonbit--maybe-enable-compilation-errors ()
  "Enable MoonBit compilation error parsing if configured."
  (when moonbit-enable-compile-errors
    (add-to-list 'compilation-error-regexp-alist-alist
                 `(moonbit ,moonbit--compile-error-regexp 1 2 3))
    (add-to-list 'compilation-error-regexp-alist 'moonbit)))

(defun moonbit--setup-common ()
  "Shared setup for MoonBit modes."
  (setq-local comment-start "// ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "//+\\s-*")
  (moonbit--setup-project-commands)
  (moonbit--maybe-enable-compilation-errors)
  (moonbit--maybe-enable-eglot))

;;;###autoload
(define-derived-mode moonbit-mode prog-mode "MoonBit"
  "Major mode for MoonBit."
  (moonbit--setup-common))

;;;###autoload
(define-derived-mode moonbit-ts-mode prog-mode "MoonBit[TS]"
  "Tree-sitter major mode for MoonBit."
  (if (treesit-ready-p 'moonbit t)
      (progn
        (treesit-parser-create 'moonbit)
        (setq-local treesit-font-lock-settings (moonbit-ts-mode--font-lock-settings))
        (setq-local treesit-font-lock-feature-list '((default)))
        (moonbit--setup-common)
        (treesit-major-mode-setup))
    (message "MoonBit tree-sitter grammar unavailable; falling back to moonbit-mode")
    (moonbit-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbt\\'" . moonbit-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbti\\'" . moonbit-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs `(moonbit-mode . ,moonbit-lsp-server-command))
  (add-to-list 'eglot-server-programs `(moonbit-ts-mode . ,moonbit-lsp-server-command)))

(provide 'moonbit-mode)
(provide 'moonbit-ts-mode)

;;; moonbit-mode.el ends here
