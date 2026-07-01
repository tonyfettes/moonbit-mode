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
(declare-function moonbit-eglot-server--eieio-childp "moonbit-mode")

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
           (derived-mode-p 'moonbit-mode 'moonbit-ts-mode)
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
  (let ((language (moonbit-ts-mode--language)))
    (if (moonbit--treesit-ready-p language)
        (progn
          (treesit-parser-create language)
          (setq-local treesit-font-lock-settings (moonbit-ts-mode--font-lock-settings language))
          (setq-local treesit-font-lock-feature-list '((default)))
          (moonbit--setup-common)
          (treesit-major-mode-setup))
      (message "MoonBit tree-sitter grammar `%s` unavailable; falling back to moonbit-mode" language)
      (moonbit-mode))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbt\\'" . moonbit-ts-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mbti\\'" . moonbit-ts-mode))

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
               `(((moonbit-mode :language-id "moonbit")
                  (moonbit-ts-mode :language-id "moonbit"))
                 . (moonbit-eglot-server . ,moonbit-lsp-server-command))))

(provide 'moonbit-mode)
(provide 'moonbit-ts-mode)

;;; moonbit-mode.el ends here
