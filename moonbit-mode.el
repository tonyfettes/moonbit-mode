;;; moonbit-mode.el --- A major-mode for editing MoonBit source code -*- lexical-binding: t -*-

;; Version: 0.0.1
;; Author: Tony Fettes
;; Url: https://github.com/tonyfettes/moonbit-mode
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))

;; This file is distributed under the term of MIT license

;;; Commentary:

;; This package implements a major-mode for editing MoonBit source code.

;;; Code:

(eval-when-compile (require 'rx))

;;; Customization

(defgroup moonbit-mode nil
  "Support for MoonBit code."
  :link '(url-link "https://www.moonbitlang.com/")
  :group 'languages)

(require 'treesit)
(require 'c-ts-mode)

(defvar moonbit-ts-font-lock-rules
  '(
    :language moonbit
    :feature comment
    ([(comment) (docstring)] @font-lock-comment-face)

    :language moonbit
    :feature function-name
    ((function_definition (function_identifier) @font-lock-function-name-face))

    :language moonbit
    :feature error
    ((error) @font-lock-error-face)

    :language moonbit
    :feature bracket
    ((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

    :language moonbit
    :feature keyword
    ((["let" "fn" "struct" "enum" "type" "trait"
       ;; visibility
       "pub" "priv" "readonly"
       ;; mutability
       "mut"
       ;; control flow
       "if" "else" "match"
       "return"
       "while" "continue" "break"]) @font-lock-keyword-face)

    :language moonbit
    :feature type
    :override t
    ((type_identifier) @font-lock-type-face)

    :language moonbit
    :feature type
    :override t
    ((qualified_type_identifier) @font-lock-type-face)

    :language moonbit
    :feature type
    ((enum_definition (identifier) @font-lock-type-face))

    :language moonbit
    :feature type
    ((struct_definition (identifier) @font-lock-type-face))

    :language moonbit
    :feature type
    ((type_definition (identifier) @font-lock-type-face))

    :language moonbit
    :feature operator
    ((["+" "-" "*" "/"
       "="
       ">=" "<=" "=="
       "&&" "||"
       "=>" "->"]) @font-lock-operator-face)

    :language moonbit
    :feature delimiter
    (([";", ":", ",", "..", "::"]) @font-lock-delimiter-face)

    :language moonbit
    :feature string
    ([(string_literal) (string_interpolation)] @font-lock-string-face)

    :language moonbit
    :feature number
    ([(float_literal) (integer_literal) (boolean_literal)] @font-lock-number-face)

    :language moonbit
    :feature function-call
    ((apply_expression (simple_expression (qualified_identifier) @font-lock-function-call-face)))

    :language moonbit
    :feature variable-use
    ((qualified_identifier) @font-lock-variable-use-face)

    :language moonbit
    :feature variable-name
    :override t
    ((pattern (simple_pattern (lowercase_identifier) @font-lock-variable-name-face)))

    :language moonbit
    :feature variable-name
    :override t
    ((parameter (lowercase_identifier) @font-lock-variable-name-face))))

;;;###autoload
(define-derived-mode moonbit-ts-mode prog-mode "MoonBit"
  "Major mode for MoonBit code."
  :group 'moonbit-mode
  :syntax-table prog-mode-syntax-table

  ;; Comments.
  ;; (c-ts-common-comment-setup)

  (when (treesit-ready-p 'moonbit)
    (treesit-parser-create 'moonbit)

    (setq-local treesit-font-lock-settings
                (apply #'treesit-font-lock-rules moonbit-ts-font-lock-rules))

    (setq-local treesit-font-lock-feature-list
                '((comment function-name)
                  (keyword string)
                  (assignment attribute builtin constant escape
                              number type)
                  (bracket delimiter error function-call operator property variable-use variable-name)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'moonbit)
    (add-to-list 'auto-mode-alist '("\\.mbt\\'" . moonbit-ts-mode)))

(provide 'moonbit-ts-mode)

;;; moonbit-mode.el ends here
