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
    ((comment) @font-lock-comment-face)

    :language moonbit
    :feature bracket
    ((["(" ")" "[" "]" "{" "}"]) @font-lock-bracket-face)

    :language moonbit
    :feature keyword
    ((["let" "mut" "struct" "enum" "type" "pub" "priv" "readonly"]) @font-lock-keyword-face)

    :language moonbit
    :feature number
    ([(float_literal) (integer_literal)] @font-lock-number-face)))

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
                '((comment definition)
                  (keyword string)
                  (assignment attribute builtin constant escape-sequence
                              number type)
                  (bracket delimiter error function operator property variable)))

    (treesit-major-mode-setup)))

(if (treesit-ready-p 'moonbit)
    (add-to-list 'auto-mode-alist '("\\.mbt\\'" . moonbit-ts-mode)))

(provide 'moonbit-ts-mode)

;;; moonbit-mode.el ends here
