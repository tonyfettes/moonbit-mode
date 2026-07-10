;;; moonbit-ts-mode-test.el --- Tests for moonbit-ts-mode -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'moonbit-ts-mode)

(ert-deftest moonbit-ts-test-semantic-token-faces-are-independent ()
  (let* ((async-face
          (moonbit-ts--semantic-tokens-token-face
           "function_call" '("async")))
         (async-snapshot (copy-tree async-face))
         (plain-face
          (moonbit-ts--semantic-tokens-token-face "function_call" nil))
         (error-face
          (moonbit-ts--semantic-tokens-token-face
           "function_decl" '("error"))))
    (should (equal async-snapshot
                   '(moonbit-ts-semantic-token-async-face)))
    (should-not plain-face)
    (should (equal error-face
                   '(moonbit-ts-semantic-token-error-face)))
    (should (equal async-face async-snapshot))))

(ert-deftest moonbit-ts-test-semantic-token-capabilities-are-attached ()
  (let* ((base '(:workspace (:applyEdit t)
                :textDocument (:hover t)))
         (capabilities
          (moonbit-ts--add-semantic-token-capabilities (copy-tree base)))
         (workspace (plist-get capabilities :workspace))
         (text-document (plist-get capabilities :textDocument))
         (semantic-tokens (plist-get text-document :semanticTokens)))
    (should (eq (plist-get (plist-get workspace :semanticTokens)
                           :refreshSupport)
                :json-false))
    (should (equal (plist-get semantic-tokens :tokenTypes)
                   ["function_call" "function_decl"]))
    (should (equal (plist-get semantic-tokens :tokenModifiers)
                   ["async" "error"]))
    (should (eq (plist-get semantic-tokens :serverCancelSupport)
                :json-false))))

(ert-deftest moonbit-ts-test-semantic-token-provider-requires-full ()
  (let ((full-provider
         '(:full t
           :legend (:tokenTypes ["function_call"]
                    :tokenModifiers ["async"]))))
    (cl-letf (((symbol-function 'eglot-server-capable)
               (lambda (&rest _) full-provider)))
      (should (eq (moonbit-ts--semantic-tokens-provider) full-provider)))
    (let ((empty-full-provider
           '(:full nil
             :legend (:tokenTypes ["function_call"]
                      :tokenModifiers ["async"]))))
      (cl-letf (((symbol-function 'eglot-server-capable)
                 (lambda (&rest _) empty-full-provider)))
        (should (eq (moonbit-ts--semantic-tokens-provider)
                    empty-full-provider))))
    (cl-letf (((symbol-function 'eglot-server-capable)
               (lambda (&rest _)
                 '(:full :json-false
                   :legend (:tokenTypes [] :tokenModifiers [])))))
      (should-not (moonbit-ts--semantic-tokens-provider)))
    (cl-letf (((symbol-function 'eglot-server-capable)
               (lambda (&rest _)
                 '(:range t
                   :legend (:tokenTypes [] :tokenModifiers [])))))
      (should-not (moonbit-ts--semantic-tokens-provider)))))

(ert-deftest moonbit-ts-test-null-semantic-tokens-clear-overlays ()
  (with-temp-buffer
    (insert "call")
    (let ((overlay (make-overlay (point-min) (point-max))))
      (overlay-put overlay 'moonbit-ts-semantic-token t)
      (setq moonbit-ts--semantic-token-overlays (list overlay))
      (moonbit-ts--semantic-tokens-apply nil nil)
      (should-not moonbit-ts--semantic-token-overlays)
      (should-not (overlay-buffer overlay))
      (should-not (overlays-in (point-min) (point-max))))))

(ert-deftest moonbit-ts-test-semantic-token-apply-is-atomic ()
  (with-temp-buffer
    (insert "call more")
    (let* ((legend '(:tokenTypes ["function_call"]
                     :tokenModifiers ["async" "error"]))
           (old-overlay (make-overlay 1 5)))
      (overlay-put old-overlay 'moonbit-ts-semantic-token t)
      (setq moonbit-ts--semantic-token-overlays (list old-overlay))
      (should-error
       (moonbit-ts--semantic-tokens-apply '(:data [0 0 4 0 0 0]) legend))
      (should (overlay-buffer old-overlay))
      (should (equal moonbit-ts--semantic-token-overlays
                     (list old-overlay)))
      (should (= (length (overlays-in (point-min) (point-max))) 1)))))

(ert-deftest moonbit-ts-test-semantic-token-creation-failure-is-atomic ()
  (dolist (fail-at '(1 2 3))
    (with-temp-buffer
      (insert "call more")
      (let* ((legend '(:tokenTypes ["function_call"]
                       :tokenModifiers ["async" "error"]))
             (old-overlay (make-overlay 6 10))
             (original-overlay-put (symbol-function 'overlay-put))
             (put-count 0))
        (overlay-put old-overlay 'moonbit-ts-semantic-token t)
        (setq moonbit-ts--semantic-token-overlays (list old-overlay))
        (cl-letf (((symbol-function
                    'moonbit-ts--semantic-tokens-position-to-point)
                   (lambda (_line character) (1+ character)))
                  ((symbol-function 'moonbit-ts--semantic-tokens-end-point)
                   (lambda (_line character length)
                     (+ 1 character length)))
                  ((symbol-function 'overlay-put)
                   (lambda (overlay property value)
                     (cl-incf put-count)
                     (when (= put-count fail-at)
                       (error "Injected overlay failure"))
                     (funcall original-overlay-put overlay property value))))
          (should-error
           (moonbit-ts--semantic-tokens-apply
            '(:data [0 0 4 0 1]) legend)))
        (should (overlay-buffer old-overlay))
        (should (equal moonbit-ts--semantic-token-overlays
                       (list old-overlay)))
        (should (= (length (overlays-in (point-min) (point-max))) 1))))))

(ert-deftest moonbit-ts-test-semantic-token-apply-replaces-overlays ()
  (with-temp-buffer
    (insert "call more")
    (let* ((legend '(:tokenTypes ["function_call"]
                     :tokenModifiers ["async" "error"]))
           (old-overlay (make-overlay 6 10)))
      (overlay-put old-overlay 'moonbit-ts-semantic-token t)
      (setq moonbit-ts--semantic-token-overlays (list old-overlay))
      (cl-letf (((symbol-function
                  'moonbit-ts--semantic-tokens-position-to-point)
                 (lambda (_line character) (1+ character)))
                ((symbol-function 'moonbit-ts--semantic-tokens-end-point)
                 (lambda (_line character length)
                   (+ 1 character length))))
        (moonbit-ts--semantic-tokens-apply '(:data [0 0 4 0 1]) legend))
      (should-not (overlay-buffer old-overlay))
      (should (= (length moonbit-ts--semantic-token-overlays) 1))
      (let ((overlay (car moonbit-ts--semantic-token-overlays)))
        (should (overlay-buffer overlay))
        (should (overlay-get overlay 'moonbit-ts-semantic-token))
        (should (equal (overlay-get overlay 'face)
                       '(moonbit-ts-semantic-token-async-face)))))))

(provide 'moonbit-ts-mode-test)

;;; moonbit-ts-mode-test.el ends here
