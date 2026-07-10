;;; moonbit-ts-mode-test.el --- Tests for moonbit-ts-mode -*- lexical-binding: t; -*-

;;; Code:

(require 'ert)
(require 'moonbit-ts-mode)

(defmacro moonbit-ts-test--with-buffer (file-name contents &rest body)
  "Create a MoonBit FILE-NAME buffer with CONTENTS and evaluate BODY."
  (declare (indent 2) (debug t))
  `(let ((language (if (string-suffix-p ".mbtp" ,file-name)
                       'moonbit_mbtp
                     'moonbit)))
     (unless (treesit-language-available-p language)
       (ert-skip (format "Tree-sitter grammar %s is unavailable" language)))
     (cl-letf (((symbol-function 'eglot-ensure) #'ignore))
       (with-temp-buffer
         (insert ,contents)
         (setq buffer-file-name ,file-name)
         (moonbit-ts-mode)
         ,@body))))

(defun moonbit-ts-test--face-at (text)
  "Return the face at the first occurrence of TEXT."
  (goto-char (point-min))
  (search-forward text)
  (get-text-property (match-beginning 0) 'face))

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

(ert-deftest moonbit-ts-test-font-lock-faces ()
  (moonbit-ts-test--with-buffer "faces.mbt"
      (concat "#foo\n"
              "using @builtin {type Int}\n"
              "fn f(x : Int) {\n"
              "  let a = Some(x)\n"
              "  let b = 1.0\n"
              "  let c = b'a'\n"
              "  let d = b\"abc\"\n"
              "  let e = re\"x+\"\n"
              "  recur()\n"
              "}\n")
    (font-lock-ensure)
    (should (eq (moonbit-ts-test--face-at "#foo")
                'font-lock-preprocessor-face))
    (should (eq (moonbit-ts-test--face-at "@builtin")
                'font-lock-constant-face))
    (should (eq (moonbit-ts-test--face-at "x :")
                'font-lock-variable-name-face))
    (should (eq (moonbit-ts-test--face-at "Some")
                'font-lock-type-face))
    (should (eq (moonbit-ts-test--face-at "1.0")
                'font-lock-number-face))
    (should (eq (moonbit-ts-test--face-at "b'a'")
                'font-lock-string-face))
    (should (eq (moonbit-ts-test--face-at "b\"abc\"")
                'font-lock-string-face))
    (should (eq (moonbit-ts-test--face-at "re\"x+\"")
                'font-lock-regexp-face))
    (should (eq (moonbit-ts-test--face-at "recur")
                'font-lock-keyword-face))))

(ert-deftest moonbit-ts-test-multiline-strings-are-comment-safe ()
  (dolist
      (case
       '(("literal.mbt"
          "fn f {\n  #| https://moonbitlang.com/path /* literal */\n}\n"
          "fn f {\n  // #| https://moonbitlang.com/path /* literal */\n}\n")
         ("interpolation.mbt"
          "fn f {\n  $| https://moonbitlang.com/path /* literal */\n}\n"
          "fn f {\n  // $| https://moonbitlang.com/path /* literal */\n}\n")
         ("literal.mbtp"
          "lemma f() {\n  #| https://moonbitlang.com/path\n}\n"
          "lemma f() {\n  // #| https://moonbitlang.com/path\n}\n")))
    (pcase-let ((`(,file-name ,before ,after) case))
      (moonbit-ts-test--with-buffer file-name before
        (goto-char (point-min))
        (search-forward "moonbitlang")
        (should-not (nth 4 (syntax-ppss)))
        (when (search-forward "literal" (line-end-position) t)
          (should-not (nth 4 (syntax-ppss))))
        (comment-dwim nil)
        (should (equal (buffer-string) after)))))
  (moonbit-ts-test--with-buffer "interpolation.mbt"
      (concat "fn f {\n"
              "  $| https://moonbitlang.com/\\{foo(/* real */ 1)}"
              " /* literal */\n"
              "}\n")
    (goto-char (point-min))
    (search-forward "moonbitlang")
    (should-not (nth 4 (syntax-ppss)))
    (search-forward "real")
    (should (nth 4 (syntax-ppss)))
    (search-forward "literal")
    (should-not (nth 4 (syntax-ppss)))))

(ert-deftest moonbit-ts-test-multiline-string-syntax-updates-after-edit ()
  (moonbit-ts-test--with-buffer "edit.mbt"
      "fn f {\n  $| // literal\n}\n"
    (goto-char (point-min))
    (search-forward "literal")
    (should-not (nth 4 (syntax-ppss)))
    (beginning-of-line)
    (delete-region (point) (line-end-position))
    (insert "  let x = 1 // real")
    (search-backward "real")
    (forward-char 2)
    (should (nth 4 (syntax-ppss)))))

(ert-deftest moonbit-ts-test-container-indentation ()
  (dolist
      (case
       '(("derive.mbt"
          "type X derive (\nA,\nB,\n)\n"
          "type X derive (\n  A,\n  B,\n)\n")
         ("imports.mbti"
          "import (\n\"moonbitlang/core/array\"\n\"moonbitlang/core/string\"\n)\n"
          "import (\n  \"moonbitlang/core/array\"\n  \"moonbitlang/core/string\"\n)\n")
         ("using.mbt"
          "using @builtin {\ntrait Show,\ntype Int,\n}\n"
          "using @builtin {\n  trait Show,\n  type Int,\n}\n")
         ("patterns.mbt"
          "fn main {\nmatch x {\nM(\na,\nb,\n) => ()\n{\n\"a\" : 1,\n\"b\" : 2,\n} => ()\n}\n}\n"
          "fn main {\n  match x {\n    M(\n      a,\n      b,\n    ) => ()\n    {\n      \"a\" : 1,\n      \"b\" : 2,\n    } => ()\n  }\n}\n")))
    (pcase-let ((`(,file-name ,before ,after) case))
      (moonbit-ts-test--with-buffer file-name before
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) after))))))

(ert-deftest moonbit-ts-test-indentation-regressions ()
  (dolist
      (case
       '(("attributes.mbt"
          "#deprecated\nfn f {\nfoo()\n}\n"
          "#deprecated\nfn f {\n  foo()\n}\n")
         ("try-catch.mbt"
          "fn f {\ng() catch {\n_ => ()\n} noraise {\nx => x\n}\n}\n"
          "fn f {\n  g() catch {\n    _ => ()\n  } noraise {\n    x => x\n  }\n}\n")
         ("lemma-sequence.mbtp"
          "lemma demo() {\nproof_assert true;\nproof_assert true;\nproof_assert true;\n()\n}\n"
          "lemma demo() {\n  proof_assert true;\n  proof_assert true;\n  proof_assert true;\n  ()\n}\n")
         ("spaces.mbt"
          "fn a {\nif true {\nmatch x {\nA => {\nfoo()\n}\n}\n}\n}\n"
          "fn a {\n  if true {\n    match x {\n      A => {\n        foo()\n      }\n    }\n  }\n}\n")))
    (pcase-let ((`(,file-name ,before ,after) case))
      (moonbit-ts-test--with-buffer file-name before
        (should-not indent-tabs-mode)
        (indent-region (point-min) (point-max))
        (should (equal (buffer-string) after))
        (should-not (string-match-p "\t" (buffer-string)))))))

(ert-deftest moonbit-ts-test-defun-names-and-imenu ()
  (dolist
      (case
       '(("fn[T] abort(String) -> T" . "abort")
         ("impl Show for (A, B) with output(self : T) -> Unit {}" . "output")
         ("test \"hello\" {}" . "\"hello\"")
         ("test { inspect(x) }" . nil)
         ("typealias @bytes.View" . "@bytes.View")
         ("typealias String as Alias" . "String as Alias")
         ("traitalias @json.FromJson" . "@json.FromJson")
         ("traitalias Source as Alias" . "Source as Alias")
         ("fnalias @math.cos" . "@math.cos")
         ("fnalias foo as bar" . "foo as bar")))
    (moonbit-ts-test--with-buffer "definition.mbt" (car case)
      (let ((node (treesit-node-child (treesit-buffer-root-node) 0 t)))
        (should-not (treesit-node-check node 'has-error))
        (should (equal (moonbit-ts-mode--defun-name node) (cdr case))))))
  (moonbit-ts-test--with-buffer "imenu.mbt"
      (concat "fn[T] abort(String) -> T\n"
              "struct S {}\n"
              "test \"hello\" {}\n"
              "traitalias @json.FromJson\n"
              "traitalias @json.ToJson\n"
              "fnalias @math.cos\n"
              "fnalias @math.sin\n"
              "typealias @bytes.View\n"
              "typealias @bytes.Buffer\n")
    (let ((index (treesit-simple-imenu)))
      (dolist (name '("abort" "S" "\"hello\""
                      "@json.FromJson" "@json.ToJson"
                      "@math.cos" "@math.sin"
                      "@bytes.View" "@bytes.Buffer"))
        (should (assoc name index)))
      (dolist (truncated-name '("@json" "@math" "@bytes"))
        (should-not (assoc truncated-name index))))))

(ert-deftest moonbit-ts-test-fold-ranges-use-direct-bodies ()
  (unless (require 'treesit-fold nil t)
    (ert-skip "treesit-fold is unavailable"))
  (dolist
      (case
       '(("match_expression"
          "fn f(x : T) {\n  match x {\n    A => {\n      foo()\n    }\n    B => {\n      bar()\n    }\n  }\n}\n"
          ("A =>" "B =>") nil)
         ("struct_definition"
          "#deprecated\nstruct S {\n  x : Int\n}\n"
          ("x : Int") ("struct S"))
         ("try_catch_expression"
          "fn f {\n  try {\n    work()\n  } catch {\n    _ => recover()\n  } noraise {\n    x => clean(x)\n  }\n}\n"
          ("work()" "recover()" "clean(x)") ("fn f"))
         ("try_catch_expression"
          "fn f {\n  try {\n    work()\n  } catch {\n    _ => recover()\n  } noraise {\n    x => clean(x)\n  } else {\n    y => done(y)\n  }\n}\n"
          ("work()" "recover()" "clean(x)" "done(y)") ("fn f"))))
    (pcase-let ((`(,node-type ,contents ,included ,excluded) case))
      (moonbit-ts-test--with-buffer "fold.mbt" contents
        (let* ((node (treesit-search-subtree
                      (treesit-buffer-root-node) node-type))
               (range (moonbit-ts-mode--treesit-fold-range-body node nil))
               (folded (and range
                            (buffer-substring-no-properties
                             (car range) (cdr range)))))
          (should range)
          (dolist (text included)
            (should (string-match-p (regexp-quote text) folded)))
          (dolist (text excluded)
            (should-not (string-match-p (regexp-quote text) folded))))))))

(provide 'moonbit-ts-mode-test)

;;; moonbit-ts-mode-test.el ends here
