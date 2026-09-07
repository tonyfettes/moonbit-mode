;;; moonbit-ts-mode-test.el --- Tests for moonbit-ts-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 MoonBit Contributors

;; Author: MoonBit Contributors
;; Assisted-by: Codex:GPT-5

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

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'hideshow)
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

(ert-deftest moonbit-ts-test-eglot-registration ()
  (should
   (equal
    (cdr (assoc '(moonbit-ts-mode :language-id "moonbit")
                eglot-server-programs))
    '("moon" "lsp"))))

(ert-deftest moonbit-ts-test-compilation-error-registration ()
  (should (memq 'moonbit-ts compilation-error-regexp-alist))
  (should
   (equal (cdr (assq 'moonbit-ts compilation-error-regexp-alist-alist))
          (list moonbit-ts--compile-error-regexp 1 2 3))))

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

(ert-deftest moonbit-ts-test-native-hideshow-ranges-use-direct-bodies ()
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
               (_ (goto-char (treesit-node-start node)))
               (range (hs-block-positions t t))
               (folded (and range
                            (buffer-substring-no-properties
                             (car range) (cadr range)))))
          (should range)
          (dolist (text included)
            (should (string-match-p (regexp-quote text) folded)))
          (dolist (text excluded)
            (should-not (string-match-p (regexp-quote text) folded))))))))

(provide 'moonbit-ts-mode-test)

;;; moonbit-ts-mode-test.el ends here
