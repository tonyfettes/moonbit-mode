# moonbit-ts-mode

MoonBit tree-sitter major mode for Emacs with Eglot support.

## Requirements

- Emacs 31.1+ for `treesit` support, structural navigation, and native folding
- MoonBit tree-sitter grammar
- MoonBit toolchain
- `moon` in `PATH` (for LSP via `moon lsp`)

## Installation
```elisp
(add-to-list 'load-path "/path/to/moonbit-ts-mode")
(require 'moonbit-ts-mode)
```

## Install from GitHub (package-vc-install)

Emacs 29+ includes `package-vc-install` for installing packages directly from
version control.

```elisp
(require 'package)
(package-vc-install "https://github.com/moonbit-community/moonbit-ts-mode")
(require 'moonbit-ts-mode)
```

## Tree-sitter grammar

Install the MoonBit grammar manually with Emacs `treesit`:

```elisp
(add-to-list 'treesit-language-source-alist
             '(moonbit "https://github.com/moonbitlang/tree-sitter-moonbit.git" "main" "src"))
(add-to-list 'treesit-language-source-alist
             '(moonbit_mbtp "https://github.com/moonbitlang/tree-sitter-moonbit.git"
                            "main" "grammars/mbtp/src"))
(treesit-install-language-grammar 'moonbit)
(treesit-install-language-grammar 'moonbit_mbtp)
```

Tree-sitter highlighting rules are defined directly in `moonbit-ts-mode.el`.

The mode is associated with `.mbt`, `.mbti`, and `.mbtp` files.

## Indentation

MoonBit buffers use `moonbit-ts-indent-offset` spaces per indentation level. The
default is 2.

```elisp
(setq moonbit-ts-indent-offset 2)
```

## Outline

MoonBit buffers provide `outline-minor-mode` headings for top-level
definitions such as functions, tests, types, traits, implementations, and
predicate definitions.

## Navigation

MoonBit buffers support tree-sitter based defun navigation for commands such as
`beginning-of-defun`, `end-of-defun`, and `mark-defun`.

Structural navigation commands such as `forward-sexp`, `backward-sexp`,
`mark-sexp`, and `transpose-sexps` use MoonBit tree-sitter nodes when
available.

MoonBit definitions are also indexed by Imenu, including functions, types,
implementations, tests, and predicate definitions.

## Folding

MoonBit definitions and block expressions support Emacs 31's native
tree-sitter-aware Hideshow integration. Enable it with `M-x hs-minor-mode` and
use commands such as `hs-toggle-hiding`, `hs-hide-all`, and `hs-show-all`.

## LSP (Eglot)

Eglot is started automatically in MoonBit buffers using `moon lsp`. Semantic
token highlighting is handled by Emacs 31's built-in
`eglot-semantic-tokens-mode`.

## Compilation

`compile-command` defaults to `moon build` in MoonBit buffers. Use
`M-x project-compile` to run it from the project root; edit the command to
`moon check` or `moon test` when needed.

## Compile error parsing

MoonBit errors from `moon build`, `moon check`, and `moon test` are clickable in
standard compilation buffers.
