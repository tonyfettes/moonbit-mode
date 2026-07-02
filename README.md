# moonbit-ts-mode

MoonBit tree-sitter major mode for Emacs with Eglot support.

## Requirements

- Emacs 30+ for `treesit` support and structural navigation
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

MoonBit buffers use `moonbit-indent-offset` spaces per indentation level. The
default is 2.

```elisp
(setq moonbit-indent-offset 2)
```

## Outline

MoonBit buffers provide `outline-minor-mode` headings for top-level
definitions such as functions, tests, types, traits, implementations, and
predicate files.

## Navigation

MoonBit buffers support tree-sitter based defun navigation for commands such as
`beginning-of-defun`, `end-of-defun`, and `mark-defun`.

Structural navigation commands such as `forward-sexp`, `backward-sexp`,
`mark-sexp`, and `transpose-sexps` use MoonBit tree-sitter nodes when
available.

MoonBit definitions are also indexed by Imenu, including functions, types,
implementations, tests, and predicate definitions.

## LSP (Eglot)

If `eglot` is available, it is started automatically in MoonBit buffers using
`moon lsp`. You can customize the command via:

```elisp
(setq moonbit-lsp-server-command '("moon" "lsp"))
```

When the MoonBit language server reports semantic tokens, the mode also
highlights async and error-raising function calls/declarations. You can disable
this with:

```elisp
(setq moonbit-enable-semantic-tokens nil)
```

## Project commands

The mode sets project defaults:

- Build: `moon build`
- Test: `moon test`

It also provides interactive commands:

- `M-x moonbit-build`
- `M-x moonbit-check`
- `M-x moonbit-test`

## Optional compile error parsing

Enable MoonBit error parsing in compilation buffers:

```elisp
(setq moonbit-enable-compile-errors t)
```

This makes errors in `moon build`, `moon check`, and `moon test` clickable in
`compilation-mode`.
