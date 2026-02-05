# moonbit-mode

MoonBit major mode for Emacs with tree-sitter highlighting and Eglot support.

## Requirements

- Emacs 29+ for `treesit` support
- MoonBit toolchain
- `moonbit-lsp` in `PATH` (for LSP)

## Installation

```elisp
(add-to-list 'load-path "/path/to/moonbit-mode")
(require 'moonbit-mode)
```

## Install from GitHub (package-vc-install)

Emacs 29+ includes `package-vc-install` for installing packages directly from
version control.

```elisp
(require 'package)
(package-vc-install "https://github.com/tonyfettes/moonbit-mode")
(require 'moonbit-mode)
```

## Tree-sitter grammar

Install the MoonBit grammar manually with Emacs `treesit`:

```elisp
(add-to-list 'treesit-language-source-alist
             '(moonbit "https://github.com/moonbitlang/tree-sitter-moonbit.git" "main" "src"))
(treesit-install-language-grammar 'moonbit)
```

Tree-sitter highlighting rules are defined directly in `moonbit-mode.el`.

## LSP (Eglot)

If `eglot` is available, it is started automatically in MoonBit buffers using
`moonbit-lsp`. You can customize the command via:

```elisp
(setq moonbit-lsp-server-command '("moonbit-lsp"))
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
