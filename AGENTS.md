# AGENTS.md

## Purpose
This repository contains `moonbit-mode`, an Emacs major mode for the MoonBit language with tree-sitter and Eglot integration.

## Key Files
- `moonbit-mode.el`: main implementation. Tree-sitter font-lock rules live in `moonbit--ts-font-lock-rules`.
- `queries/moonbit/*`: upstream tree-sitter queries for reference only (Emacs does not load these directly).
- `README.md`: usage and installation notes.

## Development Notes
- Requires Emacs 29.1+ (tree-sitter).
- Tree-sitter highlighting is defined manually in `moonbit-mode.el` (no runtime query translation).
- If you update upstream queries, port the changes into `moonbit--ts-font-lock-rules`.

## Quick Sanity Checks
- Validate the font-lock rules:
  - `emacs --batch -Q --eval "(progn (add-to-list 'load-path \"/path/to/moonbit-mode\") (require 'moonbit-mode) (treesit-query-validate 'moonbit moonbit--ts-font-lock-rules))"`
- Manual check: open a `.mbt`/`.mbti` file and run `M-x font-lock-update`.

## Commands
- `M-x moonbit-build` (runs `moon build`)
- `M-x moonbit-check` (runs `moon check`)
- `M-x moonbit-test` (runs `moon test`)
