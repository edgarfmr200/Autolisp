# Repository Guidelines

This repository currently contains only the Git metadata. The guidance below sets expectations for contributors and should be updated once source files and tooling land.

## Project Structure & Module Organization
- `src/` for AutoLISP source files (`.lsp`), grouped by feature.
- `tests/` for test scripts, fixtures, and sample drawings.
- `examples/` for usage samples or demo scripts.
- `docs/` for design notes and user-facing instructions.

If you add new top-level folders, document them here and in `README.md`.

## Build, Test, and Development Commands
No automated build or test commands are defined yet. For local development:
- Load scripts in AutoCAD/BricsCAD with `APPLOAD`, or use `(load "path\\to\\file.lsp")` from the command line.
- Keep manual verification steps in the PR description until automated tests exist.
- Always include concrete AutoCAD validation steps in changes (e.g., load file, run command, expected result).

## Coding Style & Naming Conventions
- Indentation: 2 spaces, no tabs.
- Naming: lowercase, hyphen-separated symbols (e.g., `layer-utils`), and prefix public functions with a project tag like `auto-`.
- Globals/shared state: use `*earmuff*` style (e.g., `*default-layer*`) and keep them in a single module.
- Comments: use `;;` for line comments; keep them concise and explain intent.
- No renaming of existing `C:` commands.
- Do not rename existing `src/*.lsp` files unless explicitly requested.
- Separate new work into UI (input via `getpoint`/`getreal`/`getint`/`getstring`) and core (calculate + draw/export).
- If a geometry/dimension bug is unclear, add `prompt` logs before changing geometry/cotas.
- Avoid new dependencies and keep compatibility with AutoLISP/Visual LISP.

## Testing Guidelines
No testing framework is detected. If tests are added:
- Name test files `*_test.lsp` or `test-*.lsp`.
- Store fixtures in `tests/fixtures/` and keep them small.
- Document how to run tests in `README.md` and mirror that here.

## Commit & Pull Request Guidelines
Git history is not available from this environment, so no existing convention is known. Use short, imperative commit messages (e.g., `add layer utilities`).
For PRs:
- Include a clear summary, steps to validate, and any relevant screenshots (especially for CAD UI changes).
- Link related issues or requirements if they exist.
- One ticket = one commit. Keep changes minimal per iteration.
- Do not change behavior of unrelated modules.

## Security & Configuration Tips
- Avoid hardcoded absolute paths; prefer relative paths and `findfile` when locating resources.
- Keep user-specific settings in a separate file ignored by Git.
