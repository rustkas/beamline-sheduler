# Typography & Style (Docs)

Rules and automatic validation for documentation formatting.

## Scope
- Applies to all markdown documents in `docs/`, `.github/`, and root `README.md`
- Includes gRPC service implementation guides and UI/API docs

## Style Rules
- Headings: use `#`, `##`, `###` consistently; no skipped levels
- Code blocks: specify language (e.g., `ts`, `json`, `bash`, `ps1`)
- Quotes: use backticks for inline code; blockquotes with `>` for notes
- Empty lines: keep one blank line between sections
- Consistency: align tone and terminology across docs

## Automatic Validation
- `markdownlint` checks style rules locally and in CI
- Configured via `.markdownlint.json` in repo root

## Usage
- Run `npx markdownlint-cli2 '**/*.md'` or via `scripts/validate_docs.ps1`
- Fix reported issues before merging PRs

## References
- `markdownlint` project
- Writing rules and Markdown standards
