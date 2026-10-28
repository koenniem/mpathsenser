# Agent instructions

## Project

`mpathsenser` is an R package for importing m-Path Sense JSON files into DuckDB and
working with the resulting sensor data.

The package is being developed toward version 2.0.0. Some API and database-layout
changes may be deliberate breaking changes. Do not restore 1.x behaviour or removed
features just for backward compatibility unless the task asks for it. Read the current
source, tests, and release notes before deciding what is intended.

## Documentation boundaries

Keep this file focused on how agents should work: safety, workflow, coding style, and
scope. It is not a changelog or a task journal.

- `ARCHITECTURE.md` describes the current design, data flow, and durable invariants.
  Read it before changing the importer, database schema, timestamps, or query layer;
  update it when a durable design or invariant changes.
- `NEWS.md` records user-visible changes for release.
- Roxygen comments in `R/` are the source for function documentation; regenerate
  `man/` and `NAMESPACE` rather than editing generated files by hand.
- Keep reusable synthetic benchmarks and diagnostics with their scripts in
  `data-raw/`. Put durable design conclusions in
  `ARCHITECTURE.md`; omit completed, one-off investigation notes and benchmark
  history from this file.

Source code and tests are authoritative. Documentation and prior conversation are
context, not proof that a behaviour still exists.

## Data safety

This repository is operated in sandbox mode. Never access, inspect, copy, benchmark,
ingest, or otherwise use proprietary or user data outside the current project
directory. Do not try to bypass the sandbox or ask for access to restricted data.

Use project test fixtures or synthetic data for tests and benchmarks, and create any
temporary DuckDB databases within the project or a normal temporary directory. Treat
ambiguous data as restricted. Historical real-data results are context only; do not
reproduce them with the underlying data.

## Workflow and testing

- Before editing, inspect `git status --short` and the relevant source, tests, and docs.
  Preserve existing modifications and untracked files; do not assume a branch or
  environment from past work.
- Keep changes focused. Verify current callers and tests instead of reapplying a
  change based only on conversation history.
- Add or update tests for code changes. For importer changes, cover the affected
  sensor shapes and relevant edge cases, including missing or empty data, sense
  versions, duplicate/source ordering, and transaction behaviour where applicable.
- Run focused test files first, then the relevant broader tests. Run test files
  individually when practical: many DuckDB connections in one long R session can
  cause invalid-connection flakes. Report exactly which checks were run.
- Both the R MCP (attached R session) and local `Rscript` are available for R work.
  Either may be used, with no project-level preference; choose based on task needs
  and report which path was used.
- For SQL or performance changes, compare output and correctness, not just elapsed
  time. Use only project fixtures or synthetic data.

## Coding style

Follow the [tidyverse style guide](https://style.tidyverse.org) and format with `air`
(`air.toml`: line width 100, two-space indent). The guidance below states goals and
defaults, not rituals: deviate when the code is clearer for it, and make the reason
visible when it is not obvious.

**Write for the reader.** Readable beats short, and explicit beats clever. Name things
for what they do, not how they are implemented; a name that needs a comment is the
wrong name. Avoid nested tricks unless the simple alternative is clearly worse. Let
pipelines read like a sentence with `|>` pronounced "then": prefer helper names that
complete the sentence (`... |> fill_empty_bins() |> ...`) over names that describe
mechanics (`build_cte()`, `slot_sql()`).

**Structure code in stages.** Design the stages before writing them — validation,
normalisation, the work, the return — and keep each block to one concern. If a stage
cannot be a short linear pipeline, split it. Aim for about five or six transformations
per pipeline; longer work becomes named intermediates or small helpers, split by stage
rather than by line count. Continue one result through consecutive stages
(`out <- out |> ...`) and give genuinely new artifacts their own names. One job per
helper: a small single-purpose function is fine, a branching mega-function that callers
must understand is not. Branch by behaviour: early `return()` for terminal special
cases, a bare `if` for optional stages the flow continues past, never a long `if`
trailing an `else { return(...) }`.

**Comments explain intent.** One line before a non-obvious block: what it accomplishes,
plus why when the what alone would invite "why?". Skip self-explanatory code; never
narrate line by line. A short worked example (e.g. an ASCII table) is appropriate for
genuinely tricky logic. Delete dead code instead of commenting it out.

```r
# Keep the records needed by the next step.
result <- data |>
  filter(...) |>
  mutate(...) |>
  select(...)
```

**Write clearly and naturally, as you would explain something to a colleague.** Apply this to all writing, including code comments, documentation, function descriptions, explanations, and other prose. Use plain, concrete language and established project terminology. Explain what something does and why it matters, without making ordinary operations sound more complicated than they are. Avoid unnecessarily abstract, verbose, formal, repetitive, or AI-sounding language, invented terminology, and jargon that adds no precision. Prefer clarity over comprehensiveness and natural phrasing over sophisticated-sounding prose. Do not explain implementation details that are already obvious from the code, repeat information unnecessarily, or add text merely for completeness. Keep writing concise but provide enough context for the reader to understand the important details. When writing new text, follow these principles from the outset. When revising existing text, preserve what already works and change only what genuinely needs improvement. Use technical terminology when it is established or genuinely more precise, not simply because it sounds technical. Before finalizing, ask: Would a competent developer naturally express this to a colleague, and does each sentence help the reader? If not, simplify or remove it.


**Data and database work.** Prefer tidyverse functions over base R for data
manipulation, iteration, and vectorised conditionals when they make intent clearer; use
base R when it is the simpler tool. Use `dplyr::if_else()` for vectorised conditionals
and the native `|>` pipe for new pipelines. Prefer dplyr/dbplyr operations for database
work; handwritten SQL is a last resort for operations those tools cannot express clearly
or that need DuckDB-specific behaviour. Keep SQL an implementation detail: short,
single-purpose, and behind a semantically named wrapper, never a query builder grown in
place. For fragments, prefer `dbplyr::sql()` and related dbplyr/DBI helpers over string
assembly; `dbplyr::sql()` marks a string as SQL but does not quote or sanitise it, so
quote identifiers and values with DBI helpers or parameters. Use bare names only for
functions imported in `R/mpathsenser-package.R`; qualify the rest
(`dplyr::n_distinct()`), and prefer adding an `@importFrom` for verbs used repeatedly.

**Boundaries and errors.** Validate inputs and use the package's existing `cli`
conventions for user-facing messages and errors; boundaries are part of the contract,
not an afterthought.

## Performance principles

Correctness, bounded memory, and maintainability take priority over small speedups.
Keep an optimization only when it has a repeatable benefit on project-local or
synthetic data and preserves output and important edge cases. Do not optimize based
on appearance alone.

Use `proc.time()` for importer timing, consistent with the existing implementation.
Disable DuckDB profiling/logging before timed runs because those settings persist on
the connection. If changing importer debug output, preserve the `[Xms]` timing format;
downstream log parsing depends on it.

## Scope control

- Leave `monitor_db()` and `monitor_helpers` unchanged unless the task explicitly
  includes them. Known failures in those areas are deferred work.
- Do not revive removed legacy-import functionality or removed APIs unless explicitly
  requested.
- Avoid unrelated refactors and speculative abstractions.
