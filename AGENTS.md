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

Follow the [tidyverse style guide](https://style.tidyverse.org) and the additional
preferences below. Prefer tidyverse functions over base R for data manipulation,
iteration, and vectorised conditionals when they make the intent clearer; use base R
when it is the simpler or more appropriate tool. Prefer `dplyr::if_else()` over
`ifelse()` for vectorised conditional logic where applicable. Use the native `|>` pipe
for new pipelines.

Keep code in short, coherent blocks that start with the input and assign a named
result. Aim for about five or six transformations in a pipeline; split longer work
into named intermediate results or small helpers rather than building one long chain.
Add a brief comment before a non-obvious block to explain what it accomplishes. Skip
comments for short, self-explanatory code, and avoid line-by-line narration.

```r
# Keep the records needed by the next step.
result <- data |>
  filter(...) |>
  mutate(...) |>
  select(...)
```

Prefer dplyr/dbplyr operations for database work. Use handwritten SQL only when the
needed operation cannot be expressed clearly with those tools or requires
DuckDB-specific behaviour. Keep SQL as short as practical, and put non-trivial SQL
construction in a small, descriptive internal helper so high-level R functions remain
readable.

For SQL fragments, prefer `dbplyr::sql()` and related dbplyr/DBI helpers over
assembling strings with `sprintf()`. `dbplyr::sql()` marks a string as SQL; it does
not quote or sanitise dynamic values. Quote identifiers and values with DBI helpers
(or use parameters) rather than interpolating them manually. Use the package's
existing `cli` conventions for user-facing messages and errors.

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
