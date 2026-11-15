# STATUS

Last updated: 2026-11-15 (1.1.0 submitted to CRAN; awaiting acceptance)

## Where things stand

- **On CRAN:** 1.0.5 (submitted 2026-04-21; tag `v1.0.5`, SHA 7a04d4f).
- **Development:** `dev` at 1.0.5.9002. `refactor/sc-step-unify` and `origin/dev` point
  to the same commit. The work since 1.0.5 (see `NEWS.md`):
  - unified sc-step C++ kernels;
  - truncation options split into reporting (`report_first_K`, `report_last_K`) and
    generation conditioning (`generate_at_least_K`, `generate_at_most_K`), with the
    doubly-truncated Poisson primitive `rbtpois`;
  - `atmostB` renamed to `budget_cap` (`atmostB` is kept as a deprecated alias that
    warns);
  - long-format event output (`output = "long"`);
  - new vignettes (writing intensity functions, long vs dense output).
- **Verified 2026-11-15 on `dev` (f181746), local macOS:** `R CMD check --as-cran` gave
  0 errors, 0 warnings, 0 notes. testthat: 1557 pass, 0 fail, 0 skip. Vignettes rebuild.
- **Sanitizers (R-hub, 2026-08-27, commit 681cf1f):** clang-ASan and gcc-ASan clean.
  The valgrind job fails on a small leak in the rstream dependency, not in nhppp
  code. This run predates 16 commits that changed about 190 lines in `src/` (the
  long-output path).
- **Reverse dependencies:** none on CRAN (checked 2026-11-15).

## Release checklist for 1.1.0 (tracked in GitHub issue #65)

1. [x] Merge `dev` into `main`: PR #66 merged 2026-11-15 after all CI checks passed.
2. [x] R-hub sanitizers on `main`: clang-ASan and gcc-ASan clean; valgrind reports only the rstream leak (8 bytes, `r_set_current_stream`, RngWrap.c). Run 43737308205.
3. [x] `use_github_links()`, `urlchecker::url_check()` (all OK), `build_readme()` (README
   unchanged).
4. [x] `devtools::check(remote = TRUE, manual = TRUE)`: 0 errors, 0 warnings, 2 notes (version suffix; local HTML Tidy too old). Both notes are benign.
5. [x] `devtools::check_win_devel()`: only the version-suffix NOTE (goes away at 1.1.0).
6. [x] `cran-comments.md` rewritten for 1.1.0.
7. [x] Reverse dependencies: none on CRAN.
7b. [x] Scalar samplers (`ppp`, `draw_sc_linear`, `draw_sc_loglinear`,
   `draw_cumulative_intensity`, `draw_intensity`) given the full option set (1d806d7).
   Local `check --as-cran`: 0/0/0, 1667 tests pass. win-devel (resubmitted): fully clean.
8. [x] SUBMITTED 2026-11-15 17:38 UTC (SHA a0c62ed); confirmation email approved. `usethis::use_version("minor")` -> 1.1.0 (done, 786a45e); `devtools::submit_cran()`; approve the
   confirmation email. Wait for the maintainer before doing this step.
9. [ ] After CRAN accepts: `use_github_release()`; `use_dev_version(push = TRUE)`.

## Open items / notes

- The valgrind "failure" will recur on R-hub until rstream fixes the leak. Mention it in
  `cran-comments.md` if CRAN's valgrind check reports it.
