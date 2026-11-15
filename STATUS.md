# STATUS

Last updated: 2026-11-15

## Where things stand

- **On CRAN:** 1.0.5 (submitted 2026-04-21; tag `v1.0.5`, SHA 7a04d4f).
- **Development:** `dev` at 1.0.5.9002. `refactor/sc-step-unify` and
  `origin/dev` point to the same commit. The work since 1.0.5 (see
  `NEWS.md`):
  - unified sc-step C++ kernels;
  - truncation options split into reporting (`report_first_K`,
    `report_last_K`) and generation conditioning (`generate_at_least_K`,
    `generate_at_most_K`), with the doubly-truncated Poisson primitive
    `rbtpois`;
  - `atmostB` renamed to `budget_cap` (`atmostB` is kept as a deprecated
    alias that warns);
  - long-format event output (`output = "long"`);
  - new vignettes (writing intensity functions, long vs dense output).
- **Verified 2026-11-15 on `dev` (f181746), local macOS:**
  `R CMD check --as-cran` gave 0 errors, 0 warnings, 0 notes. testthat:
  1557 pass, 0 fail, 0 skip. Vignettes rebuild.
- **Sanitizers (R-hub, 2026-08-27, commit 681cf1f):** clang-ASan and
  gcc-ASan clean. The valgrind job fails on a small leak in the rstream
  dependency, not in nhppp code. This run predates 16 commits that
  changed about 190 lines in `src/` (the long-output path).
- **Reverse dependencies:** none on CRAN (checked 2026-11-15).

## Release checklist for the next CRAN version

Merge `dev` into `main`. PR \#66 open (2026-11-15); merge once CI is
green. Open a PR `dev` -\> `main`; the PR triggers R-CMD-check on 5
platforms (CI runs on `main` and on PRs to `main` only). `main`’s one
extra commit (R-hub workflow, e637c8d) has been merged into `dev`.

Re-run R-hub sanitizers (`rhub.yaml`, manual dispatch) on the merged
code.

`devtools::check_win_devel()`, `check_win_release()`;
`urlchecker::url_check()`; `spelling::spell_check_package()`.

Reverse-dependency check: none needed (no revdeps).

Release paperwork: choose the version (1.1.0 is reasonable given the new
API), NEWS heading, rewrite `cran-comments.md` (it still describes
1.0.5), then `devtools::submit_cran()`.

## Open items / notes

- The valgrind “failure” will recur on R-hub until rstream fixes the
  leak. Mention it in `cran-comments.md` if CRAN’s valgrind check
  reports it.
