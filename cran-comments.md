## Submission

This is a minor release (1.0.5 -> 1.1.0). It unifies the C++ kernels for piecewise
constant intensities, adds options that separate reporting truncation from conditioning
on the event count, adds a long-format event output, and adds three vignettes. One
argument (`atmostB`) is renamed to `budget_cap`; the old name is kept as a deprecated
alias. See NEWS.md for details.

## Test environments

* local macOS (aarch64), R release
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder (devel)
* R-hub: clang-ASan, gcc-ASan, valgrind

## R CMD check results

0 errors | 0 warnings | 0 notes

## Sanitizers

clang-ASan and gcc-ASan report no errors. valgrind reports one leak: 8 bytes in 4 blocks
"definitely lost", allocated in `r_set_current_stream()` (`RngWrap.c`) of the rstream
dependency. No valgrind errors involve nhppp code.

## Reverse dependencies

There are no reverse dependencies on CRAN.
