## Submission

This is a patch release (1.1.0 -> 1.1.1), submitted soon after 1.1.0. It removes the
dependency on 'rstream': three R functions that had been deprecated since 0.5.0, and the
internal helpers that existed only to support them, are removed. This also removes the
valgrind leak reported for 1.1.0, which was in rstream's code, not ours. No C++ code
changed. See NEWS.md for details.

## Test environments

* local macOS (aarch64), R release
* GitHub Actions: macOS (release), Windows (release), Ubuntu (devel, release, oldrel-1)
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

* checking CRAN incoming feasibility ... NOTE
  Days since last update: 0

  This update removes a dependency (see above). We apologize for the quick
  resubmission.

## Reverse dependencies

There are no reverse dependencies on CRAN.
