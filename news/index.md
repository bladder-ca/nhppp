# Changelog

## nhppp 1.0.5

- CRAN release

## nhppp 1.0.4

- Corrected the language field in DESCRIPTION to `en`

## nhppp 1.0.3

## nhppp 1.0.2

CRAN release: 2025-01-09

## nhppp 1.0.1

- CRAN release

- fixed rare overflow error in `rztpois` where an explicit cast from
  `double` to `int` was used – now we use `safe_double_to_int` to avoid
  overflow; this overflow resulted in a segmentation fault in x86
  hardware and in silent failures in AArch64 hardware. This bug should
  not be relevant for most practical uses of the package.

- added new citation for the paper by Trikalinos and Sereda (2024,
  <doi:10.1371/journal.pone.0311311>)

## nhppp 1.0.0

CRAN release: 2024-10-23

## nhppp 0.2.1

## nhppp 0.2.0

- CRAN release

## nhppp 0.1.4

CRAN release: 2024-05-28

- Function arguments are now more consistent.

- More extensive numerical checks.

- Added C++ code for vectorized functions – for computational speed and
  memory efficiency.

## nhppp 0.1.3

CRAN release: 2024-02-02

- CRAN release

## nhppp 0.1.2

- Renamed functions to start from `draw` (instead of `nhppp_t`),
  `draw_zt` (instead of `ztnhppp_t`), and `vdraw` (instead of ending in
  `_vec()`)

- Initial CRAN submission.
