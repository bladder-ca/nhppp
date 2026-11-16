# nhppp: Simulating Nonhomogeneous Poisson Point Processes

Simulates events from one dimensional nonhomogeneous Poisson point
processes (NHPPPs) as per Trikalinos and Sereda (2024,
[doi:10.48550/arXiv.2402.00358](https://doi.org/10.48550/arXiv.2402.00358)
and 2024,
[doi:10.1371/journal.pone.0311311](https://doi.org/10.1371/journal.pone.0311311)
). Functions are based on three algorithms that provably sample from a
target NHPPP: the time-transformation of a homogeneous Poisson process
(of intensity one) via the inverse of the integrated intensity function
(Cinlar E, "Theory of stochastic processes" (1975, ISBN:0486497996));
the generation of a Poisson number of order statistics from a fixed
density function; and the thinning of a majorizing NHPPP via an
acceptance-rejection scheme (Lewis PAW, Shedler, GS (1979)
[doi:10.1002/nav.3800260304](https://doi.org/10.1002/nav.3800260304) ).

## Random numbers

All samplers, in R and in C++, draw from R's global random number
generator, so [`set.seed()`](https://rdrr.io/r/base/Random.html) makes
results reproducible. For stream-based reproducibility or common random
numbers, install a stream generator as R's global generator before
sampling, e.g. with the `rstream` package:
`s <- methods::new("rstream.mrg32k3a"); rstream::rstream.RNG(s)`.

## See also

Useful links:

- <https://bladder-ca.github.io/nhppp/>

- <https://github.com/bladder-ca/nhppp>

- Report bugs at <https://github.com/bladder-ca/nhppp/issues>

## Author

**Maintainer**: Thomas Trikalinos <thomas_trikalinos@brown.edu>
([ORCID](https://orcid.org/0000-0002-3990-1848)) \[copyright holder\]

Authors:

- Yuliia Sereda <sereda_yuliia@brown.edu>
  ([ORCID](https://orcid.org/0000-0002-4017-4561))
