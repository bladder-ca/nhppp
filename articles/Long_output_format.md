# Long event output for large simulations

## What this vignette shows

The vectorized samplers return, by default, a **dense matrix** of event
times: one row per point process, one column per event, with rows that
have fewer events than the widest row padded with `NA` on the right.
This is convenient and fast, but the matrix is allocated at the width of
the *widest* row. When event counts vary a lot across point processes –
a few heavy processes among many light ones – almost all of that memory
holds `NA`.

The `output = "long"` option returns the same draw in **long format**
instead: one entry per *event*, with no padding at all. This vignette
shows the format, verifies that the two outputs describe the same
realization, and measures the memory and speed differences.

## The two formats

A population of 1e+05 processes over $`(0, 4]`$, where most processes
have a low constant rate (about 1 expected event) and 100 “heavy”
processes have rate 10 (about 40 expected events):

``` r

rate <- c(rep(0.25, K - 100), rep(10, 100))
lambda_matrix <- matrix(rep(rate, 5), ncol = 5)
time_breaks <- seq(0, 4, length.out = 6)
```

The dense default:

``` r

Z_dense <- vdraw_sc_step(
  lambda_matrix = lambda_matrix,
  time_breaks = time_breaks
)
dim(Z_dense)
#> [1] 100000     54
mean(is.na(Z_dense)) # fraction of the matrix that is padding
#> [1] 0.9809311
```

The 100 heavy processes set the width of the matrix for everyone, so –
as printed above – nearly all cells are `NA` padding.

The long format, from the same sampler:

``` r

Z_long <- vdraw_sc_step(
  lambda_matrix = lambda_matrix,
  time_breaks = time_breaks,
  output = "long"
)
str(Z_long)
#> List of 3
#>  $ id     : int [1:104005] 2 6 6 7 8 8 8 9 11 11 ...
#>  $ time   : num [1:104005] 2.779 2.404 2.481 1.82 0.356 ...
#>  $ n_draws: int 100000
```

One entry per event: `id` is the (1-based) index of the point process,
ascending, with event times ascending within each `id`; `n_draws`
records how many processes were sampled. There is no `NA` anywhere.

**How “no events” is represented.** A process with zero events
contributes no entries – its id is simply absent. `n_draws` is what
distinguishes “process present but with no events” from “process never
sampled”:

``` r

counts <- tabulate(Z_long$id, nbins = Z_long$n_draws)
sum(counts == 0) # processes with no events: present in n_draws, absent in id
#> [1] 36696
```

For `data.table` users the long format is one line from a keyed event
table, ready to join against a population table by `id`:

``` r

events <- data.table::data.table(id = Z_long$id, time = Z_long$time)
events[c(1, .N)]
#>        id     time
#>     <int>    <num>
#> 1:      2 2.778843
#> 2: 100000 3.654499
```

## The two outputs describe the same realization

The long format is not a different sampler – the algorithms and their
use of the random number stream are identical, and only the recording of
events differs. A same-seed pair of draws contains exactly the same
event times:

``` r

set.seed(1)
Zd <- vdraw_sc_step(
  lambda_matrix = lambda_matrix[1:1000, ], time_breaks = time_breaks
)
set.seed(1)
Zl <- vdraw_sc_step(
  lambda_matrix = lambda_matrix[1:1000, ], time_breaks = time_breaks,
  output = "long"
)
# flatten the dense matrix by row (the long format's grouping) and compare
dense_events <- as.vector(t(Zd))
identical(Zl$time, dense_events[!is.na(dense_events)])
#> [1] TRUE
identical(tabulate(Zl$id, Zl$n_draws), as.integer(rowSums(!is.na(Zd))))
#> [1] TRUE
```

## Memory

The dense matrix costs $`8 \, n \max_i N_i`$ bytes; the long format
costs about $`12 \sum_i N_i`$ bytes (8 for each time, 4 for each id).
The ratio is therefore driven by $`\max_i N_i / \bar N`$ – the
*heterogeneity* of the event counts, not the number of processes. Three
populations of 1e+05 processes with the same expected total number of
events but increasing heterogeneity:

``` r

mem_case <- function(rate_vector, label) {
  lm <- matrix(rep(rate_vector, 5), ncol = 5)
  Zd <- vdraw_sc_step(lambda_matrix = lm, time_breaks = time_breaks)
  Zl <- vdraw_sc_step(lambda_matrix = lm, time_breaks = time_breaks, output = "long")
  data.frame(
    population = label,
    max_events = ncol(Zd),
    dense_MB = round(as.numeric(object.size(Zd)) / 2^20, 1),
    long_MB = round(as.numeric(object.size(Zl)) / 2^20, 1)
  )
}
# equal expected totals (~1.25 e5 events), increasingly concentrated
homogeneous <- rep(0.3125, K)
mild <- c(rep(0.25, K - 5000), rep(1.5, 5000))
extreme <- c(rep(0.25, K - 100), rep(10, 100))
tab <- rbind(
  mem_case(homogeneous, "homogeneous"),
  mem_case(mild, "mildly heterogeneous"),
  mem_case(extreme, "extremely heterogeneous")
)
knitr::kable(tab)
```

| population              | max_events | dense_MB | long_MB |
|:------------------------|-----------:|---------:|--------:|
| homogeneous             |          8 |      6.1 |     1.4 |
| mildly heterogeneous    |         15 |     11.4 |     1.4 |
| extremely heterogeneous |         56 |     42.7 |     1.2 |

With homogeneous counts the two formats cost about the same (the dense
matrix is narrow, and the long format pays 4 extra bytes per event for
the id). As heterogeneity grows, the dense matrix widens with the
heaviest row while the long format stays proportional to the actual
number of events. In the extreme case the dense matrix is dozens of
times larger. The effect scales: at $`10^7`$ processes the extreme
fixture costs gigabytes dense and tens of megabytes long.

## Speed

The long format is not a time-memory trade-off – skipping the
allocation, `NA`-filling, and trimming of the wide matrix also saves
time:

``` r

time_case <- function(rate_vector, label, n_rep = 3) {
  lm <- matrix(rep(rate_vector, 5), ncol = 5)
  td <- system.time(for (i in seq_len(n_rep)) {
    vdraw_sc_step(lambda_matrix = lm, time_breaks = time_breaks)
  })["elapsed"]
  tl <- system.time(for (i in seq_len(n_rep)) {
    vdraw_sc_step(lambda_matrix = lm, time_breaks = time_breaks, output = "long")
  })["elapsed"]
  data.frame(
    population = label,
    dense_ms = round(1000 * td / n_rep),
    long_ms = round(1000 * tl / n_rep)
  )
}
knitr::kable(rbind(
  time_case(homogeneous, "homogeneous"),
  time_case(mild, "mildly heterogeneous"),
  time_case(extreme, "extremely heterogeneous")
))
```

|          | population              | dense_ms | long_ms |
|:---------|:------------------------|---------:|--------:|
| elapsed  | homogeneous             |       23 |      17 |
| elapsed1 | mildly heterogeneous    |       26 |      17 |
| elapsed2 | extremely heterogeneous |       68 |      16 |

## The conditioned samplers work the same way

`output = "long"` composes with the reporting and
generation-conditioning options. For instance, conditioning on at least
two events and reporting in long format:

``` r

Z <- vdraw_sc_step(
  lambda_matrix = lambda_matrix[1:1000, ],
  time_breaks = time_breaks,
  generate_at_least_K = 2,
  output = "long"
)
min(tabulate(Z$id, Z$n_draws)) # every process has at least 2 events
#> [1] 2
```

Under a pure upper bound, zero-event processes have positive probability
and appear – correctly – as absent ids:

``` r

Z <- vdraw_sc_step(
  lambda_matrix = lambda_matrix[1:1000, ],
  time_breaks = time_breaks,
  generate_at_most_K = 1,
  output = "long"
)
table(tabulate(Z$id, Z$n_draws))
#> 
#>   0   1 
#> 480 520
```

[`vdraw_cumulative_intensity()`](https://bladder-ca.github.io/nhppp/reference/vdraw_cumulative_intensity.md)
also accepts `output = "long"`; on its conditioned (order-statistics)
path the long result is built without any dense intermediate, and
`Lambda_inv` is evaluated once on the event vector.

## Which format to use

- **Dense (default)**: first-event workloads (`report_first_K = 1`),
  homogeneous event counts, or any code that expects the matrix layout.
  Nothing changes for existing code.
- **Long**: many processes with ragged event counts – event-history
  simulations, heavy-tailed intensities, or whenever the events feed a
  `data.table`/long-format pipeline anyway. Memory is proportional to
  the events actually drawn, and the draw is somewhat faster.
