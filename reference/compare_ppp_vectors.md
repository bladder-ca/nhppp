# Check that two ppp vectors Q-Q agree

Compare that the deciles of two vectors have absolute difference over
average ratios less than `threshold`

## Usage

``` r
compare_ppp_vectors(ppp1, ppp2, threshold = 0.15, showQQ = TRUE)
```

## Arguments

- ppp1:

  (vector, double) the first vector

- ppp2:

  (vector, double) the second vector

- threshold:

  (double) optional: the cutoff for a large absolute threshold

- showQQ:

  (boolean) optional: show the QQ plot if the absolute value of the
  Difference vs Average ratio in any decile is bigger than the
  `threshold`

## Value

None
