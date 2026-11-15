# Long event format

Internal helpers for the long ("`output = "long"`") event
representation: `list(id, time, n_draws)` with one entry per event. `id`
is the 1-based point-process index, ascending; `time` is ascending
within `id`. A point process with zero events contributes no entries
(its id is absent); `n_draws` distinguishes "no events" from "not
sampled". No `NA` appears in long output.
