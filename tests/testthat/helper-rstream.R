# Construct an rstream MRG32k3a stream without attaching rstream (Suggests).
new_mrg32k3a <- function() {
  testthat::skip_if_not_installed("rstream")
  methods::new(methods::getClass("rstream.mrg32k3a", where = asNamespace("rstream")))
}
