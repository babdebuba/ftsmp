function_profiler <- function() {
  parameter_loader()
  Rprof(tmp <- tempfile())
  hu <- kalman_filter(yraw, pp, hh,
    prior_constant_variance,
    gg, kk, ll,
    density_size,
    dimension)
  Rprof()
  summaryRprof(tmp)
}