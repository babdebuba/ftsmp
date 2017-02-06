parameter_loader <- function() {

  # yraw <<- as.matrix(cbind(1:10))#, 11:20, 21:30))

  # y <- stock_watson_transformed[, 1:75]
  # delete_index <- y[7, ] == 65535 | y[160, ] == 65535
  # y <- y[-c(1:6, 161:164), !delete_index]
  # m <- apply(y, 2, mean)
  # s <- apply(y, 2, sd)
  # y <- t(apply(y, 1, function(x) (x - m) / s))
  # yraw <<- as.matrix(y[, 1:5])
  bench <<- stock_watson_forecast_errors_benchmark_ar$cn_rgdp_2
  y <- stock_watson_forecast_errors$cn_rgdp_2
  yraw <<- y#[, c(6,  5,  1, 18, 19)]#[, err_div_sort[1:5]]#[, -(31:34)]

  pp <<- 1#seq(1, 2, 1)
  hh <<- 2
  dd <<- dim(yraw)[2]
  tt <<- dim(yraw)[1] - pp - hh + 1
  gg <<- .01#c(.01, .1)
  kk <<- 1#seq(.9, 1, length.out = 2)
  ll <<- 1#seq(.9, 1, length.out = 2)
  prior_constant_variance <<- 10
  is_length <<- round(tt / 2)
  density_size <<- 1
  dimension <<- dd
  cores_number <<- 4
  # sub <<- 2
  # hu <<- kalman_filter(yraw, pp, hh,
  #   dd, tt, gg, kk, ll,
  #   prior_constant_variance,
  #   is_length, density_size,
  #   dimension)
}