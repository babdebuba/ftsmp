parameter_loader <- function() {

  # yraw <<- as.matrix(cbind(1:10))#, 11:20, 21:30))

  # y <- stock_watson_transformed[, 1:75]
  # delete_index <- y[7, ] == 65535 | y[160, ] == 65535
  # y <- y[-c(1:6, 161:164), !delete_index]
  # m <- apply(y, 2, mean)
  # s <- apply(y, 2, sd)
  # y <- t(apply(y, 1, function(x) (x - m) / s))
  # yraw <<- as.matrix(y[, 1:5])

  y <- stock_watson_forecast_errors$cn_rgdp_2
  yraw <<- y[, 1:2]

  pp <<- 1#seq(1, 2, 1)
  hh <<- 2
  dd <<- dim(yraw)[2]
  tt <<- dim(yraw)[1] - pp - hh + 1
  gg <<- 1#seq
  kk <<- 1
  ll <<- 1
  prior_constant_variance <<- 10
  is_length <<- round(tt / 2)
  density_size <<- 1000
  dimension <<- 2
  cores_number <<- 4
  # sub <<- 2
}