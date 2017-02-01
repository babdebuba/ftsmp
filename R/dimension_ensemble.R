#' @export
#' @import foreach
#' @description forecast using dms
#' @title model ensemble
#' @param yraw data
#' @param alpha forgetting factor
#' @param gg state variance
#' @param kk observation variance
#' @param ll prior parameter
#' @param pp lags
#' @param cores_number number of parallel cores
#' @param hh forecast horizon
#' @param prior_constant_variance expecttaion prior choice
#' @param density_size density_size
#' @param sub sub models for dms
#' @param dimensions variable to forecast
#' @return forecasts and probability of the models
# #' @example
# #' parameter <- matrix(cbind(1:10, 11:20), 2),
# #' rep(1, 10)
# #' dimension_ensemble(parameter)
dimension_ensemble <- function(yraw, alpha, gg, kk, ll,
                              pp,
                              cores_number, hh,
                              prior_constant_variance,
                              density_size, sub,
                              dimensions) {

  models <- dimensions
  dimension <- length(dimensions[[1]])
  dimension_number <- length(dimensions)
  for (i in 1:dimension_number) {
    models[[i]] <- model_ensemble_forgetting(
      yraw = yraw[, dimensions[[i]]], alpha,
      gg, kk, ll,
      pp, dimension,
      cores_number, hh,
      prior_constant_variance,
      density_size, sub)
  }
  tt <- models[[1]]$tt

  # reshape yy_predict_alpha_max and
  # alpha_probability_predict_max
  yy_predict_alpha_max <- array(NA, dim = c(dimension, 1,
    dimension_number))
  alpha_probability_predict_max <- array(NA,
    dim = c(tt, dimension_number))
  yy_predict_alpha_average <- NA * yy_predict_alpha_max
  alpha_probability_predict_average <- NA *
    alpha_probability_predict_max
  yy_predict_density_alpha_max <- array(NA,
    dim = c(dimension, density_size, dimension))
  yy_predict_density_alpha_average <- NA *
    yy_predict_density_alpha_max

  #
  for (i in 1:dimension_number) {
    yy_predict_alpha_max[, , i] <-
      models[[i]]$yy_predict_alpha_max
    alpha_probability_predict_max[, i] <-
      models[[i]]$alpha_probability_predict_max
    yy_predict_alpha_average[, , i] <-
      models[[i]]$yy_predict_alpha_average
    alpha_probability_predict_average[, i] <-
      models[[i]]$alpha_probability_predict_average
    yy_predict_density_alpha_max[, , i] <-
      models[[i]]$yy_predict_density_alpha_max
    yy_predict_density_alpha_average[, , i] <-
      models[[i]]$yy_predict_density_alpha_average
  }

  temp_max <- apply(alpha_probability_predict_max, 1,
    sum)
  temp_average <- apply(alpha_probability_predict_average, 1,
    sum)
  dimension_probability_predict_alpha_max <-
    alpha_probability_predict_max / temp_max
  dimension_probability_predict_max <- apply(
    dimension_probability_predict_alpha_max, 1, function(x)
    sort.int(x, index.return = T, decreasing = T)[[2]][1])
  dimension_probability_predict_alpha_average <-
    alpha_probability_predict_average / temp_average
  dimension_probability_predict_average <- apply(
    dimension_probability_predict_alpha_average, 1, function(x)
      sort.int(x, index.return = T, decreasing = T)[[2]][1])

  yy_predict_dimension_probability_max <-
    yy_predict_alpha_max[, ,
      dimension_probability_predict_max[tt]]
  yy_predict_density_dimension_probability_max <-
    yy_predict_density_alpha_max[, ,
      dimension_probability_predict_max[tt]]

  yy_predict_dimension_probability_average <-
    as.vector(yy_predict_alpha_average[, 1, ] *
    dimension_probability_predict_alpha_average[tt, ])
  yy_predict_density_dimension_probability_average <-
    apply(yy_predict_density_alpha_average, 2, function(x)
      x * alpha_probability_predict_average[tt, ])

  list(
  yy_predict_dimension_probability_average =
      yy_predict_dimension_probability_average,
  yy_predict_dimension_probability_max =
      yy_predict_dimension_probability_max,
  yy_predict_density_dimension_probability_average =
      yy_predict_density_dimension_probability_average,
  yy_predict_density_dimension_probability_max =
      yy_predict_density_dimension_probability_max
  )
}