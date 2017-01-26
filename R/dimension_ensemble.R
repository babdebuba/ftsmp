dimension_ensemble <- function() {

  # only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  yraw <- MTS::VARMAsim(nobs = 1000, arlags = 1,
    phi = matrix(.9 * diag(4), nrow = 4),
    sigma = matrix(.1 * diag(4), nrow = 4))$series

  tt <- dim(yraw)[1]

  alpha <- seq(.8, 1, length.out = 4)

  dimensions <- list(1:2, 1:3, 1:4)
  models <- dimensions
  dimension_model <- length(dimensions[[1]])
  dimension_number <- length(dimensions)
  for (i in 1:length(dimensions)) {
    models[[i]] <- model_ensemble_forgetting(
      yraw = yraw[, dimensions[[i]]],
      dimension = dimension_model, alpha = alpha)
  }

  # reshape yy_predict_alpha_max and
  # alpha_probability_predict_max
  yy_predict_alpha_max <- array(NA, dim = c(dimension_model,
    dimension_number))
  alpha_probability_predict_max <- array(NA,
    dim = c(tt, dimension_number))
  yy_predict_alpha_average <- NA * yy_predict_alpha_max
  alpha_probability_predict_average <- NA *
    alpha_probability_predict_max
  #
  for (i in 1:dimension_number) {
    yy_predict_alpha_max[, i] <-
      models[[i]]$yy_predict_alpha_max
    alpha_probability_predict_max[, i] <-
      models[[i]]$alpha_probability_predict_max
    yy_predict_alpha_average[, i] <-
      models[[i]]$yy_predict_alpha_average
    alpha_probability_predict_average[, i] <-
      models[[i]]$alpha_probability_predict_average
  }

  dimension_probability_predict <- NA *
    alpha_probability_predict_max
  temp <- apply(alpha_probability_predict_max, 1,
    sum)
  dimension_probability_predict <-
    alpha_probability_predict_max / temp
  dimension_probability_predict_max <- apply(
    dimension_probability_predict, 1, function(x)
    sort.int(x, index.return = T, decreasing = T)[[2]][1])
  yy_predict_dimension_probability_max <-
    yy_predict_alpha_max[,
      dimension_probability_predict_max[tt]]
  yy_predict_dimension_probability_average <-
    apply(t(yy_predict_alpha_max) *
    dimension_probability_predict[tt, ], 2, sum)
  list(
  yy_predict_dimension_probability_average =
      yy_predict_dimension_probability_average,
  yy_predict_dimension_probability_max =
      yy_predict_dimension_probability_max
  )
}