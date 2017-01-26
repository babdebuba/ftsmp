model_ensemble_forgetting <- function(yraw, dimension_model,
                                      alpha) {
  # only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  # yraw <- MTS::VARMAsim(nobs = 1000, arlags = 1,
  #   phi = matrix(.9 * diag(4), nrow = 4),
  #   sigma = matrix(.1 * diag(4), nrow = 4))$series
  # yraw <- yraw[, 1:2]
  # dimension_model <- length(dimensions[[1]])

  tt <- dim(yraw)[1]

  # alpha <- seq(.8, 1, length.out = 4)
  alpha_length <- length(alpha)
  models <- rep(list(""), alpha_length)
  for (i in 1:alpha_length) {
    models[[i]] <- model_ensemble(
      yraw,
      dimension = dimension_model, alpha = alpha[i])
  }
  # reshape yy_predict_sub_aggregate and yy_predict_aggregate
  yy_predict_sub_aggregate <- array(NA, dim = c(dimension_model,
    alpha_length))
  model_probability_predict_sub_aggregate <- array(NA,
    dim = c(tt, alpha_length))
  #
  for (i in 1:alpha_length) {
    yy_predict_sub_aggregate[, i] <-
      models[[i]]$yy_predict_sub_aggregate
    model_probability_predict_sub_aggregate[, i] <-
      models[[i]]$model_probability_predict_sub_aggregate
  }

  alpha_probability_predict <- NA *
    model_probability_predict_sub_aggregate
  temp <- apply(model_probability_predict_sub_aggregate, 1,
    sum)
  alpha_probability_predict <-
    model_probability_predict_sub_aggregate / temp
  alpha_probability_predict_max_index <- apply(
    alpha_probability_predict, 1, function(x)
      sort.int(x, index.return = T, decreasing = T)[[2]][1])
  alpha_probability_predict_max <- apply(
    model_probability_predict_sub_aggregate, 1, function(x)
      sort.int(x, index.return = T, decreasing = T)[[1]][1])
  alpha_probability_predict_average <- apply(
    model_probability_predict_sub_aggregate, 1, sum)
  yy_predict_alpha_max <-
    yy_predict_sub_aggregate[,
      alpha_probability_predict_max_index[tt]]
  yy_predict_alpha_average <-
    apply(t(yy_predict_sub_aggregate) *
        alpha_probability_predict[tt, ], 2, mean)
  list(
    yy_predict_alpha_average =
      yy_predict_alpha_average,
    yy_predict_alpha_max =
      yy_predict_alpha_max,
    alpha_probability_predict_max =
      alpha_probability_predict_max,
    alpha_probability_predict_average =
      alpha_probability_predict_average
  )
}