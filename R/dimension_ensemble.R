dimension_ensemble <- function() {

  # only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  yraw <- MTS::VARMAsim(nobs = 1000, arlags = 1,
    phi = matrix(.9 * diag(4), nrow = 4),
    sigma = matrix(.1 * diag(4), nrow = 4))$series

  tt <- dim(yraw)[1]

  dimensions <- list(1:2, 1:3, 1:4)
  models <- dimensions
  dimension_model <- length(dimensions[[1]])
  dimension_number <- length(dimensions)
  for (i in 1:length(dimensions)) {
    models[[i]] <- model_ensemble(
      yraw = yraw[, dimensions[[i]]],
      dimension = dimension_model)
  }

  # reshape yy_predict_sub_aggregate and yy_predict_aggregate
  yy_predict_sub_aggregate <- array(NA, dim = c(dimension_model,
    dimension_number))
  model_probability_predict_sub_aggregate <- array(NA,
    dim = c(tt, dimension_number))
  #
  for (i in 1:dimension_number) {
    yy_predict_sub_aggregate[, i] <-
      models[[i]]$yy_predict_sub_aggregate
    model_probability_predict_sub_aggregate[, i] <-
      models[[i]]$model_probability_predict_sub_aggregate
  }

  dimension_probability_predict <- NA *
    model_probability_predict_sub_aggregate
  temp <- apply(model_probability_predict_sub_aggregate, 1,
    sum)
  dimension_probability_predict <-
    model_probability_predict_sub_aggregate / temp
  dimension_probability_predict_max <- apply(
    dimension_probability_predict, 1, function(x)
    sort.int(x, index.return = T, decreasing = T)[[2]][1])
  yy_predict_dimension_probability_max <-
    yy_predict_sub_aggregate[,
      dimension_probability_predict_max[tt]]
  yy_predict_dimension_probability_average <-
    apply(t(yy_predict_sub_aggregate) *
    dimension_probability_predict[tt, ], 2, sum)
  list(
  yy_predict_dimension_probability_average =
      yy_predict_dimension_probability_average,
  yy_predict_dimension_probability_max =
      yy_predict_dimension_probability_max
  )
}