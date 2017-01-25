dimension_ensemble <- function() {

  # only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  yraw <- MTS::VARMAsim(nobs = 1000, arlags = 1,
    phi = matrix(.9 * diag(4), nrow = 4),
    sigma = matrix(.1 * diag(4), nrow = 4))$series

  dimensions <- list(1:2, 1:4)
  models <- rep(list(""), length(dimensions))
  for (i in 1:length(dimensions)) {
    models[[i]] <- model_ensemble(
      yraw = yraw[, dimensions[[i]]],
      dimension = length(dimensions[[1]]))
  }
  dimension_probability_predict <- array(NA,
    dim = c(dim(yraw)[1], dimension_number))
  for (i in 1:length(dimensions)) {
    dimension_probability_predict
  }
}