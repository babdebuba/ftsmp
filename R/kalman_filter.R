#' @export

kalman_filter <- function(yraw, pp, hh,
                          dd, tt, gg, kk, ll,
                          prior_constant_variance,
                          is_length, density_size,
                          dimension) {

# initialize the model ---------------------------------------
model <- model_initialize(yraw, pp, hh,
  dd, tt, gg, kk, ll,
  prior_constant_variance,
  is_length, density_size)

# assign each object in the model list to a matrix ----------
{
yy <- model$yy
zz <- model$zz
zz_predictor <- model$zz_predictor
beta_predict_expectation <- model$beta_predict_expectation
beta_predict_variance <- model$beta_predict_variance
beta_update_expectation <- model$beta_update_expectation
beta_update_variance <- model$beta_update_variance
yy_predict_expectation <- model$yy_predict_expectation
yy_predict_variance <- model$yy_predict_variance
yy_predict_variance_inverse <-
  model$yy_predict_variance_inverse
yy_predict_error_expectation <-
  model$yy_predict_error_expectation
yy_predict_error_variance <- model$yy_predict_error_variance
yy_update_variance <- model$yy_update_variance
yy_probability_predict <- model$yy_probability_predict
yy_predict <- model$yy_predict
yy_predict_density <- model$yy_predict_density
}

# kalman filter ---------------------------------------------
for (t in 1:tt) {
  zz_t_index <- (t * dd - dd + 1):(t * dd)
  temp <- zz[zz_t_index, ]
  # beta prediction
  beta_predict_expectation <- beta_update_expectation
  beta_predict_variance <- beta_update_variance / ll
  # yy prediction
  yy_predict_expectation <- temp %*% beta_predict_expectation
  yy_predict_error_expectation <- yy[t, ] -
    yy_predict_expectation
  yy_predict_error_variance <- Matrix::tcrossprod(
    yy_predict_error_expectation,
    yy_predict_error_expectation)
  # yy_update_variance
  # if (kk == 0) {
  #   # raftery setting
  #   temp <- yy_update_variance
  #   yy_update_variance <- yy_update_variance / ((t - 1) / t) +
  #     (yy_predict_error_variance - temp %*%
  #     beta_predict_variance %*% t(temp)) / t
  #   if (any(eigen(yy_update_variance)$value <= 0)) {
  #     yy_update_variance <- temp
  #   }
  # } else {
    # korobilis setting
    yy_update_variance <- yy_update_variance * kk +
      yy_predict_error_variance * (1 - kk)
  # }
  # if (dd == 1) {
  #   temp2 <- Matrix::crossprod(beta_predict_variance, temp)
  # } else {
    temp2 <- Matrix::tcrossprod(beta_predict_variance,
      temp)
  # }
  yy_predict_variance <- temp %*% temp2 + yy_update_variance
  # beta_update
  yy_predict_variance_inverse <- solve(
    yy_predict_variance)
  temp2 <- temp2 %*% yy_predict_variance_inverse
  beta_update_expectation <- beta_predict_expectation +
    temp2 %*% yy_predict_error_expectation
  beta_update_variance <- beta_predict_variance -
    (temp2 %*% temp) %*% beta_predict_variance
  # yy_probability_predict
  # if (dd == 1) {
  #   yy_probability_predict[t] <- stats::dnorm(
  #     x = yy[t, 1:dimension],
  #     yy_predict_expectation[1:dimension],
  #     yy_predict_variance[1:dimension, 1:dimension])
  #   if (yy_probability_predict[t] == 0) {
  #     yy_probability_predict[t] <- 4.940656e-324
  #   }
  # } else {
  yy_probability_predict[t] <- mvtnorm::dmvnorm(
    x = yy[t, 1:dimension],
    mean = yy_predict_expectation[1:dimension],
    sigma = as.matrix(
      yy_predict_variance[1:dimension, 1:dimension]))
  # one step ahead prediction -----------------------------
  # yy_predict
  yy_predict[t, ] <- as.vector(
    zz_predictor[zz_t_index, ] %*% beta_update_expectation)
  yy_predict_density[t, , ] <- mvtnorm::rmvnorm(
    n = density_size,
    mean = yy_predict[t, 1:dimension],
    sigma = as.matrix(
      yy_update_variance[1:dimension, 1:dimension]))
  # }
}

list(
  yy_probability_predict = yy_probability_predict,
  yy_predict = yy_predict[, 1:dimension],
  yy_predict_density = yy_predict_density,
  yy_predict_expectation = as.matrix(yy_predict_expectation),
  yy_predict_variance = as.matrix(yy_predict_variance),
  yy = yy)
}
