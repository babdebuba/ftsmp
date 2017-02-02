# model building function for the model yy = zz * beta -----------------------
model_initialize <- function(yraw, pp, hh,
  prior_constant_variance,
  gg, kk, ll,
  density_size,
  dimension, dd, tt,
  is_length) {

  # build the target variable and the predictor variables
  # time series shifted due to pp and hh
  yy <- as.matrix(yraw[-(1:(pp + hh - 1)), ])
  if (dd == 1) {
    yy_cov <- stats::var(yy[1:is_length, ])
  } else {
    yy_cov <- stats::cov(yy[1:is_length, ])
  }

  # build the variable zz
  zz <- zz_build(yraw, pp, hh, dd, predictor = 0)
  zz_predictor <- zz_build(yraw, pp, hh, dd, predictor = 1)

  # build beta_predict_expectation and beta_update_variance
  beta_update_expectation <- matrix(0, nrow = dim(zz)[2])
  beta_predict_expectation <- NA * beta_update_expectation

  # build beta_predict_variance and beta_update_variance
  temp <- c(rep(gg[1], dd))
  if (pp >= 2) {
    for (p in 2:pp) temp <- c(temp, rep(gg / p^2, dd))
  }
  beta_update_variance <- diag(c(rep(
    prior_constant_variance, dd), rep(temp, dd)))
  beta_predict_variance <- NA * beta_update_variance

  # build yy_predict_expectation, yy_predict_variance,
  # yy_update_variance, yy_predict_error_expectation,
  # yy_predict_error_variance, yy_predict_variance_inverse
  yy_predict_expectation <- matrix(NA, nrow = dd)
  yy_predict_variance <- matrix(NA, nrow = 3, ncol = 3)
  yy_predict_variance_inverse <- NA * yy_predict_variance
  yy_predict_error_expectation <- yy_predict_expectation
  yy_predict_error_variance <- yy_predict_variance
  yy_update_variance <- kk * yy_cov

  # build yy_probability_predict
  yy_probability_predict <- matrix(NA, nrow = tt)

  # build yy_predict and yy_predict_density
  yy_predict <- matrix(NA, nrow = tt, ncol = dd)
  yy_predict_density <- array(NA, dim = c(tt, dd,
    density_size))

  list(
    yy = yy, zz = zz, zz_predictor = zz_predictor,
    beta_predict_expectation = beta_predict_expectation,
    beta_predict_variance = beta_predict_variance,
    beta_update_expectation = beta_update_expectation,
    beta_update_variance = beta_update_variance,
    yy_predict_expectation = yy_predict_expectation,
    yy_predict_variance = yy_predict_variance,
    yy_predict_variance_inverse = yy_predict_variance_inverse,
    yy_predict_error_expectation =
      yy_predict_error_expectation,
    yy_predict_error_variance = yy_predict_error_variance,
    yy_update_variance = yy_update_variance,
    yy_probability_predict = yy_probability_predict,
    yy_predict = yy_predict,
    yy_predict_density = yy_predict_density
  )
}
