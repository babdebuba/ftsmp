# model building function for the model yy = zz * beta -----------------------
model_initialize <- function(yraw, pp, hh,
                             prior_constant_variance,
                             gg, kk, ll, density_size) {

  # build the variables of the model
  dd <- dim(yraw)[2]  # dimension of the time series
  # time series shifted due to pp and hh
  yy <- yraw[(pp + hh):nrow(yraw), ]
  yy_cov <- stats::cov(yy)
  tt <- dim(yy)[1]  # length of the time series
  # build the variable zz
  zz <- zz_build(yraw, pp, hh, dd, tt, predictor = 0)
  zz_predictor <- zz_build(yraw, pp, hh, dd, tt, predictor = 1)
  zz_cols_number <- dim(zz)[2]  # number of cols of zz
  # add unused NA rows for the prior at point in time 1
  # and for the lag p
  yy <- rbind(array(NA, dim = c(pp, dd)), yy)
  zz <- rbind(matrix(NA, nrow = dd * pp, ncol = zz_cols_number),
    zz)
  zz_predictor <- rbind(matrix(NA, nrow = dd * pp,
    ncol = zz_cols_number), zz_predictor)

  tt <- tt + pp

  # build beta_predict_expectation and beta_update_variance
  beta_update_expectation <- array(0, dim = c(tt,
    zz_cols_number))
  beta_predict_expectation <- NA * beta_update_expectation

  # build beta_predict_variance and beta_update_variance
  temp <- rep(1, dd)
  if (pp >= 2) {
    for (p in 2:pp) temp <- c(temp, rep(gg[1] / p^2, dd))
  }
  beta_update_variance <- diag(c(rep(prior_constant_variance,
    dd), rep(temp, dd)))
  beta_predict_variance <- NA * beta_update_variance

  # build yy_predict_expectation, yy_predict_variance,
  # yy_update_variance, yy_predict_error_expectation,
  # yy_predict_error_variance, yy_predict_variance_inverse
  yy_predict_expectation <- array(NA, dim = c(tt, dd))
  yy_predict_variance <- yy_cov + zz[1:dd, ] %*%
    beta_update_variance %*% t(zz[1:dd, ])
  yy_predict_variance_inverse <- NA * yy_predict_variance
  yy_predict_error_expectation <- yy_predict_expectation
  yy_predict_error_variance <- array(NA, dim = c(dd, dd))
  yy_update_variance <- kk * yy_cov
  # build yy_probability_predict
  yy_probability_predict <- rep(NA, length = tt)
  # build yy_predict_density
  yy_predict_density <- array(NA, dim = c(tt, dd,
    density_size))

  list(
    dd = dd, hh = hh, pp = pp, tt = tt, yy = yy, yraw = yraw,
    zz = zz, zz_predictor = zz_predictor,
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
    yy_predict_density = yy_predict_density
  )
}