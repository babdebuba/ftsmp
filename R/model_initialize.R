# model building function for the model yy = zz * beta -----------------------
model_initialize <- function(yraw, pp, hh,
                             prior_constant_variance,
                             gg, kk, ll) {

  # build the variables of the model
  dd <- dim(yraw)[2] # dimension of the time series
  yy <- yraw[(pp + hh):nrow(yraw), ] # time series shifted due to pp and hh
  tt <- dim(yy)[1] # length of the time series
  zz <- zz_build(yraw, pp, hh, dd, tt) # build the variable zz
  zz_cols_number <- dim(zz)[1] # number of rows of zz
  zz_rows_number <- dim(zz)[2] # number of cols of zz

  # build beta_predict_expectation and beta_update_variance
  beta_update_expectation <- array(0, dim = c(tt,
    zz_rows_number))
  beta_predict_expectation <- beta_update_expectation

  # build beta_predict_variance and beta_update_variance
  beta_update_variance <- diag(c(rep(prior_constant_variance, dd),
    rep(rep(1, dd), dd)))
  beta_predict_variance <- beta_update_variance

  # build yy_predict_expectation, yy_predict_variance and yy_update_variance
  yy_cov <- stats::cov(yy)
  yy_predict_expectation <- array(0, dim = c(tt, dd))
  yy_predict_variance <- yy_cov + zz[1:dd, ] %*%
    beta_update_variance %*% t(zz[1:dd, ])
  yy_update_variance <- kk * yy_cov

  list(
    dd = dd, hh = hh, pp = pp,
    tt = tt, yy = yy, yraw = yraw, zz = zz,
    zz_cols_number = zz_cols_number,
    zz_rows_number = zz_rows_number,
    beta_predict_expectation = beta_predict_expectation,
    beta_predict_variance = beta_predict_variance,
    beta_update_expectation = beta_update_expectation,
    beta_update_variance = beta_update_variance,
    yy_predict_expectation = yy_predict_expectation,
    yy_predict_variance = yy_predict_variance,
    yy_update_variance = yy_update_variance
  )
}