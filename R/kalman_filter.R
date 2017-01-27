kalman_filter <- function(yraw, pp, hh,
                          prior_constant_variance,
                          gg, kk, ll,
                          density_size,
                          dimension) {

# initialize the model ---------------------------------------
model <- model_initialize(yraw, pp, hh,
  prior_constant_variance, gg, kk, ll, density_size)

# assign each object in the model list to a matrix -----------
{
dd <- model$dd
hh <- model$hh
pp <- model$pp
tt <- model$tt
yy <- model$yy
yraw <- model$yraw
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
yy_predict_density <- model$yy_predict_density
}

# kalman filter ---------------------------------------------
for (t in (2 + pp - 1):tt) {
  # shift set minus 1 due to the prior at point in time 1
  zz_t_index <- (t * dd - dd + 1):(t * dd)
  # beta prediction
  beta_predict_expectation[t, ] <-
    beta_update_expectation[t - 1, ]
  beta_predict_variance <- beta_update_variance / ll
  # yy prediction
  yy_predict_expectation[t, ] <- zz[zz_t_index, ] %*%
    beta_predict_expectation[t, ]
  yy_predict_error_expectation[t, ] <-
    yy[t, ] - yy_predict_expectation[t, ]
  yy_predict_error_variance <-
    yy_predict_error_expectation[t, ] %*%
    t(yy_predict_error_expectation[t, ])
  # yy_update_variance
  if (kk == 0) {
    # raftery setting
    temp <- yy_update_variance
    yy_update_variance <- yy_update_variance / ((t - 1) / t) +
      (yy_predict_error_variance - zz[zz_t_index, ] %*%
      beta_predict_variance %*% t(zz[zz_t_index, ])) / t
    if (any(eigen(yy_update_variance)$value <= 0)) {
      yy_update_variance <- temp
    }
  } else {
    # korobilis setting
    yy_update_variance <- yy_update_variance * kk +
      yy_predict_error_variance * (1 - kk)
  }
  yy_predict_variance <- zz[zz_t_index, ] %*%
    beta_predict_variance %*% t(zz[zz_t_index, ]) +
    yy_update_variance
  # beta_update
  yy_predict_variance_inverse <- solve(yy_predict_variance)
  beta_update_expectation[t, ] <-
    beta_predict_expectation[t, ] + beta_predict_variance %*%
    t(zz[zz_t_index, ]) %*% yy_predict_variance_inverse %*%
    yy_predict_error_expectation[t, ]
  beta_update_variance <- beta_predict_variance -
    beta_predict_variance %*% t(zz[zz_t_index, ]) %*%
    yy_predict_variance_inverse %*% zz[zz_t_index, ] %*%
    beta_predict_variance
  # yy_probability_predict
  yy_probability_predict[t] <- mvtnorm::dmvnorm(
    x = yy[t, 1:dimension],
    mean = yy_predict_expectation[t, 1:dimension],
    sigma = yy_predict_variance[1:dimension, 1:dimension])
}

# one step ahead prediction ---------------------------------
# yy_predict
yy_predict <- t(zz_predictor[zz_t_index, ] %*%
  beta_update_expectation[t, ])
# yy_predict_density
yy_predict_density <- mvtnorm::rmvnorm(
  n = density_size,
  mean = yy_predict, yy_update_variance)

list(
  yy_probability_predict = yy_probability_predict,
  yy_predict = yy_predict,
  yy_predict_density = yy_predict_density,
  tt = tt, dd = dd)
}