kalman_filter <- function(pp = 1, hh = 1,
                          prior_constant_variance = 10,
                          gg = .1, kk = 1, ll = 1) { # function(yraw, pp, hh, prior_constant_variance, gg, kk, ll)

# only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!
# yraw <- cbind(1:10, 11:20) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
yraw <- MTS::VARMAsim(nobs = 100, arlags = 1,
  phi = matrix(c(0.2, -0.6, 0.3, 1), 2, 2),
  sigma = matrix(c(4, 0.8, 0.8, 1), 2, 2))$series
pp = 1
hh = 1
prior_constant_variance = 10
gg = .1
kk = 1
ll = 1
t = 2

# initialize the model ----------------------------------------
model <- model_initialize(yraw, pp, hh,
  prior_constant_variance, gg, kk, ll)

# assign each object in the model list to a matrix -----------
{
dd <- model$dd
hh <- model$hh
pp <- model$pp
tt <- model$tt
yy <- model$yy
yraw <- model$yraw
zz <- model$zz
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
}

# where to place ???????????????????????????????????????????
density_size = 100
model_probability_predict <- rep(1, length = tt)
yy_predict <- array(NA, dim = c(tt, dd))
yy_predict_density <- array(NA, dim = c(tt, dd, density_size))

# kalman filter ----------------------------------------------
progressbar <- utils::txtProgressBar(min = 0, max = tt - 1,
  style = 3)
for (t in 2:(tt - 1)) {
  utils::setTxtProgressBar(pb = progressbar, value = t)
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
    if (any(eigen(yy_update_variance)$value) <= 0) {
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
  # model_probability_predict
  model_probability_predict[t] <- mvtnorm::dmvnorm(
    x = yy[t, ], mean = yy_predict_expectation[t, ],
    sigma = yy_predict_variance)
  # yy_predict_density
  zz_t_index <- ((t + 1) * dd - dd + 1):((t + 1) * dd)
  yy_predict[t, ] <- zz[zz_t_index, ] %*%
    beta_update_expectation[t, ]
  yy_predict_density[t, , ] <- mvtnorm::rmvnorm(
    n = density_size,
    mean = yy_predict[t, ], yy_update_variance)
}
close(progressbar)

list(
  model_probability_predict = model_probability_predict,
  yy_predict = yy_predict,
  yy_predict_density = yy_predict_density
)
}