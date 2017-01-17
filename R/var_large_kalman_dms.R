rm(list = ls())
cat("\014")

# generate the data
# library(MTS)
# beta <- matrix(c(0.2, -0.6, 0.3, 1), 2, 2)
# sigma <- matrix(c(4, 0.8, 0.8, 1), 2, 2)
# yraw <- VARMAsim(nobs = 5, arlags = 1, phi = beta, sigma = sigma)$series
yraw <- cbind(1:3, 11:13)

# the model is yy_t = zz_t %*% beta_predict_t + error_t

# model building
# {
# build the variables of the model
# determine the lag length pp and the forecast horion hh
pp <- 1
hh <- 1

# build the variables of the model
# build the time tt, the target variable yy and the lagged variables zz
tt <- dim(yraw)[1]
dd <- dim(yraw)[2]
yy <- yraw[(pp + hh):nrow(yraw),]
yy_rows_number <- dim(yy)[1]
zz <- embed(yraw, pp + 1);
zz <- cbind(zz[1:(nrow(zz) - hh + 1),
            (dd + 1):ncol(zz)])
zz <- cbind(diag(dd) %x% zz)
temp <- zz
for (i in 1:yy_rows_number) {
  zz[((i - 1) * dd + 1):(i * dd), ] <- temp[seq(from = i, to = (i - 1) + dd * yy_rows_number, by = yy_rows_number), ]
}
zz <- cbind(t(structure(replicate(yy_rows_number, diag(dd)), dim = c(dd, yy_rows_number * dd))), zz)
tt <- yy_rows_number

# build the variables of the dynamic model selection/averaging
# build the grid for the prior gg, the observation variance kk,
# the state variance ll and the forgetting factor ff
gg <- c(1:2)
kk <- c(1:2)
ll <- c(1:2)
gg_length <- length(gg)
kk_length <- length(kk)
ll_length <- length(ll)
model_number <- gg_length * kk_length * ll_length

# build the variables
#
zz_cols_number <- dim(zz)[1]
zz_rows_number <- dim(zz)[2]

# build the variables
# beta_predict_expectation and beta_update_variance
beta_predict_expectation <- array(0, dim = c(tt,
                                            zz_rows_number,
                                            gg_length,
                                            kk_length,
                                            ll_length))
beta_update_expectation <- beta_predict_expectation

# build the variables
# beta_predict_variance and beta_update_variance
beta_predict_variance <- array(0, dim = c(zz_rows_number,
                                          zz_rows_number,
                                          gg_length,
                                          kk_length,
                                          ll_length,
                                          ff_length))
beta_update_variance <- beta_predict_variance

# build the variables
# yy_predict_expectation, y_prediction_error,
# yy_predict_variance, yy_update_variance
yy_predict_expectation <- array(0, dim = c(tt,
                                          dd,
                                          gg_length,
                                          kk_length,
                                          ll_length,
                                          ff_length))
yy_predict_error_expectation <- yy_predict_expectation
yy_predict_variance <- array(0, dim = c(dd,
                                        dd,
                                        gg_length,
                                        kk_length,
                                        ll_length,
                                        ff_length))
yy_update_variance <- yy_predict_variance

# build the variables
# model_probability_predict and model_probability_update
model_probability_predict <- array(0, dim = c(tt,
                                              gg_length * kk_length *
                                              ll_length * ff_length))
model_probability_update <- model_probability_predict
}
# prior setting
{
# pior
# beta_update_variance
prior_constant_variance <- 10
temp <- rep(1, dd)
if (pp >= 2) for (p in 2:pp) temp <- c(temp, rep(gg[1] / p^2, dd))
beta_update_variance <- structure(apply(
    beta_update_variance,
    MARGIN = 3:6,
    FUN =  function(x) diag(c(rep(prior_constant_variance, dd),
                                  rep(temp, dd)))),
  dim = dim(beta_update_variance))

# prior
# yy_predict_variance
yy_cov <- cov(yy)
yy_predict_variance <- structure(apply(
    beta_update_variance,
    MARGIN = 3:6,
    FUN = function(x) yy_cov + zz[1:dd, ] %*% x %*% t(zz[1:dd, ])),
  dim = dim(yy_predict_variance))

# prior
# yy_predict_variance
yy_update_variance <- structure(apply(
    yy_update_variance,
    MARGIN = 3:6,
    FUN = function(x) yy_cov),
  dim = dim(yy_update_variance))

# prior
# prior model_probability_predict
model_probability_predict[1, ] <- 1 / dim(model_probability_predict)[2]
}

# kalman filter
# progressbar <- utils::txtProgressBar(min = 0, max = tt, style = 3)
# for (t in 2:(tt + 1)) {
#   utils::setTxtProgressBar(pb = progressbar, value = t)
t = 2
  # kalman filter prediction
  # beta prediction
  beta_predict_expectation[t, , , , , ] <- beta_update_expectation[t, , , , , ]
  for (l in seq_len(ll_length)) {
    beta_predict_variance[, , , , l, ] <- beta_update_variance[, , , , l, ] /
                                          ll[l]
  }
  # beta_predict_variance <- structure(sapply(
  #   seq_len(ll_length),
  #   FUN = function(x, y, z) y[, , , , x, ] / z[x],
  #   y = beta_update_variance,
  #   z = ll),
  # dim = dim(beta_update_variance))
  # kalman filter prediction
  # yy prediction
  yy_predict_expectation[t, , , , , ] <- structure(apply(
      beta_predict_expectation[t, , , , , ],
      MARGIN = 2:5,
      FUN = function(x) zz[((t - 2) * dd + 1):((t - 1) * dd), ] %*%
                        x),
    dim = dim(yy_predict_expectation[t, , , , , ]))
  yy_predict_error_expectation[t, , , , ,] <- structure(apply(
      yy_predict_expectation[t, , , , , ],
      MARGIN = 2:5,
      FUN = function(x) x - yy[t, ]),
    dim = dim(yy_predict_error_expectation[t, , , , , ]))
  yy_predict_error_variance <- structure(apply(
    yy_predict_error_expectation[t, , , , , ],
    MARGIN = 2:5,
    FUN = function(x) x %*% t(x)),
    dim = c(dd, dd, gg_length, kk_length, ll_length, ff_length))
  # this is the shorer version of koop and korobilis compared to raftery's original
  for (i in seq_len(kk_length)) {
    yy_update_variance[, , , i, , ] <- yy_update_variance[, , , i, , ] * kk[i] +
                                      yy_predict_error_variance[, , , i, , ] * (1 - kk[i])
  }
  yy_predict_variance <- structure(apply(
      beta_predict_variance,
      MARGIN = 3:6,
      FUN = function(x) zz[((t - 2) * dd + 1):((t - 1) * dd), ] %*% x %*%
                        t(zz[((t - 2) * dd + 1):((t - 1) * dd), ])),
    dim = dim(yy_predict_variance))
  yy_predict_variance <- yy_predict_variance + yy_update_variance
  # kalman filter update
  yy_predict_variance_inverse <- structure(apply(
      yy_predict_variance,
      MARGIN = 3:6,
      FUN = solve),
    dim = dim(yy_predict_variance))
  temp <- structure(apply(
      yy_predict_variance_inverse,
      MARGIN = 3:6,
      FUN = function(x) t(zz[((t - 2) * dd + 1):((t - 1) * dd), ]) %*% x),
    dim = c(zz_rows_number, dd, gg_length, kk_length, ll_length, ff_length))
  temp <- structure(apply(
      temp,
      MARGIN = 3:6,
      FUN = function(x) ),
    dim = )
# }
# close(progressbar)