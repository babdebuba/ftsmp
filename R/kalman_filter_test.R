# # library(Rcpp)
# # sourceCpp("../temp/test.cpp")
# Rprof(tmp <- tempfile())
# yraw <- stock_watson_transformed[, 1:75]
# delete_index <- yraw[7, ] == 65535 | yraw[160, ] == 65535
# yraw <- yraw[-c(1:6, 161:164), !delete_index]
# sum(yraw[1, ] == 65535)
# sum(yraw[154, ] == 65535)
# # m <- apply(yraw, 2, mean)
# # s <- apply(yraw, 2, sd)
# # yraw <- t(apply(yraw, 1, function(x) (x - m) / s))
# # yraw <- stock_watson_forecast_errors$cn_rgdp_2
# yraw <- yraw[, 1:10]
#
# # settings
# gg <- 1#c(.1, .01, .001, .0001, .00001)
# kk <- 1#seq(.9, 1, length.out = 5)
# ll <- 1#seq(.9, 1, length.out = 5)
# alpha <- 1#seq(.9, 1, length.out = 5)
# pp <- 4#seq(1, 4, 1)
# cores_number <- 6
# sub <- ceiling(length(gg) * length(kk) * length(ll) *length(pp) * .01)
# dimensions <- list(1:dim(yraw)[2])
# hh <- 2
# prior_constant_variance = 10
# density_size = 5
#
# #dimension_ensemble
# models <- dimensions
# dimension <- length(dimensions[[1]])
# dimension_number <- length(dimensions)
#
# # initialize the model ---------------------------------------
# model <- model_initialize(yraw, pp, hh,
#   prior_constant_variance, gg, kk, ll, density_size)
#
# # assign each object in the model list to a matrix -----------
# {
#   dd <- model$dd
#   hh <- model$hh
#   pp <- model$pp
#   tt <- model$tt
#   yy <- model$yy
#   yraw <- model$yraw
#   zz <- model$zz
#   zz_predictor <- model$zz_predictor
#   beta_predict_expectation <- model$beta_predict_expectation
#   beta_predict_variance <- model$beta_predict_variance
#   beta_update_expectation <- model$beta_update_expectation
#   beta_update_variance <- model$beta_update_variance
#   yy_predict_expectation <- model$yy_predict_expectation
#   yy_predict_variance <- model$yy_predict_variance
#   yy_predict_variance_inverse <-
#     model$yy_predict_variance_inverse
#   yy_predict_error_expectation <-
#     model$yy_predict_error_expectation
#   yy_predict_error_variance <- model$yy_predict_error_variance
#   yy_update_variance <- model$yy_update_variance
#   yy_probability_predict <- model$yy_probability_predict
#   yy_predict_density <- model$yy_predict_density
# }
#
# # pb <- utils::txtProgressBar(min = 2 + pp - 1, max = tt,
# #   style = 3)
# # # kalman filter ---------------------------------------------
# # for (t in (2 + pp - 1):tt) {
# #   utils::setTxtProgressBar(pb, value = t)
# #   # shift set minus 1 due to the prior at point in time 1
# #   zz_t_index <- (t * dd - dd + 1):(t * dd)
# #   if (dd == 1) {
# #     temp <- t(zz[zz_t_index, ])
# #   } else {
# #     temp <- zz[zz_t_index, ]
# #   }
# #   # beta prediction
# #   beta_predict_expectation[t, ] <-
# #     beta_update_expectation[t - 1, ]
# #   beta_predict_variance <- beta_update_variance / ll
# #   # yy prediction
# #   yy_predict_expectation[t, ] <- eigenMapMatMult(temp,
# #     beta_predict_expectation[t, ])
# #     # temp %*%
# #     # beta_predict_expectation[t, ]
# #   yy_predict_error_expectation[t, ] <-
# #     yy[t, ] - yy_predict_expectation[t, ]
# #   yy_predict_error_variance <-
# #     tcrossprod(yy_predict_error_expectation[t, ],
# #       yy_predict_error_expectation[t, ])
# #     # yy_predict_error_expectation[t, ] %*%
# #     # t(yy_predict_error_expectation[t, ])
# #   # yy_update_variance
# #   # if (kk == 0) {
# #   #   # raftery setting
# #   #   temp <- yy_update_variance
# #   #   yy_update_variance <- yy_update_variance / ((t - 1) / t) +
# #   #     (yy_predict_error_variance - temp %*%
# #   #     beta_predict_variance %*% t(temp)) / t
# #   #   if (any(eigen(yy_update_variance)$value <= 0)) {
# #   #     yy_update_variance <- temp
# #   #   }
# #   # } else {
# #   # korobilis setting
# #   yy_update_variance <- yy_update_variance * kk +
# #     yy_predict_error_variance * (1 - kk)
# #   # }
# #   yy_predict_variance <- tcrossprod(
# #     # temp %*%
# #     #   beta_predict_variance
# #     eigenMapMatMult(temp, beta_predict_variance)
# #     , temp) +
# #     # temp %*%
# #     # beta_predict_variance %*% t(temp) +
# #     yy_update_variance
# #   # beta_update
# #   yy_predict_variance_inverse <- chol2inv(yy_predict_variance)
# #   beta_update_expectation[t, ] <-
# #     beta_predict_expectation[t, ] +
# #     eigenMapMatMult(beta_predict_variance, crossprod(temp,
# #       eigenMapMatMult(yy_predict_variance_inverse, yy_predict_error_expectation[t, ])
# #       #yy_predict_variance_inverse %*%
# # #            yy_predict_error_expectation[t, ]
# #       ))
# #     # beta_predict_variance %*%
# #     # crossprod(temp, yy_predict_variance_inverse %*%
# #     #     yy_predict_error_expectation[t, ])
# #     # t(temp) %*% yy_predict_variance_inverse %*%
# #     # yy_predict_error_expectation[t, ]
# #   beta_update_variance <- beta_predict_variance -
# #     # beta_predict_variance %*% t(temp) %*%
# #     eigenMapMatMult(eigenMapMatMult(eigenMapMatMult(tcrossprod(beta_predict_variance, temp),
# #       yy_predict_variance_inverse), temp), beta_predict_variance)
# #     # tcrossprod(beta_predict_variance, temp) %*%
# #     # yy_predict_variance_inverse %*% temp %*%
# #     # beta_predict_variance
# #   # yy_probability_predict
# #   if (dd == 1) {
# #     yy_probability_predict[t] <- stats::dnorm(
# #       x = yy[t, 1:dimension],
# #       yy_predict_expectation[t, 1:dimension],
# #       yy_predict_variance[1:dimension, 1:dimension])
# #     if (yy_probability_predict[t] == 0) {
# #       yy_probability_predict[t] <- 4.940656e-324
# #     }
# #   } else {
# #     yy_probability_predict[t] <- mvtnorm::dmvnorm(
# #       x = yy[t, 1:dimension],
# #       mean = yy_predict_expectation[t, 1:dimension],
# #       sigma = yy_predict_variance[1:dimension, 1:dimension])
# #   }
# # }
# pb <- utils::txtProgressBar(min = 2 + pp - 1, max = tt,
#   style = 3)
# # kalman filter ---------------------------------------------
# for (t in (2 + pp - 1):tt) {
#   utils::setTxtProgressBar(pb, value = t)
#   # shift set minus 1 due to the prior at point in time 1
#   zz_t_index <- (t * dd - dd + 1):(t * dd)
#   if (dd == 1) {
#     temp <- t(zz[zz_t_index, ])
#   } else {
#     temp <- zz[zz_t_index, ]
#   }
#   # beta prediction
#   beta_predict_expectation[t, ] <-
#     beta_update_expectation[t - 1, ]
#   beta_predict_variance <- beta_update_variance / ll
#   # yy prediction
#   yy_predict_expectation[t, ] <- temp %*%
#     beta_predict_expectation[t, ]
#   yy_predict_error_expectation[t, ] <-
#     yy[t, ] - yy_predict_expectation[t, ]
#   yy_predict_error_variance <-
#     yy_predict_error_expectation[t, ] %*%
#     t(yy_predict_error_expectation[t, ])
#   # yy_update_variance
#   # if (kk == 0) {
#   #   # raftery setting
#   #   temp <- yy_update_variance
#   #   yy_update_variance <- yy_update_variance / ((t - 1) / t) +
#   #     (yy_predict_error_variance - temp %*%
#   #     beta_predict_variance %*% t(temp)) / t
#   #   if (any(eigen(yy_update_variance)$value <= 0)) {
#   #     yy_update_variance <- temp
#   #   }
#   # } else {
#   # korobilis setting
#   yy_update_variance <- yy_update_variance * kk +
#     yy_predict_error_variance * (1 - kk)
#   # }
#   yy_predict_variance <- temp %*%
#     beta_predict_variance %*% t(temp) +
#     yy_update_variance
#   # beta_update
#   yy_predict_variance_inverse <- chol2inv(yy_predict_variance)
#   beta_update_expectation[t, ] <-
#     beta_predict_expectation[t, ] + beta_predict_variance %*%
#     t(temp) %*% yy_predict_variance_inverse %*%
#     yy_predict_error_expectation[t, ]
#   beta_update_variance <- beta_predict_variance -
#     beta_predict_variance %*% t(temp) %*%
#     yy_predict_variance_inverse %*% temp %*%
#     beta_predict_variance
#   # yy_probability_predict
#   if (dd == 1) {
#     yy_probability_predict[t] <- stats::dnorm(
#       x = yy[t, 1:dimension],
#       yy_predict_expectation[t, 1:dimension],
#       yy_predict_variance[1:dimension, 1:dimension])
#     if (yy_probability_predict[t] == 0) {
#       yy_probability_predict[t] <- 4.940656e-324
#     }
#   } else {
#     yy_probability_predict[t] <- mvtnorm::dmvnorm(
#       x = yy[t, 1:dimension],
#       mean = yy_predict_expectation[t, 1:dimension],
#       sigma = yy_predict_variance[1:dimension, 1:dimension])
#   }
# }
# Rprof()
# summaryRprof(tmp)
