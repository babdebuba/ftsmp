# yraw <- stock_watson_transformed[, 1:75]
# delete_index <- yraw[7, ] == 65535 | yraw[160, ] == 65535
# yraw <- yraw[-c(1:6, 161:164), !delete_index]
# sum(yraw[1, ] == 65535)
# sum(yraw[154, ] == 65535)
# # m <- apply(yraw, 2, mean)
# # s <- apply(yraw, 2, sd)
# # yraw <- t(apply(yraw, 1, function(x) (x - m) / s))
# # yraw <- stock_watson_forecast_errors$cn_rgdp_2
# yraw <- yraw[, 1:25]
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
# #
# system.time({test <- ftsmp::dimension_ensemble(yraw,
#   alpha, gg, kk, ll, pp,cores_number, hh,
#   prior_constant_variance, density_size,
#   sub, dimensions)})
