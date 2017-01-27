out_of_sample_forecasts <- function() {
set.seed(34)
# data
# load(file = "data/stock_watson_forecast_errors_benchmark_ar.RData")
# load(file = "data/stock_watson_forecast_errors.RData")
# load(file = "data/stock_watson_raw.RData")
# load(file = "data/stock_watson_transformed.RData")
data(stock_watson_transformed)

yraw <- stock_watson_transformed[, 1:75]
rgdp_index <- grep("rgdp", colnames(yraw))[1]
cpi_index <- grep("cpi", colnames(yraw))[1]
rtbill_index <- grep("rtbill", colnames(yraw))[1]
yraw <- yraw[-1, c(rgdp_index, cpi_index, rtbill_index)]

# from model_ensemble
gg <- seq(.01, .1, length.out = 5)
kk <- seq(.9, 1, length.out = 5)
ll <- seq(.9, 1, length.out = 5)
pp <- seq(1, 3, 1)
cores_number <- 4
sub <- floor(length(gg) * length(kk) * length(ll) *length(pp) *
    .05)
# from dimension_ensemble
alpha <- seq(.98, 1, length.out = 3)
dimensions <- list(1:dim(yraw)[2])#list(1:2, 1:4)
#
hh <- 1
prior_constant_variance = 10
density_size = 1

oos_length <- floor(dim(yraw)[1] / 2)
is_length <- dim(yraw)[1] - oos_length

yraw_pred <- yraw[-(1:is_length), ]
pred_dms <- array(NA, dim = c(oos_length, dim(yraw)[2]))
pred_mts <- pred_dms
pb <- utils::txtProgressBar(min = 0, max = oos_length, style = 3)
system.time({
  for(i in 1:oos_length) {
  utils::setTxtProgressBar(pb, i)
  pred_dms[i, ] <- dimension_ensemble(yraw[i:(is_length + i - 1), ],
    alpha, gg, kk, ll, pp,cores_number, hh,
    prior_constant_variance, density_size,
    sub, dimensions)$yy_predict_dimension_probability_average
  temp <- predict(
    vars::VAR(yraw[i:(is_length + i - 1), ],
      lag.max = pp[length(pp)]), n.ahead = hh)
  pred_mts[i, ] <- cbind(temp[[1]][[1]][1], temp[[1]][[2]][1],
    temp[[1]][[3]][1])
  }
})

error_dms <- pred_dms - yraw_pred
error_mts <- pred_mts - yraw_pred

apply(error_dms^2, 2, mean) / apply(error_mts^2, 2, mean)
}
