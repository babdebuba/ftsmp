#' @export
#' @import foreach
#' @description this is the description
#' @title main function
#' @param gg prior parameter
#' @param kk observe variance parameter
#' @param ll state variance parameter
#' @param cores_number define the number of cores
#' @examples model_ensemble()
#' @return the return
model_ensemble <- function(gg = .1, kk = 1, ll = 1, pp = 1,
  cores_number = 1) {

# only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
{  # yraw <- cbind(1:3, 11:13) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  set.seed(34)
  yraw <- MTS::VARMAsim(nobs = 1000, arlags = 1,
    phi = matrix(c(.9, 0, 0, .9), nrow = 2),
    sigma = matrix(c(.01, 0, 0, .01), 2, 2))$series
  yf <- yraw[dim(yraw)[1], ]
  gg <- seq(.0001, .1, length.out = 2)
  kk <- seq(.8, 1, length.out = 2)
  ll <- seq(.8, 1, length.out = 2)
  pp <- seq(1, 2, 1)
  cores_number <- 1
}
sub <- 1
alpha <- 1

  # set all parameter combinations (models)
  model_parameter <- expand.grid(gg = gg, kk = kk, ll = ll,
    pp = pp)
  models <- rep(list(""), dim(model_parameter)[1])
  for (i in 1:dim(model_parameter)[1]) {
    models[[i]] <- list(yraw, gg = model_parameter[i, 1],
      kk = model_parameter[i, 2], ll = model_parameter[i, 3],
      pp = model_parameter[i, 4])
  }

  # calculate the forecasts of every model
  cl <- parallel::makeCluster(cores_number)
  doParallel::registerDoParallel(cl)
  models <- foreach::foreach(i = 1:length(models),
    .packages = "ftsmp") %do%
    do.call(kalman_filter, models[[i]])
  parallel::stopCluster(cl)

  # set dimensions
  model_number <- dim(model_parameter)[1]
  dd <- models[[1]]$dd
  tt <- models[[1]]$tt
  density_size <- dim(models[[1]]$yy_predict_density)[1]

  # reshape yy_probability_predict, yy_predict and
  # yy_predict_density
  yy_probability_predict <- array(NA, dim = c(tt,
    model_number))
  yy_predict <- array(NA, dim = c(dd, model_number))
  yy_predict_density <- array(NA, dim = c(dd, density_size,
    model_number))

  for (i in 1:model_number) {
    yy_probability_predict[, i] <-
      models[[i]]$yy_probability_predict
    yy_predict[, i] <- models[[i]]$yy_predict
    yy_predict_density[, , i] <- models[[i]]$yy_predict_density
  }

  # build model_probability_predict and model_pobability_update
  model_probability_update <- array(NA, dim = c(tt,
    model_number))
  model_probability_predict <- model_probability_update
  model_probability_predict[pp + 1, ] <- 1 / model_number
  model_probability_predict_sub <-
    as.matrix(model_probability_predict[, 1:sub])

  # calculate model_probaility_predict
  offset <- .001 / model_number
  for (t in (3 + pp[length(pp)] - 1):tt) {
    temp_update <-
      sum(model_probability_predict[t - 1, ] *
          yy_probability_predict[t - 1, ])
    model_probability_update[t - 1, ] <-
      model_probability_predict[t - 1, ] *
      yy_probability_predict[t - 1, ] / temp_update
    temp_predict <-
      sum(model_probability_update[t - 1, ] ^ alpha + offset)
    model_probability_predict[t, ] <-
      (model_probability_update[t - 1, ] ^ alpha + offset) /
      temp_predict
    model_sort_2 <- as.matrix(as.data.frame(sort.int(
      model_probability_predict[t, ], index.return = T
      ,decreasing = T)))
    temp_predict_sub <-
      sum(model_probability_update[t - 1,
        model_sort_2[1:sub, 2]] ^ alpha + offset)
    model_probability_predict_sub[t, ] <-
      (model_probability_update[t - 1,
        model_sort_2[1:sub, 2]] ^ alpha + offset) /
      temp_predict_sub
  }
  list(
    apply(as.matrix(yy_predict[, model_sort_2[1:sub, 2]] *
        model_probability_predict_sub[t, ]), 1, sum),
    # apply(yy_predict_density[, , model_sort_2[1:sub, 2]] *
    #     model_probability_predict_sub[tt, ], 1:2, sum),
    yf
    # yy_predict,
    # mts,
    # models[[8]]$beta_update_expectation[dim(yraw)[1] - 1, ]
  )
}