#' @export
#' @import foreach
#' @description this is the description
#' @title main function
#' @param gg prior parameter
#' @param kk observe variance parameter
#' @param ll state variance parameter
#' @param pp number of lags
#' @param cores_number define the number of cores
#' @examples model_ensemble()
#' @return the return
model_ensemble <- function(yraw, gg = .1, kk = 1, ll = 1,
                           pp = 1, dimension,
                           cores_number = 1) {

# only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!!!!
{
  set.seed(34)
  gg <- seq(.0001, .1, length.out = 2)
  kk <- seq(.8, 1, length.out = 2)
  ll <- seq(.8, 1, length.out = 2)
  pp <- seq(1, 2, 1)
  cores_number <- 1
}
sub <- 4
alpha <- 1

  # set all parameter combinations (models)
  model_parameter <- expand.grid(gg = gg, kk = kk, ll = ll,
    pp = pp)
  models <- rep(list(""), dim(model_parameter)[1])
  for (i in 1:dim(model_parameter)[1]) {
    models[[i]] <- list(yraw, gg = model_parameter[i, 1],
      kk = model_parameter[i, 2], ll = model_parameter[i, 3],
      pp = model_parameter[i, 4],
      dimension = dimension)
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
  model_probability_predict[pp[length(pp)] + 1, ] <-
    1 / model_number
  model_probability_predict_sub <-
    as.matrix(model_probability_predict[, 1:sub])
  model_probability_predict_sub_not_normalized <-
    model_probability_predict_sub

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
    model_probability_predict_sub_not_normalized[t, ] <-
      (model_probability_update[t - 1,
        model_sort_2[1:sub, 2]] ^ alpha + offset)
  }
  list(
      apply(model_probability_predict_sub_not_normalized,
        1, mean),
      apply(as.matrix(yy_predict[1:dimension,
        model_sort_2[1:sub, 2]] *
        model_probability_predict_sub[t, ]), 1, sum)
    # apply(yy_predict_density[, , model_sort_2[1:sub, 2]] *
    #     model_probability_predict_sub[tt, ], 1:2, sum),
    # yf
    # yy_predict,
    # mts,
    # models[[8]]$beta_update_expectation[dim(yraw)[1] - 1, ]
  )
}