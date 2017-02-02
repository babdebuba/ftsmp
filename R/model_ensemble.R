model_ensemble <- function(yraw, gg, kk, ll,
                           pp, dimension, alpha,
                           cores_number, hh,
                           prior_constant_variance,
                           density_size, sub) {

  # set all parameter combinations (models)
  model_parameter <- expand.grid(gg = gg, kk = kk, ll = ll,
    pp = pp)
  models <- rep(list(""), dim(model_parameter)[1])
  for (i in 1:dim(model_parameter)[1]) {
    models[[i]] <- list(yraw = yraw,
      pp = model_parameter[i, 4],
      hh = hh, dd = dd, tt = tt, gg = model_parameter[i, 1],
      kk = model_parameter[i, 2],
      ll = model_parameter[i, 3],
      prior_constant_variance = prior_constant_variance,
      is_length = is_length,
      density_size = density_size,
      dimension = dimension)
  }

  # calculate the forecasts of every model
  cl <- parallel::makeCluster(cores_number)
  doParallel::registerDoParallel(cl)
  models <- foreach::foreach(i = 1:length(models),
    .packages = "ftsmp") %dopar%
    do.call(kalman_filter, models[[i]])
  parallel::stopCluster(cl)

  # set dimensions
  model_number <- dim(model_parameter)[1]

  # reshape yy_probability_predict, yy_predict and
  # yy_predict_density
  yy_probability_predict <- array(NA, dim = c(tt,
    model_number))
  yy_predict <- array(NA, dim = c(tt, dd, 1, model_number))
  yy_predict_density <- array(NA, dim = c(tt, dd, density_size,
    model_number))
  for (i in 1:model_number) {
    yy_probability_predict[, i] <-
      models[[i]]$yy_probability_predict
    yy_predict[, , , i] <- models[[i]]$yy_predict
    yy_predict_density[, , , i] <-
      models[[i]]$yy_predict_density
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
      , decreasing = T)))
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

  if (dd == 1) temp <- 1 else temp <- 2

  list(
    model_probability_predict_sub_not_normalized =
      apply(
        as.matrix(
          model_probability_predict_sub_not_normalized),
        1, mean),
    yy_predict_sub_aggregate =
      yy_predict[1:dimension, 1, model_sort_2[1:sub, 2]] *
        model_probability_predict_sub[t, ],
    yy_predict_density_sub_aggregate =
      apply(as.matrix(yy_predict_density[1:dimension, ,
        model_sort_2[1:sub, 2]]), temp, function(x)
          x * model_probability_predict_sub[t, ]),
    tt = tt
  )
}