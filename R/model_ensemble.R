#' @export
#' @import foreach
model_ensemble <- function(gg = .1, kk = 1, ll = 1) {

  # only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!
  # yraw <- cbind(1:3, 11:13) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  yraw <- MTS::VARMAsim(nobs = 100, arlags = 1,
    phi = matrix(c(0.2, -0.6, 0.3, 1), 2, 2),
    sigma = matrix(c(4, 0.8, 0.8, 1), 2, 2))$series
  gg <- c(.1, .2)
  kk <- c(1, 2)
  ll <- c(1, 2)

  model_parameter <- expand.grid(gg = gg, kk = kk, ll = kk)

  models <- rep(list(""), dim(model_parameter)[1])
  for (i in 1:8) {
    models[[i]] <- list(yraw, gg = model_parameter[i, 1],
      kk = model_parameter[i, 2], ll = model_parameter[i, 3])
  }

  cl <- parallel::makeCluster(2)
  doParallel::registerDoParallel(cl)

  models <- foreach::foreach(i = 1:length(models),
    .packages = "ftsmp") %dopar%
    do.call(kalman_filter, models[[i]])

  parallel::stopCluster(cl)

  models
}