#' @export
#' @import foreach
#' @param gg prior parameter
#' @param kk observe variance parameter
#' @param ll state variance parameter
#' @param cores_number define the number of cores
#' @examples model_ensemble()
#' @return the return
model_ensemble <- function(gg = .1, kk = 1, ll = 1,
  cores_number = 1) {

  # only temporary for developemnet !!!!!!!!!!!!!!!!!!!!!!!!!!
  # yraw <- cbind(1:3, 11:13) #!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  yraw <- MTS::VARMAsim(nobs = 1000, arlags = 1,
    phi = matrix(c(0.2, -0.6, 0.3, 1), 2, 2),
    sigma = matrix(c(4, 0.8, 0.8, 1), 2, 2))$series
  gg <- seq(.001, .01, length.out = 2)
  kk <- seq(.8, 1, length.out = 2)
  ll <- seq(.9, 1, length.out = 2)

  model_parameter <- expand.grid(gg = gg, kk = kk, ll = kk)

  models <- rep(list(""), dim(model_parameter)[1])
  for (i in 1:dim(model_parameter)[1]) {
    models[[i]] <- list(yraw, gg = model_parameter[i, 1],
      kk = model_parameter[i, 2], ll = model_parameter[i, 3])
  }

  cl <- parallel::makeCluster(cores_number)
  doParallel::registerDoParallel(cl)

  models <- foreach::foreach(i = 1:length(models),
    .packages = "ftsmp") %dopar%
    do.call(kalman_filter, models[[i]])

  parallel::stopCluster(cl)

  models
}