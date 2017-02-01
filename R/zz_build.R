# build the variable zz of the overall model
zz_build <- function(yraw, pp, hh, dd, tt, predictor) {
  zz <- stats::embed(yraw, pp)
  if (predictor == 0) {
    zz <- zz[1:(dim(zz)[1] - hh), ]
  } else {
    zz <- zz[-(1:hh), ]
  }
  zz <- cbind(1, zz)
  zz <- array(rep(zz, dd),
    dim = c(dim(zz)[1], dim(zz)[2] * dd))
}
