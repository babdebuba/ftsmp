# build the variable zz of the overall model
zz_build <- function(yraw, pp, hh, dd, predictor) {
  zz <- stats::embed(yraw, pp)
  if (predictor == 0) {
    zz <- zz[1:(dim(zz)[1] - hh), ]
  } else {
    zz <- zz[-(1:hh), ]
  }
  # zz <- cbind(1, zz)
  # zz <- array(rep(zz, dd),
  #   dim = c(dim(zz)[1], dim(zz)[2] * dd))
  zz <- diag(dd) %x% zz
  temp <- zz
  for (i in 1:tt) {
    zz[((i - 1) * dd + 1):(i * dd), ] <- temp[seq(from = i,
      to = (i - 1) + dd * tt, by = tt), ]
  }
  zz <- cbind(t(structure(replicate(tt, diag(dd)),
    dim = c(dd, tt * dd))), zz)
  zz <- Matrix::Matrix(zz, sparse = T)
}
