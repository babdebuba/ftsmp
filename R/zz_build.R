# build the variable zz of the overall model
zz_build <- function(yraw, pp, hh, dd, tt) {
  zz <- stats::embed(yraw, pp)
  zz <- zz[1:(dim(zz)[1] - hh), ]
  zz <- cbind(diag(dd) %x% zz)
  temp <- zz
  for (i in 1:tt) {
    zz[((i - 1) * dd + 1):(i * dd), ] <- temp[seq(from = i,
      to = (i - 1) + dd * tt, by = tt), ]
  }
  zz <- cbind(t(structure(replicate(tt, diag(dd)),
    dim = c(dd, tt * dd))), zz)
}