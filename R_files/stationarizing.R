#First removes seasonality with period d, then trend with degree n
stationarizing <- function(x, d, n) {
  if(!is.atomic(x) | !is.numeric(x)) {
    stop("First argument must be a numeric vector")
  }
  if(!all.equal(d, as.integer(d))) {
    stop("Second argument must be an integer")
  }
  if(!all.equal(n, as.integer(n))) {
    stop("Third argument must be an integer")
  }
  #deseasonalizing with period d
  x.1 = Resid(x, c("season", d))
  plot(x.1,
       type="l",
       xlab="t",
       ylab=expression("d"["t"]),
       main = expression("Plot of "*"d"["t"]))
  #removing trend with polynomial of degree n
  x.2 = Resid(x.1, c("trend", n))
  plot(x.2,
       type="l",
       xlab="t",
       ylab=expression("u"["t"]),
       main = expression("Plot of "*"d"["u"]))
  return(x.2)
}
