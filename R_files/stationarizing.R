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
  s.hat = season(x, 12)
  x.1 = x-s.hat
  plot(x.1,
       type="l",
       xlab="t",
       ylab=expression("d"["t"]),
       main = expression("Plot of "*"d"["t"]))
  #removing trend with polynomial of degree n
  m.hat = trend(x.1, 2)
  x.2 = x.1 - m.hat
  plot(x.2,
       type="l",
       xlab="t",
       ylab=expression("r"["t"]),
       main = expression("Plot of "*"r"["t"]))
  return(x.2)
}
