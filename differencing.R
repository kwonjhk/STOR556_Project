#removes seasonality and trend using differencing
#first differences with lag d to remove seasonality with period d
#then, differences n many times to remove trend component
differencing <- function(x, d, n) {
  if(!is.atomic(x) | !is.numeric(x)) {
    stop("First argument must be a numeric vector")
  }
  if(d<0 | !all.equal(d, as.integer(d))) {
    stop("Second argument must be a positive integer")
  }
  if(n<0 | !all.equal(n, as.integer(n))) {
    stop("Third argument must be a positive integer")
  }
  #deseasonlizing using differencing with lag 12
  x.1 = diff(x, lag=12)
  plot(x.1, 
       type="l", 
       xlab="t", 
       ylab=expression(nabla["12"]*"X"["t"]),
       main=expression("Plot of "*nabla["12"]*"X"["t"]))
  #removing trend using repeated differencing on the deseasonlized series
  x.2 = x.1
  for(i in 1:n) {
    x.2 = diff(x.2, lag=1)
  }
  plot(x.2, 
       type="l",
       xlab="t", 
       ylab=expression(nabla*nabla["12"]*"X"["t"]),
       main=expression("Plot of "*nabla*nabla["12"]*"X"["t"]))
  #return new time series
  return(x.2)
}