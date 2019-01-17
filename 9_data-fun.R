dfify <- function(x) {
  y <- sapply(x[2:length(x)], cbind)
  z <- as.data.frame(cbind(x[1], y))
}
