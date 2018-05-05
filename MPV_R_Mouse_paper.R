# MPV R code from Warren et al. 2018

samplemean <- function(x, d) {
  return(mean(x[d]))
}

MPVl <- function(a, b) {
  x <- boot(data=a, statistic = samplemean, R = 9999)
  m <- boot.ci(x, type="basic")$basic[1,4]
  y <- boot(data=b, statistic = samplemean, R = 9999)
  n <- boot.ci(y, type="basic")$basic[1,4]
  z <- 0.5 * (m + n)
  return(z)
}

MPVu <- function(a, b) {
  x <- boot(data=a, statistic = samplemean, R = 9999)
  o <- boot.ci(x, type="basic")$basic[1,5]
  y <- boot(data=b, statistic = samplemean, R = 9999)
  p <- boot.ci(y, type="basic")$basic[1,5]
  w <- 0.5 * (o + p)
  return(w)
}
