bootmean2 <- function(x) {
  samplemean <- function(x, d) {
    return(mean(x[d]))
  }
  boot::boot(x, samplemean, R=1000)
}
bootmean <- function(x) {
  b <- bootmean2(x)
  mean(b$t[,1])
}
bootlower <- function(x, ci_low = 0.05) {
  b <- bootmean2(x)
  quantile(b$t[,1], ci_low)
}
bootupper <- function(x, ci_upp = 0.95) {
  b <- bootmean2(x)
  quantile(b$t[,1], ci_upp)
}

samplemedian <- function(x, d) {
  return(median(x[d]))
}

#http://www.mayin.org/ajayshah/KB/R/documents/boot.html

