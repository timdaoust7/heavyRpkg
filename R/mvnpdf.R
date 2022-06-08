#' mvnpdf
#' compute the value of the density of a multivariate normal distribution
#'
#' @param x a matrix of n columns and p rows
#' @param mean a vector of means
#' @param varcovM a variance covariance matrix
#' @param Log a logical parameter to return values on a log scale, default = TRUE
#'
#' @return A list is returned with the input matrix, and a vector of density values
#' @export
#'
#' @importFrom  mvtnorm dmvnorm
#'
#' @examples
#'
#'mvnpdf(x=matrix(rep(1.96,2), nrow=2,ncol=1), Log=FALSE)
#'
# mvnpdf <- function(x, mean = rep(0,nrow(x)), varcovM=diag(nrow(x)), Log=TRUE) {
#   n <- ncol(x)
#   p <- nrow(x)
#   values <- c()
#   for(i in 1:n){
#     value <-  -p/2*log(2*pi) - 0.5*det(varcovM) - 0.5*t(x - mean)*solve(varcovM)*(x-mean)
#     values <- c(values,value)
#   }
#   if(!Log){
#     values <- exp(values)
#   }
#   res <- list(x, values)
#   return(res)
# }

mvnpdf <- function(x, mean =  rep(0, nrow(x)),
                   varcovM = diag(nrow(x)), Log = TRUE) {
  n <- ncol(x)
  p <- nrow(x)
  x0 <- x - mean
  Rinv <- solve(varcovM)
  LogDetvarcovM <- log(det(varcovM))

  y <- NULL
  for (j in 1:n) {
    yj <- - p/2 * log(2*pi) - 0.5 * LogDetvarcovM -
      0.5 * t(x0[, j]) %*% Rinv %*% x0[, j]
    y <- c(y, yj)
  }

  if (!Log) {
    y <- exp(y)
  }

  res <- list(x = x, y = y)
  return(res)
}



