#' Z test
#'
#' Test the difference between 2 coefficients.
#' @param b1 coefficients in model 1.
#' @param b2 coefficients in model 2.
#' @param se1 standard error in model 1.
#' @param se2 standard error in model 2.
#' @return Z value and P value.
#' @examples
#' b1 <- 0.122275
#' b2 <- 0.358742
#' se1 <- (0.61492-(-0.36804))/3.92
#' se2 <- (0.883344-(-0.16313))/3.92
#' z.test(b1 = b1, b2 = b2, se1 = se1, se2 = se2)
#' # $z
#' # [1] -0.6456306
#' #
#' # $p
#' # [1] 0.5185186
#' @export
z.test <- function(b1, b2, se1, se2) {

  z <- (b1-b2)/sqrt(se1^2 + se2^2)
  p <- 2*pnorm(-abs(z))
  result <- list(z=z,p=p)
  result

}
