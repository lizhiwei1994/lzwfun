#' Basic information for a vector
#'
#' Show mean, sd, min, p25, p50, p75 and max for a vector.
#' @param a vector.
#' @return a dataframe.
#' @examples
#' basis.p(1:5)
#' @export
basis.p <- function(a){
  library(dplyr)
  p = summary(a)
  sd = sd(a, na.rm = T)
  d = as.data.frame(unclass(p))
  d2 = t(d) %>% as.data.frame()
  rownames(d2) = NULL
  d3 = d2[,c('Min.', '1st Qu.', 'Median', 'Mean', '3rd Qu.', 'Max.')]
  d4 = bind_cols(d3, SD = sd)
  colnames(d4) = c('Min', 'P25', 'P50', 'Mean', 'P75', 'Max', 'SD')
  d5 = d4[,c('Mean','SD', 'Min', 'P25', 'P50', 'P75', 'Max')]
  d5
}
