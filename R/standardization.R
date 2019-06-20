#' standardization
#'
#' Standardization is a popular tool for data transformation. It should ensure zero mean and unit variance. For further details see \url{https://towardsdatascience.com/normalization-vs-standardization-quantitative-analysis-a91e8a79cebf}.
#' @param x a input vector
#' @return standardized vector \code{x}
#' 
#' @importFrom stats sd
#' 
#' @examples
#' example_vector=c(1,2,3,4,5,6,7,8,9,10)
#' standardization(example_vector)
#' @export

standardization <- function(x) {
  y<-(x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
  return(y)
}
