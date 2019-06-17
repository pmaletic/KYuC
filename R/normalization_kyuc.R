#' normalization_kyuc
#'
#' Normalization is a popular tool, which ensure same range [0,1] for all numerical variables.For further details see \url{https://towardsdatascience.com/normalization-vs-standardization-quantitative-analysis-a91e8a79cebf}.
#' @return Normalized vector \code{x}
#' @examples
#' example_vector=c(1,2,3,4,5,6,7,8,9,10)
#' normalization_kyuc(example_vector)
#' @export
normalization_kyuc <- function(x) {
  y<-(x - min(x)) / (max(x)-min(x))
  return(y)
}
