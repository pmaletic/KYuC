#' transform_log_kyuc
#'
#' Log-transformation is a popular tool for dealing with skewness.For further details see https://www.r-statistics.com/2013/05/log-transformations-for-skewed-and-wide-distributions-from-practical-data-science-with-r.
#' @return log-transformed vector \code{x}
#' @examples
#' example_vector=c(1,2,3,4,5,6,7,8,9,10)
#' transform_log(example_vector)
#' @export

transform_log_kyuc <- function(x){

  if( is.null(x) )    stop("Input vector is not allowed to be NULL.")
  if( any(is.na(x)) ) stop("There is at least one NA value in input vector.")
  if( any(x <= 0)  )  stop("There is at least one negative value.")
  if( any(is.numeric(x) == FALSE) )  stop("There is at least one non-numeric value.")
  y<-log(x)
  return(y)

}
