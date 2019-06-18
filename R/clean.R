#' Cleaning function
#' 
#' Data is cleanedd
#'
#' @param df data.frame, data to be cleaned
#' @param id_var character, columnname of dataframe which acts as the ID variable 
#' @param ... args forwarded to \code{\link{read_csv2}}. Hint: You can edit the import column type by the col_types argument.
#' @param na_threshold numeric, if NAs within a column are greater than the treshold given, this columns will be excluded
#'
#' @return data.frame, excluding columns with too many NAs and impute existing NAs in numeric fields with zero
#' 
#' @importFrom readr read_csv2
#' 
#' @export
#' @examples
#' \dontrun{
#' clean(CompanyData2018, id_var = "ID")
#' }
clean <- function(df, id_var, na_threshold = 0.9, ...) {
  
  #if data frame is empty stop
  stopifnot(nrow(df) > 0)
  
  #if id variable is not in data frame stop
  stopifnot(id_var %in% colnames(df))
  
  #if data frame has only id column than stop
  stopifnot(ncol(df) > 1)
  
  ## return an error if the ID variable contains any missing values (NA’s).
  if (any(is.na(df[[id_var]]))) stop("ID Var should not include any NAs")
  
  exclude_index <- vapply(df, function(x) mean(is.na(x)), numeric(1)) > na_threshold
  
  ## give clear warnings for all other variables which contain NA’s.
  if (sum(exclude_index) > 0) warning(paste("Column(s)", paste(colnames(df[exclude_index]), collapse = ", "), "have too many NAs and will be excluded"))
  
  #exclude variables with more than defined treshold of NAs
  df[, !exclude_index]
  
  #replace remaining NAs with zero value
  
  ind <-   which(sapply(df, is.numeric))
  for(j in ind){
    set(df, i = which(is.na(df[[j]])), j = j, value = 0)
  }
  
  df
}


