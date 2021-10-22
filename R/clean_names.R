#' @title Wrapper function that uses janitor::clean_names on a data set
#' @description Convenience wrapper that uses janitor::clean_names to clean up
#' variable names and returns a list where vars = tibble of original variable
#' names and data = the original data with cleaned variables names.
#' @param data The dataframe whose variable names need cleaning.
#' @param data_only A logical indicating if only the data should be returned
#' without the list object. Default: TRUE.
#' @return List containing two tibbles, one for the variable names and one with
#' the cleaned data, unless data_only=TRUE option which returns just the data tibble.
#'
#' @examples
#' \dontrun{
#' clean_names(caddat)
#' clean_names(caddat, data_only=FALSE)
#' }
#'
#' @export
#' @importFrom janitor clean_names
#' @importFrom tibble tibble

clean_names <- function(data, data_only=TRUE){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  # Function ----------------------------------------------------------------

  old_names <- names(data)
  data <- janitor::clean_names(data)
  new_names <- names(data)

  vars <- tibble::tibble(original=old_names, clean_vars=new_names)

  if (data_only==TRUE){
    output <- data
  } else {
    output <- list(vars=vars, data=data)
  }

  return(output)
}
