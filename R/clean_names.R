#' @title Wrapper function that uses janitor::clean_names on a data set
#' @description Convenience wrapper that uses janitor::clean_names to clean up
#' variable names and returns the dataset.  Optional behavior to return a list
#' where vars = tibble of original variable names and data = the original data
#' with cleaned variables names.
#' @param data The dataframe whose variable names need cleaning.
#' @param create_list A logical indicating if list object containing a table of
#' the transformations and an object containing the transformed data should be
#' returned (TRUE) or just the data (FALSE) Default: FALSE.
#' @return Either the cleaned dataframe or a list containing two tibbles, one for
#' the variable names and one with the cleaned data, unless data_only=TRUE option
#' which returns just the data tibble.
#'
#' @examples
#' \dontrun{
#' clean_names(caddat)
#' clean_names(caddat, create_list=TRUE)
#' }
#'
#' @export
#' @importFrom janitor clean_names
#' @importFrom tibble tibble
#' @importFrom cli cli_abort

clean_names <- function(data, create_list=FALSE){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  # Function ----------------------------------------------------------------

  old_names <- names(data)
  data <- janitor::clean_names(data)
  new_names <- names(data)

  vars <- tibble::tibble(original=old_names, clean_vars=new_names)

  if (create_list==TRUE){
    output <- list(vars=vars, data=data)
  } else {
    output <- data
  }

  return(output)
}
