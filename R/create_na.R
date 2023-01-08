#' @title Function to fill a value with NA's, defaults to -99
#' @description Works on vectors or data frames.  If a value is provided, then
#' each will occurrence with be replaced with an NA.  Defaults to look for the
#' value -99, the common NA value in Qualtrics survey datasets.
#' @param data a vector or dataframe object. Required.
#' @param value the value to be replaced. Default: -99.
#' @return A vector or dataframe object, depending on what was originally specified
#' @examples
#' \dontrun{
#' set.seed(1234)
#' m <- matrix(sample(c(-99, 1:10), 100, replace = TRUE), 10) %>% tibble::as_tibble()
#' m
#' m %>% create_na()
#' }
#' @export
#' @importFrom tibble as_tibble

create_na <- function(data, value=-99){

  #NOTE:  Add replacing blanks with NA_character_

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  # Function ----------------------------------------------------------------

  if (is.data.frame(data)){
    lapply(data, function(x) replace(x, x==value, NA)) %>%
    tibble::as_tibble()
    } else {
      ifelse(data==value, NA, data)
    }
}
