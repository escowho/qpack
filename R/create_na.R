#' @title Function to fill a value with NA's, defaults to -99
#' @description Works on vectors or data frames.  If a value is provided, then
#' each occurrence with be replaced with an NA.  Defaults to look for the
#' value -99, the common NA value in Qualtrics survey datasets.  Can also be used
#' to replace blanks in character vectors with NA values using the "blank" value
#' parameter.
#' @param data a vector or dataframe object. Required.
#' @param value the value to be replaced.  Defaults to -99 but if the word "blank"
#' is used, it will replace blanks in a character vector with NA_character_ values
#' instead. Default: -99.
#' @return A vector or dataframe object, depending on what was originally specified
#' @examples
#' \dontrun{
#' set.seed(1234)
#' test1 <- matrix(sample(c(-99, 1:10), 100, replace = TRUE), 10) %>%
#'   as.data.frame()
#'
#' test1
#'
#' test1 %>%
#'   create_na(., -99)
#'
#' test1 %>%
#'   mutate(V5 = create_na(V5, -99))
#'
#' set.seed(423234)
#' test2 <- tibble(var1=sample(c("a", "b", "c", ""), 10, replace=TRUE),
#'                 var2=sample(c("a", "b", "c", ""), 10, replace=TRUE))
#' test2
#'
#' test2 %>%
#'   create_na(., "blank")
#'
#' test2 %>%
#'   mutate(var1 = create_na(var1, "blank"))
#' }
#' @export
#' @importFrom tibble as_tibble

create_na <- function(data, value=-99){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  # Function ----------------------------------------------------------------

    if (is.data.frame(data)){
      if (tolower(value)=="blank"){
        dplyr::mutate_if(data, is.character, dplyr::na_if, "")
      } else {
        lapply(data, function(x) replace(x, x==value, NA)) %>%
          tibble::as_tibble()
      }
    } else {
      if (tolower(value)=="blank"){
        ifelse(data=="", NA_character_, data)
      } else {
        ifelse(data==value, NA, data)
      }
    }

}
