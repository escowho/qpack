#' @title Function to fill NA's with either a specified value or the mean
#' @description Works on vectors or data frames.  If a value is provided, like 0,
#' then each NA will be replaced with the value.  If replace=\"mean\" is specified,
#' then all NA's will be replaced by the mean value for that column.  This works
#' across the dataframe per column.  Median may also be chosen.
#' @param data a vector or dataframe object. Required.
#' @param replace either a value or the quoted character string \"mean\". Default: Mean.
#' @return A vector or dataframe object, depending on what was originally specified
#' @examples
#' \dontrun{
#' set.seed(1234)
#' m <- matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10) %>% tibble::as_tibble()
#' m
#'
#' m %>% fix_na("mean")
#' m %>%
#' mutate(V1 = fix_na(V1, "mean"))
#' m %>% fix_na(0)
#' }
#' @export
#' @importFrom tibble tibble

fix_na <- function(data, replace=NULL){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  #if (missing(data) == FALSE & is.data.frame(data) == FALSE){
  #  stop(call. = FALSE, "Data must be specified.")
  #}

  if (is.null(replace) == TRUE){
    warning("Replace not specified; choosing replace=\"mean\" by default.")
    replace <- "mean"
  }

  # Function ----------------------------------------------------------------

  if (is.character(replace)==TRUE){

    if (tolower(replace)=="mean"){

      if (is.data.frame(data)){
        data <- lapply(data, function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))) %>%
          tibble::as_tibble()
      } else {
        replace <- mean(data, na.rm=TRUE)
        data[is.na(data)] <- replace
      }

    } else if (tolower(replace)=="median"){

      if (is.data.frame(data)){
        data <- lapply(data, function(x) replace(x, is.na(x), median(x, na.rm = TRUE))) %>%
          tibble::as_tibble()
      } else {
        replace <- median(data, na.rm=TRUE)
        data[is.na(data)] <- replace
      }

    } else {
      stop(call. = FALSE, "Replace must be either a numeric value, \"mean\" or \"median\".")
    }

  } else {
    data[is.na(data)] <- replace
  }

  return(data)
}
