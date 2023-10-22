#' @title Function to rescale a vector to a new minimum and maximum range
#' @description Rescales a vector so that range conforms to a defined minimum
#' and maximum.  If no minimum or maximum is provided, then the default is to
#' rescale to a minimum of 0 and a maximum of 1.  This only works on vectors and
#' not dataframes.
#' @param data a vector to be rescaled. Required.
#' @param min_val a value to indicate the new minimum value required after rescaling.
#' Default: 0.
#' @param max_val a value to indicate the new maximum value required after rescaling.
#' Default: 1.
#' @param flip a logical indicating if the vector should simply be flipped so that
#' the max is recoded to the min and the min recoded to the max.  Default: FALSE.
#' @return A vector or dataframe object, depending on what was originally specified
#' @examples
#' \dontrun{
#' set.seed(1234)
#' m <- data.frame(matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10))
#'
#' m %>%
#'   dplyr::select(X1) %>%
#'   dplyr::mutate(new = rescale(X1))
#'
#' m %>%
#'   dplyr::select(X2) %>%
#'   dplyr::mutate(new = rescale(X2, min_val=1, max_val=100))
#'
#' m %>%
#'   dplyr::select(X3) %>%
#'   dplyr::mutate(new = rescale(X3, min_val=1, max_val=7))
#'
#' m %>%
#'   dplyr::select(X1) %>%
#'   dplyr::mutate(new = rescale(X1, flip=TRUE))
#' }
#' @export

rescale <- function(data, min_val, max_val, flip=FALSE){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  if (flip==TRUE){

    new_min <- max(data, na.rm=TRUE)
    new_max <- min(data, na.rm=TRUE)

  } else {

    if (missing(min_val) == TRUE){
      warning(call. = FALSE, "Using MIN_VAL = 0")
      new_min <- 0
    } else {
      new_min <- min_val
    }

    if (missing(max_val) == TRUE){
      warning(call. = FALSE, "Using MAX_VAL = 1")
      new_max <- 1
    } else {
      new_max <- max_val
    }
  }

  # Function ----------------------------------------------------------------

  old_min <- min(data, na.rm=TRUE)
  old_max <- max(data, na.rm=TRUE)

  data <- (data - old_min) * (new_max - new_min) / (old_max - old_min) + new_min

  return(data)
}
