#' @title Flip transposes a dataframe with t() but outputs a Tibble
#' @description Uses t() to transpose a dataframe but outputs a Tibble instead.
#' Maintains the row names that a Tibble typically removes.
#' @param data A dataframe to be transposed.  Required.
#' @return A Tibble of the original dataframe, transposed.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  flip(caddat)
#'  }
#' }
#' @export
#' @importFrom tibble as_tibble
#' @importFrom cli cli_abort

flip <- function(data){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  if (is.data.frame(data) == FALSE){
    cli::cli_abort("Specified file must be a dataframe.")
  }

  # Function ----------------------------------------------------------------

  output <- cbind(data.frame(`_V_`=rownames(t(data))), t(data)) %>%
    tibble::as_tibble()

  names(output) <- c("column", paste0("row_", 1:(ncol(output)-1)))

  return(output)
}
