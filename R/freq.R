#' @title Wrapper for a 1-way table from janitor::tabyl
#' @description Generates a frequency table that is outputed as a janitor::tabyl
#' object.  Also uses quasiquotation from rlang so that variables need not be quoted.
#' @param data Dataframe object containing the variable
#' @param var Variable for the frequency
#' @param ... Additional arguments passed to janitor::tabyl
#' @return A tabyl object

#' @examples
#' \dontrun{
#' freq(caddat, speclty)
#' freq(data=caddat, var=speclty)
#' freq(caddat$speclty)
#' }
#' @export
#' @importFrom janitor tabyl adorn_totals adorn_pct_formatting
#' @importFrom rlang enquo eval_tidy

freq <- function(data, var, ...){

  #tabyl's error checks are sufficient

  if (missing(var)){
    output <- janitor::tabyl(data, ...)

  } else {
    DATA <- rlang::enquo(data)
    VAR <- rlang::enquo(var)
    output <- janitor::tabyl(dat = rlang::eval_tidy(DATA), var1 = !!VAR, ...)
  }

  if ("valid_percent" %in% names(output)){
    output <- output %>%
      janitor::adorn_totals() %>%
      janitor::adorn_pct_formatting(,,,percent:valid_percent)
  } else {
    output <- output %>%
      janitor::adorn_totals() %>%
      janitor::adorn_pct_formatting(,,,percent)
  }

  return(output)
}
