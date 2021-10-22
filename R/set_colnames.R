#' @title Wrapper for a naming columns with pipes from magrittr::set_colnames
#' @description Convenience wrapper for naming columns with pipes from
#' magrittr::set_colnames so do not have to write out the function name.
#' @param ... Arguments passed to magrittr::set_colnames
#' @return Nothing returned, just assigns column names

#' @examples
#' \dontrun{
#' caddat %>%
#'   select(1:2) %>%
#'   set_colnames(c("r", "s"))
#' }
#' @export
#' @importFrom magrittr set_colnames

set_colnames <- function(...){
  return(magrittr::set_colnames(...))
}
