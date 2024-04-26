#' @title zap_labels
#' @description Removes labels often found when importing SPSS datasets.  Uses
#' \code{sjlabelled::remove_all_labels} to remove variable labels and value labels,
#' also uses \code{haven::zap_formats} and  \code{haven::zap_widths} to remove
#' extraneous formats and widths missed by \code{sjlabelled::remove_all_labels}.
#' @param data Dataframe to be zapped
#' @return Dataframe identical to original but without the labels
#' @examples
#' \dontrun{
#' if(interactive()){
#'  head(test1)
#'  head(zap_labels(test1))
#'  }
#' }
#' @export
#' @importFrom haven zap_formats zap_widths
#' @importFrom sjlabelled remove_all_labels

zap_labels <- function(data){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }


  # Function ----------------------------------------------------------------

  data %>%
    haven::zap_formats(.) %>%
    haven::zap_widths(.) %>%
    sjlabelled::remove_all_labels(.)
}
