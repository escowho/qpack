#' @title Creates a codebook or data dictionary for a dataframe
#' @description Creates a codebook or data dictionary for a dataframe in dataframe
#' format that can be exported to Excel.  Provides a list of the variables (Column),
#' randomly pulls an example value for each one (Example), indicates the type of
#' variable (Type), identifies the number of unique levels (Unique) as well as
#' the percent of records that are missing (Missing).  Leaves spaces for a
#' Description and a Note in the file.
#' @param data Name of the dataframe upon which to build the codebook.  Required.
#' @param output Character string containing the complete path and file name of
#' an XLSX file for exporting the resulting dataframe to Excel format.  Optional.
#' @return Either a dataframe or exports an Excel file in .xlsx format.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_codebook(caddat)
#'  }
#' }
#' @export
#' @importFrom dplyr mutate_if na_if mutate everything slice n_distinct left_join
#' @importFrom tibble tibble
#' @importFrom purrr map_dfr
#' @importFrom lubridate is.POSIXt is.Date
#' @importFrom tidyr pivot_longer
#' @importFrom fs path_dir
#' @importFrom sjlabelled remove_all_labels

create_codebook <- function(data, output=NULL){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  # Helper Function ---------------------------------------------------------

  pull_random <- function(v){
    v <- v[!is.na(v)]
    set.seed(2345)
    v <- v[sample(1:length(v), 1)]
    return(v)
  }

  # Function ----------------------------------------------------------------

  data <- dplyr::mutate_if(data, is.character, dplyr::na_if, "") %>%
    sjlabelled::remove_all_labels()

  c1 <- tibble::tibble(Column=names(data)) %>%
    dplyr::mutate(Description="")

  c2 <- purrr::map_dfr(data, pull_random) %>%
    dplyr::mutate_if(is.numeric, as.character) %>%
    dplyr::mutate_if(is.logical, as.character) %>%
    dplyr::mutate_if(lubridate::is.POSIXt, as.character) %>%
    dplyr::mutate_if(lubridate::is.Date, as.character) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="Column", values_to="Example")

  c3 <- purrr::map_dfr(data, class) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="Column", values_to="Type")

  c4  <- purrr::map_dfr(data, dplyr::n_distinct) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="Column", values_to="Unique")

  c5 <- purrr::map_dfr(data, ~sum(is.na(.))/length(.)) %>%
    tidyr::pivot_longer(everything(), names_to="Column", values_to="Missing") %>%
    dplyr::mutate(Missing = round(Missing, digits=3),
                  Note="")

  codebook <- dplyr::left_join(c1, c2, by="Column") %>%
    dplyr::left_join(c3, by="Column") %>%
    dplyr::left_join(c4, by="Column") %>%
    dplyr::left_join(c5, by="Column")

  if (is.null(output) == FALSE){
    if (file.exists(fs::path_dir(output))==FALSE){
      stop(call. = FALSE,
           paste0("Specified path does not exist:",
                  "\n",fs::path_dir(output)))
    } else {
      write_xlsx(data=codebook, file=output, overfile=TRUE)
    }
  } else {
    return(codebook)
  }
}







