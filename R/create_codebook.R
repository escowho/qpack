#' @title Creates a codebook or data dictionary for a dataframe
#' @description Creates a codebook or data dictionary for a dataframe in dataframe
#' format that can be exported to Excel.  Provides a list of the variables (Column),
#' randomly pulls an example value for each one (Example), indicates the type of
#' variable (Type), identifies the number of unique levels (Unique) as well as
#' the percent of records that are missing (Missing).  Leaves spaces for a
#' Description and a Note in the file.  Option to also create frequencies for
#' each variable in the dataframe and export to a separate Excel file.  Changes
#' to API mean that value labels are no longer exported directly.  It is recommended
#' that you download an SPSS file and read in using haven::read_spss to preserve
#' these labels, however, a metadata option is provided if this is not possible.
#' Please be aware that the application of value labels from metadata is not perfect
#' and may result in unusual or incorrect label application, so use at your own
#' risk.
#' @param data Name of the dataframe upon which to build the codebook.  Required.
#' @param metadata Name of the list output from qualtRics::metadata('surveyID').
#' Optional.
#' @param output Character string containing the complete path and file name of
#' an XLSX file for exporting the resulting dataframe to Excel format.  If
#' frequencies are also created, the name of the frequency file will be identical
#' but have " - Frequencies" appended.  Optional.
#' @param freqs A logical value indicating if create_frequencies should be called
#' and the output incorporated into the codebook output set with frequencies for
#' each variable also generated. Default: FALSE
#' @param level_cutoff A cut-off value for printing frequencies.  A variable with
#' this number of values or less will have a frequency table generated, if specified,
#' otherwise a frequency table will not be generated. Default: 55
#' @param keep_na A logical value indicating if NA values should be excluded when
#' output to Excel.  In most instances, this value should be kept at FALSE but
#' on some occasions, output excluding NA's creates problems with formatting.
#' Default: FALSE
#' @return Either a dataframe or exports an Excel file in .xlsx format.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_codebook(caddat)
#'  create_codebookd(dat1, metadata=mdata1)
#'  create_codebook(test1, freqs=TRUE)
#'  }
#' }
#' @export
#' @importFrom dplyr mutate_if na_if mutate select everything slice n_distinct left_join
#' @importFrom labelled remove_labels
#' @importFrom purrr map_dfr
#' @importFrom lubridate is.POSIXt is.Date
#' @importFrom tidyr pivot_longer
#' @importFrom fs path_dir file_exists file_delete
#' @importFrom openxlsx createStyle createWorkbook addWorksheet writeData setColWidths addStyle saveWorkbook
#' @importFrom cli cli_abort cli_warn

create_codebook <- function(data, output=NULL, metadata=NULL, level_cutoff=55, keep_na=FALSE, freqs=FALSE){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  # Helper Functions --------------------------------------------------------

  pull_random <- function(v){
    v <- v[!is.na(v)]
    set.seed(2345)
    v <- v[sample(1:length(v), 1)]
    return(v)
  }

  # Function ----------------------------------------------------------------

  data <- dplyr::mutate_if(data, is.character, dplyr::na_if, "")

  if (is.null(metadata)){
    label_data <- qpack::pull_labels(data=data)
  } else {
    label_data <- qpack::pull_labels(data=data, meta_file=metadata)
  }

  c1 <- label_data$variable_labels %>%
    dplyr::mutate(n = 1:nrow(.)) %>%

    dplyr::select(Number=n, Variable = variable, Description = variable_label)

  c2 <- labelled::remove_labels(data) %>%
    purrr::map_dfr(., pull_random) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="Variable", values_to="Example")

  c3 <- labelled::remove_labels(data) %>%
    purrr::map_dfr(., class) %>%
    dplyr::slice(1) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="Variable", values_to="Type")

  c4  <- purrr::map_dfr(data, dplyr::n_distinct) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="Variable", values_to="Unique")

  c5 <- labelled::remove_labels(data) %>%
    purrr::map_dfr(., ~sum(is.na(.))/length(.)) %>%
    tidyr::pivot_longer(everything(), names_to="Variable", values_to="Missing") %>%
    dplyr::mutate(Missing = round(Missing, digits=3), Note=NA_character_)

  codebook <- dplyr::left_join(c1, c2, by="Variable") %>%
    dplyr::left_join(c3, by="Variable") %>%
    dplyr::left_join(c4, by="Variable") %>%
    dplyr::left_join(c5, by="Variable")

  if (is.null(output) == FALSE){
    if (file.exists(fs::path_dir(output))==FALSE){
      cli::cli_abort("Specified path does not exist: {fs::path_dir(output)}")
    } else {

      if (fs::file_exists(output)==TRUE){
        fs::file_delete(output)
      }

      comma_style <- openxlsx::createStyle(numFmt="COMMA")
      percent_style <- openxlsx::createStyle(numFmt="PERCENTAGE")

      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, sheet="Codebook")
      openxlsx::writeData(wb=wb, sheet="Codebook", x=codebook,
                          keepNA=keep_na, rowNames=FALSE)
      openxlsx::setColWidths(wb, sheet="Codebook", cols=c(1:8),
                             widths=c(10, 20, 50, 25, 10.78, 10.78, 10.78, 50))
      openxlsx::addStyle(wb, sheet="Codebook", style=comma_style, rows=1:ncol(data)+1, cols=6)
      openxlsx::addStyle(wb, sheet="Codebook", style=percent_style, rows=1:ncol(data)+1, cols=7)
      openxlsx::saveWorkbook(wb, file = output, overwrite = TRUE)

      if (freqs==TRUE){
        create_frequencies(data, output=output, metadata=metadata,
                           level_cutoff=level_cutoff, keep_na=keep_na)
      }

    }
  } else {
    if (freqs==TRUE){
      return(list(codebook=codebook,
                  frequencies=create_frequencies(data, level_cutoff=level_cutoff)$frequencies))

    } else {
      return(codebook)
    }
  }
}
