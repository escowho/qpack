#' @title Creates a codebook or data dictionary for a dataframe
#' @description Creates a codebook or data dictionary for a dataframe in dataframe
#' format that can be exported to Excel.  Provides a list of the variables (Column),
#' randomly pulls an example value for each one (Example), indicates the type of
#' variable (Type), identifies the number of unique levels (Unique) as well as
#' the percent of records that are missing (Missing).  Leaves spaces for a
#' Description and a Note in the file.  Option to also create frequencies for
#' each variable in the dataframe and export to a separate Excel file.
#' @param data Name of the dataframe upon which to build the codebook.  Required.
#' @param output Character string containing the complete path and file name of
#' an XLSX file for exporting the resulting dataframe to Excel format.  If
#' frequencies are also created, the name of the frequency file will be identical
#' but have " - Frequencies" appended.  Optional.
#' @param level_cutoff A cut-off value for printing frequencies.  A variable with
#' this number of values or less will have a frequency table generated, if specified,
#' otherwise a frequency table will not be generated. Default: 55
#' @param freqs A logical value indicating if frequencies for each variable should
#' also be generated. Default: FALSE
#' @return Either a dataframe or exports an Excel file in .xlsx format.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_codebook(caddat)
#'  }
#' }
#' @export
#' @importFrom dplyr mutate_if na_if rename everything slice n_distinct mutate left_join filter select mutate_at full_join all_of
#' @importFrom labelled remove_labels
#' @importFrom purrr map_dfr pluck
#' @importFrom lubridate is.POSIXt is.Date
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom rlang enquo
#' @importFrom stringr str_detect str_replace
#' @importFrom fs path_dir
#' @importFrom openxlsx createStyle createWorkbook addWorksheet writeData setColWidths addStyle saveWorkbook


create_codebook <- function(data, output=NULL, level_cutoff=55, freqs=FALSE){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
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

  label_data <- pull_labels(data)

  c1 <- label_data$variable_labels %>%
    dplyr::rename(Column = variable, Description = variable_label)

  data <- data %>%
    #sjlabelled::remove_all_labels() %>%
    labelled::remove_labels()

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

  if (freqs==TRUE){
    frequencies <- vector(mode="list", length = 0)

    flist <- codebook %>%
      dplyr::filter(Unique > level_cutoff) %>%
      dplyr::select(Column) %>%
      purrr::pluck(1)

    too_many <- function(x, cutoff=level_cutoff){
      x <- paste0("More than ", cutoff, " levels detected, frequency not generated")
    }

    data <- data %>%
      dplyr::mutate_at(dplyr::vars(dplyr::all_of(flist)), too_many, level_cutoff)

    run_freq <- function(data, variable, value_labels){
      VARIABLE <- rlang::enquo(variable)
      output <- freq(data, !!VARIABLE) %>%
        dplyr::rename(value = !!VARIABLE)

      labs <- dplyr::select(value_labels, value, !!VARIABLE) %>%
        dplyr::mutate(value = as.character(value)) %>%
        tidyr::drop_na(!!VARIABLE)

      if (nrow(labs) > 0) {
        output <- dplyr::full_join(output, labs, by="value") %>%
          dplyr::rename(label = !!VARIABLE) %>%
          dplyr::relocate(label, .after="value")

        title <- tibble::tibble(x = character(),
                                value = character(),
                                label = character(),
                                n = numeric(),
                                percent = character())

        output <- dplyr::bind_rows(title, output) %>%
          dplyr::mutate(x = ifelse(is.na(x), "", x)) %>%
          dplyr::rename(!!rlang::quo_name(VARIABLE) := x)  %>%
          dplyr::mutate(label = ifelse(is.na(label) & value == "Total", "Total", label))

      } else {
        output <- output %>%
          dplyr::mutate(label = "") %>%
          dplyr::relocate(label, .after="value")

        title <- tibble::tibble(x = character(),
                                value = character(),
                                label = character(),
                                n = numeric(),
                                percent = character())

        output <- dplyr::bind_rows(title, output) %>%
          dplyr::mutate(x = ifelse(is.na(x), "", x)) %>%
          dplyr::rename(!!rlang::quo_name(VARIABLE) := x)
      }

      return(output)
    }

    for (var in names(data)){
      frequencies[[var]] <- run_freq(data, {{var}}, label_data$value_labels)
    }

  }

  if (is.null(output) == FALSE){
    if (file.exists(fs::path_dir(output))==FALSE){
      stop(call. = FALSE,
           paste0("Specified path does not exist:",
                  "\n",fs::path_dir(output)))
    } else {

      comma_style <- openxlsx::createStyle(numFmt="COMMA")
      percent_style <- openxlsx::createStyle(numFmt="PERCENTAGE")

      #write_xlsx(data=codebook, file=output, overfile=TRUE)
      wb <- openxlsx::createWorkbook()
      openxlsx::addWorksheet(wb, sheet="Codebook")
      openxlsx::writeData(wb=wb, sheet="Codebook", x=codebook,
                          keepNA=FALSE, rowNames=FALSE)
      openxlsx::setColWidths(wb, sheet="Codebook", cols=c(1:7),
                             widths=c(20, 50, 25, 10.78, 10.78, 10.78, 50))
      openxlsx::addStyle(wb, sheet="Codebook", style=comma_style, rows=1:ncol(data)+1, cols=5)
      openxlsx::addStyle(wb, sheet="Codebook", style=percent_style, rows=1:ncol(data)+1, cols=6)
      openxlsx::saveWorkbook(wb, file = output, overwrite = TRUE)


      if (freqs==TRUE){
        output_f <- stringr::str_replace(output, ".xlsx", " - Frequencies.xlsx")

        Key <- codebook %>%
          dplyr::mutate(name_length = nchar(Column),
                        Sheet = 1:nrow(codebook)) %>%
          dplyr::select(Sheet, Variable=Column, name_length)

        #if (max(Key$name_length) > 30) {
          Key <- dplyr::select(Key, -name_length)
          write_xlsx(data=Key, file=output_f, sheet="Key", overfile=TRUE)
          for (var in 1:length(names(data))){
            write_xlsx(data=frequencies[[var]], file=output_f,
                       sheet=as.character(Key$Sheet[[var]]), keepna=TRUE, overfile=FALSE)
          }

        #} else {
        #  for (var in names(data)){
        #    if (var == names(data)[1]) {
        #      write_xlsx(data=frequencies[[var]], file=output_f, sheet=var, keepna=TRUE, overfile=TRUE)
        #    } else {
        #      write_xlsx(data=frequencies[[var]], file=output_f, sheet=var, keepna=TRUE, overfile=FALSE)
        #    }
        #  }
        #}


      }
    }
  } else {

    if (freqs==FALSE){
      return(codebook)
    } else {
      return(list(codebook=codebook, frequencies=frequencies))
    }


  }
}
