#' @title Creates a set of frequency tables for each variable in a dataframe
#' @description Creates a set of frequency tables for each variable in a dataframe
#' and exports a list of tables that can be exported to Excel.
#' @param data Name of the dataframe upon which to build the codebook.  Required.
#' @param output Character string containing the complete path and file name of
#' an XLSX file for exporting the resulting dataframe to Excel format.  Optional.
#' @param level_cutoff A cut-off value for printing frequencies.  A variable with
#' this number of values or less will have a frequency table generated, if specified,
#' otherwise a frequency table will not be generated. Default: 55
#' @return Either a dataframe or exports an Excel file in .xlsx format.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_frequencies(test1)
#'  }
#' }
#' @export
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr n_distinct everything filter select mutate_at vars all_of rename mutate full_join relocate bind_rows
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom rlang enquo quo_name
#' @importFrom tibble tibble
#' @importFrom fs path_dir file_exists

create_frequencies <- function(data, output=NULL, level_cutoff=55){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  # Determine if this was called from create_codebook or not ----------------
  if (exists("codebook")==TRUE & exists("label_file")==TRUE){
    from_create_codebook <- TRUE
  } else {
    from_create_codebook <- FALSE
  }

  frequencies <- vector(mode="list", length = 0)

  if (from_create_codebook==FALSE){
    flist  <- purrr::map_dfr(data, dplyr::n_distinct) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to="Column", values_to="Unique")  %>%
      dplyr::filter(Unique > level_cutoff) %>%
      dplyr::select(Column) %>%
      purrr::pluck(1)

    label_data <- qpack::pull_labels(data)
  } else {
    flist <- codebook %>%
      dplyr::filter(Unique > level_cutoff) %>%
      dplyr::select(Column) %>%
      purrr::pluck(1)
  }



  # Helper Function ---------------------------------------------------------
  too_many <- function(x, cutoff=level_cutoff){
    x <- paste0("More than ", cutoff, " levels detected, frequency not generated")
  }
  # -------------------------------------------------------------------------



  data <- data %>%
    dplyr::mutate_at(dplyr::vars(dplyr::all_of(flist)), too_many, level_cutoff)

  # Helper Function ---------------------------------------------------------
  run_freq <- function(data, variable, value_labels){
    VARIABLE <- rlang::enquo(variable)
    output <- qpack::freq(data, !!VARIABLE) %>%
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
  # -------------------------------------------------------------------------

  for (var in names(data)){
    frequencies[[var]] <- run_freq(data, {{var}}, label_data$value_labels)
  }

  Key <- label_data$variable_labels %>%
    dplyr::mutate(Number = 1:nrow(.)) %>%
    dplyr::select(Number, Variable=variable)

  if (is.null(output) == FALSE){
    if (file.exists(fs::path_dir(output))==FALSE){
      stop(call. = FALSE,
           paste0("Specified path does not exist:",
                  "\n",fs::path_dir(output)))
    } else {

      if (fs::file_exists(output)==FALSE){
        qpack::write_xlsx(data=Key, file=output, sheet="Key", overfile=TRUE)
      }

      for (var in 1:length(names(data))){
        qpack::write_xlsx(data=frequencies[[var]], file=output,
                          sheet=as.character(Key$Variable[[var]]), keepna=TRUE, overfile=FALSE)
      }
    }
  } else {
    if (from_create_codebook==TRUE){
      return(list(key=codebook, frequencies=frequencies))
    } else {
      return(list(key=Key, frequencies=frequencies))
    }
  }
}
