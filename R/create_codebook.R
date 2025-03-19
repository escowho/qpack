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
#' an XLSX file for exporting the resulting dataframe to Excel format. Optional.
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

  if (ncol(data)>1048576){
    cli::cli_abort("The number of variables {scales::comma(ncol(data))} exceeds the maximum number of rows in Excel (1,048,576).  Codebook cannot be generated.")
  }

  if (freqs==TRUE & ncol(data)>65530){
    cli::cli_abort("The number of variables {scales::comma(ncol(data))} exceeds the maximum number of hyperlinks allowed in Excel (65,530).  Frequencies cannot be run.")
  }

  if (is.null(output)==TRUE & freqs==TRUE){
    cli::cli_abort("Must specifiy output location for Excel file if freqs=TRUE.")
  }

  #if (!is.null(output) & file.exists(fs::path_dir(output))==FALSE){
  #  cli::cli_abort("Specified output path does not exist: {fs::path_dir(output)}")
  #}

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

  if (is.null(output) == TRUE){
    return(codebook)
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

      openxlsx::freezePane(wb, sheet="Codebook", firstRow=TRUE)

      if (freqs==TRUE){

        #Add hyperlinks

        for (v in 1:length(codebook$Variable)) {
          openxlsx::writeFormula(wb, "Codebook",
                                 startCol=2, startRow = 1+v,
                                 x = openxlsx::makeHyperlinkString(
                                   sheet = v, row = 1, col = 1,
                                   text = codebook$Variable[v]))
        }

      openxlsx::saveWorkbook(wb, file = output, overwrite = TRUE)

      create_frequencies(data, output=output, metadata=metadata,
                                 level_cutoff=level_cutoff, keep_na=keep_na)

      } else {

        openxlsx::saveWorkbook(wb, file = output, overwrite = TRUE)
    }
  }
}




#' @title Create Frequencies for Codebook
#' @description Generate frequency tables that are exported into a codebook
#' @param data Name of the dataframe upon which to build the codebook.  Required.
#' @param metadata Name of the list output from qualtRics::metadata('surveyID').
#' Changes to Qualtrics API mean that value labels are no longer exported directly.
#' It is recommended that you download an SPSS file and read in using haven::read_spss
#' to preserve these labels, however, a metadata option is provided if this is not
#' possible.  Please be aware that the application of value labels from metadata
#' is not perfect and may result in unusual or incorrect label application, so
#' use at your own risk.  Optional.
#' @param output Character string containing the complete path and file name of
#' an XLSX file for exporting the resulting dataframe to Excel format.  Optional.
#' @param keep_na A logical value indicating if NA values should be excluded when
#' output to Excel.  In most instances, this value should be kept at FALSE but
#' on some occasions, output excluding NA's creates problems with formatting.
#' Default: FALSE
#' @param level_cutoff A cut-off value for printing frequencies.  A variable with
#' this number of values or less will have a frequency table generated, if specified,
#' otherwise a frequency table will not be generated. Default: 55
#' @return Generates frequencies exported as excel tabs for codebook
#' @keywords internal
#' @importFrom cli cli_abort
#' @importFrom purrr map_dfr pluck
#' @importFrom dplyr n_distinct everything filter select all_of rename mutate group_by tally slice pull mutate_at vars full_join relocate bind_rows case_when arrange desc
#' @importFrom tidyr pivot_longer drop_na
#' @importFrom rlang enquo
#' @importFrom labelled remove_labels
#' @importFrom tibble tibble
#' @importFrom openxlsx loadWorkbook addWorksheet writeData freezePane writeFormula makeHyperlinkString saveWorkbook
#' @importFrom beepr beep

create_frequencies <- function(data, output=NULL, metadata=NULL,
                               level_cutoff=55, keep_na=TRUE){

  flist  <- purrr::map_dfr(data, dplyr::n_distinct) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="Column", values_to="Unique")  %>%
    dplyr::filter(Unique > level_cutoff) %>%
    dplyr::select(Column) %>%
    purrr::pluck(1)

  if (is.null(metadata)){
    label_data <- qpack::pull_labels(data=data)
  } else {
    label_data <- qpack::pull_labels(data=data, meta_file=metadata)
  }

  # Helper Function ---------------------------------------------------------
  run_freq <- function(data, variable, cutoff, problems=flist,
                       value_labels=label_data$value_labels){

    VARIABLE <- rlang::enquo(variable)

    data <- data %>%
      labelled::remove_labels(data)

    if({{variable}} %in% problems){

      new_level <- as.numeric(paste(rep(9, nchar(cutoff)+1), collapse=""))

      acceptable_levels <- data %>%
        dplyr::select(dplyr::all_of({{variable}})) %>%
        dplyr::rename(x={{variable}}) %>%
        dplyr::mutate(y=1) %>%
        dplyr::group_by(x) %>%
        dplyr::tally() %>%
        dplyr::slice(1:cutoff) %>%
        dplyr::select(-n) %>%
        dplyr::pull()

      recode_level <- function(x){
        x <- ifelse(!x %in% acceptable_levels, new_level, x)
      }

      output <- data %>%
        dplyr::select(dplyr::all_of({{variable}})) %>%
        dplyr::rename(VALUE={{variable}}) %>%
        dplyr::mutate_at(dplyr::vars(VALUE), recode_level) %>%
        qpack::freq(., VALUE)

    } else {
      output <- data %>%
        dplyr::rename(VALUE={{variable}}) %>%
        qpack::freq(., VALUE)
    }

    labs <- dplyr::select(value_labels, VALUE, dplyr::all_of(!!VARIABLE)) %>%
      dplyr::mutate(VALUE = as.character(VALUE)) %>%
      tidyr::drop_na(dplyr::all_of(!!VARIABLE))

    if (nrow(labs) > 0) {
      output <- dplyr::full_join(output, labs, by="VALUE") %>%
        dplyr::rename(label = {{variable}}) %>%
        dplyr::relocate(label, .after="VALUE")

      if ("valid_percent" %in% names(output)){
        title <- tibble::tibble(x = character(),
                                VALUE = character(),
                                label = character(),
                                n = numeric(),
                                percent = character(),
                                valid_percent = character())
      } else {
        title <- tibble::tibble(x = character(),
                                VALUE = character(),
                                label = character(),
                                n = numeric(),
                                percent = character())
      }

      output <- dplyr::bind_rows(title, output) %>%
        dplyr::rename({{variable}} := x)  %>%
        dplyr::mutate(label = ifelse(is.na(label) & VALUE == "Total", "Total", label)) %>%
        dplyr::mutate(label = ifelse(VALUE=="999", "All other values (only a sample shown)", label))

    } else {
      output <- output %>%
        dplyr::mutate(label = NA) %>%
        dplyr::relocate(label, .after="VALUE")

      if ("valid_percent" %in% names(output)){
        title <- tibble::tibble(x = character(),
                                VALUE = character(),
                                label = character(),
                                n = numeric(),
                                percent = character(),
                                valid_percent = character())
      } else {
        title <- tibble::tibble(x = character(),
                                VALUE = character(),
                                label = character(),
                                n = numeric(),
                                percent = character())
      }

      output <- dplyr::bind_rows(title, output) %>%
        dplyr::mutate(x = ifelse(is.na(x), NA_character_, x)) %>%
        dplyr::rename({{variable}} := x) %>%
        dplyr::mutate(label = ifelse(VALUE=="999", "All other values (only a sample shown)", label))

    }

    output <- output  %>%
      dplyr::mutate(order1 = 1:nrow(.),
                    order2 = dplyr::case_when(VALUE=="Total" ~ 1,
                                              VALUE=="999" ~ 2,
                                              .default=3)) %>%
      dplyr::arrange(dplyr::desc(order2), order1, VALUE) %>%
      dplyr::select(-c(order1, order2))

    return(output)
    }
  # End Helper Function -----------------------------------------------------

  if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
    cat("Running Frequencies:\n")
  }

  frequencies <- vector(mode="list", length = 0)

  if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
    pb1 <- txtProgressBar(min = 0, max = length(names(data)), style = 3, width = 50, char = "=")
  }

  for (var in names(data)){
    k <- which(names(data)==var)

    if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
      setTxtProgressBar(pb1, k)
    }

    frequencies[[var]] <- run_freq(data, variable=var, cutoff=level_cutoff)
  }

  if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
    cat("\nExporting Frequencies:\n")
  }

  if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
    pb2 <- txtProgressBar(min = 0, max = length(names(data)), style = 3, width = 50, char = "=")
  }

  wb <- openxlsx::loadWorkbook(output)

  for (var in 1:length(names(data))){

    if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
      setTxtProgressBar(pb2, var)
    }

    sheet_name <- as.character(var)

    openxlsx::addWorksheet(wb, sheet=sheet_name)
    openxlsx::writeData(wb=wb, sheet=sheet_name, x=frequencies[[var]],
                        keepNA=keep_na, rowNames=FALSE)
    openxlsx::freezePane(wb, sheet=sheet_name, firstRow=TRUE)
    openxlsx::writeFormula(wb, sheet=sheet_name,
                           startCol=1, startRow = 1,
                           x = openxlsx::makeHyperlinkString(
                             sheet = "Codebook", row = var+1, col = 2,
                             text=names(frequencies[[var]])[1]))
  }

  openxlsx::saveWorkbook(wb, file = output, overwrite = TRUE)

  if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
    beepr::beep()
    cat("\nExport complete.\n")
  }
}

