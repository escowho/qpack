#' @title Creates a set of frequency tables for each variable in a dataframe
#' @description Creates a set of frequency tables for each variable in a dataframe
#' and exports a list of tables that can be exported to Excel.
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

create_frequencies <- function(data, metadata=NULL, output=NULL, level_cutoff=55,
                               keep_na=FALSE){

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

  if (from_create_codebook==FALSE){
    flist  <- purrr::map_dfr(data, dplyr::n_distinct) %>%
      tidyr::pivot_longer(dplyr::everything(), names_to="Column", values_to="Unique")  %>%
      dplyr::filter(Unique > level_cutoff) %>%
      dplyr::select(Column) %>%
      purrr::pluck(1)
  } else {
    flist <- codebook %>%
      dplyr::filter(Unique > level_cutoff) %>%
      dplyr::select(Column) %>%
      purrr::pluck(1)
  }

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

    #if(rlang::f_text(VARIABLE) %in% problems){
    #if(variable %in% problems){
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

      title <- tibble::tibble(x = character(),
                              VALUE = character(),
                              label = character(),
                              n = numeric(),
                              percent = character())

      output <- dplyr::bind_rows(title, output) %>%
        dplyr::rename({{variable}} := x)  %>%
        dplyr::mutate(label = ifelse(is.na(label) & VALUE == "Total", "Total", label)) %>%
        dplyr::mutate(label = ifelse(VALUE=="999", "All other values (only a sample shown)", label))

    } else {
      output <- output %>%
        dplyr::mutate(label = NA) %>%
        dplyr::relocate(label, .after="VALUE")

      title <- tibble::tibble(x = character(),
                              VALUE = character(),
                              label = character(),
                              n = numeric(),
                              percent = character())

      output <- dplyr::bind_rows(title, output) %>%
        dplyr::mutate(x = ifelse(is.na(x), NA_character_, x)) %>%
        dplyr::rename({{variable}} := x) %>%
        dplyr::mutate(label = ifelse(VALUE=="999", "All other values (only a sample shown)", label))

    }

    return(output)

  }
  # -------------------------------------------------------------------------

  frequencies <- vector(mode="list", length = 0)
  for (var in names(data)){
    frequencies[[var]] <- run_freq(data, variable=var, cutoff=level_cutoff)
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

      if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
        pb <- txtProgressBar(min = 0, max = length(names(data)), style = 3, width = 50, char = "=")
      }

      for (var in 1:length(names(data))){

        if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
          setTxtProgressBar(pb, var)
        }

        qpack::write_xlsx(data=frequencies[[var]], file=output,
                          sheet=as.character(Key$Number[[var]]),
                          keepna=keep_na, overfile=FALSE)
      }
    }
  } else {
    if (from_create_codebook==TRUE){
      return(list(key=codebook, frequencies=frequencies))
    } else {
      return(list(key=Key, frequencies=frequencies))
    }
  }

  if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
    beepr::beep()
  }

}
