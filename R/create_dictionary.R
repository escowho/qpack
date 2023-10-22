#' @title Wrapper function that uses sjPlot::view_df to create an html data dictionary
#' @description Convenience wrapper that uses sjPlot::view_df to create an html
#' data dictionary.  sjPlot::view_df is particularly good at pulling out labelled
#' data that is usually found in Qualtrics datasets downloaded via the API or
#' from an SPSS file.  The default output will list the variable name, type, label
#' (if found), number of missing, values, value labels (if found), and frequencies.
#' Options set to also display limited number of character variables.  A more
#' basic version is also available that just shows variable labels and values
#' labels wit the type option.  Output is in html format and is best viewed by
#' launching outside R/Rstudio.
#' @param data Name of the dataframe from which to build the dictionary.  Required.
#' @param output Character string containing the path and file name of the html
#' file.  If nothing is specified it will default to \"Dictionary.html\" in the
#' working folder.  Required.
#' @param type A setting to indicate if the default output or the simple output
#' should be returned. Default: 'default'
#' @param select_vars List of variables to be included in a dplyr::select statement
#' just before the dictionary is created to limit variables included. Default: everything()
#' @param ... Any additional parameters to be passed onto sjPlot::view_df.
#' @return Returns an html file to the location and with the name specified in output
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_directory(test1, output="./output/dictionary.html")
#'  }
#' }
#' @export
#' @importFrom fs path_ext file_exists path_dir path file_delete
#' @importFrom dplyr select mutate_if
#' @importFrom sjPlot view_df
#' @importFrom rlang enquo f_name
#' @importFrom cli cli_abort cli_warn

create_dictionary <- function(data,
                              output=NULL,
                              type="default",
                              select_vars=everything(),
                              ...){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  if (is.null(output)){
    cli::cli_warn("Output not specified; will use \'dictionary.html\'.")
    output <- "dictionary.html"
  } else {
    if (fs::path_ext(output) != "html"){
      fs::path_ext(output) <- "html"
    }

    if (fs::file_exists(fs::path_dir(output))==FALSE){
      cli::cli_abort("Specified path does not exist: {fs::path_dir(output)}")
    } else {
      if(fs::file_exists(fs::path(output))==TRUE){
        invisible(fs::file_delete(fs::path(output)))
      }
    }
  }

  if (! type %in% c("default", "simple")){
    cli::cli_warn("Type specified, {type}, not known. Will use default instead.")

    type <- "default"
  }

  OUTPUT <- rlang::enquo(output)

  # Function ----------------------------------------------------------------

  if(type=="simple"){

    data %>%
      dplyr::select({{select_vars}}) %>%
      dplyr::mutate_if(is.numeric, as.factor) %>%
      sjPlot::view_df(x=., file=rlang::f_name(OUTPUT), ...)

  }  else {

    data %>%
      dplyr::select({{select_vars}}) %>%
      dplyr::mutate_if(is.numeric, as.factor) %>%
      sjPlot::view_df(x=.,
                      file=rlang::f_name(OUTPUT),
                      show.type=TRUE,
                      show.string.values=TRUE,
                      show.frq=TRUE,
                      show.prc=TRUE,
                      show.na=TRUE,
                      max.len=11, ...)
  }
}
