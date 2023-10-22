#' @title Generate Variable and Value Labels Exported Data Containing Labels
#' @description Generates a list of two dataframes containing variable and value
#' label information that is found from Qualtrics and exported via qualtRics::fetch_survey
#' or imported from an SPSS .sav file using haven::read_spss. Changes to API
#' mean that value labels are no longer exported directly.  It is recommended that
#' you download an SPSS file and read in using haven::read_spss to preserve these
#' labels, however, a meta_file option is provided if this is not possible.  Please
#' be aware that the application of value labels from metadata is not perfect and
#' may result in unusual or incorrect label application, so use at your own risk.
#' @param data Name of the dataframe after importing.  Required.
#' @param meta_file Name of the list output from qualtRics::metadata('surveyID').
#' Optional.
#' @return A list containing two dataframes; var_labels and val_labels.  var_labels
#' contains two columns, variable and variable_label and each row lists the name
#' of a variable in the dataframe along with the variable label found.  val_labels
#' contains multiple columns.  The first is value, which lists all numeric values
#' possible in the dataframe and then a column for each variable in the dataset
#' with the value labels that correspond to the values in the value column.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  pull_labels(dat1, meta_file=mdat1)
#'  }
#' }
#' @export
#' @importFrom purrr map is_null reduce map_dfr
#' @importFrom tibble enframe tribble as_tibble tibble
#' @importFrom tidyr unnest pivot_wider pivot_longer
#' @importFrom rlang enquo
#' @importFrom magrittr set_colnames
#' @importFrom utils stack
#' @importFrom dplyr mutate_if slice mutate full_join arrange filter select left_join
#' @importFrom stringr str_replace_all
#' @importFrom cli cli_abort

pull_labels <- function(data, meta_file=NULL){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  # Helper Functions --------------------------------------------------------

  #Variable Label Helper
  var_tb <- function(data){
    purrr::map(data, ~attr(.,'label')) %>%
      purrr::map(~ifelse(purrr::is_null(.),NA, .)) %>%
      purrr::map(~ifelse(!is.character(.), NA, .)) %>%
      tibble::enframe(name = "var", value = "var_label") %>%
      tidyr::unnest(cols = c(var_label)) %>%
      magrittr::set_colnames(c("variable", "variable_label")) %>%
      dplyr::mutate(variable_label = stringr::str_replace_all(variable_label, c("\r"=" ", "\n" = " ")))
  }

  #Helper for when reading up SPSS file
  val_tb <- function(x, variable){
    VARIABLE <- rlang::enquo(variable)
    if(is.null(attr(x[[variable]],'labels'))){
      tibble::tribble(~x, ~y, NA_integer_, NA_integer_) %>%
        magrittr::set_colnames(c("VALUE", variable))
    } else {
      x[[variable]] %>%
        attr('labels') %>%
        utils::stack() %>%
        tibble::as_tibble() %>%
        magrittr::set_colnames(c("VALUE", variable)) %>%
        dplyr::mutate_if(is.factor, as.character)
    }
  }

  #Helper when using meta_file file
  get_labels <- function(data, question){

    output <- data %>%
      .[[question]] %>%
      .[["choices"]] %>%
      purrr::map_dfr(., ~.[["description"]]) %>%
      qpack::flip()

    if (nrow(output)>0){
      output <- output %>%
        dplyr::mutate(column = as.numeric(column)) %>%
        dplyr::arrange(column)
    } else {
      output <- tibble::tibble(column = NA, label=NA)
    }

    output <- output %>%
      qpack::set_colnames(c("VALUE", var_names$variable[[question]]))
    return(output)
  }

# Variable Labels ---------------------------------------------------------

  dat0 <- data %>%
    dplyr::slice(0)

  var_labs <- var_tb(dat0)

# Value Labels ------------------------------------------------------------

    var_names <- names(dat0)

    hold <- vector(mode="list", length = 0)

    for (name in var_names){
      hold[[name]] <- val_tb(dat0, name)
    }

    var_lbls <- hold %>%
      purrr::reduce(dplyr::full_join, by="VALUE") %>%
      dplyr::arrange(VALUE)

  if (is.null(meta_file)==FALSE){

    mdat <- meta_file %>%
      .[["questions"]]

    var_names <- purrr::map(mdat, ~.[["questionName"]]) %>%
      tibble::enframe() %>%
      tidyr::unnest(cols=c(value)) %>%
      tidyr::pivot_wider(id_cols = "name", names_from="value") %>%
      qpack::clean_names() %>%
      tidyr::pivot_longer(-name, names_to="variable") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::select(id=name, variable)

    #get value labels
    hold <- vector(mode="list", length = 0)

    for (i in 1:length(mdat)){
      hold[[i]] <- get_labels(mdat, i)
    }

    var_lbls2 <- hold %>%
      purrr::reduce(dplyr::full_join, by="VALUE") %>%
      dplyr::arrange(VALUE)

    temp1 <- dplyr::select(var_lbls2, VALUE)
    temp2 <- dplyr::select(var_lbls, -VALUE)
    temp3 <- dplyr::bind_cols(temp1, temp2)
    temp3[names(var_lbls2)] <- var_lbls2

    var_lbls <- temp3
  }

  output <- list(variable_labels=var_labs, value_labels=var_lbls)
  return(output)

}
