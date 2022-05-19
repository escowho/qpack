#' @title Generate Variable and Value Labels Exported Data Containing Labels
#' @description Generates a list of two dataframes containing variable and value
#' label information that is found from Qualtrics and exported via qualtRics::fetch_survey
#' or imported from an SPSS .sav file using haven::read_spss.
#' @param data Name of the dataframe after importing.  Required.
#' @return A list containing two dataframes; var_labels and val_labels.  var_labels
#' contains two columns, variable and variable_label and each row lists the name
#' of a variable in the dataframe along with the variable label found.  val_labels
#' contains multiple columns.  The first is value, which lists all numeric values
#' possible in the dataframe and then a column for each variable in the dataset
#' with the value labels that correspond to the values in the value column.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  pull_labels(dat1)
#'  }
#' }
#' @export
#' @importFrom purrr map is_null reduce
#' @importFrom tibble enframe tribble as_tibble
#' @importFrom tidyr unnest
#' @importFrom rlang enquo
#' @importFrom magrittr set_colnames
#' @importFrom utils stack
#' @importFrom dplyr mutate_if slice mutate full_join arrange
#' @importFrom stringr str_replace_all

pull_labels <- function(data){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  # Helper Functions --------------------------------------------------------

  var_tb <- function(dat0){
    look <- purrr::map(dat0, ~attr(.,'label')) %>%
      purrr::map(~ifelse(purrr::is_null(.),NA, .)) %>%
      purrr::map(~ifelse(!is.character(.), NA, .)) %>%
      tibble::enframe(name = "var", value = "var_label") %>%
      tidyr::unnest(cols = c(var_label))
  }

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

  # Function ----------------------------------------------------------------

  dat0 <- data %>%
    dplyr::slice(0)

  #Variable Labels

  var_labs <- var_tb(dat0) %>%
    magrittr::set_colnames(c("variable", "variable_label")) %>%
    dplyr::mutate(variable_label = stringr::str_replace_all(variable_label, c("\r"=" ", "\n" = " ")))

  #Value Labels
  var_names <- names(dat0)

  hold <- vector(mode="list", length = 0)

  for (name in var_names){
    hold[[name]] <- val_tb(dat0, name)
  }

  var_lbls <- hold %>%
    purrr::reduce(dplyr::full_join, by="VALUE") %>%
    dplyr::arrange(VALUE)

  output <- list(variable_labels=var_labs, value_labels=var_lbls)
  return(output)

}
