#' @title Provides descriptive statistics for a dataframe, similar to Python's Describe
#' @description Provides descriptive statistics for a dataframe, similar to Python's
#' Describe.  Returns the variable name, mean, standard deviation, number of valid
#' responses, number of missing responses, minimum value, 10\%, 25\%, median,
#' 75\%, 90\%, and maximum value quartiles.  Currently limited to numeric data only;
#' will exclude any non-numeric data from output and listed as NA in output.
#' @param data Dataframe to be described.  Required.
#' @param round A logical value indicating if output should be rounded for
#' legibility. Default: TRUE
#' @return Outputs a tibble with the summary statistics
#' @examples
#' \dontrun{
#' if(interactive()){
#'  describe(caddat)
#'  }
#' }
#' @export
#' @importFrom dplyr select_if summarize_all everything mutate arrange select left_join
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom stringr str_sub
#' @importFrom magrittr set_colnames
#' @importFrom tibble tibble
#' @importFrom labelled remove_labels

describe <- function(data, round=TRUE){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  # Function ----------------------------------------------------------------
  data <- labelled::remove_labels(data)

  all <- names(data)

  n <- data %>%
    dplyr::select_if(., is.numeric)

  vars <- names(data)

  n <- n %>%
    dplyr::summarize_all(list(avg = ~mean(., na.rm=TRUE),
                              std = ~sd(., na.rm=TRUE),
                              nnn = ~sum(!is.na(.)),
                              xxx = ~sum(is.na(.)),
                              min = ~min(., na.rm=TRUE),
                              q10 = ~quantile(., probs=.10, na.rm=TRUE, names=FALSE),
                              q25 = ~quantile(., probs=.25, na.rm=TRUE, names=FALSE),
                              med = ~median(., na.rm=TRUE),
                              q75 = ~quantile(., probs=.75, na.rm=TRUE, names=FALSE),
                              q90 = ~quantile(., probs=.90, na.rm=TRUE, names=FALSE),
                              max = ~max(., na.rm=TRUE)
    )) %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="var", values_to="value") %>%
    dplyr::mutate(stat = stringr::str_sub(var, -3),
                  var = stringr::str_sub(var, 1, -5)) %>%
    tidyr::pivot_wider(values_from=value, names_from=stat) %>%
    dplyr::mutate(var = factor(var, levels=vars )) %>%
    dplyr::arrange(var) %>%
    dplyr::mutate(var = as.character(var)) %>%
    dplyr::select(var, avg, std, nnn, xxx, min, q10, q25, med, q75, q90, max) %>%
    magrittr::set_colnames(c("Variable", "Mean", "SD", "n", "Missing",
                             "Min", "Q10", "Q25", "Med", "Q75", "Q90", "Max"))

  n <- tibble::tibble(Variable=all) %>%
    dplyr::left_join(., n, by="Variable")

  if (round==TRUE){
    n <- n %>%
      dplyr::mutate(Mean = round(Mean, 2),
                    SD = round(SD, 2),
                    n = round(n, 1),
                    Missing = round(Missing, 1),
                    Min = round(Min, 1),
                    Q10 = round(Q10, 1),
                    Q25 = round(Q25, 1),
                    Med = round(Med, 1),
                    Q75 = round(Q75, 1),
                    Q90 = round(Q90, 1),
                    Max = round(Max, 1))
  }

  return(n)
}
