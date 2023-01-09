#' @title Function to calculate NPS score from Likelihood to Recommend data
#' @description Calculates the NPS Score (100, 0, -100) from vector of data containing
#' 0 to 10 Likelihood to Recommend data.
#' @param data The dataframe column containing the LTR data
#' @return Vector of numbers indicating the NPS score
#' @examples
#' \dontrun{
#' if(interactive()){
#'  set.seed(423234)
#'  tibble::tibble(ltr=sample(0:11, 20, replace=TRUE)) %>%
#'    dplyr::mutate(score = nps_score(ltr))
#'  }
#' }
#' @export
#' @importFrom dplyr case_when

nps_score <- function(data){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data column must be specified.")
  }

# Function ----------------------------------------------------------------

  if (is.data.frame(data)){
    stop(call. = FALSE, "Function only works on vectors.")
  } else {
    dplyr::case_when(data >= 9 ~ 100,
              data >= 7 ~ 0,
              data >= 0 ~ -100,
              TRUE ~ NA_real_)
  }
}
