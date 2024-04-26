#' @title nps_segment
#' @description Creates NPS segment variables using either LTR data (ranging from
#' 0 to 10) or from NPS data (-100, 0, +100).  Creates four variables appended to
#' the dataframe: nps_segment (1= Promoter, 2 = Passive, 3 = Detractor), nps_pro
#' (1 = Promoter, 0 = Not Promoter), nps_pas (1 = Passive, 0 = Not Passive),
#' nps_det (1 = Detractor, 0 = Not Detractor).  The new variables will be located
#' immediately behind the variable that contains the data to be used for classification.
#' @param data Dataframe containing the data.  Required.
#' @param variable Variable inside the dataframe that contains the relevant score
#' to be used to classify into NPS segment.  Must be Likelihood to Recommend type
#' data where the minimum will be 0 and the maximum will be 10, or NPS type category
#' data where there are three possible values, -100, 0, or 100.
#' @return Dataframe identical to the input dataframe but with 4 new variables
#' inserted.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  dat1 <- qpack::test2 %>%
#'  dplyr::mutate(q4 = dplyr::case_when(q3<= 6 ~ -100,
#'                                      q3<= 8 ~ 0,
#'                                      q3>= 9 ~ 100))
#'  nps_segment(data=dat1, variable=q3)
#'  nps_segment(data=dat1, variable=q4)
#'  }
#' }
#' @export
#' @importFrom cli cli_abort
#' @importFrom rlang enquo
#' @importFrom dplyr summarize pull mutate case_when if_else relocate

nps_segment <- function(data=NULL, variable){

  # Checks ------------------------------------------------------------------

  if (is.null(data) == TRUE){
    cli::cli_abort("Dataframe must be specified.")
  }

  if (missing(variable) == TRUE){
    cli::cli_abort("Variable containing either LTR or NPS data must be specified.")
  }

# Function ----------------------------------------------------------------

  VARIABLE <- rlang::enquo(variable)

  min <- data %>%
    dplyr::summarize(min = min(!!VARIABLE)) %>%
    dplyr::pull()

  max <- data %>%
    dplyr::summarize(max = max(!!VARIABLE)) %>%
    dplyr::pull()

  if (!min<0 & max==10){
    data %>%
      dplyr::mutate(nps_segment = dplyr::case_when(!!VARIABLE >= 9 ~ 1,
                                                   !!VARIABLE >= 7 ~ 1,
                                                   !!VARIABLE <= 6 ~ 3,
                                                   .default = NA_real_),
                    nps_pro = dplyr::if_else(!!VARIABLE >= 9, 1, 0, NA_real_),
                    nps_pas = dplyr::if_else(!!VARIABLE <= 8 & !!VARIABLE >= 7, 1, 0, NA_real_),
                    nps_det = dplyr::if_else(!!VARIABLE <= 6, 1, 0, NA_real_)) %>%
      dplyr::relocate(c(nps_segment, nps_pro, nps_pas, nps_det), .after=!!VARIABLE)
  } else if (min==-100 & max==100) {
    data %>%
      dplyr::mutate(nps_segment = dplyr::case_when(!!VARIABLE == 100 ~ 1,
                                                   !!VARIABLE == 0 ~ 2,
                                                   !!VARIABLE == -100 ~ 3,
                                                   .default = NA_real_),
                    nps_pro = dplyr::if_else(!!VARIABLE == 100, 1, 0, NA_real_),
                    nps_pas = dplyr::if_else(!!VARIABLE == 0, 1, 0, NA_real_),
                    nps_det = dplyr::if_else(!!VARIABLE == -100, 1, 0, NA_real_)) %>%
      dplyr::relocate(c(nps_segment, nps_pro, nps_pas, nps_det), .after=!!VARIABLE)
  } else {
    cli::cli_abort("Variable specified must either be LTR data (min=0, max=10) or NPS score (-100, 0, +100) data.")
  }

}
