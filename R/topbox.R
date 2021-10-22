#' @title Function to recode data into Top X Box data
#' @description Works on vectors to recode the data into 0,1 values based on Top1,
#' Top2, Top3, or TopX values.  A maximum value can be indicated (maxval) but the
#' function will find the maximum in the data if not provided.  The replacena
#' logical allows for replacing any NA with 0, if desired.
#' @param data A vector object to be converted.  Required.
#' @param top Numeric indicator of the top X objects to code as 1. Default: 2.
#' @param maxval Numeric value of the maximum value (will identify from data if not provided).
#' @param replacena Logical value to replace NA with 0. Default: FALSE.
#' @return A converted vector object.
#' @examples
#' \dontrun{
#' caddat %>%
#' dplyr::select(q19_1, q19_2) %>%
#'   dplyr::mutate(q19_2 = ifelse(q19_2<=3, NA, q19_2),
#'                 test1 = topbox(q19_1, top=2),
#'                 test2 = topbox(q19_1),
#'                 test3 = topbox(q19_1, top=1),
#'                 test4 = topbox(q19_2, top=2),
#'                 test5 = topbox(q19_2, top=2, replacena=TRUE)) %>%
#'   summary()
#' }
#' @export

topbox <- function(data, top=2, maxval, replacena=FALSE){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  if (is.numeric(top)==FALSE){
    stop(call. = FALSE, "Top variable must be a number.")
  }

  if (missing(maxval)){
    maxval <- max(data, na.rm=TRUE)
    warning(call. = FALSE,
            paste0("Determining maxval from the data.",
                   "\n",
                   "Using the maxval of ", maxval))
  }

  # Function ----------------------------------------------------------------

  tb_fun <- function(data, maxval, top, replacena){
    if (is.na(data)==TRUE){
      if (replacena==TRUE){
        data <- 0
      } else {
        data <- NA
      }
    } else if (data > (maxval - top)) {
      data <- 1
    } else if (data <= (maxval - top)) {
      data <- 0
    } else {
      data <- NULL
    }
    return(data)
  }

  vapply(data, tb_fun, maxval, top, replacena, FUN.VALUE=double(1))

}

#' @title Function to recode data into Bottom X Box ratings data
#' @description Works on vectors to recode the data into 0,1 values based on Bot1,
#' Bot2, Bot3, or BotX values.  A minimum value can be indicated (minval) but the
#' function will find the minimum in the data if not provided.  The replacena
#' logical allows for replacing any NA with 0, if desired.
#' @param data A vector object to be converted.  Required.
#' @param bot Numeric indicator of the bottom X objects to code as 1. Default: 2.
#' @param minval Numeric value of the minimum value (will identify from data if not provided).
#' @param replacena Logical value to replace NA with 0. Default: FALSE.
#' @return A converted vector object.
#' @examples
#' \dontrun{
#' caddat %>%
#' dplyr::select(q19_1, q19_2) %>%
#'   dplyr::mutate(q19_2 = ifelse(q19_2>=6, NA, q19_2),
#'                 test1 = botbox(q19_1, bot=2),
#'                 test2 = botbox(q19_1),
#'                 test3 = botbox(q19_1, bot=1),
#'                 test4 = botbox(q19_2, bot=2),
#'                 test5 = botbox(q19_2, bot=2, replacena=TRUE)) %>%
#'   summary()
#' }
#' @export

botbox <- function(data, bot=2, minval, replacena=FALSE){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
  }

  if (is.numeric(bot)==FALSE){
    stop(call. = FALSE, "Bot variable must be a number.")
  }

  if (missing(minval)){
    minval <- min(data, na.rm=TRUE)
    warning(call. = FALSE,
            paste0("Determining minval from the data.",
                   "\n",
                   "Using the minval of ", minval))
  }

  # Function ----------------------------------------------------------------

  bb_fun <- function(data, minval, bot, replacena){
    if (is.na(data)==TRUE){
      if (replacena==TRUE){
        data <- 0
      } else {
        data <- NA
      }
    } else if (data < (minval + bot)) {
      data <- 1
    } else if (data >= (minval + bot)) {
      data <- 0
    } else {
      data <- NULL
    }
    return(data)
  }

  vapply(data, bb_fun, minval, bot, replacena, FUN.VALUE=double(1))

}
