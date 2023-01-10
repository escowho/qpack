#' @title Function to calculate the week number from data data
#' @description Calculates the week number for a vector of date data.
#' @param date_column The dataframe column containing the date data
#' @param start_day Parameter passed to lubridate::floor_date parameter week_start
#' where 1 = Monday, 7 = Sunday, etc. Default: 1
#' @return Vector of numbers indicating the week number
#' @examples
#' \dontrun{
#' if(interactive()){
#'  set.seed(43792)
#'  dat1 <- sample(seq(as.Date('1999/01/01'), as.Date('2000/01/01'), by="day"), 12) %>%
#'    as_tibble() %>%
#'    dplyr::arrange(value) %>%
#'    dplyr::mutate(x=1:n()) %>%
#'    qpack::set_colnames(c("date", "number"))
#'
#'  dat1 %>%
#'    mutate(week_number = week_number(date))
#'  }
#' }
#' @export
#' @importFrom lubridate is.Date floor_date

week_number <- function(date_column, start_day = 1) {

  # Checks ------------------------------------------------------------------

  if (missing(date_column) == TRUE){
    stop(call. = FALSE, "Date column must be specified.")
  }

  #if (lubridate::is.Date(date_column) == FALSE){
  #  stop(call. = FALSE, "Date data not found in specified column.")
  #}

  # Function ----------------------------------------------------------------

  #wk_start:  1 = Monday, 7 = Sunday
  wnum <- lubridate::floor_date(date_column, 'week', week_start = start_day)
  match(wnum, unique(wnum))
}
