#' @title Creates choropleth calendar that shows the relative occurrence of observations
#' from typical Qualtrics date fields
#' @description Creates a choropleth calendar using the calendR function.  It shows
#' a yearly calendar where every day that registers an observation is colored a
#' shade of red such that the darker the red, the more observations recorded for
#' that date.
#' @param data data Name of the dataframe that contains the date fields.  Required.
#' @param date_var Unquoted name of one of the typical Qualtrics date fields, either
#' start_date, end_date, or recorded date.  Default: recorded_date.
#' @param year Numeric value indicating the year of the calendar to return.  If
#' none is specified, one choropleth calendar for each year found in the data will
#' be returned.  Optional.
#' @return calendR graph object
#' @examples
#' \dontrun{
#' if(interactive()){
#'  response_calendar(test3)
#'  }
#' }
#' @export
#' @importFrom rlang enquo quo
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter group_by summarize left_join rename n
#' @importFrom lubridate year date
#' @importFrom calendR calendR

response_calendar <- function(data, date_var, year=NULL, test=FALSE){

  # Checks ------------------------------------------------------------------

  if (missing(data)){
    stop(call. = FALSE, "Data must be specified.")
  }

  if (missing(date_var) == FALSE){
    DATE <- rlang::enquo(date_var)
  } else {
    DATE <- rlang::quo(recorded_date)
  }

  if (! rlang::f_text(DATE) %in% c("start_date", "end_date", "recorded_date")){
    stop(call. = FALSE, "Date must be either start_date, end_date, or recorded_date.")
  }

  #if (! rlang::quo_name(DATE) %in% colnames(data)){
  #    stop(call. = FALSE, paste0("Date variable, ",
  #                               rlang::f_text(DATE),
  #                               ", not found in data."))
  #}

  # Function ----------------------------------------------------------------

  response_year <- function(data, year){
    temp1 <- tibble::tibble(d = seq(as.Date(paste0(year, "-01-01")),
                                    as.Date(paste0(year, "-12-31")),
                                    by="1 day"))

    temp2 <- data %>%
      dplyr::mutate(y = lubridate::year(d)) %>%
      dplyr::filter(y==year) %>%
      dplyr::mutate(d = lubridate::date(d)) %>%
      dplyr::group_by(d) %>%
      dplyr::summarize(n=dplyr::n()) %>%
      dplyr::mutate(n = rank(n) / length(n))

    temp <- dplyr::left_join(temp1, temp2, by="d") %>%
      dplyr::mutate(n = ifelse(is.na(n), 0, n))

    c <-
      calendR::calendR(year = year,
                       special.days = temp$n,
                       gradient = TRUE,
                       low.col = "#FFFFFF",
                       special.col = "#FF0000",
                       weeknames = c("M", "T", "W", "T", "F", "S", "S"),
                       col="grey")
    if (test==TRUE){
      return(c)
    } else {
      print(c)
    }
  }

  data <- data %>%
    dplyr::rename(d = !!DATE)

  if (is.null(year)){

    dates <- get_dates(data) %>%
      dplyr::mutate(min = lubridate::year(min),
                    max = lubridate::year(max))
    min_year <- min(dates$min)
    max_year <- max(dates$max)

    for(y in min_year:max_year){
      response_year(data=data, year=y)
    }

  } else {
    response_year(data=data,year=year)
  }



}
