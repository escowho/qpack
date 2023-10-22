#' @title Pulls basic descriptives for the three common date fields from a Qualtrics dataset
#' @description Pulls the min and max values for start_date, end_date, and recorded_date,
#' if found.  Also calculates the percent of days between the min and max value that
#' contains a record in the dataframe in order to assess if data collection is ongoing
#' (higher percentages) or only at specific times (lower percentages).  Will only
#' pull descriptives for the three typical data fields.
#' @param data Name of the dataframe that contains the date fields.  Required.
#' @return A dataframe with the name of the date field, the min and max values,
#' and the percent of days between the two that contain observations
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_dates(test3)
#'  }
#' }
#' @export
#'
#' @importFrom lubridate date
#' @importFrom dplyr summarise_all everything left_join mutate mutate_all group_by slice ungroup summarize pull select n
#' @importFrom tidyr pivot_longer
#' @importFrom cli cli_abort

get_dates <- function(data){

  # Checks ------------------------------------------------------------------

  if (missing(data)){
    cli::cli_abort("Data must be specified.")
  }

  if (!"start_date" %in% names(data) &
      !"end_date" %in% names(data) &
      !"recorded_date" %in% names(data)){
    cli::cli_abort("Cannot locate start_date, end_date, or recorded_date within dataframe.")
  }


  # Function ----------------------------------------------------------------

  data <- data[,names(data) %in% c("start_date","end_date","recorded_date")] %>%
    dplyr::mutate_all(., ~lubridate::date(.))

  min_vals <- data %>%
    dplyr::summarise_all(min)

  min_vals <- min_vals %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="date", values_to="min")

  max_vals <- data %>%
    dplyr::summarise_all(max)

  max_vals <- max_vals %>%
    tidyr::pivot_longer(dplyr::everything(), names_to="date", values_to="max")

  out <- dplyr::left_join(min_vals, max_vals, by="date") %>%
    dplyr::mutate(diff = as.numeric(difftime(max, min, units="days")),
                  days = 0)

  if ("start_date" %in% out$date){
    start_days <- data %>%
      dplyr::group_by(start_date) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(n=dplyr::n()) %>%
      dplyr::pull()

    out <- out %>%
      dplyr::mutate(days = ifelse(date=="start_date", start_days, days))

  }

  if ("end_date" %in% out$date){
    start_days <- data %>%
      dplyr::group_by(end_date) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(n=dplyr::n()) %>%
      dplyr::pull()

    out <- out %>%
      dplyr::mutate(days = ifelse(date=="end_date", start_days, days))

  }

  if ("recorded_date" %in% out$date){
    start_days <- data %>%
      dplyr::group_by(recorded_date) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::summarize(n=dplyr::n()) %>%
      dplyr::pull()

    out <- out %>%
      dplyr::mutate(days = ifelse(date=="recorded_date", start_days, days))

  }

  out <- out %>%
    dplyr::mutate(diff = as.numeric(difftime(max, min), units="days"),
                  percent = round(days / diff, 3)) %>%
    dplyr::select(-diff, -days)
  return(out)

}
