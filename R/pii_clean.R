#' @title pii_clean
#' @description Removes common P.I.I. variables from dataset found in Qualtrics
#' surveys, including IPAddress, LocationLatitude, LocationLongitude, RecipientLastName,
#' RecipientFirstName, and RecipientEmail.  Additional variables can be dropped
#' by specifying a vector of column names in the vars option.  Function will only
#' remove these columns if found; no error or warning is thrown if not found.
#' @param data Name of the dataframe to used.  Required.
#' @param vars Vector of column names to be removed, if found.  Optional.
#' @return Dataframe with the variables specified removed but otherwise identical.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  test1 <- pii_clean(test1)
#'  test1 <- pii_clean(test1, vars=c("q1", "q2"))
#'  }
#' }
#' @export
#' @importFrom cli cli_abort

pii_clean <- function(data=NULL, vars=NULL){

  # Checks ------------------------------------------------------------------

  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  # Default Removals --------------------------------------------------------

  default <- c("IPAddress", "ip_address",
               "LocationLatitude", "location_latitude",
               "LocationLongitude", "location_longitude",
               "RecipientLastName", "recipient_last_name",
               "RecipientFirstName", "recipient_first_name",
               "RecipientEmail", "recipient_email")

  data <- data[, !names(data) %in% default]

  # Additional Removals -----------------------------------------------------

  if (!is.null(vars)){
    data <- data[, !names(data) %in% vars]
  }
  return(data)
}
