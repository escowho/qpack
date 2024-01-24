#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param delete PARAM_DESCRIPTION, Default: TRUE
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @importFrom cli cli_abort cli_warn cli_text cli_alert_success

archive_mail <- function(delete=TRUE){

  if(dir.exists("./email/")==FALSE){
    cli::cli_abort("./email/ folder doesn't exist within current working directory.")
  }

  current_directory <- getwd()
  setwd("./email/")

  zip_file <- file.path("email.zip")

  if (file.exists(zip_file)){
    invisible(file.remove(zip_file))
  }

  file_list <- list.files()

  if(length(file_list)==0){
    cli::cli_warn("No email to archive.")
  }
  if (length(file_list)==1 & file_list[1]=="./email.zip") {
    cli::cli_warn("Email already archived.")
  }

  invisible(zip(zip_file, file_list, flags="-X9"))

  if(delete != FALSE){
    cli::cli_text()
    for (file in file_list){
      cli::cli_text("__deleting: {file}")
      file.remove(file)
    }
  }
  setwd(current_directory)
  cli::cli_alert_success("archive COMPLETE.")
}
