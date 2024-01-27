#' @title Archive all folder contents within email folder
#' @description If an email folder is found within the current working directory,
#' all the folder contents will be zipped into a single file called email.zip
#' and the contents deleted.
#' @param delete Logical indicating if email should be deleted after archiving.
#' Default: TRUE
#' @return zip file containing any folder contents of the email folder
#' @examples
#' \dontrun{
#' if(interactive()){
#'  archive_email()
#'  archive_email(delete=FALSE)
#'  }
#' }
#' @export
#' @importFrom cli cli_abort cli_warn cli_text cli_alert_success

archive_email <- function(delete=TRUE){

  if(dir.exists("./email/")==FALSE){
    cli::cli_abort("./email/ folder doesn't exist within current working directory.")
  }

  zip_file <- file.path("./email/email.zip")

  if (file.exists(zip_file)){
    invisible(file.remove(zip_file))
  }

  file_list <- list.files(path="./email/", full.names=TRUE)

  if(length(file_list)==0){
    cli::cli_abort("No email to archive.")
  }

  if (Sys.getenv("QPACK_SETUP_TEST")==TRUE){
    invisible(zip(zip_file, file_list, flags="-qjX9"))
  } else {
    invisible(zip(zip_file, file_list, flags="-jX9"))
  }

  if(delete != FALSE){
    cli::cli_text()
    for (file in file_list){

      if (Sys.getenv("QPACK_SETUP_TEST") != TRUE){
        cli::cli_text("__deleting: {file}")
      }
      file.remove(file)
    }
  }
}
