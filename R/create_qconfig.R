#' @title Create an invisible qconfig file with credentials necessary for qualtRics
#' @description Create an invisible .qconfig file in the current working directory
#' that holds the credentials necessary to access Qualtrics surveys and data through
#' the Qualtrics API using the qualtRics package.  It will prompt you to paste your
#' API Key and your Base URL that will be stored in the .qconfig file.  This file
#' is automatically read in as environment variables every time qpack::set_up is
#' run.
#' @param key the Qualtrics API Key.  It is assumed you will not specify this
#' in syntax and so will be prompted to paste the value into the console for the
#' function to set.  But provided as an option. Default: NULL
#' @param url the Qualtrics Base URL.  It is assumed you will not specify this
#' in syntax and so will be prompted to paste the value into the console for the
#' function to set.  But provided as an option. Default: NULL
#' @return creates a hidden text file named .qconfig in the working directory
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_qconfig()
#'  }
#' }
#' @export
#' @importFrom gtools ask

create_qconfig <- function(key=NULL, url=NULL){

  if (is.null(key)){
    key <- gtools::ask(msg="Enter the API Key:")
  }
  if (is.null(url)){
    url <- gtools::ask(msg="Enter the Base URL:")
  }
  cat(paste0("QUALTRICS_API_KEY = \"", key, "\""), file="./.qconfig", sep="\n")
  cat(paste0("QUALTRICS_BASE_URL = \"", url, "\""), file="./.qconfig", append=TRUE)
}

#' @title View and edit the contents of the current .qconfig file
#' @description View and edit the contents of the current .qconfig file, if found.
#' @return Nothing is returned but the qconfig file is opened
#' @examples
#' \dontrun{
#' if(interactive()){
#'  view_qconfig()
#'  }
#' }
#' @export

view_qconfig <- function(){
  if(file.exists(".qconfig")){
    file.edit(".qconfig")
  } else {
    stop(call. = FALSE, "No qconfig file found in the current working directory.")
  }
}

#' @title Delete the current .qconfig file
#' @description Delete the current .qconfig file, if found.
#' @return Nothing is returned but the qconfig file is deleted
#' @examples
#' \dontrun{
#' if(interactive()){
#'  delete_qconfig()
#'  }
#' }
#' @export

delete_qconfig <- function(){
  if (!file.exists(".qconfig")){
    stop(call. = FALSE, "No qconfig file found in the current working directory.")
  }
  outcome <- file.remove(".qconfig")
  if(outcome=="TRUE"){
    message("qconfig deleted.")
  } else {
    print(outcome)
  }
}
