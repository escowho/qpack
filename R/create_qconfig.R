#' @title Create an invisible qconfig file with credentials necessary for qualtRics
#' @description Create an invisible .qconfig file in the current working directory
#' that holds the credentials nesseary to access Qualtrics surveys and data through
#' the Qualtrics API using the qualtRics package.  It will prompt you to paste your
#' API Key and your Base URL that will be stored in the .qconfig file.  This file
#' is automatically read in as environment variables every time qpack::set_up is
#' run.

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

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export

view_qconfig <- function(){
  file.edit(".qconfig")
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION

#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export

delete_qconfig <- function(){
  if(file.exists(".qconfig")){
    file.remove(".qconfig")
  }
}
