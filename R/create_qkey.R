#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION, Default: NULL
#' @param url PARAM_DESCRIPTION, Default: NULL
#' @param token PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @importFrom gtools ask
#' @importFrom keyring key_list key_set_with_value

create_qkey <- function(name=NULL, url=NULL, token=NULL){
  if (is.null(name)){
    name <- gtools::ask(msg="Enter a unique name for the key stored in keyring:")
  }
  if (is.null(token)){
    token <- gtools::ask(msg="Enter the API token:")
  }
  if (is.null(url)){
    url <- gtools::ask(msg="Enter the Base URL:")
  }

  if (!dir.exists("./.qkey/")){
    dir.create("./.qkey/")
  }

  file_name <- paste0(".", name)

  if (name %in% keyring::key_list()$service == TRUE){
    warning(call. = FALSE,
            paste0("qkey created but keyring NOT updated; \"", name, "\" already found in keyring.\n",
                   "Use keyring::key_list() to find the key and keyring::key_delete(), if necessary."))
  } else {
    keyring::key_set_with_value(service=name, password=token)
  }

  cat(paste0("Sys.setenv(\"QUALTRICS_API_KEY\" = keyring::key_get(\"", name, "\"))"),
      file=paste0("./.qkey/", file_name), sep="\n")

  cat(paste0("Sys.setenv(\"QUALTRICS_BASE_URL\" = \"", url, "\")"),
      file=paste0("./.qkey/", file_name), append=TRUE)
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export

get_qkey <- function(name=NULL){
  file_name <- paste0(".", name)

  if(file.exists(paste0("./.qkey/", file_name))){
    source(paste0("./.qkey/", file_name))
  } else {
    stop(call. = FALSE, paste0("No qkey file found with the name: ", name))
  }
}

#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param name PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @importFrom keyring key_get

show_token <- function(name=NULL){
  keyring::key_get(name)
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

clear_qkey <- function(){
  Sys.unsetenv("QUALTRICS_API_KEY")
  Sys.unsetenv("QUALTRICS_BASE_URL")
}
