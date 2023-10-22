#' @title Create an invisible qkey file that triggers credentials for using the API and qualtRics package
#' @description Create an invisible qkey file inside an invisible folder within
#' the current working directory that holds the credentials necessary to access
#' Qualtrics surveys and data through the Qualtrics API using the qualtRics package.
#' Credentials are automatically saved in your operating system's credential store
#' using the keyring package.  It will prompt you to name the key, paste your
#' API Key and your Base URL.  The API Key will be saved in the data store under
#' the name provided and syntax for loading the credentials are stored in the
#' qkey file itself.  This file is automatically read in as environment variables
#' every time qpack::set_up is run if only one qkey file is detected.  Can also
#' be loaded using get_qkey function and removed with clear_qkey function.
#' @param name the name of the key entry under which the token will be stored in
#' the OS credential store.  Must be unique and recommended to be short.  Used
#' with get_qkey if accessing multiple credentials in one session.
#' @param url the Qualtrics Base URL.  It is assumed you will not specify this
#' in syntax and so will be prompted to paste the value into the console for the
#' function to set.  But provided as an option. Default: NULL
#' @param token the Qualtrics API Key.  It is assumed you will not specify this
#' in syntax and so will be prompted to paste the value into the console for the
#' function to set.  But provided as an option. Default: NULL
#' @return creates a hidden text file named qkey in the hidden folder inside the
#' current working directory
#' @examples
#' \dontrun{
#' if(interactive()){
#'  create_qkey()
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

  if (file.exists(paste0("./.qkey/", file_name))){
    unlink(paste0("./.qkey/", file_name), force=TRUE)
  }

  cat(paste0("Sys.setenv(\"QUALTRICS_API_KEY\" = keyring::key_get(\"", name, "\"))"),
      file=paste0("./.qkey/", file_name), sep="\n")

  cat(paste0("Sys.setenv(\"QUALTRICS_BASE_URL\" = \"", url, "\")"),
      file=paste0("./.qkey/", file_name), append=TRUE)
}

#' @title Automatically reads stored qkey credentials and creates the necessary
#' system variables for qualtRics package to access surveys through the API
#' @description Executes code stored in the hidden qkey file that accesses the
#' credential store and creates the necessary system variables used by the qualtRics
#' package (), e.g. QUALTRICS_API_KEY and QUALTRICS_BASE_URL.
#' @param name Character string of the name of a qkey created previously with
#' create_qkey. Required.
#' @return Nothing is returned but two system variables will be created.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_qkey("Study Name")
#'  }
#' }
#' @export

get_qkey <- function(name=NULL){

  if(is.null(name)){
    stop(call. = FALSE, "Name must be specified")
  }

  file_name <- paste0(".", name)

  if(file.exists(paste0("./.qkey/", file_name))){
    source(paste0("./.qkey/", file_name))
  } else {
    stop(call. = FALSE, paste0("No qkey file found with the name: ", name))
  }
}

#' @title Show the saved token for a given qkey name
#' @description Useful when troubleshooting a duplicate name within a key store,
#' the function shows the saved token for a given name.
#' @param name Character string of the name of a qkey created previously with
#' create_qkey. Required.
#' @return returns the saved token to the console
#' @examples
#' \dontrun{
#' if(interactive()){
#'  show_token("Study Name")
#'  }
#' }
#' @export
#' @importFrom keyring key_get

show_token <- function(name=NULL){

  if(is.null(name)){
    stop(call. = FALSE, "Name must be specified")
  }

  keyring::key_get(name)
}

#' @title Clears out any active system variable entries created by a qkey function
#' @description Clears out current system variables used by the qualtRics
#' package (), e.g. QUALTRICS_API_KEY and QUALTRICS_BASE_URL.
#' @return Nothing is returned.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  clear_qkey()
#'  }
#' }
#' @export

clear_qkey <- function(){
  Sys.unsetenv("QUALTRICS_API_KEY")
  Sys.unsetenv("QUALTRICS_BASE_URL")
}
