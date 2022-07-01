#' @title Deprecated function to create an invisible qconfig file with credentials necessary for qualtRics
#' @description All qconfig functions now found in the qkey family of functions (create_qkey, get_qkey, clear_qkey, show_token).

create_qconfig <- function(){
  stop(call. = FALSE, "view_qconfig has been deprecated in favor of the create_qkey family of functions.")
}

#' @title Deprecated function to view and edit the contents of the current .qconfig file
#' @description All qconfig functions now found in the qkey family of functions (create_qkey, get_qkey, clear_qkey, show_token).

view_qconfig <- function(){
  stop(call. = FALSE, "view_qconfig has been deprecated in favor of the create_qkey family of functions.")
}

#' @title Deprecated function to delete the current .qconfig file
#' @description All qconfig functions now found in the qkey family of functions (create_qkey, get_qkey, clear_qkey, show_token).

delete_qconfig <- function(){
  stop(call. = FALSE, "delete_qconfig has been deprecated in favor of the create_qkey family of functions.")

}
