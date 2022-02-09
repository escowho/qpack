#' @title Refreshes github install of qpack
#' @description Convenience function to perform a remotes::install_github for qpack.
#' @param pack_list A character vector with the names of packages to update. Default: qpack.
#' @param dev A logical to indicate if the development version of qpack should be
#' installed.  Will only work if qpack is the only package listed in the pack_list.
#' Default: FALSE.
#' @return Nothing, updates package(s).
#' @examples
#' \dontrun{
#' refresh()
#' refresh(dev=TRUE)
#' }
#' @export
#' @importFrom remotes install_github

refresh <- function(pack_list="qpack", dev=FALSE){

  # Helper Functions --------------------------------------------------------

  update_pack <- function(pack){
    remotes::install_github(paste0("mshefferq/", pack))
  }

  update_dev <- function(pack){
    remotes::install_github("mshefferq/qpack", ref="dev")
  }

  # Function ----------------------------------------------------------------

  if(missing(pack_list)){
    if (Sys.getenv("QPACK_SETUP_PACKS")==""){
      pack_list <- c("qpack")
    } else {
      pack_list <- strsplit(Sys.getenv("QPACK_SETUP_PACKS"), ",")[[1]]
      pack_list <- invisible(lapply(pack_list, trimws))
    }
  }

  if (dev==TRUE & length(pack_list)==1 & pack_list=="qpack"){
    invisible(update_dev())
  } else if (dev==TRUE & length(pack_list)>1){
    stop(call. = FALSE, "dev cannot be TRUE if any pack other than qpack is listed in the pack_list.")
  } else {
    invisible(lapply(pack_list, update_pack))
  }

}
