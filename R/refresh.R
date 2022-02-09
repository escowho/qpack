#' @title Refreshes github install of qpack
#' @description Convenience function to perform a remotes::install_github for qpack.
#' @param pack_list A character vector with the names of packages to update. Default: qpack.
#' @return Nothing, updates package(s).
#' @examples
#' \dontrun{
#' refresh()
#' refresh(c("qpack"))
#' }
#' @export
#' @importFrom remotes install_github

refresh <- function(pack_list="qpack"){

  if(missing(pack_list)){
    if (Sys.getenv("QPACK_SETUP_PACKS")==""){
      pack_list <- c("qpack")
    } else {
      pack_list <- strsplit(Sys.getenv("QPACK_SETUP_PACKS"), ",")[[1]]
      pack_list <- invisible(lapply(pack_list, trimws))
    }
  }

  update_pack <- function(pack){
    remotes::install_github(paste0("mshefferq/", pack))
  }

  invisible(lapply(pack_list, update_pack))
}
