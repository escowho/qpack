#' @title Deletes a project folder and all its contents
#' @description Deletes the project folder and the descriptor file in the study
#' folder.  The function needs to detect the presence of the archive file in order
#' for the delete to be executed.
#' @param client An optional character string of the client name.  If specified,
#' will be a parent folder inside of which the project folder will have been
#' located, i.e. client/project.
#' @param project A character string with a short moniker to indicate either the
#' project name or number, e.g. 013-898.  Will be the parent folder, inside of
#' which all project sub-folders will have been created.  Required.
#' @param root A optional character string that points to a location for
#' locating the file path.  This would have been specified in the original set_up
#' to override the default start_path that may have been set in the Renviron
#' QPACK_SETUP_ROOT variable.
#' @param override_archive_check A logical indicating if the archive check should
#' be overridden, likely for testing purposes only.  Default:  FALSE.
#' @return Returns noting but deletes folders and the name file.
#' @examples
#' \dontrun{
#' delete_project("013-898")
#' delete_project(client="Toyota", project="LS2019")
#' }
#' @export
#' @importFrom fs path dir_exists dir_delete dir_info file_delete file_exists
#' @importFrom cli cli_abort cli_warn
#'
delete_project <- function(project, client, root=NULL, override_archive_check=FALSE){

  # CLIENT/PROJECT or PROJECT Check -----------------------------------------

  if (missing(project)) {
    cli::cli_abort("A project must be specified.  Client is optional.")
  }

  if (!missing(client)) {
    c_check <- 1
  } else {
    c_check <- 0
  }

  # SET START_PATH ----------------------------------------------------------

  start_wd <- getwd()

  #Manual Start Path
  if (is.null(root) == FALSE){

    if (fs::dir_exists(root) == FALSE){
      cli::cli_abort("Specified root does not exist: {root}")
    } else {
      start_path <- fs::path(root)
    }
    #Read QPACK_SETUP_ROOT Path
  } else if (Sys.getenv("QPACK_SETUP_ROOT")=="") {
    start_path <- Sys.getenv("HOME")
    cli::cli_warn("QPACK_SETUP_ROOT not found in .Renviron file. Using the default path instead: {start_path}")
  } else {
    start_path <- Sys.getenv("QPACK_SETUP_ROOT")
  }

  # DIRECTORY CHECK ---------------------------------------------------------

  if (c_check == 1) {
    QPACK_SETUP_ROOT <- fs::path(start_path, client, project)
    project_name <- paste(client, project, sep=" ")
  } else {
    QPACK_SETUP_ROOT <- fs::path(start_path, project)
    project_name <- project
  }

  if (!fs::dir_exists(QPACK_SETUP_ROOT)){
    cli::cli_abort("{QPACK_SETUP_ROOT} cannot be found.")
  }

  # ARCHIVE FILE CHECK ------------------------------------------------------

  if (override_archive_check != TRUE){
    archive_path <- fs::path(start_path, project, ext="zip")
    if (!fs::file_exists(archive_path)){
      cli::cli_abort("The archive file {archive_path} must be found to delete the project. Run archive_project first, and try again.")
    }
  }

  # DELETE FILES ------------------------------------------------------------

  fs::dir_delete(QPACK_SETUP_ROOT)

  # DELETE OUTSIDE DESCRIPTOR -----------------------------------------------

  if (Sys.getenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR") != FALSE) {

    archive_path <- fs::path_split(QPACK_SETUP_ROOT)[[1]]

    if (c_check == 1){
      name_path <- archive_path[-length(archive_path)]
      archive_path <- archive_path[1:(length(archive_path)-1)]
    } else {
      name_path <- archive_path[-length(archive_path)]
      archive_path <- archive_path[-length(archive_path)]
    }

    archive_path <- fs::path_join(archive_path)
    name_path <- fs::path_join(name_path)

    if (c_check == 1){
      name_part <- paste0("_", project)
    } else {
      name_part <- paste0("_", project_name)
    }

    name_file <- fs::dir_info(name_path, all=TRUE, type="file")
    name_file <- name_file[grepl(".txt", name_file$path),]
    name_file <- name_file[grepl(name_part, name_file$path),]
    name_file <- name_file[["path"]]

    fs::file_delete(name_file)

    }

  setwd(start_wd)
}
