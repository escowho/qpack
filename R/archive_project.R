#' @title Archives a project into a zip file
#' @description Uses base zip function to archive all the files and non-empty folders
#' inside a project folder.  Ignores the .git and .Rproj.user folder.  Creates a zip
#' file names after the study in the general study directory.  Contents of data
#' folder are excluded by default, until data=TRUE parameter is used.
#' @param client An optional character string of the client name.  If specified,
#' will be a parent folder inside of which the project folder will have been
#' located, i.e. client/project.
#' @param project A character string with a short moniker to indicate either the
#' project name or number, e.g. 013-898.  Will be the parent folder, inside of
#' which all project sub-folders will have been created.  Required.
#' @param root An optional character string that points to a location for
#' locating the file path.  This would have been specified in the original set_up
#' to override the default start_path that may have been set in the Renviron
#' QPACK_SETUP_ROOT variable.
#' @param data A logical indicating if the data subfolder should be included in
#' the resulting archive file. Default: FALSE.
#' @return Creates a zip file in the relevant folder structure containing all project
#' files and non-empty folders (excluding .git and .Rproj.user) but otherwise does
#' not return any object.
#' @examples
#' \dontrun{
#' archive_project("013-898")
#' archive_project(client="Toyota", project="LS2019")
#' }
#' @export
#' @importFrom fs path dir_exists path_split path_join dir_info path_rel
#' @importFrom cli cli_abort cli_warn
#' @importFrom zip zip zip_append
#'
archive_project <- function(project=NULL, client=NULL, root=NULL, data=FALSE){

  # CLIENT/PROJECT or PROJECT Check -----------------------------------------

  if (is.null(project)) {
    cli::cli_abort("A project must be specified.  Client is optional.")
  }

  if (!is.null(client)) {
    c_check <- 1
  } else {
    c_check <- 0
  }

  # SET START_PATH ----------------------------------------------------------

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

  start_wd <- getwd()

  # ARCHIVE FILES -----------------------------------------------------------

  full_path <- fs::path_split(QPACK_SETUP_ROOT)[[1]]
  name_path <- fs::path_join(full_path)
  archive_path <- fs::path_join(full_path[1:length(full_path)-1])

  #if (c_check == 1){
  #  name_path <- archive_path[-length(archive_path)]
  #  archive_path <- archive_path[1:(length(archive_path)-1)]
  #} else {
  #  name_path <- archive_path[-length(archive_path)]
  #  archive_path <- archive_path[-length(archive_path)]
  #}

  #archive_path <- fs::path_join(archive_path)
  #name_path <- fs::path_join(name_path)
  archive_file <- fs::path(archive_path, project_name, ext="zip")

  file_list <- fs::dir_info(name_path, recurse = TRUE, all=TRUE, type="file")
  file_list <- file_list[!grepl("Rproj.user", file_list$path),]
  file_list <- file_list[!grepl(".git", file_list$path),]
  file_list <- file_list[!grepl(".DS_Store", file_list$path),]
  file_list <- file_list[!grepl(".tmp$", file_list$path),]
  file_list <- file_list[!grepl(".qconfig", file_list$path),]
  if (data != TRUE){
    file_list <- file_list[!grepl("/data/", file_list$path),]
  }
  file_list$path <- fs::path_rel(file_list$path, start=archive_path)
  file_list <- file_list[["path"]]

  setwd(archive_path)

  zip::zip(archive_file, file_list)


# ADD OUTSIDE DESCRIPTOR --------------------------------------------------

  if (Sys.getenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR") != FALSE) {
    if (c_check == 1){
      name_part <- paste0("_", project)
    } else {
      name_part <- paste0("_", project_name)
    }

    name_file <- fs::dir_info(name_path, all=TRUE, type="file")
    name_file <- name_file[grepl(".txt", name_file$path),]
    name_file <- name_file[grepl(name_part, name_file$path),]
    name_file <- name_file[["path"]]
    name_file <- fs::path_rel(name_file)

    zip::zip_append(archive_file, name_file)
  }

  setwd(start_wd)
}
