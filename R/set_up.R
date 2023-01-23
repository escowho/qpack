#' @title Set-up code to create project directories and load needed packages
#' @description Part of the qpack package of functions, this sets up project
#' directories, sets the working directory, loads specified packages, and sources
#' functions and/or config files, if specified.  Can automatically load a discovered
#' qkey file.  Option to load qpack package.
#' @param client An optional character string of the client name.  If specified,
#' will be a parent folder inside of which the project folder will be located,
#' i.e. client/project.  Optional.
#' @param project A character string with a short moniker to indicate either the
#' project name or number, e.g. 013-898.  Will be the parent folder, inside of
#' which all project sub-folders will be created.  Recommend keeping short as
#' possible.  Required.
#' @param task A character string with a short moniker to indicate the task or
#' sub-folder underneath project for situations where there is a need for several
#' different task-related sub-projects in one project folder, e.g. EDA.  Recommend
#' keeping as short as possible.  Optional.
#' @param root A optional character string that points to a root or starting location
#' for creating the file path.  This overrides the default root that may be set
#' in the Renviron QPACK_SETUP_ROOT variable.  Optional.
#' @param descriptor A character sting assigning a descriptive name to the project
#' that is used to create two descriptor files in the folder structure which help
#' identify what is inside the project folder.  One is placed outside the folder
#' structure to identify the main folder from others, and the other is placed inside
#' the folder to act as reminder of the project when inside a file manager.  If
#' the Renviron variable QPACK_SETUP_EXTERNAL_DESCRIPTOR is set to FALSE, then the
#' external descriptor file will be suppressed.  Required.
#' @param folders A character string or vector of strings containing the
#' names of folders to be created under the project folder.  This overrides any
#' folders that may be set in the Renviron QPACK_SETUP_FOLDERS variable.  If not specified
#' and no QPACK_SETUP_FOLDERS variable is specified in Renviron, then set_up defaults
#' to creating two folders, data and output.
#' @param pack_load A character string or vector of strings containing the names
#' of the packages to be loaded in the order to be loaded.  Optional.
#' @param pack_check A character string or vector of strings containing the names
#' of the packages to be verified as installed but NOT loaded.  Optional.
#' @param source An optional string or vector of strings containing the name of
#' any R files that should be sourced, perhaps containing project-wide functions
#' or configuration information.  Each file name should end in .R and a relative
#' file path from the project directory should be included in each character string,
#' if not housed inside the project folder, e.g. "function.R" or "./config/functions.R"
#' if  is housed in a sub-folder named "config".  Optional.
#' @param qpack A logical indicating the preference to automatically load the
#' qpack package after all other packages specified in packages parameter.
#' Default: TRUE.
#' @return Creates a folder structure and generates two text files but otherwise
#' does not return any object.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  set_up(client="Toyota",
#'         project="LS2019",
#'         descriptor="Lead Scoring 2019",
#'         packages=c("caret", "Cairo", "doParallel", "pROC"),
#'         source=c("functions.R", "config.R"),
#'         qpack=TRUE)
#'  }
#' }
#' @export
#' @importFrom fs dir_exists path path_dir dir_create file_exists file_create
#' @importFrom rstudioapi isAvailable

set_up <- function(client=NULL, project=NULL, task=NULL, root=NULL,
                   descriptor=NULL, folders=NULL, pack_load=NULL, pack_check=NULL,
                   source=NULL, qpack=TRUE){

  # Basic Checks ------------------------------------------------------------

  if (rstudioapi::isAvailable() != TRUE & (is.null(root) | is.null(folders))){
    warning(call. = FALSE,
            paste0("qpack assumes use of Rstudio IDE.",
                   "\n",
                   "Expect warnings below unless using root option AND specifying folders."))
  }

  if (is.null(project)) {
    stop(call. = FALSE, "A project must be specified.  Client is optional.")
  } else if(nchar(trimws(project))==0){
    stop(call. = FALSE, "A project must be specified.  Client is optional.")
  }

  if (is.null(descriptor)){
    stop(call. = FALSE, "Descriptor must be specified.")
  } else if(nchar(trimws(descriptor))==0){
    stop(call. = FALSE, "Descriptor must be specified.")
  }

  # Type Check --------------------------------------------------------------

  if (!is.null(client)) {
    if (client != ""){
      client_true <- 1
    }
  } else {
    client_true <- 0
  }

  if (!is.null(task)) {
    if (task != ""){
      task_true <- 1
  }
  } else {
    task_true <- 0
  }

  # GET NAMES ---------------------------------------------------------------

  #Rproj Name
  if (task_true==1){
    Rproj_name <- paste0(project, " (", task, ")")
  } else {
    Rproj_name <- project
  }

  #Inside Name

  #Outside Name

  # GET PATHS ---------------------------------------------------------------

  #start_path >- Manual or QPACK_SETUP_ROOT or Default

  #Manual Start Path
  if (is.null(root) == FALSE){
    if (fs::dir_exists(root) == FALSE){
      stop(call. = FALSE,
           paste0("Specified root path does not exist:",
                  "\n",root))
    } else {
      start_path <- fs::path(root)
    }

  #Read QPACK_SETUP_ROOT Path or Default
  } else if (Sys.getenv("QPACK_SETUP_ROOT")=="") {
    start_path <- Sys.getenv("HOME")
    warning(call. = FALSE,
            paste0("QPACK_SETUP_ROOT not found in .Renviron file.",
                   "\n",
                   "Using the default path instead: ", start_path))
  } else {
    start_path <- Sys.getenv("QPACK_SETUP_ROOT")
  }

  #inside_path and outside_path based on project type
  if (client_true == 1) {
    if (task_true == 1){
      inside_path <- fs::path(start_path, client, project, task)
      outside_path <- fs::path(start_path, client)
    } else {
      inside_path <- fs::path(start_path, client, project)
      outside_path <- fs::path(start_path, client)
    }

  } else {
    if (task_true == 1){
      inside_path <- fs::path(start_path, project, task)
      outside_path <- fs::path(start_path)
    } else {
      inside_path <- fs::path(start_path, project)
      outside_path <- fs::path(start_path)
    }
  }

  #Rproj Path
  Rproj_path <- fs::path(inside_path, paste0(Rproj_name, ".Rproj"))

  # LOCATION CHECK AND SET WORKING DIRECTORY --------------------------------

  already_in_wd <- FALSE
  matches_setup <- TRUE
  #if the override isn't set
  if (Sys.getenv('OVERRIDE_FOR_TESTING')!=TRUE){
    #if the Rproj file is found in the working directory
    if (file.exists(paste0(Rproj_name, ".Rproj"))==TRUE){
      #if the location of the Rproj file matches the current working directory
      if (fs::path_dir(fs::path_abs(paste0(Rproj_name, ".Rproj"))) == getwd()){
        already_in_wd <- TRUE
      }
      #if the current Rproj is where the set_up file thinks it should be
      if (fs::path_dir(fs::path_abs(paste0(Rproj_name, ".Rproj"))) != inside_path){
        matches_setup <- FALSE
      }
    }
  }

  if (already_in_wd==TRUE & matches_setup==TRUE){
    #do nothing
  } else if (already_in_wd==TRUE & matches_setup==FALSE){
    #fix paths to match current
    inside_path <- fs::path_dir(fs::path_abs(paste0(Rproj_name, ".Rproj")))
    outside_path <- fs::path_split(inside_path)[[1]]
    outside_path <- fs::path_join(outside_path[1:length(outside_path)-1])
    Rproj_path <- fs::path_abs(paste0(Rproj_name, ".Rproj"))
  } else if (already_in_wd==FALSE) {
    fs::dir_create(inside_path)
  }
  setwd(file.path(inside_path))

  # CREATE SUB DIRECTORIES --------------------------------------------------

  #Get Sub-Directories
  if (is.null(folders)){
    if (Sys.getenv("QPACK_SETUP_FOLDERS")==""){
      sub_directories <- c("data", "output")
      warning(call. = FALSE,
              paste0("No directories specified and QPACK_SETUP_FOLDERS not found in .Renviron file.",
                     "\n",
                     "Using two default sub-directories instead: data, output"))
    } else {
      sub_directories <- strsplit(Sys.getenv("QPACK_SETUP_FOLDERS"), ",")[[1]]
      sub_directories <- invisible(lapply(sub_directories, trimws))
    }
  } else {
    sub_directories <- folders
  }

  create_subdirectory <- function(x, project_path) {
    path_to_create <- fs::path(project_path, x)
    fs::dir_create(path_to_create)
  }

  invisible(lapply(sub_directories, create_subdirectory, project_path=inside_path))

  # CREATE FILES ------------------------------------------------------------

  #Rproj File
  if (!fs::file_exists(Rproj_path)) {
    x <- c("Version: 1.0",
           "",
           "RestoreWorkspace: Default",
           "SaveWorkspace: Default",
           "AlwaysSaveHistory: Default",
           "",
           "EnableCodeIndexing: Yes",
           "Encoding: UTF-8",
           "",
           "AutoAppendNewline: Yes",
           "StripTrailingWhitespace: Yes",
           "LineEndingConversion: Posix",
           "",
           "BuildType: Package",
           "PackageUseDevtools: Yes",
           "PackageInstallArgs: --no-multiarch --with-keep.source",
           "PackageRoxygenize: rd,collate,namespace")

    cat(paste(x, collapse="\n"), file=Rproj_path)
  }

  #Exterior Descriptor
  if (Sys.getenv("QPACK_SETUP_EXTERNAL_DESCRIPTOR") != FALSE) {
    desc_outside <- fs::path(outside_path, paste0("_", project, " - ", descriptor, ".txt"))
    if (!fs::file_exists(desc_outside) & task_true == 0) {
      fs::file_create(desc_outside)
    }
  }

  #Interior Descriptor
  if (task_true==1){
    descriptor_file <- paste0(descriptor, " (", task, ")")
  } else {
    descriptor_file <- descriptor
  }
  desc_inside <- fs::path(inside_path, paste0("_", descriptor_file, ".txt"))

  if (!fs::file_exists(desc_inside)) {
    fs::file_create(desc_inside)
  }

  # LOAD SOURCE FILES -------------------------------------------------------

  load_source <- function(source_file){
    if (!file.exists(source_file)){
      stop(call. = FALSE, sprintf("%s not found in the working directory", source_file))
    } else {
      source(source_file)
    }
  }

  if (!is.null(source)){
    invisible(lapply(source, load_source))
  }

  # LOAD PACKAGES -----------------------------------------------------------

  if (is.null(pack_load)) {
    packs_to_load <- ""
  } else {
    packs_to_load <- trimws(pack_load)
  }

  if (qpack==TRUE){
    packs_to_load <- c(packs_to_load, "qpack")
  }

  packs_to_load <- unique(packs_to_load[packs_to_load != ""])

  if (length(packs_to_load) > 0){
    invisible(lapply(packs_to_load, require, character.only=TRUE))
  }

  # VERIFY INSTALLED BUT NOT LOADED PACKAGES --------------------------------

  if (is.null(pack_check)) {
    packs_to_check <- ""
  } else {
    packs_to_check <- trimws(pack_check)
  }

  packs_to_check <- unique(packs_to_check[packs_to_check != ""])

  if (length(packs_to_check) > 0){
    pack_installed <- function(x){
      length(find.package(x, quiet=TRUE))
    }

    check <- data.frame(p = packs_to_check, i = sapply(packs_to_check, pack_installed))
    check <- check$p[check$i==0]

    if (length(check) > 1){
      check <- paste(check, collapse=", ")
      warning(paste("Packages from pack_check not found:", check, sep="\n"))
    } else if(length(check) == 1){
      warning(paste("Package from pack_check not found:", check, sep=" "))
    }
  }

  # QKEY --------------------------------------------------------------------

  if (dir.exists("./.qkey/")){
    f <- fs::dir_info("./.qkey/", all=TRUE)
    if (nrow(f)==1){
      tryCatch(
        #try to do this
        {
          get_qkey(f$path[[1]])
        },
        #if an error occurs, tell me the error
        error=function(e) {
          message('A qkey Error Occurred')
          print(e)
        },
        #if a warning occurs, tell me the warning
        warning=function(w) {
          message('A qkey Warning Occurred')
          print(w)
          return(NA)
        }
      )
    }
  }

}



