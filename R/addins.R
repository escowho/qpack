#' Syntax skeleton code for \code{set_up}
#'
#' Addin code to put syntax skeleton in script window for set_up new project
#'
#' @return Inserts text into the current script via Addin function
#'
#' @noRd
addin_1_project_setup <- function() {
  rstudioapi::insertText(location=rstudioapi::document_position(1,1), text=
"#==============================================================================#
qpack::set_up(project = \"\",
              descriptor = \"\",
              pack_load = c(),
              source = c(),
              qpack = TRUE)
#==============================================================================#




# Cleanup -----------------------------------------------------------------
rm()
#
#
#
#
#
#
#
#
#
#")
}

#' Syntax skeleton code for \code{set_up}
#'
#' Addin code to put syntax skeleton in script window for set_up new project
#'
#' @return Inserts text into the current script via Addin function
#'
#' @noRd
addin_2_project_setup <- function() {
  rstudioapi::insertText(location=rstudioapi::document_position(1,1), text=
    "#==============================================================================#
qpack::set_up(client = \"\",
              project = \"\",
              descriptor = \"\",
              pack_load = c(),
              source = c(),
              qpack = TRUE)
#==============================================================================#




# Cleanup -----------------------------------------------------------------
rm()
#
#
#
#
#
#
#
#
#
#")
}


