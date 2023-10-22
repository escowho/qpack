#' @title Generates a unique ID value for each row in a dataset
#' @description Generates a unique ID value for each row in a dataset that's an
#' ordered sequence of numbers ranging from 1 to the number of rows in the data.
#' This is to replace a potentially arbitrary and non-numerical case ID number
#' found in the original data.  The dataset is first sorted (ascending) on up to
#' two other variables (var1 and var2, if specified) to ensure replicability of
#' ordering.  If duplicates are detected, a warning is issued.  There's an
#' option to keep the original id variable (default) or to remove the original
#' id variable (remove = TRUE).  The ID value will be in the first position.
#' @param data A dataframe containing the original data.  Required.
#' @param var1 The name of a primary sorting variable, perhaps an existing
#' identifier upon which the new ID will be based.  Uses tidyeval, so no quotation
#' is needed.  Optional.
#' @param var2 The name of a secondary sorting variable, perhaps due to gaps or
#' issues with the primary sorting variable, upon which the new ID will be based.
#' Uses tidyeval, so no quotation is needed.  Optional.
#' @param name The name of the ID variable in the dataset.  Uses tidyeval, so no
#' quotation is needed.  Default:  id
#' @param remove Logical indicating if the sorting variables (var1, var2) should
#' be removed.  Default: False.
#' @return A dataframe with the new ID number in the first position.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  dat1 <- data.frame(v1 = c(1,2,3,4,NA,NA,NA,NA), v2 = c(NA,NA,NA,NA,1,2,3,4))
#'  dat2 <- create_id(dat1)
#'  dat3 <- create_id(dat1, v1, v2)
#'  dat4 <- create_id(dat1, v1, v2, name="respnum", remove=TRUE)
#'  dat1 %>%
#'    create_id(v1, v2)
#'  }
#' }
#' @export
#' @importFrom rlang enquo quo_name f_text
#' @importFrom cli cli_abort cli_warn

create_id <- function(data, var1, var2, name, remove=FALSE){
  VAR1 <- rlang::enquo(var1)
  VAR2 <- rlang::enquo(var2)

  if (missing(name)==TRUE){
    name <- "id"
  }
  NAME <- rlang::enquo(name)

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    cli::cli_abort("Data must be specified.")
  }

  if (is.data.frame(data) == FALSE){
    cli::cli_abort("Specified file must be a dataframe.")
  }

  if (rlang::quo_name(NAME) %in% colnames(data)){
    cli::cli_abort("New ID variable = {rlang::f_text(NAME)} already found in data.  Rename or drop existing variable.")
  }


  # Sort Function -----------------------------------------------------------

  sort.data.frame <- function(x, decreasing=FALSE, by=1, ... ){
    f <- function(...) order(...,decreasing=decreasing)
    i <- do.call(f,x[by])
    x[i,,drop=FALSE]
  }

  # Function ----------------------------------------------------------------

  if (missing(var1)==FALSE & missing(var2)==TRUE){
    #Sort if using 1 sort variable, warn if duplicates
    var1dupe <- nrow(unique(data[rlang::quo_name(VAR1)]))

    if (var1dupe != nrow(data)){
      cli::cli_warn("Duplicates found when sorting on {rlang::f_text(VAR1)}. Consider switching or adding a second variable (var2) to ensure ID is replicable.")
    }

    data <- sort(data, by=rlang::quo_name(VAR1))

  } else if (missing(var1)==FALSE & missing(var2)==FALSE){
    #Sort if using 2 sort variables, warn if duplicates
    z <- data.frame(v1=as.character(data[[rlang::quo_name(VAR1)]]),
                    v2=as.character(data[[rlang::quo_name(VAR2)]]))
    z$v3 <- paste(z$v1, z$v2, sep="*****")
    bothdupe <- nrow(unique(z["v3"]))

    if (bothdupe != nrow(data)){
      cli::cli_warn("Duplicates found when sorting on {rlang::f_text(VAR1)} and {rlang::f_text(VAR2)}.  ID may not be replicable.")
    }

    data <- sort(data, by=c(rlang::quo_name(VAR1), rlang::quo_name(VAR2)))

  }

  #Create variable name vector to reorder
  data_names <- c(rlang::quo_name(NAME), names(data))
  #Create ID variable
  data[rlang::quo_name(NAME)] <- 1:nrow(data)
  #Reorder
  data <- data[data_names]
  #Drop sorting variables, if requested

  if (remove==TRUE){
    if (missing(var1)==FALSE){
      data[rlang::quo_name(VAR1)] <- NULL
    }
    if (missing(var2)==FALSE){
      data[rlang::quo_name(VAR2)] <- NULL
    }
  }

  rownames(data) <- NULL
  return(data)
}
