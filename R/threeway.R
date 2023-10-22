#' @title Performs 3-way crosstab using qpack::crosstab split on a third variable
#' @description Generates a 3-way crosstab, or a qpack::crosstab for each value
#' of the third variable specified.  Built on janitor::tabyl but outputs a dataframe
#' to maintain visual elements suitable for exporting (but likely not for the console).
#' Object shows counts, row percents, column percents, and total percents for each
#' level of third variable.  Also uses quasiquotation from rlang so that variables
#' need not be quoted.  var1 and var2 specify the crosstab and var3 specifies the split.
#' @param data the dataframe object containing the variable. Required.
#' @param var1 the row variable for the crosstab. Required.
#' @param var2 the column variable for the crosstab. Required.
#' @param var3 the column variable to break the crosstab. Required.
#' @param ... additional arguments for janitor::tabyl. Optional.
#' @return A tibble object.
#' @examples
#' \dontrun{
#' output <- threeway(caddat, q19_1, q19_2, speclty)
#' }
#' @export
#' @importFrom rlang enquo f_text
#' @importFrom cli cli_abort

threeway <- function(data, var1, var2, var3, ...){

  if (missing(data) | missing(var1) | missing(var2) | missing(var3)){
    cli::cli_abort("Must specify DATA and 3 VARIABLES")
  }

  DATA <- rlang::enquo(data)
  VAR1 <- rlang::enquo(var1)
  VAR2 <- rlang::enquo(var2)
  VAR3 <- rlang::enquo(var3)

  #output <- data %>%
  #  dplyr::select(!!VAR1, !!VAR2, BY_VAR = !!VAR3) %>%
  #  dplyr::mutate_all(as.factor) %>%
  #  split(.$BY_VAR) %>%
  #  purrr::map(qpack::crosstab, !!VAR1, !!VAR2, ...)

  output <- data[c(rlang::quo_name(VAR1), rlang::quo_name(VAR2), rlang::quo_name(VAR3))]
  output[] <- lapply(output, factor)
  names(output)[3] <- "BY_VAR"
  output <- split(output, output$BY_VAR)
  output <- lapply(output, qpack::crosstab, !!VAR1, !!VAR2)

  NAMES <- names(output)
  VARIABLE <- rlang::f_text(VAR3)

  for (i in seq_along(output)){
    names(output[[i]])[1] <- paste0(NAMES[i])
    temp <- output[[i]]
    names <- c(colnames(temp), "")
    temp$spacer <- ""
    names(temp) <- c(paste0("t", i, "_", 1:ncol(temp)))
    temp$num <- 1:nrow(temp)
    temp <- rbind(temp, c(names, 0))
    temp$num <- as.numeric(temp$num)
    temp <- temp[order(temp$num),]
    output[[i]] <- temp
  }

  #output <- purrr::reduce(output, dplyr::left_join, by="num")
  output <- Reduce(function(x, y) merge(x, y, by="num" ), output)
  output$num <- NULL
  names(output) <- c(VARIABLE, rep("", ncol(output)-1))
  return(output)
}
