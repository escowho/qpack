#' @title Wrapper for a 2-way table from janitor::tabyl
#' @description Generates a 2-way frequency table that is outputed as a janitor::tabyl
#' object that shows counts, row percents, column percents, and total percents.
#' Also uses quasiquotation from rlang so that variables need not be quoted.
#' @param data dataframe object containing the variable
#' @param var1 the row variable for the crosstab
#' @param var2 the column variable for the crosstab
#' @param ... additional arguments passed to janitor::tabyl
#' @return A tabyl object
#' @examples
#' \dontrun{
#' crosstab(caddat, q19_1, q19_2)
#' }
#' @export
#' @importFrom janitor tabyl adorn_totals adorn_title adorn_percentages adorn_pct_formatting
#' @importFrom rlang enquo eval_tidy
#' @importFrom cli cli_abort

crosstab <- function(data, var1, var2, ...){

  #tabyl's error checks mostly sufficient

  if (missing(var1) == TRUE & missing(var2)){
    cli::cli_abort("Variables must be specified")
  }

  if (missing(data) == FALSE & missing(var2)){
    cli::cli_abort("Missing either data or a variable specification")
  }

  DATA <- rlang::enquo(data)
  VAR1 <- rlang::enquo(var1)
  VAR2 <- rlang::enquo(var2)

  t1 <- janitor::tabyl(dat=rlang::eval_tidy(DATA), var1=!!VAR1, var2=!!VAR2, ...) %>%
    janitor::adorn_totals(where=c("row", "col")) %>%
    janitor::adorn_title(placement="top")

  t2 <- janitor::tabyl(dat=rlang::eval_tidy(DATA), var1=!!VAR1, var2=!!VAR2, ...) %>%
    janitor::adorn_totals(where=c("row", "col")) %>%
    janitor::adorn_percentages(denominator="row") %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_title(placement="top", row_name="Row%")

  t3 <- janitor::tabyl(dat=rlang::eval_tidy(DATA), var1=!!VAR1, var2=!!VAR2, ...) %>%
    janitor::adorn_totals(where=c("row", "col")) %>%
    janitor::adorn_percentages(denominator="col") %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_title(placement="top", row_name="Col%")

  t4 <- janitor::tabyl(dat=rlang::eval_tidy(DATA), var1=!!VAR1, var2=!!VAR2, ...) %>%
    janitor::adorn_totals(where=c("row", "col")) %>%
    janitor::adorn_percentages(denominator="all") %>%
    janitor::adorn_pct_formatting() %>%
    janitor::adorn_title(placement="top", row_name="Tot%")

  output <- rbind(t1, rep("", ncol(t1)), t2, rep("", ncol(t1)), t3, rep("", ncol(t1)), t4)
  return(output)
}
