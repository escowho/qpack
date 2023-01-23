#' @title Wrapper for corrplot that generates a reasonably consistent corrplot
#' @description Generates a corrplot that tends to look good in most situations.
#' The corrplot will be exported as a jpg if the output parameter is specified.
#' The underlying correlation table and p values is also automatically exported
#' with the same name but with an .xlsx extension.
#' Note that the on-screen version is not optimally designed and that this function
#' is optimized for the outputted jpg version.  Possible to provide a list of names
#' to replace variable names (the default action).  Can also specify colors but
#' there is a standardized set of colors as default from red to white to green for
#' correlation color coding.  Option to sort based on overall correlation size
#' (happens by default).  Can identify the name of a variable that show first in
#' the output, which is particularly useful for dependent variables like NPS score
#' (but this will disengage any other sorting).
#' @param data Name of the dataframe upon which to build the corrplot.  All variables
#' will be included.  Required.
#' @param names Vector of quoted strings to be used in place of the variable names
#' in the resulting plot.  Must be the same number of names as variables in the
#' data. Optional.
#' @param first The name of a variable to appear first, or left-most, on the corrplot.
#' Use of the first parameter disables sorting.  Be sure to change the order of
#' any supplied names when using the first option. Optional.
#' @param colors Vector of quoted hexadecimal color codes to be used for coloring
#' the correlations.  Default is to go from dark green to light green for positive
#' correlations, white for no correlation, and light red to dark red for negative
#' correlations. Optional.
#' @param output Character string containing the complete path and file name of
#' a jpg file of the resulting corrplot.  If not provided, the plot will be shown
#' on the screen instead but may not be optimized for screen viewing.  The underlying
#' correlation table and p values is also automatically exported with the same
#' name but with an .xlsx extension. Optional.
#' @param sort A logical value to indicate if the output should be ordered by
#' strength of correlation.  If set to TRUE, this sets the parameter order = c("hclust")
#' in the corrplot function. Default: TRUE
#' @param skeleton A logical value to indicate if the syntax should be printed to
#' the console.  This is useful if the default selections need tweaking so the
#' user can see the raw syntax that can be used in place of the function.
#' Default: FALSE
#' @param ... Pass additional parameters to corrplot.  Optional.
#' @return Outputs a jpg file of the corrplot if output path specified, otherwise
#' generates plot on screen.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  test1 %>%
#'  dplyr::select(q1:q6) %>%
#'    qcor_plot(., names=c("Age", "Gender", "Race", "Insurance", "Income", "Education"))

#'  test1 %>%
#'    dplyr::select(q1:q6) %>%
#'    qcor_plot(data=., first="q5", sort=FALSE)
#'  }
#' }
#' @export
#' @importFrom rlang enquo quo_name f_text
#' @importFrom dplyr relocate mutate_if
#' @importFrom fs path_ext path_ext_remove
#' @importFrom psych corr.test
#' @importFrom corrplot corrplot
#' @importFrom tibble rownames_to_column

qcor_plot <- function(data=NULL, first=NULL, names=NULL, colors=NULL,
                      output=NULL, sort=TRUE, skeleton=FALSE,...){

  # Skeleton ----------------------------------------------------------------

  if (skeleton==TRUE){
    cat(paste0("corr <- psych::corr.test(DATASET_NAME_HERE)
jpeg(file_plot, width=5000, height=5000, res=300, pointsize=16)
corrplot::corrplot(corr=corrs$r,
                   col=c(\"#ff2424\", \"#ff7269\", \"#ffaba8\", \"#fde1e1\",
                         \"#ffffff\",
                         \"#d0dcd2\", \"#99c69f\", \"#62af6c\", \"#1a9635\"),
                   method=\"color\",
                   type=\"lower\",
                   diag=FALSE,
                   outline=TRUE,
                   order=\"hclust\",
                   tl.col=\"black\",
                   tl.srt=45,
                   addCoef.col=\"black\",
                   number.cex=1.3,
                   number.digits=2,
                   p.mat=corrs$p,
                   insig=\"pch\",
                   pch=\"X\",
                   pch.col=\"black\",
                   pch.cex=2)
invisible(dev.off())"))
  } else {
    # Checks ------------------------------------------------------------------

    if (is.null(data) == TRUE){
      stop(call. = FALSE, "Data must be specified.")
    }

    if (is.null(first) == FALSE){
      FIRST <- rlang::enquo(first)

      if (! rlang::quo_name(FIRST) %in% colnames(data)){
        stop(call. = FALSE, paste0("First parameter variable, ",
                                   rlang::f_text(FIRST),
                                   " not found in data."))
      }

      data <- data %>%
        dplyr::relocate({{first}})
    }

    if (is.null(names) == FALSE){
      if (ncol(data) != length(names)){
        stop(call. = FALSE,
             paste0("Number of names specified not equal to number of columns in data", "\n",
                    "# of cols in the data:", ncol(data), "\n",
                    "length of name vector:", length(names)))
      } else {
        data <- data %>%
          set_colnames(names)
      }
    }

    if (is.null(colors) == TRUE){
      colors <- c("#ff2424", "#ff7269", "#ffaba8", "#fde1e1",
                           "#ffffff",
                           "#d0dcd2", "#99c69f", "#62af6c", "#1a9635")
    }

    if (is.null(output) == FALSE){
      if(fs::path_ext(output) %in% c("jpg", "jpeg")){
        file_plot <- output
        file_data <- paste0(fs::path_ext_remove(output), ".xlsx")
      } else {
        file_plot <- paste0(output, ".jpg")
        file_data <- paste0(output, ".xlsx")
      }
    }

    if(sort==TRUE & is.null(first)==FALSE){
      warning(call. = FALSE,
              "Can't sort output since the first option is being used.")
      order_entry <- "original"
    } else if(sort==TRUE & is.null(first==TRUE)){
      order_entry <- "hclust"
    } else {
      order_entry <- "original"
    }

    # Function ----------------------------------------------------------------

    corrs <- psych::corr.test(data)

    # Screen Version ----------------------------------------------------------

    if(is.null(output)){
      corrplot::corrplot(corr=corrs$r,
                         col=colors,
                         method="color",
                         type="lower",
                         diag=FALSE,
                         outline=TRUE,
                         order=order_entry,
                         tl.col="black",
                         tl.srt=45,
                         addCoef.col="black",
                         number.cex=.5,
                         number.digits=2,
                         p.mat=corrs$p,
                         insig="pch",
                         pch="X",
                         pch.col="black",
                         pch.cex=1.5,
                         ...)
    } else {

      # Output Version ----------------------------------------------------------

      jpeg(file_plot, width=5000, height=5000, res=300, pointsize=16)
      corrplot::corrplot(corr=corrs$r,
                         col=colors,
                         method="color",
                         type="lower",
                         diag=FALSE,
                         outline=TRUE,
                         order=order_entry,
                         tl.col="black",
                         tl.srt=45,
                         addCoef.col="black",
                         number.cex=1.3,
                         number.digits=2,
                         p.mat=corrs$p,
                         insig="pch",
                         pch="X",
                         pch.col="black",
                         pch.cex=2,
                         ...)
      invisible(dev.off())

      out_r <- corrs$r %>%
        data.frame() %>%
        tibble::rownames_to_column(., "variable") %>%
        write_xlsx(., file_data, sheet="cors", overfile=TRUE)

      out_p <- corrs$p %>%
        data.frame() %>%
        tibble::rownames_to_column(., "variable") %>%
        dplyr::mutate_if(is.numeric, ~round(., digits=4)) %>%
        write_xlsx(., file_data, sheet="p_vals")
    }
  }

}
