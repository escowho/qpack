#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param data PARAM_DESCRIPTION, Default: NULL
#' @param names PARAM_DESCRIPTION, Default: NULL
#' @param colors PARAM_DESCRIPTION, Default: NULL
#' @param output PARAM_DESCRIPTION, Default: NULL
#' @return OUTPUT_DESCRIPTION
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @export
#' @importFrom fs path_ext path_ext_remove
#' @importFrom psych corr.test
#' @importFrom corrplot corrplot
#' @importFrom tibble rownames_to_column

qcor_plot <- function(data=NULL, names=NULL, colors=NULL, output=NULL){

  # Checks ------------------------------------------------------------------

  if (is.null(data) == TRUE){
    stop(call. = FALSE, "Data must be specified.")
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
    #colors <- c("#d65c45", "#ed9482", "#fbcbc2", "#fde1e1",
    #            "#ffffff",
    #            "#d0dcd2", "#a3baa7", "#77997d", "#4c7956")

    colors <- c("#ff2424", "#ff7269", "#ffaba8", "#fde1e1",
                "#ffffff",
                "#d0dcd2", "#99c69f", "#62af6c", "#1a9635")
  }

  if (is.null(output) == TRUE){
    warning(call. = FALSE,
            "Specify an output location to automatically save jpg of graph \nand an excel file with raw correlation data.")
  } else {

    if(fs::path_ext(output) %in% c("jpg", "jpeg")){
      file_plot <- output
      file_data <- paste0(fs::path_ext_remove(output), ".xlsx")
    } else {
      file_plot <- paste0(output, ".jpg")
      file_data <- paste0(output, ".xlsx")
    }

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
                       order="hclust",
                       tl.col="black",
                       tl.srt=45,
                       addCoef.col="black",
                       #number.cex lowered from 1 to .5 for screen;
                       number.cex=.5,
                       number.digits=2,
                       p.mat=corrs$p,
                       insig="pch",
                       pch.col="black",
                       #pch.cex lowered from 2 to 1.5 for screen
                       pch.cex=1.5)
  } else {
    jpeg(file_plot, width=5000, height=5000, res=300, pointsize=15)
    corrplot::corrplot(corr=corrs$r,
                       col=colors,
                       method="color",
                       type="lower",
                       diag=FALSE,
                       outline=TRUE,
                       order="hclust",
                       tl.col="black",
                       tl.srt=45,
                       addCoef.col="black",
                       number.cex=1,
                       number.digits=2,
                       p.mat=corrs$p,
                       insig="pch",
                       pch.col="black",
                       pch.cex=2)
    invisible(dev.off())

    out_r <- corrs$r %>%
      data.frame() %>%
      tibble::rownames_to_column(., "variable") %>%
      write_xlsx(., file_data, sheet="cors", overfile=TRUE)

    out_p <- corrs$p %>%
      data.frame() %>%
      tibble::rownames_to_column(., "variable") %>%
      mutate_if(is.numeric, ~round(., digits=4)) %>%
      write_xlsx(., file_data, sheet="p_vals")

    #Options? No insig
  }

}
