#' @title Original name for qplot_cor function
#' @description Original name for the qplot_cor function.  Please use that function
#' instead.

#' @export


qcor_plot <- function(data=NULL, first=NULL, names=NULL, colors=NULL,
                      output=NULL, sort=TRUE, skeleton=FALSE){

  warning("This function will soon be depricated.  Please use qplot_cor instead.")

  qplot_cor(data=data, first=first, names=names, colors=colors,
            output=output, sort=sort, skeleton=skeleton)

}
