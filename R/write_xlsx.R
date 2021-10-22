#' @title Wrapper function for openxlsx that exports a data frame to an XLSX file
#' @description Takes a dataframe or other table and exports it as an XLSX file.
#' Sheet name can be specified to add to an existing sheet.  Options include
#' ability to overwrite the sheet(oversheet), overwrite the file (overfile), or
#' to use a default (over = TRUE) that overwrites both the sheet and the file.
#' A keepna option controls whether NA values appear in the sheet.
#' @param data A dataframe containing to be exported. Required.
#' @param file The file path and name, ending in .xlsx, to be written. Required.
#' @param sheet  The name of the sheet inside the Excel file.  If missing then
#' the name of the dataframe will be used.  Optional.
#' @param over Logical that sets the overwriting defaults to what is the likely
#' most common values (over = TRUE is the same as overfile = TRUE and oversheet
#' = TRUE). Default: FALSE.
#' @param overfile Logical to indicate if the Excel file should be overwritten
#' or not. Default: FALSE.
#' @param oversheet Logical to indicate if the Sheet inside the file should be
#' overwritten or not. Default: True.
#' @param keepna Logical to set if NA values are kept in the written file
#' or not. Default: FALSE.
#' @param rownames Logical to set if row names should be written to the file
#' or note.  Default: FALSE.
#' @return This outputs an XLSX file and does not return anything
#' @examples
#' \dontrun{
#' write_xlsx(df, file="C:/TEMP/test.xlsx", over = TRUE, keepna=TRUE)
#' write_xlsx(df, file="C:/TEMP/test.xlsx", overfile = TRUE,  oversheet=TRUE, keepna=TRUE)
#' }
#' @export
#' @import openxlsx

write_xlsx <- function(data, file, sheet=NULL, over=FALSE, overfile=FALSE,
                       oversheet=TRUE, keepna=FALSE, rownames=FALSE){

  # Checks ------------------------------------------------------------------
  if (missing(data) == TRUE){
    stop(call. = FALSE, "Nothing specified to save.")
  }

  if (missing(file) == TRUE){
    stop(call. = FALSE, "File name and path must be specified.")
  }

  # Function ----------------------------------------------------------------
  #If sheet not specified then using dataset name
  if (is.null(sheet) == TRUE) {
    sheet <- as.character(substitute(data))
  }

  #Over is a shorthand to adjust overwriting the file for my preferred situations
  if (over == TRUE) {
    overfile = TRUE
    oversheet = TRUE
  }

  #File existance and overfile checks
  if (file.exists(file) & overfile == TRUE){
    file.remove(file)
    wb <- openxlsx::createWorkbook()
  } else if (file.exists(file) & overfile != TRUE){
    #This assumes we're adding to the workbook, could be a warning location
    wb <- openxlsx::loadWorkbook(file)
  } else {
    wb <- openxlsx::createWorkbook()
  }

  #Sheet existance and oversheet checks
  if (sheet %in% names(wb) & oversheet != TRUE){
    stop("call. = FALSE, Sheet already exists but oversheet set to FALSE")
  } else if (sheet %in% names(wb) & oversheet == TRUE){
    openxlsx::removeWorksheet(wb, sheet)
    openxlsx::addWorksheet(wb, sheetName = sheet)
  } else {
    openxlsx::addWorksheet(wb, sheetName = sheet)
  }

  #Write data to sheet
  openxlsx::writeData(wb=wb, sheet=sheet, x=data, keepNA=keepna, rowNames=rownames)

  #Save the workbook
  openxlsx::saveWorkbook(wb, file = file, overwrite = TRUE)

}
