#' Import/Export any file
#'
#' Imports CSV, XLS, XLSX, DTA (stata), and SAS7BDAT (sas) files.
#' Exports to CSV, XLS, XLSX files.
#' It auto detects the file extension in the \code{filepath} or \code{writepath} to read/write the file in your desired extension.
#' @usage
#' \code{readwrite(filepath, writepath, varname = TRUE, sheet = 1, skip = 0,
#'     read = TRUE, write = FALSE)}
#' @param filepath The path of the file that is to be imported. In case of exporting a file, it is the file that is to be exported.
#' @param writepath Only when exporting (write = \code{TRUE}). The path where the file is to be exported.
#' @param varname Default set to \code{TRUE}, suggesting that first row contains variable names. Set to \code{FALSE} if there is no variable name in the first row.
#' @param sheet In case of importing an excel file, it refers to the sheet that is to be imported from the file. In case of exporting an excel file, it refers to the name of the sheet (in double quotes) that you would like to give.
#' @param skip In case of importing an excel file, it refers to the number of rows to be skipped while importing. Starts from 1 which is the second row, as first row is considered as column name, if \code{varname} is set \code{TRUE}.
#' @param read If importing, set it to \code{TRUE} (default). If exporting, set it to \code{FALSE}.
#' @param write If importing, set it to \code{FALSE} (default). If exporting, set it to \code{TRUE}.
#' @details This functions reads CSV, XLS, XSLX, DTA (stata files), SAS7BDAT (sas files), and writes CSV, XLS, XLSX files. Can be used to Import and Export data frames.
#' @note
#' Only data frames (\code{data.frame}) can be exported. This function coerces the table in a data frame, while writing it to a file.
#'
#' The fuction relies on Java (with the same architechture as your R) to export files. Credit goes to the authors of "xlsx" package.
#' @examples
#' ##Import
#' file <- readwrite("C:/Users/Personal/Documents/file.xlsx", varname = TRUE,
#'     sheet = 2)
#' ## Export
#' readwrite(file, "C:/Users/Personal/Documents/filesheet2.xlsx",
#'     sheet = "sheet name", read = FALSE, write = TRUE)
#' @export
readwrite <- function(filepath, writepath, varname = TRUE, sheet = 1, skip = 0, read = TRUE, write = FALSE){
  if (read){
    if (stringr::str_sub(filepath,(stringr::str_length(filepath)-2),-1) == "csv"){
    read.csv(filepath, header = varname, skip = skip)
    } else if (stringr::str_sub(filepath,(stringr::str_length(filepath)-2),-1) == "xls" ||
               stringr::str_sub(filepath,(stringr::str_length(filepath)-3),-1) == "xlsx") {
        readxl::read_excel(filepath, sheet = sheet, trim_ws = TRUE, skip = skip, col_names = varname)
    } else if (stringr::str_sub(filepath,(stringr::str_length(filepath)-2),-1) == "dta"){
      haven::read_dta(filepath)
    } else if (stringr::str_sub(filepath,(stringr::str_length(filepath)-7),-1) == "sas7bdat"){
      haven::read_sas(filepath)
    }
  } else if (write){
    if (stringr::str_sub(writepath,(stringr::str_length(writepath)-2),-1) == "csv"){
      write.csv(x = filepath, file = writepath)
    } else if (stringr::str_sub(writepath,(stringr::str_length(writepath)-2),-1) == "xls" ||
               stringr::str_sub(writepath,(stringr::str_length(writepath)-3),-1) == "xlsx") {
      xlsx::write.xlsx(as.data.frame(filepath), writepath, sheetName = sheet, row.names = FALSE)
    }
  }
}

#' Import/Export file function by SAM
#' Can be used to import/export any file, except SPSS files.
#' Who the FUCK uses SPSS anyaway?!
#' Dependencies: "stringr", "readxl", "haven", "readr", "xlsx"
#' NOTE: Package "xlsx" dependes on "rJava" which needs your Java to be on the same architechture as R
#' "haven" (with dependency on readr) used for importing SAS & STATA files
#' For exporting (write = TRUE) filepath is the dataset to be exported.
#' Only data.frame() can be exported -- "xlsx"
