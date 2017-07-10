#' @export

readwrite <- function(filepath, writepath, sheet = 1, skip = 0, read_write){
  if (read_write == "read"){
    if (stringr::str_sub(filepath,(stringr::str_length(filepath)-2),-1) == "csv"){
    read.csv(filepath)
    } else if (stringr::str_sub(filepath,(stringr::str_length(filepath)-2),-1) == "xls" ||
               stringr::str_sub(filepath,(stringr::str_length(filepath)-3),-1) == "xlsx") {
        readxl::read_excel(filepath, sheet = sheet, trim_ws = TRUE, skip = skip)
    } else if (stringr::str_sub(filepath,(stringr::str_length(filepath)-2),-1) == "dta"){
      haven::read_dta(filepath)
    } else if (stringr::str_sub(filepath,(stringr::str_length(filepath)-7),-1) == "sas7bdat"){
      haven::read_sas(filepath)
    }
  } else if (read_write == "write"){
    if (stringr::str_sub(writepath,(stringr::str_length(writepath)-2),-1) == "csv"){
      write.csv(x = filepath, file = writepath)
    } else if (stringr::str_sub(writepath,(stringr::str_length(writepath)-2),-1) == "xls" ||
               stringr::str_sub(writepath,(stringr::str_length(writepath)-3),-1) == "xlsx") {
      xlsx::write.xlsx(filepath, writepath, sheetName = sheet, row.names = FALSE)
    } 
  } else if (!(read_write == "read") && !(read_write == "write")){
      print.noqoute("read/write argument ERROR")
  }
}

# Import/Export file function by SAM
# Can be used to import any file, except SPSS files.
# Who the FUCK uses SPSS anyaway?!
# Dependencies: "stringr", "readxl", "haven", "readr", "xlsx"
# "haven" (with dependency on readr) used for importing SAS & STATA files
# For exporting (write = TRUE) filepath is the dataset to be exported. And, sheet should be in double quotes. eg - "sheet1"
