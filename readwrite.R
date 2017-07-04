#' @export

readwrite <- function(filepath, writepath, sheet = 1, skip = 0, read = TRUE, write = FALSE){
  if (read){
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
  } if (write){
    if (stringr::str_sub(writepath,(stringr::str_length(writepath)-2),-1) == "csv"){
      write.csv(x = filepath, file = writepath)
    } else if (stringr::str_sub(writepath,(stringr::str_length(writepath)-2),-1) == "xls" ||
               stringr::str_sub(writepath,(stringr::str_length(writepath)-3),-1) == "xlsx") {
      WriteXLS::write_excel(filepath, writepath, sheet = sheet)
    }
  }
}

# Import/Export file function by SAM
# Can be used to import/export any file, except SPSS files.
# Who the FUCK uses SPSS anyaway?!
# Dependencies: "stringr", "readxl", "haven", "readr", "WriteXLS"
# "haven" (with dependency on readr) used for importing SAS & STATA files
# For exporting (write = TRUE) filepath is the dataset to be exported.