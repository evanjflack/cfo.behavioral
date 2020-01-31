#' Combine multiple years of data into one data.table
#' 
#' @param lib_base_data the base directory for data
#' @param file base file name
#' @param years vecotor of years
#' @param pct string, sample percentage
#' @param loud, logical, if TRUE then prints years
#' 
#' @return combined data.table
#' 
#' @importFrom data.table setnames data.table fread
#' 
#' @export
read_and_combine <- function(lib_base_data, file, years, pct, loud = FALSE) {
  DT <- data.table()
  for (i in years) {
    if (loud == T) {
      print(i)
    }
    DT1 <- fread(paste0(lib_base_data, file, "_", i, "_",
                        pct, ".csv")) %>%
      setnames(names(.), tolower(names(.)))
    DT %<>% rbind(DT1)
  }
  return(DT)
}
