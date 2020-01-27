#' Start a log file
#' 
#' @param log_file logical, if TRUE then starts a log file
#' @param file_name path to log file (relative to current directory)
#' 
#' @export
start_log_file <- function(log_file, file_name) {
  if (log_file == T) {
    full_name <- paste0(file_name, ".log")
    con <- file(full_name)
    sink(con)
    sink(con, type = "message")
    message(paste(rep("-", 80), collapse = ""))
    message(full_name)
    tic()
    message(Sys.time())
    message("")
  }
}

#' End a log file
#' @param log_file logicial, if TRUE then closes connection to current log file
#' 
#' @export
end_log_file <- function(log_file) {
  if (log_file == T) {
    message("")
    toc()
    message(Sys.time())
    message(paste(rep("-", 80), collapse = ""))
    sink()
    sink(type="message")
  }
}