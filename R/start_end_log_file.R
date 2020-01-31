#' Start a .log file
#' 
#' @param log_file logical (default TRUE), if TRUE then opens a connection to 
#'  a .log file and diverts messages and output to that file
#' @param file_name string, path to log file (relative to current directory), 
#'  do not include .log in the path
#' @param print, logical (defaut TRUE), if TRUE checks for common parameters in
#'  the global environment and prints them at the top of the .log file
#' 
#' @export
start_log_file <- function(log_file = TRUE, file_name = NULL, print = TRUE) {
  if (log_file == TRUE) {
    if (is.null(file_name)) {
      stop("File name required")
    }
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
  if (print == TRUE) {
    if (print == T) {
      if (exists("pct")) {
        message("pct = ", pct)
      }
      if (exists("first_year") & exists("last_year")) {
        message("years = ", first_year, "-", last_year)
      }
      if (exists("resp_var")) {
        message("resp_var = ", resp_var)
      }
    }
  }
}

#' End a .log file
#' 
#' Checks to see if a connection is open to a .log file, and ends the connection
#'  if one exists. 
#' 
#' @export
end_log_file <- function() {
  if (length(showConnections(all = FALSE))) {
    message("")
    toc()
    message(Sys.time())
    message(paste(rep("-", 80), collapse = ""))
    sink()
    sink(type="message")
  }
}