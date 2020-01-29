#' Unpack command line options
#' 
#' @param option_list option list from command line
#' @param print logical, if TRUE then checks for common elemtns of option_list 
#'  and prints them
#' 
#' @export
unpack_opt <- function(option_list) {
  opt <- suppressWarnings(parse_args(OptionParser(option_list=option_list)))
  opt$help <- NULL
  invisible(list2env(opt, .GlobalEnv))
  if (exists("first_year") & exists("last_year")) {
    years <- list(years = seq(opt$first_year, opt$last_year))
    invisible(list2env(years, .GlobalEnv))
  }
  # Print Options
}