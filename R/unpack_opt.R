#' Unpack command line options
#' 
#' Take elements of option list and put them in the global environment
#' 
#' @param option_list option list from command line
#' 
#' @importFrom optparse parse_args OptionParser
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