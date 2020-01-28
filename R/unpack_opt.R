#' Unpack command line options
#' 
#' @param option_list option list from command line
#' 
#' @export
unpack_opt <- function(option_list, print = TRUE) {
  opt <- suppressWarnings(parse_args(OptionParser(option_list=option_list)))
  opt$help <- NULL
  invisible(list2env(opt, .GlobalEnv))
  if (exists("first_year") & exists("last_year")) {
    years <- list(years = seq(first_year, last_year))
    invisible(list2env(years, .GlobalEnv))
  }
  # Print Options
  if (print == T) {
    if ("pct" %in% names(opt)) {
      message("pct = ", opt$pct)
    }
    if ("first_year" %in% names(opt) & "last_year" %in% names(opt)) {
      message("years = ", first_year, "-", last_year)
    }
    if ("resp_var" %in% names(opt)) {
      message("resp_var = ", resp_var)
    }
  }
}