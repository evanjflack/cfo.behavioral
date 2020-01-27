#' Unpack command line options
#' 
#' @param option_list option list from command line
#' 
#' @export
unpack_opt <- function(option_list) {
  opt <- suppressWarnings(parse_args(OptionParser(option_list=option_list)))
  opt$help <- NULL
  invisible(list2env(opt, .GlobalEnv))
  # if (exists("first_year") & exists("last_year")) {
  #   years <- seq(first_year, last_year)
  #   environment(years) <- .GlobalEnv
  # }
}

#' Print sample options
#' 
#' @param print_list list of things to print
#' 
#' @export
print_opts <- function(print_list){
  print_vec <- unlist(print_list)
  p <- ""
  for (i in names(print_vec)) {
    p <-  paste0(p, i, " = ", print_vec[i], "\n")
  }
  message(p)
}