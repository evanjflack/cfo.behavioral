#' Combine list of data.tables into one data.table
#'
#' @param x list of data.tables
#' @param ... additional arguments
#'
#' @return Combined data.table
#'
#' @export
comb <- function(x, ...) {
  DT <- lapply(seq_along(x),
               function(i) c(x[[i]], lapply(list(...), function(y) y[[i]])))
  return(DT)
}
