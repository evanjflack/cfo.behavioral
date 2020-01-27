#' Format linear fit output as a data.table
#' 
#' @param fit the linear fit
#' @param primary the vaiable name of the rimary coefficient
#' @param interacts names of the interaction variables (factors)
#' 
#' @return a data.table()
#' 
#' @export
fit_to_dt <- function(fit, primary, interacts = NULL) {
  dtp_fit <- tidy(fit) %>%
    as.data.table() %>%
    .[grep(primary, term), ] %>%
    .[, term := gsub(paste0(primary, ":"), "", term)]
  
  if (!is.null(interacts)) {
    for (i in interacts) {
      dtp_fit %<>%
        .[, term := gsub(paste0("factor\\(", i, "\\)"), "", term)]
    }
    for (i in 1:length(interacts)) {
      dtp_fit %<>%
        .[, interacts[i] := str_split_fixed(term, ":", length(interacts))[, i]]
    }
  }
  dtp_fit %<>%
    .[, `:=`(lb = estimate - 1.96*std.error, ub = estimate + 1.96*std.error)]
  return(dtp_fit)
}