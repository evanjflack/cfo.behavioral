#' Format linear fit output as a data.table
#' 
#' fit_to_dt() is a function that converts the results of a model into a 
#'  data.table with easily manipulable columns of interaction labels.
#'  
#' @param fit the linear fit (usually lm, lm_robust, or iv_robust)
#' @param primary the vaiable name of the rimary coefficient
#' @param interacts names of the interaction variables (factors)
#' 
#' @return data.table with all coefficients of primary variable
#' 
#' @examples 
#' DT <- as.data.table(mtcars)
#' fit <- lm(mpg ~ wt:factor(cyl) + factor(cyl), data = DT)
#' dt_fit <- fit_to_dt(fit, primary = "wt", interacts = "cyl")
#' 
#' @importFrom broom tidy
#' @importFrom data.table as.data.table
#' @importFrom stringr str_split_fixed
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

if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("term", "estimate", "std.error"))
} 