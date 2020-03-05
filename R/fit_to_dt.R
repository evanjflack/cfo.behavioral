#' Format (specific) linear fit output as a data.table
#' 
#' fit_to_dt() is a function that converts the results of a linear model with 
#'  one continuous regressor of interest (e.g. first_mo) that is interacted with 
#'  factor variable (e.g. risk_cut) to a data.table with easily manipulable 
#'  columns of interaction labels. This is useful for plotting the results of 
#'  the elasticty by risk models.
#'  
#' @param fit the linear fit (usually lm, lm_robust, or iv_robust). The fit must
#'  be specified to estimate the coefficient on the continuous variable for all 
#'  levels of interactions (e.g. atc4_C10AA ~ first_mo:factor(pred_cut) + 
#'  factor(pred_cut))
#' @param primary character, the vaiable name of the continuous regressor of 
#'  interest (usually first_mo)
#' @param interacts character vector, names of the interaction variables 
#'  (factors)
#' 
#' @return data.table with the estiated coefficients of the primary variable
#'  for each interction combination (along with SEs and CIs).
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