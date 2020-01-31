#' Reseahpe wide data from BSF file to long
#' 
#' @param DT data.table
#' @param id_vars string vector of id column name(s)
#' @param reshape_vars string vector, names of variables to reshape to long
#' @param value_name name for the new long variable
#' 
#' @return long data.table
#' 
#' @importFrom data.table melt setnames setorderv
#' 
#' @export
reshape_month_level <- function(DT, id_vars, reshape_vars, value_name) {
  DT %>%
    .[, c(id_vars, reshape_vars), with = F] %>%
    setnames(reshape_vars, as.character(seq(1, 12))) %>%
    melt(id.var = id_vars,
         variable.name = "month",
         value.name = value_name) %>%
    .[, month := as.numeric(as.character(month))] %>%
    setorderv(c(id_vars, "month"), rep(1, length(id_vars) + 1))
}