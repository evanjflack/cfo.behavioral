make_atc_indicators <- function(DT, DT_id, id_vars, atc_ind) {
  DT_atc <- DT %>% 
    merge(atc_ind, by = "lab_prod") %>% 
    .[, lab_prod := NULL] %>% 
    .[, lapply(.SD, max), by = id_vars] %>% 
    fill_in_zeros(DT_id, id_vars)
  return(DT_atc)
}