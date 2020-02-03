#' Unpack IP Claims
#' 
#' @param ip data.table of inpatient claims with the columns bene_id, clm_id,
#'  from_dt, and dgnscd1
#' @param num_prcdr integer (1-6), number of procedure codes to use
#'  
#' @return list with 2 elements
#' \item{ip_diag}{data.table of ip diagnosis}
#' \item{ip_prcdr}{data.table of ip prcedures}
#' 
#' @importFrom data.table melt
#' 
#' @export 
unpack_ip <- function(ip, num_prcdr) {
  ip_diag <- ip %>%
    .[clm_ln == 1, ] %>%
    .[, c("bene_id", "clm_id", "from_dt", "dgnscd1"), with = FALSE]
  
  prcdr_vars <- paste0("prcdrcd", seq(1, num_prcdr))
  dt_vars <- paste0("prcdrdt", seq(1, num_prcdr))
  
  ip_prcdrcd <- ip %>%
    .[clm_ln == 1, ] %>%
    .[, c("bene_id", "clm_id", prcdr_vars), with = F] %>%
    melt(id.var = c("bene_id", "clm_id"),
         value.name = "prcdrcd", variable.name = "code_num")
  
  ip_prcdrdt <- ip %>%
    .[clm_ln == 1, ] %>%
    .[, c("bene_id", "clm_id", dt_vars), with = F] %>%
    melt(id.var = c("bene_id", "clm_id"),
         value.name = "prcdrdt", variable.name = "code_num")
  
  ip_prcdr <- cbind(ip_prcdrcd, ip_prcdrdt[, c("prcdrdt"), with = F]) %>%
    .[!(is.na(prcdrcd) | prcdrdt == ""), ]
  
  return(list(ip_diag = ip_diag, ip_prcdr = ip_prcdr))
  
}

#' Create IP Diagnosis Indicators
#' 
#' @param DT data.table with ip claims
#' @param DT_id data.table with all id_vars
#' @param id_var character vecotr with names of coumns to aggregate on
#' @param ami_codes character vector of ami icd9 codes
#' @param stroke_codes character vector of stroke icd9 codes
#' @param diab_codes character vector of diabetes icd9 codes
#' 
#' @return data.table with diagnoisis indicators
#' 
#' @export
create_diag_indicators <- function(DT, DT_id, id_var, ami_codes, stroke_codes, 
                                   diab_codes) {
  # Make indicators
  DT %<>%
    .[, resp_fail := ifelse(substr(dgnscd1, 1, 4) == "5188", 1, 0)] %>%
    .[, resp_arr := ifelse(substr(dgnscd1, 1, 4) == "7991", 1, 0)] %>%
    .[, ami := ifelse(substr(dgnscd1, 1, 3) %in% ami_codes, 1, 0)] %>%
    .[, stroke := ifelse(substr(dgnscd1, 1, 3) %in% stroke_codes, 1, 0)] %>%
    .[, comp_diab := ifelse(dgnscd1 %in% diab_codes, 1, 0)]
  
  # Aggregate and fill in 0s
  DT %<>%
    .[, lapply(.SD, max), by = id_var, 
      .SDcols = c("ami", "stroke", "resp_fail", "resp_arr", "comp_diab")] %>%
    fill_in_zeros(DT_id, "bene_id")
  
  return(DT)
}

#' Create IP Procedure Indicators
#' 
#' @param DT data.table with ip claims
#' @param DT_id data.table with all id_vars
#' @param id_var character vecotr with names of coumns to aggregate on
#' 
#' @return data.table with procedure indicators
#' 
#' @export
create_prcdr_indicators <-  function(DT, DT_id, id_var) {
  
  DT %<>% 
    .[, tube := ifelse(substr(prcdrcd, 1, 4) == "9604", 1, 0)] %>% 
    .[, vent := ifelse(substr(prcdrcd, 1, 3) == "967", 1, 0)] 
  
  DT %<>% 
    .[, lapply(.SD, max), by = id_var, 
      .SDcols = c("tube", "vent")] %>% 
    fill_in_zeros(DT_id, "bene_id")
  
  return(DT)
}

# Deal with R CMD check
if(getRversion() >= "2.15.1") {
  utils::globalVariables(c("clm_ln", "prcdrcd", "prcdrdt", "dgnscd1", 
                           "ami", "comp_diab", "resp_arr", "resp_fail",
                           "stroke", "tube", "vent"))
}