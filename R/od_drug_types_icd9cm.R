#' Find drug types based on ICD-9-CM.
#'
#' Find any drug and other opioids from ICD-9-CM
#'
#' @param data: input data
#' @param diag_ecode_col: indices of principal diagnosis and first E code
#'
#' @return any_drug_icd9cm, any_opioid_icd9cm, non_heroin_icd9cm, and
#'   heroin_icd9cm
#' @export
#'
#' @examples
#' ibrary(tidyverse)
#' filter(hosp_set, year == 2014) %>% od_drug_types_icd9cm(diag_ecode_col = c(3, 6)) %>% sample_n(5)
od_drug_types_icd9cm <- function(data, diag_ecode_col) {
    
    cdc_drugs_icd9cm_regex_ <- "^9[67]|^E85[0-8]|^E950[0-5]|^E9620|^E980[0-5]"
    
    cdc_opioid_icd9cm_regex_ <- "^9650[0129]|^E850[012]"
    
    cdc_non_heroin_icd9cm_regex_ <- "^9650[029]|^E850[12]"
    
    cdc_heroin_icd9cm_regex_ <- "^96501|^E8500"
    
    data %>% mutate(any_drug_icd9cm = od_create_diag(., expr = cdc_drugs_icd9cm_regex_, 
        colvec = diag_ecode_col), any_opioid_icd9cm = od_create_diag(., expr = cdc_opioid_icd9cm_regex_, 
        colvec = diag_ecode_col), non_heroin_icd9cm = od_create_diag(., expr = cdc_non_heroin_icd9cm_regex_, 
        colvec = diag_ecode_col), heroin_icd9cm = od_create_diag(., expr = cdc_heroin_icd9cm_regex_, 
        colvec = diag_ecode_col))
}
