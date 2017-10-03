#' Find any drug.
#'
#' Find any drug as defined by CDC
#'
#' @param data: input data
#' @param diag_ecode_col: inumeric vector of the selected
#' diagnosis and E codes
#'
#' @return variable any_drug_icd10cm
#' @export
#'
#' @examples
#' library(tidyverse)
#' library(overdoser)
#' hosp_set %>% od_any_drug_icd10cm(diag_ecode_col = c(3,6)) %>% sample_n(10)
od_any_drug_icd10cm <- function(data, diag_ecode_col) {
    
    cdc_drugs_icd10cm_regex7_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(1|2|3|4)(A|D|$)|((T3[679]9|T414|T427|T4[3579]9)(1|2|3|4).(A|D|$))"
    data %>% mutate(any_drug_icd10cm = od_create_diag(., expr = cdc_drugs_icd10cm_regex7_, 
        colvec = diag_ecode_col))
}
