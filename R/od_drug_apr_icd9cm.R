#' Find drug types based on ICD-9-CM.
#'
#' Find any drug and other opioids from ICD-9-CM
#'
#' @param data: input data
#' @param diag_col: indices of principal diagnosis
#' @param ecode_col: indices of ecodes
#' @return any_drug, any_opioid, non_heroin_opioid, and
#'   heroin
#' @export
#'
#' @examples
#' library(tidyverse)
#' filter(hosp_set, year == 2014) %>% od_drug_apr_icd9cm(diag_col = 3, ecode_col c(3:5)) %>% sample_n(5)
#'
od_drug_apr_icd9cm <- function(data, diag_col, ecode_col) {

    drugs_icd9cm1_ <- "^9[67]"
    drugs_icd9cm2_ <- "^E85[0-8]|^E950[0-5]|^E9620|^E980[0-5]"

    opioid_icd9cm1_ <- "^9650[0129]"
    opioid_icd9cm2_ <- "^E850[012]"

    non_heroin_icd9cm1_ <- "^9650[029]"
    non_heroin_icd9cm2_ <- "^E850[12]"

    heroin_icd9cm1_ <- "^96501"
    heroin_icd9cm2_ <- "^E8500"

    data %>% mutate(any_drug = od_create_diag_9(., expr1 = drugs_icd9cm1_, colvec1 = diag_col, expr2 = drugs_icd9cm2_, colvec2 = ecode_col),

    								any_opioid = od_create_diag_9(., expr1 = opioid_icd9cm1_, colvec1 = diag_col, expr2 = opioid_icd9cm2_, colvec2 = ecode_col),

    								non_heroin_opioid = od_create_diag_9(., expr1 = non_heroin_icd9cm1_, colvec1 = diag_col, expr2 = non_heroin_icd9cm2_, colvec2 = ecode_col),
    								heroin = od_create_diag_9(., expr1 = heroin_icd9cm1_, colvec1 = diag_col, expr2 = heroin_icd9cm2_, colvec2 = ecode_col)) %>%
    	mutate( non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid))
}
