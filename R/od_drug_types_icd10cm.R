#' Find drug types from ICD-10-CM.
#'
#' Find any drug and selected opioids
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return any_drug_icd10cm, any_opioid_icd10cm, non_heroin_icd10cm, and
#'   heroin_icd10cm
#' @export
#'
#' @examples to be added
#' library(tidyverse)
#' filter(hosp_set, year == 2016) %>% od_drug_types_icd10cm(diag_ecode_col = c(3, 6)) %>% sample_n(5)
od_drug_types_icd10cm <- function(data, diag_ecode_col) {
  cdc_drugs_icd10cm_regex7_ <- "^(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..[1-4](A|$)|((T3[679]9|T414|T427|T4[3579]9)[1-4].(A|$))"

  cdc_opioid_icd10cm_regex7_ <- "(T40[01234].|T406[09])[1-4](A|$)"

  cdc_non_heroin_icd10cm_regex7_ <- "(T40[0234].|T406[09])[1-4](A|$)"

  cdc_heroin_icd10cm_regex7_ <- "T401.[1-4](A|$)"
  cdc_opioid_t40_2_icd10cm_regex7_ <- "T402.[1-4](A|$)"
  cdc_opioid_t40_3_icd10cm_regex7_ <- "T403.[1-4](A|$)"
  cdc_opioid_t40_4_icd10cm_regex7_ <- "T404.[1-4](A|$)"
  cdc_cocaine_t40_5_icd10cm_regex7_ <- "T405.[1-4](A|$)"
  cdc_stimulant_t43_6_icd10cm_regex7_ <- "T436.[1-4](A|$)"



  data %>% mutate(any_drug_icd10cm = od_create_diag(
    ., expr = cdc_drugs_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), any_opioid_icd10cm = od_create_diag(
    ., expr = cdc_opioid_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), non_heroin_icd10cm = od_create_diag(
    ., expr = cdc_non_heroin_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), heroin_icd10cm = od_create_diag(
    ., expr = cdc_heroin_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), opioid_t40_2_icd10cm = od_create_diag(
    ., expr = cdc_opioid_t40_2_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), opioid_t40_3_icd10cm = od_create_diag(
    ., expr = cdc_opioid_t40_3_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), opioid_t40_4_icd10cm = od_create_diag(
    ., expr = cdc_opioid_t40_4_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), cocaine_t40_5_icd10cm = od_create_diag(
    ., expr = cdc_cocaine_t40_5_icd10cm_regex7_,
    colvec = diag_ecode_col
  ), stimulant_t43_6_icd10cm = od_create_diag(
    ., expr = cdc_stimulant_t43_6_icd10cm_regex7_,
    colvec = diag_ecode_col
  )) %>% 
  mutate(non_heroin_icd10cm = ifelse(heroin_icd10cm == 1, 0, non_heroin_icd10cm))
}
