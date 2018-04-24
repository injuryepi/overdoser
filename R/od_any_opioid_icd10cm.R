#' Any opioid case.
#'
#' @param data: input data
#' @param diag_ecode_col: indices of diagnosis and
#' E codes of interest
#' @param any_opioid_icd10cm: choose the name for the created variable
#'
#' @return variable any_opioid_icd10cm (or the chosen name)
#' @export
#'
#' @examples to be added
od_any_opioid_icd10cm <- function(data, diag_ecode_col, any_opioid_icd10cm = any_opioid_icd10cm) {
	any_opioid_icd10cm <- enquo(any_opioid_icd10cm)
	any_opioid_icd10cm_name <- quo_name(any_opioid_icd10cm)
	cdc_opioid_icd10cm_regex7_ <- "(T40[01234].|T406[09])(1|2|3|4)(A|$)"
	data %>% mutate(!!any_opioid_icd10cm_name := od_create_diag(., expr = cdc_opioid_icd10cm_regex7_,
																															colvec = diag_ecode_col))
}
