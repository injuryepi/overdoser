#' Title
#'
#' @param data: input data
#' @param diag_ecode_col: indices of diagnosis and
#' E codes of interest
#'
#' @return variable any_opioid_icd10cm
#' @export
#'
#' @examples
add_any_opioid_icd10cm <- function(data, diag_ecode_col){

	cdc_opioid_icd10cm_regex7_ <- "(T40[01234].|T406[09])(1|2|3|4)(A|D|$)"
	data %>%
		mutate(any_opioid_icd10cm =
					 	od_create_diag(., expr = cdc_opioid_icd10cm_regex7_,
					 							colvec = diag_ecode_col))
}
