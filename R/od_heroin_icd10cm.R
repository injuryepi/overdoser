#' Find heroin based on ICD-10-CM.
#'
#' Find heroin based on ICD-10-CM.
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return heroin_icd10cm
#' @export
#'
#' @examples to be added
od_heroin_icd10cm <- function(data, diag_ecode_col){

	cdc_heroin_icd10cm_regex7_ <- "T401.(1|2|3|4)(A|D|$)"
	data %>%
		mutate(heroin_icd10cm =
					 	create_diag(., expr = cdc_heroin_icd10cm_regex7_,
					 							colvec = diag_ecode_col))
}
