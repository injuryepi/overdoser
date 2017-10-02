#' Title
#'
#' @param data
#' @param diag_ecode_col
#'
#' @return
#' @export
#'
#' @examples
od_drug_types_icd9cm <- function(data, diag_ecode_col){

	cdc_drugs_icd9cm_regex_ <- "^9[67]|^E85[0-8]|^E950[0-5]|^E9620|^E980[0-5]"

	cdc_opioid_icd9cm_regex_ <- "^9650[0129]|^E850[012]"

	cdc_non_heroin_icd9cm_regex_ <- "^9650[029]|^E850[12]"

	cdc_heroin_icd9cm_regex_ <- "^96501|^E8500"

	data %>%
		mutate(any_drug_icd9cm =
					 	od_create_diag(., expr = cdc_drugs_icd9cm_regex_,
					 							colvec = diag_ecode_col),
					 any_opioid_icd9cm =
					 	od_create_diag(., expr = cdc_opioid_icd9cm_regex_,
					 							colvec = diag_ecode_col),
					 non_heroin_icd9cm =
					 	od_create_diag(., expr = cdc_non_heroin_icd9cm_regex_,
					 							colvec = diag_ecode_col),
					 heroin_icd9cm =
					 	od_create_diag(., expr = cdc_heroin_icd9cm_regex_,
					 							colvec = diag_ecode_col)
		)
}
