add_any_opioid_icd10cm <- function(data, diag_ecode_col){

	cdc_opioid_icd10cm_regex7_ <- "(T40[01234].|T406[09])(1|2|3|4)(A|D|$)"
	data %>%
		mutate(any_opioid_icd10cm =
					 	create_diag(., expr = cdc_opioid_icd10cm_regex7_,
					 							colvec = diag_ecode_col))
}
