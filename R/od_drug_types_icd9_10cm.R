od_drug_types_icd9_10cm <- function(data, diag_ecode_col){

	cdc_drugs_icd9_10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(1|2|3|4)(A|D|$)|((T3[679]9|T414|T427|T4[3579]9)(1|2|3|4).(A|D|$))|(?:^9[67]|^E85[0-8]|^E950[0-5]|^E9620|^E980[0-5])"

	cdc_opioid_icd9_10cm_regex_ <- "(T40[01234].|T406[09])(1|2|3|4)(A|D|$)|(^9650[0129]|^E850[012])"

	cdc_non_heroin_icd9_10cm_regex_ <- "(T40[0234].|T406[09])(1|2|3|4)(A|D|$)|(^9650[029]|^E850[12])"

	cdc_heroin_icd9_10cm_regex_ <- "T401.(1|2|3|4)(A|D|$)|(^96501|^E8500)"

	data %>%
		mutate(any_drug_icd9_10cm =
					 	od_create_diag(., expr = cdc_drugs_icd9_10cm_regex_,
					 							colvec = diag_ecode_col),
					 any_opioid_icd9_10cm =
					 	od_create_diag(., expr = cdc_opioid_icd9_10cm_regex_,
					 							colvec = diag_ecode_col),
					 non_heroin_icd9_10cm =
					 	od_create_diag(., expr = cdc_non_heroin_icd9_10cm_regex_,
					 							colvec = diag_ecode_col),
					 heroin_icd9_10cm =
					 	od_create_diag(., expr = cdc_heroin_icd9_10cm_regex_,
					 							colvec = diag_ecode_col)
		)
}
