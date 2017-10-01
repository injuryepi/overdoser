od_intent_icd9_10cm <- function(data, diag_ecode_col){

	unintentional_drugs_icd9_10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(1)|((T3[679]9|T414|T427|T4[3579]9)(1))|(^E85[0-8])"

	self_harm_drugs_icd9_10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(2)|((T3[679]9|T414|T427|T4[3579]9)(2))|(^E950[0-5])"

	assault_drugs_icd9_10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(3)|((T3[679]9|T414|T427|T4[3579]9)(3))|(^E9620)"

	undetermined_drugs_icd9_10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(4)|((T3[679]9|T414|T427|T4[3579]9)(4))|(^E980[0-5])"

	data %>%
		mutate(unintentional_drugs_icd9_10cm =
					 	od_create_diag(., expr = unintentional_drugs_icd9_10cm_regex_,
					 							colvec = diag_ecode_col),
					 self_harm_drugs_icd9_10cm =
					 	od_create_diag(., expr = self_harm_drugs_icd9_10cm_regex_,
					 							colvec = diag_ecode_col),
					 assault_drugs_icd9_10cm =
					 	od_create_diag(., expr = assault_drugs_icd9_10cm_regex_,
					 							colvec = diag_ecode_col),
					 undetermined_drugs_icd9_10cm =
					 	od_create_diag(., expr = undetermined_drugs_icd9_10cm_regex_,
					 							colvec = diag_ecode_col)) %>%
					 	mutate(intent_drugs = ifelse(
					 		unintentional_drugs_icd9_10cm == 1, 1,
					 		ifelse(self_harm_drugs_icd9_10cm == 1, 2,
					 		ifelse(assault_drugs_icd9_10cm == 1, 3,
					 		ifelse(undetermined_drugs_icd9_10cm == 1, 4, NA))))) %>%
		select(-unintentional_drugs_icd9_10cm, -self_harm_drugs_icd9_10cm,
					 -assault_drugs_icd9_10cm, -undetermined_drugs_icd9_10cm)
}




