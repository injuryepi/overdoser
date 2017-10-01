od_intent_icd10cm <- function(data, diag_ecode_col){

	unintentional_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(1)|((T3[679]9|T414|T427|T4[3579]9)(1))"

	self_harm_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(2)|((T3[679]9|T414|T427|T4[3579]9)(2))"

	assault_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(3)|((T3[679]9|T414|T427|T4[3579]9)(3))"

	undetermined_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(4)|((T3[679]9|T414|T427|T4[3579]9)(4))"

	adverse_effect_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(5)|((T3[679]9|T414|T427|T4[3579]9)(5))"

	underdosing_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(6)|((T3[679]9|T414|T427|T4[3579]9)(6))"



	data %>%
		mutate(unintentional_drugs_icd10cm =
					 	od_create_diag(., expr = unintentional_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 self_harm_drugs_icd10cm =
					 	od_create_diag(., expr = self_harm_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 assault_drugs_icd10cm =
					 	od_create_diag(., expr = assault_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 undetermined_drugs_icd10cm =
					 	od_create_diag(., expr = undetermined_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 adverse_effect_drugs_icd10cm =
					 	od_create_diag(., expr = adverse_effect_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 underdosing_drugs_icd10cm =
					 	od_create_diag(., expr = underdosing_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col)) %>%
					 	mutate(intent_drugs6 = ifelse(
					 		unintentional_drugs_icd10cm == 1, 1,
					 		ifelse(self_harm_drugs_icd10cm == 1, 2,
					 		ifelse(assault_drugs_icd10cm == 1, 3,
					 		ifelse(undetermined_drugs_icd10cm == 1, 4,
					 		ifelse(adverse_effect_drugs_icd10cm == 1, 5,
					 		ifelse(underdosing_drugs_icd10cm == 1, 6, NA))))))) %>%
		select(-unintentional_drugs_icd10cm, -self_harm_drugs_icd10cm,
					 -assault_drugs_icd10cm, -undetermined_drugs_icd10cm,
					 adverse_effect_drugs_icd10cm, underdosing_drugs_icd10cm)
}




