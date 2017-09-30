od_intent_icd9cm <- function(data, diag_ecode_col){

	unintentional_drugs_icd9cm_regex_ <- "^E85[0-8]"

	self_harm_drugs_icd9cm_regex_ <- "^E950[0-5]"

	assault_drugs_icd9cm_regex_ <- "^E9620"

	undetermined_drugs_icd9cm_regex_ <- "^E980[0-5]"

	data %>%
		mutate(unintentional_drugs_icd9cm =
					 	od_create_diag(., expr = unintentional_drugs_icd9cm_regex_,
					 							colvec = diag_ecode_col),
					 self_harm_drugs_icd9cm =
					 	od_create_diag(., expr = self_harm_drugs_icd9cm_regex_,
					 							colvec = diag_ecode_col),
					 assault_drugs_icd9cm =
					 	od_create_diag(., expr = assault_drugs_icd9cm_regex_,
					 							colvec = diag_ecode_col),
					 undetermined_drugs_icd9cm =
					 	od_create_diag(., expr = undetermined_drugs_icd9cm_regex_,
					 							colvec = diag_ecode_col)) %>%
					 	mutate(intent_drugs = ifelse(
					 		unintentional_drugs_icd9cm == 1, 1,
					 		ifelse(self_harm_drugs_icd9cm == 1, 2,
					 		ifelse(assault_drugs_icd9cm == 1, 3,
					 		ifelse(undetermined_drugs_icd9cm == 1, 4, NA))))) %>%
		select(-unintentional_drugs_icd9cm, -self_harm_drugs_icd9cm,
					 -assault_drugs_icd9cm, -undetermined_drugs_icd9cm)
}
