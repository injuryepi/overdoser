#' Find six categories intent.
#'
#' Find six categories intent from ICD-10-CM
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return intent_drugs6
#' @export
#'
#' @examples to be added
od_intent_icd10cm_6 <- function(data, diag_ecode_col){

	unintentional_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(1)|((T3[679]9|T414|T427|T4[3579]9)(1))"

	self_harm_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(2)|((T3[679]9|T414|T427|T4[3579]9)(2))"

	assault_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(3)|((T3[679]9|T414|T427|T4[3579]9)(3))"

	undetermined_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(4)|((T3[679]9|T414|T427|T4[3579]9)(4))"

	adverse_effect_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(5)|((T3[679]9|T414|T427|T4[3579]9)(5))"

	underdosing_drugs_icd10cm_regex_ <- "(?!(T3[679]9|T414|T427|T4[3579]9))(T3[6-9]|T4[0-9]|T50)..(6)|((T3[679]9|T414|T427|T4[3579]9)(6))"



	data %>%
		mutate(unintentional_drugs_icd10cm_6 =
					 	od_create_diag(., expr = unintentional_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 self_harm_drugs_icd10cm_6 =
					 	od_create_diag(., expr = self_harm_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 assault_drugs_icd10cm_6 =
					 	od_create_diag(., expr = assault_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 undetermined_drugs_icd10cm_6 =
					 	od_create_diag(., expr = undetermined_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 adverse_effect_drugs_icd10cm_6 =
					 	od_create_diag(., expr = adverse_effect_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col),
					 underdosing_drugs_icd10cm_6 =
					 	od_create_diag(., expr = underdosing_drugs_icd10cm_regex_,
					 							colvec = diag_ecode_col)) %>%
					 	mutate(intent_drugs6 = ifelse(
					 		unintentional_drugs_icd10cm_6 == 1, 1,
					 		ifelse(self_harm_drugs_icd10cm_6 == 1, 2,
					 		ifelse(assault_drugs_icd10cm_6 == 1, 3,
					 		ifelse(undetermined_drugs_icd10cm_6 == 1, 4,
					 		ifelse(adverse_effect_drugs_icd10cm_6 == 1, 5,
					 		ifelse(underdosing_drugs_icd10cm_6 == 1, 6, NA))))))) %>%
		select(-unintentional_drugs_icd10cm_6, -self_harm_drugs_icd10cm_6,
					 -assault_drugs_icd10cm_6, -undetermined_drugs_icd10cm_6,
					 - adverse_effect_drugs_icd10cm_6, - underdosing_drugs_icd10cm_6)
}




