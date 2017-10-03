#' Add selected drug overdose as defined by CDC using ICD-10.
#'
#' cdc_any_drugs: Any drug overdose death,
#' that is an underlying cause-of-death ICD-10 codes of
#' X40–X44, X60–X64, X85, and Y10–Y14.
#' The following specific drug overdose deaths are drug overdose deaths
#' (defined in cdc_any_drugs) that involved the specified ICD-10 codes
#' in the multiple-cause-of-death fields
#' cdc_any_opioid: Any opioid death with
#' T40.0, T40.1, T40.2, T40.3, T40.4, or T40.6
#' cdc_heroin_t40_1: Heroin death with T40.1
#' cdc_opioid_t40_2: Natural and semisynthetic opioids (T40.2)
#' cdc_opioid_t40_3: Methadone with T40.3
#' cdc_opioid_t40_4: Synthetic opioids excluding methadone (T40.4)
#' cdc_cocaine_t40_5: Cocaine (T40.5)
#' cdc_stimulant_t43_6: Psychostimulants with abuse potential (T43.6).
#' these drug categories are not mutually exclusive
#'
#' @param data the death dataset in which to add the new drug variables
#' @param underly_col The column index for the with the ICD-10 underlying cause of death
#' @param mult_col The column indices of the multiple-cause-of-death codes
#'
#' @return The original dataset with the added drug fields.1 means drug present and 0 otherwise
#' @export
#'
#' @examples Examples will be added later
od_fatal_drug_list <- function(data, underly_col, mult_col){
	data %>%
		mutate(cdc_any_drugs = od_create_diag(., expr = "X4[0-4]|X6[0-4]|X85|Y1[0-4]", colvec= underly_col)) %>%
		mutate(cdc_any_opioid = od_create_cond_diag(.,expr = "T40[0-46]" , colvec = mult_col, cond.var = cdc_any_drugs),
					 cdc_heroin_t40_1 = od_create_cond_diag(.,expr = "T401" , colvec = mult_col, cond.var = cdc_any_drugs),
					 cdc_opioid_t40_2 = od_create_cond_diag(.,expr = "T402" , colvec = mult_col, cond.var = cdc_any_drugs),
					 cdc_opioid_t40_3 = od_create_cond_diag(.,expr = "T403" , colvec = mult_col, cond.var = cdc_any_drugs),
					 cdc_opioid_t40_4 = od_create_cond_diag(.,expr = "T404" , colvec = mult_col, cond.var = cdc_any_drugs),
					 cdc_rxopioid_t40_234 = od_create_cond_diag(.,expr = "T40[2-4]" , colvec = mult_col, cond.var = cdc_any_drugs),
					 cdc_cocaine_t40_5 = od_create_cond_diag(.,expr = "T405" , colvec = mult_col, cond.var = cdc_any_drugs),
					 cdc_stimulant_t43_6 = od_create_cond_diag(.,expr = "T436" , colvec = mult_col, cond.var = cdc_any_drugs))

}
