#' Add new drug variables based on both ICD-9-CM and ICD-10-CM.
#'
#' Use date to apply ICD-9_CM criteria to pre fiscal year 2016 and ICD-10-CM
#' criteria to the rest.
#'
#' @param data: the input dataset
#' @param diag_ecode_col_9: indices(numeric) of diagnosis and E codes of interest
#' @param diag_ecode_col_10: indices(numeric) of diagnosis and E codes of interest
#' @param date: date of discharge
#'
#' @return additional variables: any_drug, any_opioid, non_heroin_opioid ,
#'   heroin
#' @export
#'
#' @examples
#' hosp_set %>%
#' od_drug_types_trans(diag_ecode_col_9 = c(3, 6), diag_ecode_col_10 = c(3, 6), date = discharge_date) %>% select(-age, -diagnosis_2, -diagnosis_3, -ecode2, -year) %>% sample_n(10)
#'
od_drug_types_trans <- function(data, diag_ecode_col_9, diag_ecode_col_10, date) {

	date <- enquo(date)
	# pull the fiscal years
	year_f <- data %>% pull(od_fed_fiscal_year(!!date))
	# assert that pre 2016 fiscal year and after 2015 fiscal year are both included
	stopifnot(any(year_f < 2016) & any(year_f > 2015))

	data1 <- data %>% filter(od_fed_fiscal_year(!!date) < 2016) %>% od_drug_types_icd9cm(diag_ecode_col = diag_ecode_col_9) %>%
		rename(any_drug = any_drug_icd9cm, any_opioid = any_opioid_icd9cm,
					 non_heroin_opioid = non_heroin_icd9cm, heroin = heroin_icd9cm)

	data2 <- data %>% filter(od_fed_fiscal_year(!!date) > 2015) %>% od_drug_types_icd10cm(diag_ecode_col = diag_ecode_col_10) %>%
		rename(any_drug = any_drug_icd10cm, any_opioid = any_opioid_icd10cm,
					 non_heroin_opioid = non_heroin_icd10cm, heroin = heroin_icd10cm) %>%
	        select(-opioid_t40_2_icd10cm, -opioid_t40_3_icd10cm, -opioid_t40_4_icd10cm, -cocaine_t40_5_icd10cm, -stimulant_t43_6_icd10cm)

	data <- dplyr::bind_rows(data1, data2)

	data %>% mutate(non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid)) 
}
