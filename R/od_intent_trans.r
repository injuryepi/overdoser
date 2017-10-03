#' Find intent for ICD-9-CM and ICD-10-CM.
#'
#' Use date to apply ICD-9_CM intent criteria to pre fiscal year 2016 and
#' ICD-10-CM criteria to the rest.
#'
#' @param data: input data
#' @param diag_ecode_col numeric vector of the selected diagnosis and E codes
#' @param date: date of discharge
#'
#' @return intent_drugs with values 1, 2, 3, 4 for unintentional, self-harm,
#'   assault and undetermined
#' @export
#'
#' @examples
#' #' od_intent_trans(diag_ecode_col = c(3, 6), date = discharge_date) %>% select(-age, -diagnosis_2, -diagnosis_3, -ecode2, -year) %>% sample_n(10)
#'
od_intent_trans <- function(data, diag_ecode_col, date){

	date <- enquo(date)

	data1 <- data %>%
		filter(od_fed_fiscal_year(!!date) < 2016) %>%
		od_intent_icd9cm(diag_ecode_col = diag_ecode_col)

	data2 <- data %>%
		filter(od_fed_fiscal_year(!!date) > 2015) %>%
		od_intent_icd10cm(diag_ecode_col = diag_ecode_col)

	data <- dplyr::bind_rows(data1, data2)

	data

}
