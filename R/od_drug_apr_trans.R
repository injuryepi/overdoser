#' Add new drug variables based on both ICD-9-CM and ICD-10-CM.
#'
#' Use date to apply ICD-9_CM criteria to pre fiscal year 2016 and ICD-10-CM
#' criteria to the rest.
#'
#' @param data: the input dataset
#' @param diag_col: ndices(numeric) of  diag of interest for icd-9
#' @param diag_ecode: indices(numeric) of  ecodes of interest for icd-9
#' @param diag_ecode_col: indices(numeric) of diagnosis and E codes of interest
#' @param date: date of discharge
#'
#' @return additional variables: any_drug, any_opioid, non_heroin_opioid ,
#'   heroin
#' @export
#'
#' @examples
#' to be added
#'
#'
od_drug_apr_trans <- function(data, diag_col, ecode_col, diag_ecode_col, date) {

	date <- enquo(date)
	# pull the fiscal years
	year_f <- data %>% pull(od_fed_fiscal_year(!!date))
	# assert that pre 2016 fiscal year and after 2015 fiscal year are both included
	stopifnot(any(year_f < 2016) & any(year_f > 2015))

	data1 <- data %>% filter(od_fed_fiscal_year(!!date) < 2016) %>% od_drug_apr_icd9cm(diag_col = diag_col, ecode_col = ecode_col)

	data2 <- data %>% filter(od_fed_fiscal_year(!!date) > 2015) %>% od_drug_apr_icd10cm(diag_ecode_col = diag_ecode_col)

	data <- dplyr::bind_rows(data1, data2)

	data %>% mutate(non_heroin_opioid = ifelse(heroin == 1, 0, non_heroin_opioid))
}
