#' Title
#'
#' @param data: input data
#' @param diag_ecode_col numeric vector of the
#' selected diagnosis and E codes
#' @param date: date of discharge
#'
#' @return intent_drugs with values 1, 2, 3, 4 for
#' unintentional, self-harm, assault and undetermined
#' @export
#'
#' @examples
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
