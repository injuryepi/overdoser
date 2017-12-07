#' Find diagnoses across ICD-9 and ICD-10 periods.
#'
#'
#' @param data input data
#' @param underly_col underlying column index
#' @param expr9 the ICD-9 regular expressions
#' @param expr10 the ICD-9 regular expressions
#' @param var_name name for the new variable
#' @param expr10 the date column
#'
#' @return new variable named with var_name
#' @export
#'
#' @examples to be added
od_diag9_10cm <- function(data, diag_ecode_col, expr9,
													 expr10, var_name, date) {

	date <- enquo(date)
	expr9 <- enquo(expr9)
	expr10 <- enquo(expr10)
	var_name <- enquo(var_name)
	var_name <- quo_name(var_name)


	data1 <- data %>% filter(od_fed_fiscal_year(!!date) < 2016) %>%
		mutate(!!var_name := od_create_diag(., expr = !!expr9, colvec = diag_ecode_col))


	data2 <- data %>% filter(od_fed_fiscal_year(!!date) > 2015) %>% mutate(!!var_name := od_create_diag(., expr = !!expr10, colvec = diag_ecode_col))

	data <- dplyr::bind_rows(data1, data2)

	data
}
