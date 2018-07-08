#' Find andidepressant poisoning
#'
#'
#' @param data input data
#' @param underly_col underlying cause column index
#' @param mult_col multicause index
#'
#' @return antidepressant
#' @export
#'
#' @examples to be added
#'
od_fatal_antidep <- function(data, underly_col, mult_col) {
	data %>%
		mutate(any_drugs = od_create_diag(., expr = "X4[0-4]|X6[0-4]|X85|Y1[0-4]", colvec = underly_col)) %>%
		mutate(antidepressant = od_create_cond_diag(., expr = "T43[012]", colvec = mult_col, cond.var = any_drugs)) %>%
		select(-any_drugs)
}
