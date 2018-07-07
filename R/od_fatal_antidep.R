#' Find any Antidepressant poisoning.
#'
#' Find any drug based on CDC definitions
#'
#' @param data input data
#' @param underly_col underlying column index
#' @param var_name propose a name for the new Antidepressant variable. The default is "antidepressant"
#'
#' @return antidepressant
#' @export
#'
#' @examples to be added
od_fatal_antidep <- function(data, underly_col, var_name = "antidepressant") {
	var_name <- enquo(var_name)
	var_name <- quo_name(var_name)
	data %>% mutate(!!var_name :=  od_create_diag(., expr = "T43[012]",
																								colvec = underly_col))
}

