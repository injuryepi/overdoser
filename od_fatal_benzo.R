#' Find any Benzodiazepines poisoning.
#'
#' Find any drug based on CDC definitions
#'
#' @param data input data
#' @param underly_col underlying column index
#' @param var_name propose a name for the new Benzo variable. The default is "benzo"
#'
#' @return benzo
#' @export
#'
#' @examples to be added
od_fatal_benzo <- function(data, underly_col, var_name = "benzo") {
	var_name <- enquo(var_name)
	var_name <- quo_name(var_name)
	data %>% mutate(!!var_name :=  od_create_diag(., expr = "T424",
																								colvec = underly_col))
}

