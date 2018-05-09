#' Find any alcohol poisoning.
#'
#' Find any drug based on CDC definitions
#'
#' @param data input data
#' @param underly_col underlying column index
#' @param var_name propose a name for the new alcohol variable. The default is "alcohol"
#'
#' @return alcohol
#' @export
#'
#' @examples to be added
od_fatal_alcohol <- function(data, underly_col, var_name = "alcohol") {
	var_name <- enquo(var_name)
	var_name <- quo_name(var_name)
	data %>% mutate(!!var_name :=  od_create_diag(., expr = "X45|X65|Y15",
																								colvec = underly_col))
}

