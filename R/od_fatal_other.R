#' Find other drug poisonings.
#' excluding opioid, benzo, antidepressant,
#' psycho-stimulant, cocaine
#'
#' Find any drug based on CDC definitions
#'
#' @param data input data
#' @param underly_col causes column index
#' @param cond.var conditional variable
#' @param var_name propose a name for the new other variable. The default is "other"
#'
#' @return other
#' @export
#'
#' @examples to be added
od_fatal_other <- function(data, underly_col, cond.var = cdc_any_drugs, var_name = "other") {
	var_name <- enquo(var_name)
	var_name <- quo_name(var_name)
	data %>%
		mutate(!!var_name :=
					 	od_create_cond_diag(.,
					 											expr = "^(?!(T40[0-456]|T424|T43[0126]))",
					 											colvec = underly_col,
					 											cond.var = cond.var))
}
