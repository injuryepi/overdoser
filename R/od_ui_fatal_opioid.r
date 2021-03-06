#' Find any opioid.
#'
#' Using CDC definitions.
#'
#' @param data input data
#' @param underly_col underlying column index
#' @param mult_col multiple causes column indices
#' @param var_name propose a name for the new alcohol variable. The default is "ui_opioid"
#'
#' @return cdc_any_drugs, cdc_any_opioid,
#'
#' @export
#'
#' @examples to be added
#'
#'
od_ui_fatal_opioid <- function (data, underly_col, mult_col, var_name = "ui_opioid"){
	var_name <- enquo(var_name)
	var_name <- quo_name(var_name)
	data %>% mutate(ui_drug = od_create_diag(., expr = "X4[0-4]",
																					 colvec = underly_col)) %>%
		mutate(!!var_name :=  od_create_cond_diag(.,
																					 expr = "T40[0-46]",
																					 colvec = mult_col,
																					 cond.var = ui_drug)) %>%
		select(-ui_drug)
}


