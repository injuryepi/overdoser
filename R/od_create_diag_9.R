#' Create a new variable based on patterns.
#'
#' Create a new variable based on pattern in the argument expr
#'
#' @param data: input data
#' @param expr1: regular expression describing the pattern of interest
#' @param colvec1: indices of variables of interest
#' @param expr2: regular expression describing the pattern of interest
#' @param colvec2: indices of variables of interest
#' @param ignore.case logical
#' @param perl logical
#'
#' @return new variable matching the pattern described in the regular expressions
#' @export
#'
#' @examples
#' library(tidyverse)
#' \thosp_set %>%
#' \tfilter(year == 2015, quarters(discharge_date) == "Q1") %>%
#' \tod_create_diag2(., expr1 = "^96501", colvec1 = 1,
#' \texpr2 = "^E8500", colvec2 = c(1:5)) %>% sample_n(10)
#'
#'
od_create_diag_9 <- function(data, expr1, colvec1, expr2, colvec2, ignore.case = T, perl = T) {
	#expr = regular expressions
	# colvec = vector of the columns of interest (columns with the diagnoses). indices
	# or variable names without quotation marks
	colvec1 = enquo(colvec1)
	colvec2 = enquo(colvec2)
	# assign '1' if the regular expression matched
	f1 = function(x) as.numeric(grepl(expr1, x, ignore.case = ignore.case, perl = perl))

	# assign '1' if the regular expression matched
	f2 = function(x) as.numeric(grepl(expr2, x, ignore.case = ignore.case, perl = perl))

	# any one in the diagnosis field suffices
	g = function(x){as.numeric(rowSums(x, na.rm = TRUE) > 0)}

data1 <- data %>% select(!!colvec1) %>%
		mutate_all(funs(as.character)) %>%
		map_df(f1) %>%
		mutate(new_diag1 = g(.)) %>%
		pull(new_diag1)


data2 <- data %>% select(!!colvec2) %>%
	mutate_all(funs(as.character)) %>%
	map_df(f2) %>%
	mutate(new_diag2 = g(.)) %>%
	pull(new_diag2)

tibble(new_diag1 = data1, new_diag2 = data2) %>% mutate(new_diag = ifelse(new_diag1 == 1 | new_diag2 == 1, 1, 0)) %>% pull(new_diag)

}
