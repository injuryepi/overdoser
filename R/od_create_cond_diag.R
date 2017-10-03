#' create conditionnally a new variable.
#'
#' create conditionnally a new variable
#'
#' @param data: input data
#' @param expr: regular expressions
#' @param colvec: indices of variables of interest
#' @param ignore.case logical
#' @param perl logical
#' @param cond.var: conditional variable
#'
#' @return new variable defined by the regular expressions
#' @export
#'
#' @examples to be added
od_create_cond_diag <- function(data, expr, colvec, ignore.case = T, perl = T, cond.var){
	# expr=regular expressions of interest
	# colvec = vector of the columns of interest (columns with the diagnosis code)
	# cond.var = the conditional column with 0 and 1
	require(dplyr, quietly = T)
	require(tidyr, quietly = T)

	cond.var2 <- eval(substitute(cond.var),data, parent.frame())
	fac2num <- function(f) as.numeric(levels(f))[f]
	cond.var2 <- fac2num(cond.var2)

	# select the variables of interest
	sel <- names(data)[colvec]


	# ensure they are all character vector
	df <- as_data_frame(data[sel]) %>% mutate_all(funs(as.character))

	# a function to assign "1" if the regular expression matched or "0" otherwise
	f <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl)+0
	df <- sapply(df, f)
	df <- as_data_frame(df)  %>% mutate(new_diag = rowSums(., na.rm = TRUE)) %>%
		select(new_diag) %>% mutate(new_diag = as.numeric(new_diag > 0))

	df <- df %>% mutate(cond.var2 = cond.var2) %>%
		mutate(new_diag2 = rowSums(., na.rm = TRUE)) %>%
		select(new_diag2)

	as.factor(as.numeric((df[,1] == 2)))

}
