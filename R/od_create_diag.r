#' Title
#'
#' @param data
#' @param expr
#' @param colvec
#' @param ignore.case
#' @param perl
#'
#' @return
#' @export
#'
#' @examples
od_create_diag <- function(data, expr, colvec, ignore.case = T, perl = T){
	#regexp=regular expressions for the data
	# colvec = vector of the columns of interest (columns with the diagnoses)
	require(dplyr, quietly = T)
	require(tidyr, quietly = T)
	# select the variables of interest
	sel <- names(data)[colvec]
	# ensure they are all the variables are character vectors and create a data subset of the variables
	df <- as_data_frame(data[sel]) %>% mutate_all(funs(as.character))
	# a function to assign "1" if the regular expression matched or "0" otherwise
	f <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl)+0
	# apply the function above to all the cells in the data frame "df"
	df <- sapply(df, f)
	df <- as_data_frame(df)  %>% mutate(new_diag = rowSums(., na.rm = TRUE)) %>% select(new_diag)
	# the vector of the new variable (new diagnosis) to add to the data frame (data)
	as.factor(as.numeric((df[,1] > 0)))

}
