#' Title
#'
#' @param date
#'
#' @return
#' @export
#'
#' @examples
od_fed_fiscal_year <- function(date){
	#October 1, year_n to September 30, year_n+1
	require(lubridate, quietly = T)
	stopifnot(is.Date(date)) # check the date format
	year(date %m+% months(3))
}
