#' Calculate federal fiscal year.
#'
#' Calculate fiscal year as October 1, year_n to September 30, year_n+1
#'
#' @param date date
#'
#' @return federal fiscal year
#' @export
#'
#' @examples
#' od_fed_fiscal_year(as.Date(c("2015-05-15", "2015-11-20", "2016-02-10")))
#' # 2015 2016 2016
od_fed_fiscal_year <- function(date){
	#October 1, year_n to September 30, year_n+1
	require(lubridate, quietly = T)
	stopifnot(is.Date(date)) # check the date format
	year(date %m+% months(3))
}
