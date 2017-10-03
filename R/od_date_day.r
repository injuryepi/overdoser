#' Complete trucated date.
#'
#' @param x truncated date
#'
#' @return Return complete date with 15/ inserted as day
#' @export
#'
#' @examples
#' od_full_date(c("03/2015", "11/2014", "09/2017"))
#' # "2015-03-15" "2014-11-15" "2017-09-15"
od_date_day <- function(x){
	# add 15/ between month and year for truncated date
	suppressWarnings(suppressMessages(require(lubridate)))

	mdy(gsub( "(?<=^.{3})", "15/", x, perl=TRUE ))
}
