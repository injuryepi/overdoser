#' Complete trucated date.
#'
#' @param x only truncated date of mm/yyyy or m/yyyy
#'
#' @return Return complete date with 15/ inserted as day
#' @export
#'
#' @examples
#' od_date_day(c('03/2015', '11/2014', '09/2017'))
#' # '2015-03-15' '2014-11-15' '2017-09-15'
od_date_day <- function(x) {
    # add 15/ between month and year for truncated date
    suppressWarnings(suppressMessages(require(lubridate)))

    mdy(gsub("(?<=^.{3})", "15/",
    					 stringr::str_pad(x, 7, "left", "0"),
    					 perl = TRUE))
    }
