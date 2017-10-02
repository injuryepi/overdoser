od_full_date <- function(x){
	# add 15/ between month and year for truncated date
	mdy(gsub( "(?<=^.{3})", "15/", x, perl=TRUE ))
}
