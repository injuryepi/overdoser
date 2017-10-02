#' Title
#'
#' @param data
#' @param diag_ecode_col
#'
#' @return
#' @export
#'
#' @examples
add_non_heroin_icd10cm <- function(data, diag_ecode_col){

cdc_non_heroin_icd10cm_regex7_ <- "(T40[0234].|T406[09])(1|2|3|4)(A|D|$)"
	data %>%
		mutate(non_heroin_icd10cm =
					 	od_create_diag(., expr = cdc_non_heroin_icd10cm_regex7_,
					 							colvec = diag_ecode_col))
}
