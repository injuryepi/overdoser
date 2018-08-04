#' Find cocaine based on ICD-10-CM.
#'
#' Find cocaine based on ICD-10-CM.
#'
#' @param data input data
#' @param diag_ecode_col column indices
#'
#' @return cocaine_icd10cm
#' @export
#'
#' @examples to be added
od_cocaine_icd10cm <- function(data, diag_ecode_col) {

	cdc_cocaine_icd10cm_regex7_ <- "T405.(1|2|3|4)(A|$)"
	data %>% mutate(cocaine_icd10cm = od_create_diag(., expr = cdc_cocaine_icd10cm_regex7_, colvec = diag_ecode_col))
}
