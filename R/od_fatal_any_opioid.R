#' Find any opioid.
#'
#' Using CDC definitions.
#'
#' @param data input data
#' @param underly_col underlying column index
#' @param mult_col multiple causes column indices
#'
#' @return cdc_any_drugs, cdc_any_opioid,
#'
#' @export
#'
#' @examples to be added
od_fatal_any_opioid <- function(data, underly_col, mult_col) {
    data %>% mutate(cdc_any_drug = od_create_diag(., expr = "X4[0-4]|X6[0-4]|X85|Y1[0-4]", 
        colvec = underly_col)) %>% mutate(cdc_any_opioid = od_create_cond_diag(., 
        expr = "T40[0-46]", colvec = mult_col, cond.var = cdc_any_drugs)) %>% 
        select(-cdc_any_drugs)
}
