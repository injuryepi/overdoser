#' Title
#'
#' @param data
#' @param underly_col
#' @param mult_col
#'
#' @return
#' @export
#'
#' @examples
od_fatal_any_opioid <- function(data, underly_col, mult_col){
  data %>%
    mutate(cdc_any_drugs =
            od_create_diag(., expr = "X4[0-4]|X6[0-4]|X85|Y1[0-4]",
                        colvec = underly_col)) %>%
    mutate(cdc_any_opioid =
            od_create_cond_diag(., expr = "T40[0-46]",
                             colvec = mult_col,
                             cond.var = cdc_any_drugs)) %>%
            select(-cdc_any_drugs)
}
