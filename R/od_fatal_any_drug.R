#' Title
#'
#' @param data
#' @param underly_col
#'
#' @return
#' @export
#'
#' @examples
od_fatal_any_drug <- function(data, underly_col){
  data %>%
    mutate(cdc_any_drug =
            od_create_diag(., expr = "X4[0-4]|X6[0-4]|X85|Y1[0-4]",
                        colvec = underly_col))
}
