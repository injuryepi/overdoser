#' Find any alcohol poisoning.
#'
#' Find any drug based on CDC definitions
#'
#' @param data input data
#' @param underly_col underlying column index
#'
#' @return alcohol
#' @export
#'
#' @examples to be added
od_fatal_alcohol <- function(data, underly_col) {
    data %>% mutate(alcohol = od_create_diag(., expr = "X45|X65|Y15",
        colvec = underly_col))
}
