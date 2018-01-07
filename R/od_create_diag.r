#' Create a new variable based on pattern.
#'
#' Create a new variable based on pattern in the argument expr
#'
#' @param data: input data
#' @param expr: regular expression describing the pattern of interest
#' @param colvec: indices of variables of interest
#' @param ignore.case logical
#' @param perl logical
#'
#' @return new variable matching the pattern described in the regular expression
#' @export
#'
#' @examples
#' heroin_icd9cm_regex_ <- '^96501|^E8500'
#' library(tidyverse)
#' \thosp_set %>%
#' \tfilter(year == 2015) %>%
#' \tod_create_diag(expr = heroin_icd9cm_regex_,
#' \tcolvec = c(1:5)) %>% sample_n(10)
#'
od_create_diag <- function(data, expr, colvec, ignore.case = T, perl = T) {
   require(dplyr, quietly = T)
  require(tidyr, quietly = T)
  require(purrr, quietly = T)
  #a function to assign '1' if the regular expression matched 
  f1 = function(x) as.numeric(grepl(expr, x, ignore.case = ignore.case, perl = perl))
  f2 = function(x){as.numeric(rowSums(x, na.rm = TRUE) > 0)} 
 
  data %>% select(colvec) %>% 
    mutate_all(funs(as.character)) %>% 
    map_df(f1) %>% 
    mutate(new_diag = f2(.)) %>% 
    pull(new_diag)
}
