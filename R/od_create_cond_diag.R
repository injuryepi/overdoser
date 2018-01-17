#' create conditionnally a new variable.
#'
#' create conditionnally a new variable
#'
#' @param data: input data
#' @param expr: regular expressions
#' @param colvec: indices of variables of interest
#' @param ignore.case logical
#' @param perl logical
#' @param cond.var: conditional variable
#'
#' @return new variable defined by the regular expressions
#' @export
#'
#' @examples to be added
od_create_cond_diag <- function(data, expr, colvec, ignore.case = T, perl = T, 
    cond.var) {
    # expr=regular expressions of interest colvec = vector of the columns of
    # interest (columns with the diagnosis code) cond.var = the conditional
    # column with 0 and 1
    colvec = enquo(colvec)
    cond_var = enquo(cond.var)
  
  data %>% 
    mutate(new_diag = od_create_diag(., expr = expr, colvec = !!colvec, ignore.case = ignore.case, perl = perl)) %>%  
    mutate_at(vars(new_diag), funs(ifelse((!!cond_var) == 1, new_diag, 0 ))) %>% pull(new_diag)

}
