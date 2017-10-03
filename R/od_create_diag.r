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
#' \thosp_net %>%
#' \tfilter(year == 2015) %>%
#' \tod_create_diag(expr = heroin_icd9cm_regex_,
#' \tcolvec = c(1:5)) %>% sample_n(10)
#'
od_create_diag <- function(data, expr, colvec, ignore.case = T, perl = T) {
    # regexp=regular expressions for the data colvec = vector of the columns of
    # interest (columns with the diagnoses)
    require(dplyr, quietly = T)
    require(tidyr, quietly = T)
    # select the variables of interest
    sel <- names(data)[colvec]
    # ensure they are all the variables are character vectors and create a data
    # subset of the variables
    df <- as_data_frame(data[sel]) %>% mutate_all(funs(as.character))
    # a function to assign '1' if the regular expression matched or '0'
    # otherwise
    f <- function(x) grepl(expr, x, ignore.case = ignore.case, perl = perl) + 
        0
    # apply the function above to all the cells in the data frame 'df'
    df <- sapply(df, f)
    df <- as_data_frame(df) %>% mutate(new_diag = rowSums(., na.rm = TRUE)) %>% 
        select(new_diag)
    # the vector of the new variable (new diagnosis) to add to the data frame
    # (data)
    as.factor(as.numeric((df[, 1] > 0)))
    
}
