od_und_fatal_opioid <- function(data, underly_col, mult_col) {
    data %>% mutate(und_drug = od_create_diag(., expr = "Y1[0-4]", 
        colvec = underly_col)) %>% mutate(und_intent_opioid = od_create_cond_diag(., 
        expr = "T40[0-46]", colvec = mult_col, cond.var = und_drug)) %>% 
        select(-und_drug)
}
