od_cond_replace <- function(data, cond_var, values = c(1:9), replacement, ...){
  
  # ... variables with values to replace with with 'replacement' 
  # no quotations marks or vector of numeric indices
  
  sel = quos(...)
  cond_var = enquo(cond_var)
  data %>% 
    mutate_at(vars(!!!sel, !!cond_var), funs(ifelse((!!cond_var) %in% values, replacement, . )))
}
