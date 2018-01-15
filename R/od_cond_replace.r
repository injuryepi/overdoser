cond_replace <- function(data, cond_vars, values = c(1:9), replacement, ...){
  
  # ... variables with values to replace with with 'replacement' 
  # no quotations marks or vector of numeric indices
  
  sel = quos(...)
  cond_vars = enquo(cond_vars)
  data %>% 
    mutate_at(vars(!!!sel, !!cond_vars), funs(ifelse((!!cond_vars) %in% values, replacement, . )))
}
