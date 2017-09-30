
# load(".../.R_Cache/ice10codes2@.RData")
# library(dplyr)
#
ice_code10 <- ice10codes2 %>% rename(underly = underly3)

devtools::use_data(ice_code10 , compress = "xz", overwrite = T)

devtools::use_data(ice_code10 , compress = "xz", internal = TRUE)
