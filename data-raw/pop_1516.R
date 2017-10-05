
load(".../population/.R_Cache/pop_15_16@.RData")

library(dplyr)
pop_15_16 <- pop_15_16 %>%
	filter(age < 111) %>%
	mutate(population = round(population/200))

devtools::use_data(pop_15_16 , compress = "xz", overwrite = T)
