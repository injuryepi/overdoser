
# load(".../CHARS/.R_Cache/icd10cm18@.RData")

devtools::use_data(icd10cm18 , compress = "xz", overwrite = T)

# devtools::use_data(icd10cm18 , compress = "xz", internal = TRUE)
