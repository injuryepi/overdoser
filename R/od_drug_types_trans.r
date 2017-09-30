od_drug_types_trans <- function(data, diag_ecode_col, date){

	date <- enquo(date)

	data1 <- data %>% 
		filter(od_fed_fiscal_year(!!date) < 2016) %>% 
		od_drug_types_icd9cm(diag_ecode_col = diag_ecode_col) %>% 
		rename(any_drug = any_drug_icd9cm,
					 any_opioid = any_opioid_icd9cm,
					 non_heroin_opioid = non_heroin_icd9cm,
					 heroin = heroin_icd9cm)
	
	data2 <- data %>% 
		filter(od_fed_fiscal_year(!!date) > 2015) %>% 
		od_drug_types_icd10cm(diag_ecode_col = diag_ecode_col) %>% 
		rename(any_drug = any_drug_icd10cm,
					 any_opioid = any_opioid_icd10cm,
					 non_heroin_opioid = non_heroin_icd10cm,
					 heroin = heroin_icd10cm)
	
	data <- dplyr::bind_rows(data1, data2)
		
	data
}
