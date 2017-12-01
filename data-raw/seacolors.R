#https://teamcolorcodes.com/seattle-seahawks-color-codes/

col_names <- c("college_navy", "action_green", "wolf_gray", "seahawks_blue")
seacolors <- c("#001433", "#4DFF00", "#A5ACAF", "#245998")
library(tidyverse)
seacolors <- seacolors %>%
	set_names(col_names)


devtools::use_data(seacolors , overwrite = T)
