#https://teamcolorcodes.com/seattle-seahawks-color-codes/

#https://teamcolorcodes.com/seattle-sounders-fc-color-codes/

library(tidyverse)

hawks_names <- c("college_navy", "action_green", "wolf_gray", "seahawks_blue")
seahawks <- c("#001433", "#4DFF00", "#A5ACAF", "#245998") %>%
	set_names(hawks_names)

sounders_names <- c("sounder_blue", "rave_green", "cascade_shale")
sounders <- c("#005695", "#5D9732", "#132530") %>%
	set_names(sounders_names)

seacolors <- c(seahawks, sounders)



devtools::use_data(seacolors , overwrite = T)
