# filename <- "D:/DATA/r/atm/cal_example_EK80.xml"
# filename <- "D:/DATA/r/atm/cal_example_EK60.txt"

vessel.name <- "Reuben Lasker"
survey.name <- "1907RL"
cal.group <- "SWFSC"

tmp <- atm::extract_cal(filename, vessel.name, survey.name, cal.group = "SWFSC ")

cal.files <- fs::dir_ls("data")

vessel.name <- "Reuben Lasker"
survey.name <- "1806RL"
cal.group <- "SWFSC"

library(tidyverse)
cal.res <- data.frame()
for (i in cal.files) {

  cal.res <- cal.res %>%
    dplyr::bind_rows(atm::extract_cal(i, vessel.name, survey.name, cal.group = "SWFSC ")$cal.res)

}

cal.res %>% select(txdr_freq, txdr_athw_ang_sens, txdr_alon_ang_sens) %>%
  arrange(txdr_freq)

