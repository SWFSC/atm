filename <- "D:/DATA/r/atm/cal_example_EK80.xml"
# filename <- "D:/DATA/r/atm/cal_example_EK60.txt"
vessel.name <- "Reuben Lasker"
survey.name <- "1907RL"
cal.group <- "SWFSC"

tmp <- cal_extract(filename, vessel.name, survey.name, cal.group = "SWFSC ")
