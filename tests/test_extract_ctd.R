header.filename.ctd <- "C:/CODE/atm/data/CTD/test.hdr"
cast.filename.ctd   <- "C:/CODE/atm/data/CTD/test_processed.asc"
# type = "CTD"

tmp.ctd <- extract_ctd(header.filename.ctd, cast.filename.ctd, type = "CTD ")

header.filename.uctd <- "C:/CODE/atm/data/UCTD/test.asc"
cast.filename.uctd   <- "C:/CODE/atm/data/UCTD/test_processed.asc"

tmp.uctd <- extract_ctd(header.filename.uctd, cast.filename.uctd, type = "UCTD ")


