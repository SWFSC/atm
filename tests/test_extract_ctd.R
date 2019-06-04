# header.filename.ctd <- "C:/CODE/atm/data/CTD/test.hdr"
# cast.filename.ctd   <- "C:/CODE/atm/data/CTD/test_processed.asc"

header.filename.ctd <- "D:/CODE/R_packages/atm/data/CTD/test.hdr"
cast.filename.ctd   <- "D:/CODE/R_packages/atm/data/CTD/test_processed.asc"


extract_ctd_header(header.filename.ctd, type = "CTD")
extract_ctd_cast(cast.filename.ctd, type = "CTD")

# header.filename.uctd <- "C:/CODE/atm/data/UCTD/test.asc"
# cast.filename.uctd   <- "C:/CODE/atm/data/UCTD/test_processed.asc"

header.filename.uctd <- "D:/CODE/R_packages/atm/data/UCTD/test.asc"
cast.filename.uctd   <- "D:/CODE/R_packages/atm/data/UCTD/test_processed.asc"


extract_ctd_header(header.filename.uctd, type = "UCTD")
extract_ctd_cast(cast.filename.uctd, type = "UCTD")


