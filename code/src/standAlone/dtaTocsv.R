library(foreign)

# Set working directory to file location
# -----------------------------------------------------------------------------
this.dir = dirname(parent.frame(2)$ofile)
setwd(this.dir)

dtaFileName = "data_ut_11Juni2019.dta"
csvFileName = "data_ut_11Juni2019.csv"

# Transform to R
data = read.dta("data_ut_11Juni2019.dta")

# Write to CSV
write.csv2(data,  file = csvFileName, row.names = FALSE)