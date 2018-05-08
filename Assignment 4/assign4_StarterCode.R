# =======================================================
# Problem 2 : Starter Code : Fetching data from Quandl
# =======================================================

# API for Quandl data
install.packages("Quandl")      
library(Quandl)

# -------------------------------------------------------
# Fetch data from Quandl
#
# Data Source    : WWDI/
# Country ID     : SGP_
# Indicator ID   : NY_GDP_PCAP_CD
data <- Quandl("WWDI/SGP_NY_GDP_PCAP_CD", type = "ts")
str(data)
plot(data, col = "blue", lwd = 3, type = "l")

# List of Country IDs   : country_codes.txt    (attached)
# List of Indicator IDs : wwdi_indicators.txt  (attached)
