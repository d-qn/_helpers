source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###   SETTINGS
############################################################################################

infile <- '2000vs2013_nonOrdered.csv'

# Load xls sheet
readFile <- read.csv(infile, stringsAsFactors = F)

iorder <- match(canton_names$order, readFile$ID)

data <- data.frame(canton_names$iso2, variationDensite_2000_2013 = readFile[iorder, 3])


write.csv(data, file = "densite_2000_2013.csv", row.names = FALSE)



