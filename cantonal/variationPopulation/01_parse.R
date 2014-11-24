source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###   SETTINGS
############################################################################################

infile <- 'final-Table 1.csv'

# Load xls sheet
readFile <- read.csv(infile, stringsAsFactors = F)

iorder <- match(canton_names$order, as.numeric(rownames(canton_names)))

data <- data.frame(canton_names$iso2, variationPopulation_2000_2013 = readFile[iorder, 2])


write.csv(data, file = "population_2000_2013.csv", row.names = FALSE)



