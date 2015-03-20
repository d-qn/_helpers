source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###   SETTINGS
############################################################################################

infile <- 'cc-f-01.05.01.11.csv'

# Load xls sheet
readFile <- read.csv(infile, stringsAsFactors = F, check.names = F )

iorder <- match(canton_names$order, as.numeric(rownames(canton_names)))

data <- data.frame(canton_names$iso2, readFile[iorder, 2:ncol(readFile)], check.names = F)

# check
sumCheck <- data[,2] + data[,3] + data[,4] + data[,5] + data[,6] + data[,7]
if( ! all.equal(sumCheck, rep(100, length(sumCheck)))) {
	stop("Percentages do not add up to 100%!")
}

write.csv(data, file = "tailleMenage_2014.csv", row.names = FALSE)



