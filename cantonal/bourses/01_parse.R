#source("~/swissinfo/_helpers/helpers.R")
library(swiMap)

############################################################################################
###   SETTINGS
############################################################################################

infile <- 'bourseTertiaires.csv'

# Load xls sheet
readFile <- read.csv(infile, row.names = 1, check.names = F)

## MAP canton name to 2 letters abbrevation
iso2c <- canton_namesStrict(rownames(readFile))
rownames(readFile) <- iso2c

# reorder df by iso2 canton abbreviations
iorder <- match(canton_CH$iso2, rownames(readFile))
df <- readFile[iorder,]

# newCol <- 'Part en % des forfaits fiscaux dans les recettes fiscales cantonales'
# data[,newCol] <- (data[,"impôt.cantonal"] / data[,'Recettes.fiscales.cantonales']) * 100
#
# coln.sub <- c("Nombre.de.forfaits", "impôt.cantonal", "totale.des.recettes.fiscales", newCol)

write.csv(df[,-c(1:4)], file = "boursesTertiaires_2013.csv")



