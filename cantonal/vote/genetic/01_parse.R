#source("~/swissinfo/_helpers/helpers.R")
library(swiMap)

############################################################################################
###   SETTINGS
############################################################################################

infile <- 'concat_genetic.csv'

# Load xls sheet
df <- read.csv(infile, row.names = 1, check.names = F)


## MAP canton name to 2 letters abbrevation
iso2c <- canton_namesStrict(rownames(df))
rownames(df) <- iso2c

# reorder df by iso2 canton abbreviations
iorder <- match(canton_CH$iso2, rownames(df))
df <- df[iorder,]

write.csv(df, "geneticVotes.csv")


















