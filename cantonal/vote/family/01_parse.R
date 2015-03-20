source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###   SETTINGS
############################################################################################

infile <- 'concat_family.csv'


# Load xls sheet
df <- read.csv(infile, row.names = 1)


## MAP canton name to 2 letters abbrevation
iso2c <- canton_namesStrict(rownames(df))
rownames(df) <- iso2c

# reorder df by iso2 canton abbreviations
iorder <- match(canton_names$iso2, rownames(df))
df <- df[iorder,]

write.csv(df, "familyInitiatives.csv")
















