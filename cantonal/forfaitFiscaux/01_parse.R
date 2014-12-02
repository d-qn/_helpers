source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###   SETTINGS
############################################################################################

infile <- 'forfaitFiscaux_recettesFiscales_sub.csv'

# Load xls sheet
readFile <- read.csv(infile, row.names = 1)

# order by canton names
iorder <- match(canton_names$iso2, rownames(readFile))
data <- readFile[iorder,]

newCol <- 'Part en % des forfaits fiscaux dans les recettes fiscales cantonales'
data[,newCol] <- (data[,"impôt.cantonal"] / data[,'Recettes.fiscales.cantonales']) * 100

coln.sub <- c("Nombre.de.forfaits", "impôt.cantonal", "totale.des.recettes.fiscales", newCol)

write.csv(data[,coln.sub], file = "forfaitFiscaux_2012.csv")



