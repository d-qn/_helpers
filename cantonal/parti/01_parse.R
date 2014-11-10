source("~/swissinfo/_helpers/helpers.R")
#library(pxR)
############################################################################################
###   SETTINGS
############################################################################################

infile <- '2011-Table 1.csv'

# Load xls sheet
readFile <- read.csv(infile, sep =",")

## remove all non-numeric values
df <- data.frame(as.character(readFile[,1]), sapply(readFile[2:ncol(readFile)], function(c) as.numeric(c)))


############################# !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! -------------<<<<<<<<<<<<<<<<<<<

iorder <- match(canton_names$iso2, colnames(df))

data <- df[,iorder]

# deal with some duplicated indicators!
rown <- gsub(" {2,}", " ", paste(df[,2], df[,1]))
rown[which(duplicated(rown))] <- paste(rown[which(duplicated(rown))], "2")

rownames(data) <- rown

row.sub <- c('Population Habitants en milliers', 'Population par km2', 'Population Etrangers en %', "Population 0–19", "Population 20–64", "Population 65 ou plus", "Population Population urbaine en %",
	"Langue principale en % Allemand" , "Appartenance à une religion en % Catholiques romains"  , "Appartenance à une religion en % Evangéliques réformés" ,
	"Surface en km2 Surfaces d'habitat et d'infrastructure en %"  , "Travail Taux de chômage en % (selon le SECO)", "Economie Produit intérieur brut par habitant, en francs",
	"Mobilité Voitures de tourisme pour 1000 habitants", "Logements Taux de logements vacants", "Niveau de formation (dès 25 ans) en % Sans formation postobligatoire",
	"Niveau de formation (dès 25 ans) en % Degré secondaire II", "Niveau de formation (dès 25 ans) en % Degré tertiaire" ,
	"Protection sociale Taux d'aide sociale ")

write.csv(data[row.sub,], file = "cantonal_sub.csv")



