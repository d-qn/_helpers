source("~/swissinfo/_helpers/helpers.R")
#library(pxR)
############################################################################################
###   SETTINGS
############################################################################################

infile <- 'je-f-21-03-02-xls_refined.tsv'

# Load xls sheet
readFile <- read.csv(infile, sep ="\t", stringsAsFactors = F)

## remove all non-numeric values
df <- cbind(readFile[1:3], sapply(readFile[4:ncol(readFile)], function(c) as.numeric(c)))

iorder <- match(canton_names$iso2, colnames(df))

data <- df[,iorder]

# deal with some duplicated indicators!
rown <- gsub(" {2,}", " ", paste(df[,2], df[,1]))
rown[which(duplicated(rown))] <- paste(rown[which(duplicated(rown))], "2")

rownames(data) <- rown

# Create an additonal row with UDC and small right wings parties
nrowName <- 'Partis politiques en % (élections au Conseil national) UDC et petits partis de droite'

data[nrowName,] <- colSums(data[c('Partis politiques en % (élections au Conseil national) UDC',
	'Partis politiques en % (élections au Conseil national) Petits partis de droite'),], na.rm = T)

row.sub <- c('Population Habitants en milliers', 'Population par km2', 'Population Etrangers en %', "Population 0–19", "Population 20–64", "Population 65 ou plus",
	"Population Population urbaine en %", "Langue principale en % Allemand" , "Appartenance à une religion en % Catholiques romains"  , "Appartenance à une religion en % Evangéliques réformés" ,
	"Surface en km2 Surfaces d'habitat et d'infrastructure en %"  , "Travail Taux de chômage en % (selon le SECO)", "Economie Produit intérieur brut par habitant, en francs",
	"Mobilité Voitures de tourisme pour 1000 habitants", "Logements Taux de logements vacants", "Niveau de formation (dès 25 ans) en % Sans formation postobligatoire",
	"Niveau de formation (dès 25 ans) en % Degré secondaire II", "Niveau de formation (dès 25 ans) en % Degré tertiaire" ,
	"Protection sociale Taux d'aide sociale ", 'Partis politiques en % (élections au Conseil national) PS',
	"Partis politiques en % (élections au Conseil national) PBD",
	'Partis politiques en % (élections au Conseil national) UDC', "Partis politiques en % (élections au Conseil national) PLR ",
	"Partis politiques en % (élections au Conseil national) PDC", 'Partis politiques en % (élections au Conseil national) Petits partis de droite',
	nrowName)

write.csv(t(data[row.sub,]), file = "cantonal_sub.csv")



