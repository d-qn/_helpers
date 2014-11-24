source("~/swissinfo/_helpers/helpers.R")
#library(pxR)
############################################################################################
###   SETTINGS
############################################################################################

infile <- '2011-Table 1.csv'
partis <- c('PLR', 'PDC', 'PS', 'UDC', 'PES')


# Load xls sheet
readFile <- read.csv(infile, sep =",")

## remove all non-numeric values
df <- data.frame(as.character(readFile[,1]), sapply(readFile[2:ncol(readFile)], function(c) as.numeric(as.character(c))))

## MAP canton name to 2 letters abbrevation
iso2c <- sapply(1:nrow(df), function(i) {
  	c <- df[i,1]

    idx <- match(c, canton_names[,'allemand2'])
    if(!is.na(idx)) {
		as.character(canton_names[idx, 'iso2'])
	} else {
		as.character(canton_names[agrep(c, canton_names[,'allemand2'], costs = list(sub = 10, del = 3)), 'iso2'])
	}
  })

if(any(duplicated(iso2c)) || !is.vector(iso2c)) {
	stop("Problem with the 2 letters mapping!")
}

cantons.init <- df[,1]
df <- df[,-c(1)]
rownames(df) <- iso2c

iorder <- match(canton_names$iso2, rownames(df))
df <- df[iorder,]

# test sum % add up to 100 and partis of interest are present
stopifnot(all(apply(df, 1, sum, na.rm = T) >= 99.9 | apply(df, 1, sum, na.rm = T) <= 100.1))
stopifnot(all(partis %in%  colnames(df)))

sapply(partis, function(p) {
	df[,p] / 100
})


















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



