source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###   SETTINGS: PATH TO INDICATOR FILES
############################################################################################

outputFile <- "cantonalIndicators.csv"

ifiles <- c("~/swissinfo/_helpers/cantonal/portraitCantonaux/cantonal_sub.csv",
	"~/swissinfo/_helpers/cantonal/densitePopulation/densite_2000_2013.csv",
	"~/swissinfo/_helpers/cantonal/variationPopulation/population_2000_2013.csv",
	"~/swissinfo/_helpers/cantonal/forfaitFiscaux/forfaitFiscaux_2012.csv",
	"~/swissinfo/_helpers/cantonal/vote/populist/popuInitiatives.csv")

############################################################################################
###   SETTINGS: PATH TO INDICATOR FILES
############################################################################################


indicators <- do.call(cbind, lapply(ifiles, function (csv) {
	ind <- read.csv(csv, row.names = 1)
	# ensure rownames (cantons) are ordered correctly
	if(!identical(match(canton_names[,1], rownames(ind)) , 1:26)) {
		stop("Problem with", csv, "!", "\t its rownames (2 letters canton name) are not ordered alphabetically")
	}
	ind
}))

write.csv(indicators, file = outputFile)
