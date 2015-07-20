# source("~/swissinfo/_helpers/helpers.R")
library(swiMap)

############################################################################################
###   SETTINGS: PATH TO INDICATOR FILES
############################################################################################

outputFile <- "cantonalIndicators.csv"

ifiles <- c("~/swissinfo/_helpers/cantonal/portraitCantonaux/cantonal_sub.csv",
	"~/swissinfo/_helpers/cantonal/densitePopulation/densite_2000_2013.csv",
	"~/swissinfo/_helpers/cantonal/variationPopulation/population_2000_2013.csv",
	# "~/swissinfo/_helpers/cantonal/forfaitFiscaux/forfaitFiscaux_2012.csv",
	"~/swissinfo/_helpers/cantonal/vote/populist/popuInitiatives.csv",
	#"~/swissinfo/_helpers/cantonal/vote/family/familyInitiatives.csv",
	#"~/swissinfo/_helpers/cantonal/menage/tailleMenage_2014.csv",
	"~/swissinfo/_helpers/cantonal/vote/genetic/geneticVotes.csv",
	"~/swissinfo/_helpers/cantonal/bourses/boursesTertiaires_2013.csv"
)

############################################################################################
###   SETTINGS: PATH TO INDICATOR FILES
############################################################################################


indicators <- do.call(cbind, lapply(ifiles, function (csv) {
	ind <- read.csv(csv, row.names = 1, check.names = FALSE)
	# ensure rownames (cantons) are ordered correctly
	if(!identical(match(canton_CH[,'iso2'], rownames(ind)) , 1:26)) {
		stop("Problem with", csv, "!", "\t its rownames (2 letters canton name) are not ordered alphabetically")
	}
	ind
}))

write.csv(indicators, file = outputFile)
