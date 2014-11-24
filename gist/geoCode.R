library(ggmap)
source("~/swissinfo/_helpers/helpers.R")


lonlat <- geocode(paste(as.character(canton_names[,'english']), "Switzerland"))


canton_names <- cbind(canton_names, lonlat)

write.csv(canton_names, file ="CantonCH_iso.csv", row.names = F)


