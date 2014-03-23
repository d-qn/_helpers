library(ggplot2)
library(RColorBrewer)
library(reshape)
library(plyr)
library(scales)
library(extrafont)
require(gridExtra)
suppressWarnings(loadfonts())


############################################################################################
###    Minimalistic ggplot theme, no fuss
############################################################################################

ggtheme <- {
    #eliminates baground, gridlines, and chart border
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank()
 )
}

ggtheme_ygrid <- {
    #eliminates baground, gridlines, and chart border
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
   panel.grid.minor.y = element_blank(),
   panel.border = element_blank(), panel.background = element_blank()
 )
}


ggtheme2 <- {
    #eliminates baground, gridlines, chart border and axis
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
   axis.text = element_blank()
 )
}


colpal_qual <- c("#2D343C", "#C4C4C4", "#D62B22", "#131D26", "#131D26", "#4C1120", "#5C7964", "#DDCB8D", "#BB4E53")


############################################################################################
###   Path to swiss maps shapfiles
############################################################################################

swissMapShp.path <- "~/swissinfo/swiss-maps/swiss-maps/shp/ch"
swissMapShp2012.path <- "~/swissinfo/swiss-maps/swiss-maps/shp2012/ch"
swissMapShp2010.path <- "~/swissinfo/swiss-maps/swiss-maps/shp2010/ch"


## Layers available: country, lakes, municipalities, districts, cantons

############################################################################################
###   format shapefiles for ggplot (https://github.com/hadley/ggplot2/wiki/plotting-polygon-shapefiles)
############################################################################################

formatShp <- function(shpF) {
	shpF@data$id <- rownames(shpF@data)
	shpF.points <- fortify(shpF, region = "id")
	shpF.df <- join(shpF.points, shpF@data, by = "id")
	shpF.df
}


