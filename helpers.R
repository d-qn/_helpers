library(ggplot2)
library(RColorBrewer)
library(reshape)
library(plyr)
library(scales)
library(countrycode)
library(extrafont)
loadfonts(quiet = TRUE)
require(gridExtra)



############################################################################################
###    Minimalistic ggplot theme, no fuss
############################################################################################

ggtheme <- {
    #based theme_bw eliminates baground, gridlines, and chart border
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank()
 )
}

ggtheme_ygrid <- {
    #based theme_bw eliminates baground, x gridlines and ticks, and chart border
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
   panel.grid.minor.y = element_blank(), panel.background = element_blank()
 )
}

ggtheme_xgrid <- {
    #based theme_bw eliminates baground, x gridlines and ticks, and chart border
  theme_bw() + theme(
   plot.background = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
   panel.grid.minor.x = element_blank(), panel.background = element_blank()
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
swi_22palette <- c("#336666", "#368596", "#669999", "#366096",
	"#333366", "#666699", "#996699", "#663333",
	"#ab3d3f", "#996666", "#ac673e", "#ac7f3e",
	"#666633", "#999966", "#89a23a", "#3a9736",
	"#aa8959", "#bfa681", "#d4c3aa", "#e5dbcd",
	"#efe9e0", "#f7f5ed")

swi_9palette <- swi_22palette[c(1, 4, 6, 8, 9, 11, 13, 17, 20)]

############################################################################################
###   Path to swiss maps shapfiles
############################################################################################

swissMapShp.path <- "~/swissinfo/swiss-maps/shp2013/ch"
swissMapShp2012.path <- "~/swissinfo/swiss-maps/shp2012/ch"
swissMapShp2010.path <- "~/swissinfo/swiss-maps/shp2010/ch"


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


