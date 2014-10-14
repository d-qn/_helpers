library(ggplot2)
library(RColorBrewer)
library(reshape)
library(plyr)
library(scales)
library(countrycode)
library(extrafont)
library(magrittr)
library(png)
library(RSvgDevice)
library(directlabels)
loadfonts(quiet = TRUE)
require(gridExtra)


############################################################################################
###    Minimalistic ggplot theme, no fuss
############################################################################################
font <- "Open Sans"

swi_logo <- readPNG("~/swissinfo/_helpers/SWI-RGB.png")

colpal_qual <- c("#2D343C", "#C4C4C4", "#D62B22", "#131D26", "#131D26", "#4C1120", "#5C7964", "#DDCB8D", "#BB4E53")
swi_22palette <- c("#336666", "#368596", "#669999", "#366096",
	"#333366", "#666699", "#996699", "#663333",
	"#ab3d3f", "#996666", "#ac673e", "#ac7f3e",
	"#666633", "#999966", "#89a23a", "#3a9736",
	"#aa8959", "#bfa681", "#d4c3aa", "#e5dbcd",
	"#efe9e0", "#f7f5ed")


r22palette <- c(9, 6, 3, 8, 1, 4, 7, 11, 16, 5, 14, 13, 15, 2, 10, 18, 20, 19, 12, 17, 22, 21)
swi_22rpalette <- swi_22palette[r22palette]
swi_9palette <- swi_22palette[c(1, 4, 6, 8, 9, 11, 13, 17, 20)]

quandlAPIkey <- 'zd85sxxvRZZVhJye3sPy'

ggtheme <- {
    #based theme_bw eliminates baground, gridlines, and chart border
  theme_bw() + theme(text = element_text(family = font),
   plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_line(size = 0.2),
   plot.title = element_text(hjust = 0),panel.grid.major = element_line(colour = "#efe9e0")
 )
}

ggtheme_ygrid <- {
    #based theme_bw eliminates baground, x gridlines and ticks, and chart border
  theme_bw() + theme(text = element_text(family = font),
   plot.background = element_blank(), panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),
   panel.grid.minor.y = element_blank(), panel.border = element_blank(),panel.background = element_blank(),
   axis.ticks = element_line(size = 0.2),plot.title = element_text(hjust = 0), panel.grid.major = element_line(colour = "#efe9e0")
 )
}

ggtheme_xgrid <- {
    #based theme_bw eliminates baground, y gridlines and ticks, and chart border
  theme_bw() + theme(text = element_text(family = font),
   plot.background = element_blank(), panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank(),
   panel.grid.minor.x = element_blank(), panel.background = element_blank(),axis.ticks = element_line(size = 0.2),
   plot.title = element_text(hjust = 0),panel.grid.major = element_line(colour = "#efe9e0")
 )
}

ggtheme2 <- {
    #eliminates baground, gridlines, chart border and axis
  theme_bw() + theme(text = element_text(family = font),
   plot.background = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
   panel.border = element_blank(), panel.background = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(),
   axis.text = element_blank(), plot.title = element_text(hjust = 0)
 )
}

paneltheme <- {
	theme(strip.background = element_rect(colour = "white", fill = "white", size = 0.1),
		strip.text = element_text(colour = "black", hjust = 0.1, family = font))
}



# Multiple plot function from : http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


############################################################################################
###   Load files to transpose country and swiss cantons
############################################################################################

country_names <- read.csv("~/swissinfo/_helpers/countrynames.csv", sep =";")
canton_names  <- read.csv("~/swissinfo/_helpers/CantonCH_iso.csv")

############################################################################################
###   Path to swiss maps shapfiles
############################################################################################

worldMapShp.path <- "~/swissinfo/_helpers/TM_WORLD_BORDERS_SIMPL-0"
swissMapShp.path <- "~/swissinfo/swiss-maps/shp2013/ch"
swissMapShp2012.path <- "~/swissinfo/swiss-maps/shp2012/ch"
swissMapShp2010.path <- "~/swissinfo/swiss-maps/shp2010/ch"
swissMapShp2012.path <- "~/swissinfo/swiss-maps/shp2012/ch"

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

############################################################################################
###   Create multiple version SVG from a svg and text to translate files
############################################################################################

getTextFromSVG <- function(input = NULL, ouputFileAppend = "_text.csv") {
	if(!file.exists(input)) stop (input, " cannot be found")
	if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")

	data <- readLines(input, warn = F)

	# get all the text elements
	idx <- grep(">(.*)</tspan></text>", data)
	texts <- gsub(".*>(.*)</tspan></text>", "\\1", data[idx])

	# discard text elements which are only numbers
	write.csv(texts[grep("^\\d+$", texts, invert = T)], file = gsub("\\.svg", ouputFileAppend, input))
}
createTranslatedSVG <- function(input = NULL, text = NULL, inDirectory = "trad", overwrite = FALSE, ...) {
	if(!file.exists(input)) stop (input, " cannot be found")
	if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")
	if(!file.exists(text)) stop (text, " cannot be found")
	if(!grepl("\\.csv$", text)) stop(text, " needs to be a csv file")

	data <- readLines(input, warn = F)
	text <- read.csv(text, header = TRUE, stringsAsFactors = FALSE)
	stopifnot(is.data.frame(text))

	if(inDirectory != "") {
		if(!file.exists(inDirectory)) dir.create(inDirectory)
	}

	# Find non matching elements
	sapply(text[[1]], function(tt) {
		m <- which(grepl(tt, data, ignore.case = T))
		if(length(m)< 1) warning (tt, " not matched!",  call. = FALSE)
	})

	invisible(sapply(colnames(text)[-1], function(lang) {
		ndata <- paste(data, collapse = "\n")
		for(l in 1:nrow(text)) {
			#browser()
			tgt <- paste(">", text[l,1], "<", sep = "")
			new <- paste(">", text[l,lang], "<", sep = "")
			ndata <- gsub(tgt, new, ndata, ignore.case = T)
		}
		outfile <- paste(gsub("(^.*)\\.svg$", "\\1", input), "_", lang, ".svg", sep = "")
		if(inDirectory != "") outfile <- paste(inDirectory, "/", outfile, sep = "")
		if(file.exists(outfile) && !overwrite) {
			stop(outfile, " already exists! Delete it or use 'overwrite = TRUE")
		} else {
			cat(ndata, file = outfile)
			cat("\n", outfile, " saved!", "\n")
		}
	}))
}

