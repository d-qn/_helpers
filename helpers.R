library(ggplot2)
library(RColorBrewer)
library(scales)
library(countrycode)
library(extrafont)
library(magrittr)
library(reshape)
# library(plyr)
library(dplyr)
#library(tidyr) #devtools::install_github("hadley/tidyr")
library(png)
library(RSvgDevice)
library(directlabels)
loadfonts(quiet = TRUE)
require(gridExtra)
library(XML)


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

# get the the different canton abbreviatiosn or languages
canton_namesStrict <- function(query, output = 'iso2') {
	stopifnot(exists("canton_names"))
	if (!output %in% colnames(canton_names) || length(output) != 1) {
		stop ("output needs to be one of:", paste(colnames(canton_names), collapse =" "))
	}

	result <- query
	for(i in 1:length(query)) {
		q <- query[i]
		nrow <- which(q == canton_names, T)
		if(length(nrow) == 0) {
			warning("\n", q, "could not be matched! q", "returned.")
		}
		if(length(unique(nrow[,1])) > 1) {
			warning("\n", q, "matched multiple cantons! q", "returned.")
		}
		result[i] <- as.character(canton_names[unique(nrow[,1]),output])
	}
	result
}


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
	shpF.df <- plyr::join(shpF.points, shpF@data, by = "id")
	shpF.df
}

############################################################################################
###   Create multiple version SVG from a svg and text to translate files
############################################################################################


parseSVG <- function(input = NULL) {
	if(!file.exists(input)) stop (input, " cannot be found")
	if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")
	xmlTreeParse(input, useInternalNodes = T)
}

parseTxt <- function(xmlTree) {
	# get all the tspan within text elements
	# I don't really understand the parsing, adapted from: http://www.omegahat.org/SVGAnnotation/SVGAnnotationPaper/SVGAnnotationPaper.html#bib:XPathXPointer
	getNodeSet(xmlTree, "//x:text/x:tspan", "x")
}

get_nonNumericNonEmptyString <- function(textNodes) {
	# Get the index of text which are not empty and not only numeric
	!sapply(textNodes, xmlValue) %in% c("", " ", "\n") & !grepl("^\\d+$", sapply(textNodes, xmlValue))
}

createTextToTranslate <- function(input, ouputFileAppend = "_text.csv") {
	textNodes <- parseTxt(parseSVG(input))
	idx <- get_nonNumericNonEmptyString(textNodes)

	# Get the value of text elements
	write.csv(unique(sapply(textNodes[idx], xmlValue)),
	  file = gsub("\\.svg", ouputFileAppend, input), row.names = FALSE)
}


input <- "areaChart_europeanCountries_ygrid.svg"
tradFile <- "trad.csv"
inDirectory <- "trad"
createTranslatedSVG <- function(input = NULL, tradFile = NULL, inDirectory = "trad", overwrite = FALSE, ...) {
		if(!file.exists(input)) stop (input, " cannot be found")
		if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")
		if(!file.exists(tradFile)) stop (tradFile, " cannot be found")
		if(!grepl("\\.csv$", tradFile)) stop(tradFile, " needs to be a csv file")

	    # Parse the XML
		xmlTree <- parseSVG(input)

	    # Get the text from the input svg to translate
		textNodes <- parseTxt(xmlTree)
		idx <- get_nonNumericNonEmptyString(textNodes)
		text.ori <- sapply(textNodes[idx], xmlValue)

		# Get the translations
		trad <- read.csv(tradFile, header = TRUE, stringsAsFactors = FALSE)

		if(ncol(trad) <= 1) stop("Translation file needs at least 2 columns")

		# Match the translation first column to the original text
		idx <- match(text.ori, trad[,1])
		if(any(is.na(idx))) stop("Translation file do not match the text from the svg!")

		for(j in 1:length(textNodes[idx])) {
			xmlValue(textNodes[idx][[j]])
		}


		# Loop trough the different columns and create svg files
		sapply(2:ncol(trad), function(i) {
			lang <- colnames(trad)[i]

			for(j in 1:length(textNodes[idx])) {
				xmlValue(textNodes[idx][[j]]) <- trad[idx[j], lang]
			}

			saveXML(xmlTree, "testTranslate.svg")
		})
}




# getTextFromSVG <- function(input = NULL, ouputFileAppend = "_text.csv") {
# 	if(!file.exists(input)) stop (input, " cannot be found")
# 	if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")
#
# 	data <- readLines(input, warn = F)
#
# 	# get all the text elements
# 	idx <- grep(">(.*)</tspan></text>", data)
# 	texts <- gsub(".*>(.*)</tspan></text>", "\\1", data[idx])
#
# 	# discard text elements which are only numbers
# 	write.csv(texts[grep("^\\d+$", texts, invert = T)], file = gsub("\\.svg", ouputFileAppend, input))
# }
#
#
# createTranslatedSVG <- function(input = NULL, text = NULL, inDirectory = "trad", overwrite = FALSE, ...) {
# 	if(!file.exists(input)) stop (input, " cannot be found")
# 	if(!grepl("\\.svg$", input)) stop(input, " needs to be a svg file")
# 	if(!file.exists(text)) stop (text, " cannot be found")
# 	if(!grepl("\\.csv$", text)) stop(text, " needs to be a csv file")
#
# 	data <- readLines(input, warn = F)
# 	text <- read.csv(text, header = TRUE, stringsAsFactors = FALSE)
# 	stopifnot(is.data.frame(text))
#
# 	if(inDirectory != "") {
# 		if(!file.exists(inDirectory)) dir.create(inDirectory)
# 	}
#
# 	# Find non matching elements
# 	sapply(text[[1]], function(tt) {
# 		m <- which(grepl(tt, data, ignore.case = T))
# 		if(length(m)< 1) warning (tt, " not matched!",  call. = FALSE)
# 	})
#
# 	invisible(sapply(colnames(text)[-1], function(lang) {
# 		ndata <- paste(data, collapse = "\n")
# 		for(l in 1:nrow(text)) {
# 			#browser()
# 			tgt <- paste(">", text[l,1], "<", sep = "")
# 			new <- paste(">", text[l,lang], "<", sep = "")
# 			ndata <- gsub(tgt, new, ndata, ignore.case = T)
# 		}
# 		outfile <- paste(gsub("(^.*)\\.svg$", "\\1", input), "_", lang, ".svg", sep = "")
# 		if(inDirectory != "") outfile <- paste(inDirectory, "/", outfile, sep = "")
# 		if(file.exists(outfile) && !overwrite) {
# 			stop(outfile, " already exists! Delete it or use 'overwrite = TRUE")
# 		} else {
# 			cat(ndata, file = outfile)
# 			cat("\n", outfile, " saved!", "\n")
# 		}
# 	}))
# }
#
