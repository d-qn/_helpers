pkgname <- "swiRcharts"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
base::assign(".ExTimings", "swiRcharts-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('swiRcharts')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("swiRcharts-package")
### * swiRcharts-package

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: swiRcharts-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: swiRcharts-package swiRcharts
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("swiRcharts-package", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("swi_rcharts")
### * swi_rcharts

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: hSeries
### Title: Helper functions for rCharts
### Aliases: hChart2responsiveHTML hSeries

### ** Examples

## Not run: 
##D Example for hSeries to create a labelled bubble scatterchart with rCharts/highcharts
##D 
##D library(swiTheme)
##D a <- rCharts::Highcharts$new()
##D x <- 1:10
##D y <- seq(1, 100, 10)
##D z <- 10:1
##D name <- c("a", "b", "c", "d", "e", "f", "g", "h", "i", "j")
##D series <- c(rep(c("blob", "poop", "doop"), 3), "asdf")
##D a$series(hSeries(x,y,z,name, series))
##D 
##D # tweak the bubble plot
##D a$chart(zoomType = "xy", type = "bubble")
##D a$plotOptions(bubble = list(dataLabels = list(enabled = T, style = list(textShadow = 'none') ,
##D color = '#aa8959', formatter = "#! function() { return this.point.name; } !#")))
##D 
##D a$colors(swi_rpal)
##D a$tooltip(formatter = "#! function() { return this.point.name + ':' +this.x + ', ' + this.y; } !#")
##D a$xAxis(title = list(text = "important indicator", align = "high"), lineColor = list ('#FF0000'))
##D a
##D 
##D hChart.html <- tempfile("hchart_labelledBubble.html")
##D a$save(hChart.html)
## End(Not run)
##' \dontrun{
# Example of converting a highcharts-rCharts html chart into a responsive one

hChart2responsiveHTML(hChart.html, source = "source: stupid data")
}



base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("swi_rcharts", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
