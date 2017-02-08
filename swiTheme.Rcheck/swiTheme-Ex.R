pkgname <- "swiTheme"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('swiTheme')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("colors_swi")
### * colors_swi

flush(stderr()); flush(stdout())

### Name: swi_pal
### Title: swissinfo.ch standard color palette
### Aliases: swi_dpal swi_dpal2 swi_pal swi_rpal swi_spal
### Keywords: datasets

### ** Examples

pie(rep(1,length(swi_pal)), col=swi_pal)
pie(rep(1,length(swi_rpal)), col=swi_rpal)
pie(rep(1,length(swi_dpal)), col=swi_dpal)
pie(rep(1,length(swi_dpal)), col=swi_dpal2)
pie(rep(1,length(swi_spal)), col=swi_spal)



cleanEx()
nameEx("multiplot")
### * multiplot

flush(stderr()); flush(stdout())

### Name: multiplot
### Title: Multiple plot function
### Aliases: multiplot

### ** Examples

q1 <- qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi2()
q2 <- qplot(mpg, data = mtcars, geom = "dotplot") + theme_swi()
multiplot(list(q1, q2))



cleanEx()
nameEx("swiTheme-package")
### * swiTheme-package

flush(stderr()); flush(stdout())

### Name: swiTheme-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: swiTheme-package swiTheme
### Keywords: ggplot2, pdf, grid, scales

### ** Examples

~~ simple examples of the most important functions ~~



cleanEx()
nameEx("theme_swi")
### * theme_swi

flush(stderr()); flush(stdout())

### Name: theme_swi
### Title: swissinfo.ch's chart theme
### Aliases: swi_theme theme_swi theme_swi2 theme_swiMin theme_swiYLines

### ** Examples

# swi_theme() with annotations
gg <- qplot(1:10, 1:10) + swi_theme()
# add y-axis label 
gg <- gg + geom_label(aes(x = 0, y = 10, label="y label text"),
  family = "OpenSans-CondensedLight", size=3.5, 
  hjust=0, label.size=0, color="#2b2b2b")
# basic axis scale and breaks
gg <- gg + scale_x_continuous(expand=c(0,0), breaks = seq(0, 10, by=2), 
  labels = seq(0, 10, by=2), limits=c(0, 10.2), name = NULL)
gg <- gg + scale_y_continuous(expand=c(0,0.3), breaks=seq(1, 10, by=1), limits=c(0, 10.2))
# footnote / caption / subtitle
caption <- "Note: Vacancies are counted as the number of days between a justice's death, retirement or resignation and the successor justice's swearing in (or commissioning in the case of a recess appointment) as a member of the court.Sources: U.S. Senate, 'Supreme Court Nominations, present-1789'; Supreme Court, 'Members of the Supreme Court of the United States'; Pew Research Center calculations"
caption  <- paste0(strwrap(caption, 160), sep="", collapse="\n")
subtitle <- "Here is a subtitle text. It describes what is displayed in the chart. It can mention many important details to under the graphic."
subtitle  <- paste0(strwrap(subtitle, 160), sep="", collapse="\n") 
gg <- gg + labs(x=NULL, y=NULL, title="This is a title, choose it wisely",
subtitle=subtitle, caption=caption)
#annotations TODO !!!!

qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi()
qplot(mtcars$mpg) + theme_swi()


qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swiYLines()
qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi2()
 qplot(1:10, 1:10, size = 10:1) + theme_swiMin() + ggtitle("Minimal theme: no axis & no ticks")



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
