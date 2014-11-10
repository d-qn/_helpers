pkgname <- "swiTheme"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('swiTheme')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("swiTheme-package")
### * swiTheme-package

flush(stderr()); flush(stdout())

### Name: swiTheme-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: swiTheme-package swiTheme
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



cleanEx()
nameEx("theme_swi")
### * theme_swi

flush(stderr()); flush(stdout())

### Name: theme_swi
### Title: swissinfo.ch's chart theme
### Aliases: pdfswi swi_dpal swi_pal swi_rpal theme_swi theme_swiYLines
### Keywords: datasets

### ** Examples

qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swi()

qplot(mtcars$mpg) + theme_swi()
qplot(1:10, 1:10, size = 10:1) + xlab("axis x label") + ylab ("y axis label") + theme_swiYLines()
pie(rep(1,length(swi_pal)), col=swi_pal)
pie(rep(1,length(swi_rpal)), col=swi_rpal)
pie(rep(1,length(swi_dpal)), col=swi_dpal)



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
