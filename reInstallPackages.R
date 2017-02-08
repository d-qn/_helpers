package_df <- as.data.frame(installed.packages("~/Library/R/3.1/library"))
package_list <- as.character(package_df$Package)
install.packages(package_list)


# ‘isotope’, ‘parsetR’, ‘rcdimple’, ‘rCharts’, ‘rMaps’, ‘rstudio’, ‘rWordCloud’, ‘shinyapps’, ‘slopegraph’,
# ‘streamgraph’, ‘sweetalertR’, ‘swiMap’, ‘swiRcharts’, ‘swiTheme’

dev.p <- c('parsetR', 'rCharts', 'rstudio', 'rWordCloud', 'shinyapps', 'slopegraph',
'streamgraph', 'swiMap', 'swiRcharts', 'swiTheme')