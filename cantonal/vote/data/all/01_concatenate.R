source("~/swissinfo/_helpers/helpers.R")
require(XLConnect)


infile <- 'je-f-17.03.02.04.cz.580.k.xls'
yearIntervals <- 1999:2012



table <- do.call(rbind, lapply(yearIntervals, function(year) {
	cat("\n", year)
	data.read <- readWorksheetFromFile(infile,
		sheet = as.character(year), startRow = 6, endRow = 36)

	# delete NA rows
	data.read <- data.read[!sapply(1:nrow(data.read), function(i) all(is.na(data.read[i,]))),]
	# take the first and last columns
	data.read <- data.read[,c(1, ncol(data.read))]
	# remove the number parenthesis in country names
	data.read[,1] <- gsub(" \\d+\\)$", "", data.read[,1])

	colnames(data.read) <- c("country", "value")
	# cluster
	clusterf <- factor(names(clusters)[match(data.read[,1], clusters)])
	tmp <- tapply(data.read[,2], clusterf, sum, na.rm =T)
	data.read <- rbind(data.read, data.frame(country = names(tmp), value = as.vector(tmp)))

	# discard clustered countries
	data.read <- data.read[! data.read[,1] %in% clusters,]
	rownames(data.read) <- NULL
	cbind(data.read, year = year)
}))
