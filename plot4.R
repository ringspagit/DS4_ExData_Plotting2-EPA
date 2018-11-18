#################################################################################################
## Project	: Module 4 / Week 4 (Plotting - EPA data)
## Author	: Paul Ringsted
## Date		: 2018-11-16
## Function	: plot4()
## Description 	: 4th plot - Emissions from coal combustion related sources by year
#################################################################################################

library('ggplot2')

plot4 <- function(onscreen=FALSE) {
	## Load the NEI data
	filename <- file.path(".","summarySCC_PM25.rds")
	NEI <- readRDS(filename)
	print(paste("NEI file loaded, rows: ",nrow(NEI), " cols: ",ncol(NEI)))

	## Load the SCC data
	filename <- file.path(".","Source_Classification_Code.rds")
	SCC <- readRDS(filename)
	print(paste("SCC file loaded, rows: ",nrow(SCC), " cols: ",ncol(SCC)))

	if (!onscreen) {
		print("Opening PNG file...")
		png(filename="plot4.png")
	}

	## Select all references to coal in the SCC data as lookup list
	SCCcoal <- SCC$SCC[grepl("coal",tolower(SCC$Short.Name))]
	NEIsub <- NEI[NEI$SCC %in% SCCcoal,]
	print(paste("Subset of data, rows: ",nrow(NEIsub), " cols: ",ncol(NEIsub)))
	
	## Aggregate the subset by year
	tot <- with(NEIsub,aggregate(Emissions,by=list(year),FUN=sum))
	names(tot) <- c("year","Emissions")
	tot$Emissions <- tot$Emissions/1000
	print(tot)
	
	## Plot by type with regression line to see trend
	g <- ggplot(tot,aes(year,Emissions))
	g <- g + geom_point() + geom_smooth(method="lm",se=FALSE)
	g <- g + labs(y="Emissions (thousand tons)")
	g <- g + labs(title="Plot4 - Coal-Related Emissions by Year")
	print(g)

	if (!onscreen) {
		dev.off()
		print("Closed PNG file")
	}
	
}
