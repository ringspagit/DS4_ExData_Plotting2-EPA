#################################################################################################
## Project	: Module 4 / Week 4 (Plotting - EPA data)
## Author	: Paul Ringsted
## Date		: 2018-11-16
## Function	: plot3()
## Description 	: 3rd plot - Total Emissions for Baltimore City by Year and Type
#################################################################################################

library('ggplot2')

plot3 <- function(onscreen=FALSE) {
	## Load the NEI data
	filename <- file.path(".","summarySCC_PM25.rds")
	NEI <- readRDS(filename)
	print(paste("file loaded, rows: ",nrow(NEI), " cols: ",ncol(NEI)))

	if (!onscreen) {
		print("Opening PNG file...")
		png(filename="plot3.png")
	}

	## Select data for Baltimore
	NEIsub <- NEI[NEI$fips==24510,]
	print(paste("Subset of data, rows: ",nrow(NEIsub), " cols: ",ncol(NEIsub)))
	
	tot <- with(NEIsub,aggregate(Emissions,by=list(year,type),FUN=sum))
	names(tot) <- c("year","type","Emissions")
	tot$Emissions <- tot$Emissions/1000
	print(tot)
	
	## Plot by type with regression line to see trend
	g <- ggplot(tot,aes(year,Emissions))
	g <- g + geom_point() + geom_smooth(method="lm",se=FALSE) + facet_grid(.~type)
	g <- g + labs(y="Emissions (thousand tons)")
	g <- g + labs(title="Plot3 - Total Emissions for Baltimore City by Year and Type")
	print(g)

	if (!onscreen) {
		dev.off()
		print("Closed PNG file")
	}
	
}
