#################################################################################################
## Project	: Module 4 / Week 4 (Plotting - EPA data)
## Author	: Paul Ringsted
## Date		: 2018-11-16
## Function	: plot2()
## Description 	: 2nd plot - Total Emissions for Baltimore City by Year
#################################################################################################

plot2 <- function(onscreen=FALSE) {
	## Load the NEI data
	filename <- file.path(".","summarySCC_PM25.rds")
	NEI <- readRDS(filename)
	print(paste("file loaded, rows: ",nrow(NEI), " cols: ",ncol(NEI)))

	if (!onscreen) {
		print("Opening PNG file...")
		png(filename="plot2.png")
	}

	## Select data for Baltimore
	NEIsub <- NEI[NEI$fips==24510,]
	print(paste("Subset of data, rows: ",nrow(NEIsub), " cols: ",ncol(NEIsub)))
	
	tot <- with(NEIsub,aggregate(Emissions,by=list(year),FUN=sum))
	names(tot) <- c("year","Emissions")
	tot$Emissions <- tot$Emissions/1000
	print(tot)
	
	## Plot with regression line to see trend
	plot(tot,ylab="Emissions (thousand tons)")
	regline<-with(tot,lm(Emissions ~ year))
	abline(regline,lwd=2)
	title(main="Plot2 - Total Emissions for Baltimore City by Year")

	if (!onscreen) {
		dev.off()
		print("Closed PNG file")
	}
	
}
