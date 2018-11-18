#################################################################################################
## Project	: Module 4 / Week 4 (Plotting - EPA data)
## Author	: Paul Ringsted
## Date		: 2018-11-16
## Function	: plot1()
## Description 	: 1st plot - total emissions by year
#################################################################################################

plot1 <- function(onscreen=FALSE) {
	## Load the NEI data
	filename <- file.path(".","summarySCC_PM25.rds")
	NEI <- readRDS(filename)
	print(paste("file loaded, rows: ",nrow(NEI), " cols: ",ncol(NEI)))

	if (!onscreen) {
		print("Opening PNG file...")
		png(filename="plot1.png")
	}

	tot <- with(NEI,aggregate(Emissions,by=list(year),FUN=sum))
	names(tot) <- c("year","Emissions")
	tot$Emissions <- tot$Emissions/1000000
	print(tot)
	
	## Plot with regression line to see trend
	plot(tot,ylab="Emissions (million tons)")
	regline<-with(tot,lm(Emissions ~ year))
	abline(regline,lwd=2)
	title(main="Plot1 - Total Emissions by Year")

	if (!onscreen) {
		dev.off()
		print("Closed PNG file")
	}
	
}
