#################################################################################################
## Project	: Module 4 / Week 4 (Plotting - EPA data)
## Author	: Paul Ringsted
## Date		: 2018-11-16
## Function	: plot5()
## Description 	: 5th plot - Emissions from motor vehcile sources in Baltimore by year
#################################################################################################

library('ggplot2')

plot5 <- function(onscreen=FALSE) {
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
		png(filename="plot5.png")
	}

	## Find all the SCC codes (4) which have vehicle in the sector
	SCCveh <- SCC$SCC[grepl("vehicle",tolower(SCC$EI.Sector))]

	## Select data for Baltimore and motor vehicles
	NEIsub <- NEI[NEI$fips==24510 & NEI$SCC %in% SCCveh,]
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
	g <- g + labs(title="Plot 5 - Motor Vehicle Emissions for Baltimore City by Year")
	print(g)

	if (!onscreen) {
		dev.off()
		print("Closed PNG file")
	}
	
}
