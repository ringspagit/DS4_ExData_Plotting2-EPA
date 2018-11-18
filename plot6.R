#################################################################################################
## Project	: Module 4 / Week 4 (Plotting - EPA data)
## Author	: Paul Ringsted
## Date		: 2018-11-16
## Function	: plot6()
## Description 	: 6th plot - Emissions from motor vehicle sources in Baltimore & LA by year
#################################################################################################

plot6 <- function(onscreen=FALSE) {
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
		png(filename="plot6.png")
	}

	## Find all the SCC codes (4) which have vehicle in the sector
	SCCveh <- SCC$SCC[grepl("vehicle",tolower(SCC$EI.Sector))]

	## Select data for motor vehicles
	NEIsub <- NEI[NEI$SCC %in% SCCveh,]
	print(paste("Subset of data, rows: ",nrow(NEIsub), " cols: ",ncol(NEIsub)))

	tot <- with(NEIsub,aggregate(Emissions,by=list(fips,year),FUN=sum))
	names(tot) <- c("fips","year","Emissions")
	tot$Emissions <- tot$Emissions/1000

	## Get totals and % change for Baltimore
	tot_balt <- tot[tot$fips == "24510",]
	balt_99 <- tot_balt$Emissions[tot_balt$year==1999]
	tot_balt$abs_change <- (tot_balt$Emissions-balt_99)
	tot_balt$pct_change <- 100*(tot_balt$abs_change)/balt_99
	print(tot_balt)

	## Get totals and % change for Baltimore
	tot_la <- tot[tot$fips == "06037",]
	la_99 <- tot_la$Emissions[tot_la$year==1999]
	tot_la$abs_change <- (tot_la$Emissions-la_99)
	tot_la$pct_change <- 100*(tot_la$abs_change)/la_99
	print(tot_la)

	## Plot two panels - emissions trend and % change
	
	par(mfrow=c(1,2),mar=c(4,4,2,1),oma=c(1,1,2,1))

	with(tot_balt,plot(year,Emissions,pch=17,lwd=2,col="blue",type='b',ylim=c(0,6),ylab="Emissions (thousand tons)"))
	with(tot_la,lines(year,Emissions,pch=8,lwd=2,col="red",type='b'))
	legend("topright",pch=c(17,8),col=c("blue","red"),legend=c("Baltimore City","Los Angeles County"))

	with(tot_balt,plot(year,pct_change,pch=17,lwd=2,col="blue",type='b',ylim=c(-100,100),ylab="% Change in Emissions"))
	with(tot_la,lines(year,pct_change,pch=8,lwd=2,col="red",type='b'))
	legend("topright",pch=c(17,8),col=c("blue","red"),legend=c("Baltimore City","Los Angeles County"))

	mtext("Plot6 - Motor Vehicle Emissions for Baltimore City vs. Los Angeles County",outer=TRUE)
	
	if (!onscreen) {
		dev.off()
		print("Closed PNG file")
	}
	
}
