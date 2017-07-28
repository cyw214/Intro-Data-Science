# Load ggmap for ggplot2
library(ggmap)
# citation for ggmap:
# D. Kahle and H. Wickham. ggmap: Spatial Visualization with ggplot2.
# The R Journal, 5(1), 144-161. URL
# http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
library(mapproj)
# loading pbs mapping library
library(PBSmapping)
# loading maptools library
library(maptools)
# enabling rgeos library with map tools
gpclibPermit()
# adding ggplot2 library 
library(ggplot2)	
# add dplyr library



####################################
# Raster Image of PA In Background #
####################################
paRaster <- get_map(location= 'pennsylvania', zoom = 7)

#############################
# Read In County Shape File #
#############################
paPolys <- importShapefile("tl_2010_42_county10/tl_2010_42_county10",readDBF=TRUE)

#################################
# Plotting Outlines of Counties #
#################################
plotPolys(paPolys, xlim = c(-8963428.8121, -8314397.3302), ylim = c(4825307.3888, 5238600.7631), ()xlab="Longitude", ylab="Latitude")
title("Pennsylvania Counties Median Income (2008-2012)")

#####################################
# Adding County Name Labels To Plot #
#####################################

# Get Shape File Poly Data
paData<- attr(paPolys, "PolyData")
# Determine County Centroids
countyCentroids <- calcCentroid (paPolys, rollup = 1)
# Get County Names with PID
countyNames <- paData[,c("PID", "NAME10")]
# Merge Coordinates With Names
countyNameLables <- merge(countyNames, countyCentroids, by= intersect(names(countyNames), names(countyCentroids)), by.countyNames = by, by.CountyCentroids = by)
# Create Names For Label Data
names(countyNameLables) <- c("PID", "label", "X","Y")
# Adding Labels
addLabels(as.PolyData(countyNameLables), placement="DATA",  polys=paPolys, col=1)

#####################################
#    Getting Median Income Data     #
#####################################
# taking from census data with data dictionary item code "INC110212" representing the Median household income between 2008-2012 
library(dplyr)
# Reading Lines Out of Fips The Max Width Of The Lines Is 43
FIPS <- readLines("FIPS_CountyName.txt")
# Figure Out The Max Width of Lines
maxWidth <- max(nchar(FIPS))
# Load Data Into Data Frame
FIPS <- read.fwf("FIPS_CountyName.txt", width = c(5, (maxWidth - 5)), strip.white = TRUE)
# Assign Names
names(FIPS) <- c("fips", "CountyState")
# Split off County And States, Seperating By ",""
FIPSCsplit <- strsplit(as.character(FIPS$CountyState), ',')
# Clean Up Leading White Spaces
county <- trimws(lapply(FIPSCsplit, function(x){x[1]}), "l")
state <- trimws(lapply(FIPSCsplit, function(x){x[2]}), "l")
# Create Clean FIPS Data Frame
FIPS <- data.frame(factor(FIPS$fips), county, state)
# Assign New Names To Clean FIPS Data Frame
names(FIPS) <- c("fips", "county", "state")
# Read In The Data Set
DataSet <- read.delim("DataSet.txt", sep=",", header = TRUE)
# Create Names For Subset of DataSet Used For Modeling
modelData <- data.frame(factor(DataSet$fips), DataSet$INC110212)
# Create Names For Subset of DataSet Used For Modeling
names(modelData) <- c("fips", "income")
# Perform Left Join To Match fips Identifier And Associated County From FIPS With modelData
modelData <- left_join(modelData, FIPS, by="fips")
# Re-Assign The Names To Make Things Easier Later
names(modelData) <- c("fips", "income", "county", "postal")
# Extract The Postal Codes The Model Data
postal <- unique(modelData[!is.na(modelData$postal),]$postal)
# Extract The State Names From The Model Data In The Same Order
state <- modelData[is.na(modelData$postal),]$county
# Create New Data Frame with Matching Postal Code and State Name In Each Row
namesAndAbbr <- data.frame(postal, state[2:52])
# Name The Columns
names(namesAndAbbr) <- c("postal", "state")
# Get Rid Of The NAs In The modelData Data Set
modelDataNoNA <- na.omit(modelData)
# Join modelDataNoNA With The Name Of Each State
dataModelFinal <- left_join(modelDataNoNA, namesAndAbbr, by="postal")
# dropping unnecessary columns
cleanIncomeModel <- dataModelFinal[dataModelFinal$postal=="PA", c("county", "income")]
# merging county and poly data 


# Determine County Centroids
countyCentroids <- calcCentroid (paPolys, rollup = 1)
# Get County Names with PID
countyNames <- paData[,c("PID", "")]
# Merge Coordinates With Names
countyNameLables <- merge(countyNames, countyCentroids, by= intersect(names(countyNames), names(countyCentroids)), by.countyNames = by, by.CountyCentroids = by)
# Create Names For Label Data
names(countyNameLables) <- c("PID", "label", "X","Y")
# Adding Labels
addLabels(as.PolyData(countyNameLables), placement="DATA",  polys=paPolys, col=1)














myMap<-readShapePoly(fn="us/cb_2014_us_state_20m.shp")	


myMap<-readShapePoly(fn="tl_2010_42_county10/tl_2010_42_county10.shp")	
myMapDf	<-	fortify(myMap)	
m0	<-ggplot(data=myMapDf)+xlim(-8963428.8121, -8314397.3302)+ylim(4825307.3888, 5238600.7631)
m1	<-	m0	+	geom_path(aes(x=long,	y=lat,	group=group))	
m2	<-	m1	+	geom_polygon(aes(x=long,	y=lat,	group=group,	fill=id))	





