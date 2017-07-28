# Load Libraries
library(PBSmapping)
library(dplyr)
library(ggplot2)
library(maptools)
library(ggmap)
library(stringr)
library(dplyr)
library(plyr)

#####################################
#    Getting Median Income Data     #
#####################################

FIPS <- readLines("FIPS_CountyName.txt")
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
CensusSet <- read.delim("DataSet.txt", sep=",", header = TRUE)
# Create Names For Subset of DataSet Used For Modeling
CensusIncomeData <- data.frame(factor(CensusSet$fips), CensusSet$INC110212)
# Create Names For Subset of DataSet Used For Modeling
names(CensusIncomeData) <- c("fips", "income")
# Perform Left Join To Match fips Identifier And Associated County From FIPS With modelData
CensusDataJoined <- left_join(CensusIncomeData, FIPS, by="fips")
# Get Rid Of The NAs In The modelData Data Set
CensusDataJoinedNoNa <- na.omit(CensusDataJoined)
# Extract Pa counties and median income
paIncomeData <- CensusDataJoinedNoNa[CensusDataJoinedNoNa$state == "PA", c("county", "income")]
# drop the term county from the county strings
paIncomeData$county <- lapply(paIncomeData$county, function(x) str_replace(x, " County", ""))
# make Counties toLower
paIncomeData$county <- sapply(paIncomeData$county, tolower)
# rename
names(paIncomeData) <- c("subregion","income")


# Load the raster
paRaster <- get_map(location= 'pennsylvania', zoom = 7)
# Read In US Shape File
states <- map_data("state")
# Read In US Counties Shape File
counties <- map_data("county")
# Make Data Frame For PA
paDF <- subset(states, region == "pennsylvania")
# Get PA Counties
paCounty <- subset(counties, region == "pennsylvania")
# Join With PA Data
paIncome <- inner_join(paCounty, paIncomeData, by ="subregion")
# Calculate Centroids to place County Titles
paCountyLabels <- ddply(paIncome, .(subregion), summarize, lat = mean(lat), long = mean(long))
# Plot Raster
paMap0 <- ggmap(paRaster) + xlim(-80.6, -74.5)+ylim(39.5,42.5)
# Plot State
paMap1 <- paMap0 +  coord_fixed(1.3) + geom_polygon(data= paDF,aes(x = long, y = lat, group = group), color = "black", fill = NA) 
# Plot Counties With Income Gradient
paMap2 <- paMap1 + geom_polygon(data = paIncome, aes(x = long, y = lat, group = group, fill=income), color="black", alpha=.5)
# Make Gradient More Colurful and Add Titles with Geom Text
paMap3 <- paMap2 + scale_fill_gradientn(colours = rev(rainbow(2))) + geom_text(data = paCountyLabels, aes(x = long, y = lat, label=subregion, size=.01),show.legend=FALSE) + ggtitle("PA Median Income")


