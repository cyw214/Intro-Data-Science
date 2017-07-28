###############################################################################
###                ### Cleaning The FIPS Data Set  ###                      ###
###                ###################################                      ### 
###                                                                         ###    
###############################################################################

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


###############################################################################
###                   ### Joining FIPS And DataSet ###                      ###
###                   ################################                      ### 
###                                                                         ###    
###############################################################################

# Use The dplyr Library install.packages(dplyr) If Not Already Isntalled
library(dplyr)
# Read In The Data Set
DataSet <- read.delim("DataSet.txt", sep=",", header = TRUE)
# Create Names For Subset of DataSet Used For Modeling
modelData <- data.frame(factor(DataSet$fips), DataSet$PST045213, DataSet$EDU685212)
# Create Names For Subset of DataSet Used For Modeling
names(modelData) <- c("fips", "population", "education")
# Perform Left Join To Match fips Identifier And Associated County From FIPS With modelData
modelData <- left_join(modelData, FIPS, by="fips")
# Re-Assign The Names To Make Things Easier Later
names(modelData) <- c("fips", "population", "education", "county", "postal")

###############################################################################
###           ### Getting Proper Titles And Eliminating NA's ###            ###
###           ##################################################            ### 
###                                                                         ###    
###############################################################################

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

###############################################################################
###                        ### Modeling Data ###                            ###
###                        #####################                            ### 
###                                                                         ###    
###############################################################################


# Using Scale Library To Scale Y Axis
# Install Scales Library install.packages("Scale")
library(scales)
# Loading ggplot2
library(ggplot2)
#plotting points and using lm to create smooth line
plot <- ggplot(dataModelFinal, aes(education, population)) + geom_point() + geom_smooth(se=FALSE, method = "lm")
# Creating Breaks With log10 Scaling On Y Axis To Better Capture Low/High Populations In Counties For Linear Model
# Also Adding Respective Labels To Ticks 
# Used Example From CookBook for R to scale y: http://www.cookbook-r.com/Graphs/Axes_(ggplot2)/
plot <- plot + scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),labels = trans_format("log10", math_format(10^.x))) 
# Using facet_wrap To Create Subplot For Each State
plot <- plot + facet_wrap(~state)
# Creating Titles
plot <- plot + ggtitle("State Population v.s Post Secondary Education by County")
# Creating Labels
plot <- plot + ylab("County Population (2013)") + xlab("% College Educated County Population")
