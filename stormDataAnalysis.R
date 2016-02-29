## Set working directort
setwd("C:/Users/Lisa/version-control/RepData_PeerAssessment2")

## Load libraries
library(R.utils)
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)

## Create directory and data
if(!file.exists("./data")){
        dir.create("./data")
}

# Download file
if (!"stormData.csv.bz2" %in% dir("./data/")) {
        print ("lets download file")
        download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2", 
                      destfile="data/stormData.csv.bz2")
}

#unzip bz2 file to csv file
if(!file.exists ("./data/StormData.csv")) {
        filePath <- "./data/StormData.csv.bz2"
        destPath <- "./data/StormData.csv"
        bunzip2(filePath, destPath, overwrite=TRUE, remove=FALSE)
}


# read the source .csv file
if(!"stormData" %in% ls()) {
stormData <- read.csv("./Data/StormData.csv", header = TRUE, sep = ",")
}

# Take a look at yearly data

stormData$year <- as.numeric(format(as.Date(stormData$BGN_DATE, format = "%m/%d/%Y %H:%M:%S"), "%Y"))

# Visulization

hist(stormData$year, breaks=30)

#Reduce the datasize, only keep the necessary columes
reduced_stormData<- select(stormData, STATE, EVTYPE, FATALITIES, INJURIES, PROPDMG, 
                           PROPDMGEXP, CROPDMG, CROPDMGEXP, year)


#Consolidate the EVTTYPE

reduced_stormData$EVTCatagory <- NA
reduced_stormData[grepl("precipitation|rain|hail|drizzle|wet|percip|burst|depression|
                        fog|wall cloud", reduced_stormData$EVTYPE, ignore.case=TRUE), 
                  "EVTCatagory"] <- "Precipitation and Fog"
reduced_stormData[grepl("wind|storm|wnd|tornado|waterspout", reduced_stormData$EVTYPE, 
                        ignore.case = TRUE), "EVTCatagory"] <- "Wind & Storm"
reduced_stormData[grepl("Slide|erosion|slump", reduced_stormData$EVTYPE, ignore.case=TRUE),
                  "EVTCatagory"] <- "Landslide and erosion"
reduced_stormData[grepl("warmth|warm|heat|dry|hot|drought|thermia|temperature record|
                        record temperature|record high", reduced_stormData$EVTYPE, 
                        ignore.case = TRUE), "EVTCatagory"] <- "Heat and drought"
reduced_stormData[grepl("cold|chill|ice|Wind Chill|Frost|Freeze|Winter Weater|Winter|
                       Blizzard|Snow|Avalanche", reduced_stormData$EVTYPE, ignore.case=TRUE), 
                  "EVTCatagory"] <- "Wintery Weather"
reduced_stormData[grepl("Tide|Marine|Current|Tsunami|wave|seas|surf|typhoon|hurricane", reduced_stormData$EVTYPE, 
                       ignore.case=TRUE), "EVTCatagory"] <- "Ocean Weather"
reduced_stormData[grepl("flood|blow-out|swells|fld|dam break", reduced_stormData$EVTYPE, 
                        ignore.case=TRUE), "EVTCatagory"] <- "Flood"
reduced_stormData[grepl("dust storm|dust devil", reduced_stormData$EVTYPE, ignore.case=TRUE), 
                  "EVTCatagory"] <- "Dust"
reduced_stormData[grepl("fire|volcanic", reduced_stormData$EVTYPE, ignore.case=TRUE), 
                  "EVTCatagory"] <- "Fire and Volcano"
reduced_stormData[grepl("Thunderstorm|lightning", reduced_stormData$EVTYPE, ignore.case=TRUE), 
                  "EVTCatagory"] <- "Thunderstorm and Lightning"
reduced_stormData$EVTCatagory[is.na(reduced_stormData$EVTCatagory)] <- "Others"

#cast the cagagory as factor
reduced_stormData$EVTCatagory <- as.factor(reduced_stormData$EVTCatagory)
                              
#remove EVTYPE column
reduced_stormData$EVTYPE <- NULL

# Calculate the damage with conversion

expConvert <- function(x) {
        if(is.numeric(x)) {
                x <- x
        }
        else if (grepl("h", x, ignore.case=TRUE)) {
                x <- 2
        }
        else if (grepl("k", x, ignore.case=TRUE)) {
                x <- 3
        }
        else if (grepl("m", x, ignore.case=TRUE)) {
                x <- 6
        }
        else if (grepl("b", x, ignore.case=TRUE)) {
                x <- 9
        }
        else if(x==""||x==" ") {
                x <- 0
        }
        else{
                x <- NA
        }
        x
}


calamt <- function(num, exp) {
        pow<-expConvert(exp)
        if(is.numeric(num)){
          num <- num * (10^pow)
        }
        
        if(!is.numeric(num)) {
                num <- 0
        }
        
        num
        
}
        
# Getting the absolute value for PropDamage and CropDamage, then add them together to 
# create total damage, 3 new columns are created. 

reduced_stormData$propDamage <- mapply(calamt, reduced_stormData$PROPDMG, reduced_stormData$PROPDMGEXP)
reduced_stormData$cropDamage <- mapply(calamt, reduced_stormData$CROPDMG, reduced_stormData$CROPDMGEXP)
reduced_stormData$totalDamage <- reduced_stormData$propDamage + reduced_stormData$cropDamage

# Remove columns not needed to reduce dataframe size
reduced_stormData$PROPDMG <- NULL
reduced_stormData$PROPDMGEXP <- NULL
reduced_stormData$CROPDMG <- NULL
reduced_stormData$CROPDMGEXP <- NULL

#Furthur reduce the data set by only track the datapoints after 1990

recent_stormData<- filter(reduced_stormData, year >= 1990)

#Extream Weather impact on population health
#Aggregate the data by event
fatality <- aggregate(FATALITIES ~ EVTCatagory, data=recent_stormData, FUN=sum)
fatality <- fatality %>% arrange(desc(FATALITIES))
injuries <- aggregate(INJURIES ~ EVTCatagory, data=recent_stormData, FUN=sum)
injuries <- injuries %>% arrange(desc(INJURIES))
humanDamage <- merge(fatality, injuries, by="EVTCatagory")
humanDamage

#Fatility plot
ggplot(data=humanDamage, aes(y=FATALITIES, x=reorder(EVTCatagory, -FATALITIES))) + 
        geom_bar(stat="identity", fill="red") +
        scale_y_continuous("Number of Fatalities") +
        ggtitle("Total Fatalities by Severe Weather in US from 1990-2011") + 
        xlab("Weather Event Type") + 
        theme(axis.text.x=element_text(angle=90, hjust=1), text=element_text(size=20))

#Injuries plot                                                                  
ggplot(data=humanDamage, aes(y=INJURIES, x=reorder(EVTCatagory,-INJURIES))) + 
        geom_bar(stat="identity", fill="orange") + 
        scale_y_continuous("Number of Injuries") +
        ggtitle("Total Injuries by Severe Weather in US from 1990-2011") +
        xlab("Weather Event Type") + 
        theme(axis.text.x=element_text(angle=90, hjust=1), text=element_text(size=20))

#Extream Weather impact on economy
# Aggregate data for total damage
EconDamage <- aggregate(totalDamage~EVTCatagory, data=recent_stormData, FUN=sum)
EconDamage <- EconDamage %>% arrange(desc(totalDamage))
EconDamage

#Plot 
ggplot(data=EconDamage, aes(x=reorder(EVTCatagory, -totalDamage), y=(totalDamage/10^9))) + 
        geom_bar(stat="identity", fill="seagreen4") + 
        scale_y_continuous("Total Economic Damage in US Dollars (billion)") + 
        xlab("Severe Weater Event Type") + 
        ggtitle("Total Economic Damage by Severe Weather in US from 1990-2011")+
        theme(axis.text.x=element_text(angle=90, hjust=1), text=element_text(size=10))
