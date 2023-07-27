###############################################
# R procedures
# Author: Tomonori Nagano <tnagano@lagcc.cuny.edu>
# Date: Wednesday, November 11, 2020 (Updated on Wednesday, July 26, 2023)
# Script purpose: This R script will analyze the IR data about languages spoken at LaGuardia Community College
###############################################
# clear the cache
rm(list = ls())

# change the default width
width.default <- getOption("width"); options(width=120)
# set the current workd directly
setwd("~/Dropbox/Documents/USB/600Research/631ResearchLanguageAccessNYC/LaGuardiaInstitutionalResearchLanguagesSpokenAtLaGuardia/")

library(ggplot2); library(xtable); library(gdata); library(Hmisc); library(RColorBrewer); library(foreign); 
library(reshape2);library(plyr)

temp <- read.csv("LanguagesSpokenAtLaGuardiaDataFall2016.csv")
tempData <- cbind(temp,"Fall2016")
names(tempData)[3] <- "Semester"

temp <- read.csv("LanguagesSpokenAtLaGuardiaDataFall2017.csv")
tempData2 <- cbind(temp,"Fall2017")
names(tempData2)[3] <- "Semester"
tempData <- rbind.fill(tempData,tempData2)

temp <- read.csv("LanguagesSpokenAtLaGuardiaDataFall2018.csv")
tempData2 <- cbind(temp,"Fall2018")
names(tempData2)[3] <- "Semester"
tempData <- rbind.fill(tempData,tempData2)

temp <- read.csv("LanguagesSpokenAtLaGuardiaDataFall2019.csv")
tempData2 <- cbind(temp,"Fall2019")
names(tempData2)[3] <- "Semester"
tempData <- rbind.fill(tempData,tempData2)

temp <- read.csv("LanguagesSpokenAtLaGuardiaDataFall2020.csv")
tempData2 <- cbind(temp,"Fall2020")
names(tempData2)[3] <- "Semester"
tempData <- rbind.fill(tempData,tempData2)

temp <- read.csv("LanguagesSpokenAtLaGuardiaDataFall2021.csv")
tempData2 <- cbind(temp,"Fall2021")
names(tempData2)[3] <- "Semester"
tempData <- rbind.fill(tempData,tempData2)

temp <- read.csv("LanguagesSpokenAtLaGuardiaDataFall2022.csv")
tempData2 <- cbind(temp,"Fall2022")
names(tempData2)[3] <- "Semester"
tempData <- rbind.fill(tempData,tempData2)

thisData <- drop.levels(as.data.frame(tempData),reorder=FALSE)
#rm(temp); rm(tempData); rm(tempData2)
names(thisData) <- c("Languages","Speakers","Semester")
thisData[,thisData$Languages==""]
thisData[thisData$Languages=="","Languages"] <- "No response"
thisData.noNA <- drop.levels(thisData[thisData$Languages!="No response",], reorder=FALSE)
summary(thisData.noNA)

thisTable <- xtabs(Speakers ~ Languages + Semester, data=thisData)
thisTable

thisTable.noNA <- xtabs(Speakers ~ Languages + Semester, data=thisData.noNA)
thisTable.noNA

options(scipen = 15)
getOption("scipen")
thisTable.sorted <- thisTable[order(-thisTable[,1]),]
thisTable.prop <- prop.table(thisTable.sorted,2)*100
thisTable.prop

thisTable.noNA_sorted <- thisTable.noNA[order(-thisTable.noNA[,1]),]
thisTable.noNA_prop <- prop.table(thisTable.noNA_sorted,2)*100
thisTable.noNA_prop

write.table(thisTable.sorted,"LanguagesSpokenAtLaGuardiaAllDataSummary.txt",sep="\t")
write.csv(thisTable.sorted, file= "LanguagesSpokenAtLaGuardiaAllDataTable.csv", row.names = TRUE)
write.csv(thisTable.prop, file= "LanguagesSpokenAtLaGuardiaAllDataTableProp.csv", row.names = TRUE)
write.csv(thisTable.noNA_sorted, file= "LanguagesSpokenAtLaGuardiaAllDataNoNATable.csv", row.names = TRUE)
write.csv(thisTable.noNA_prop, file= "LanguagesSpokenAtLaGuardiaAllDataNoNATableProp.csv", row.names = TRUE)


# the numbers of languages in each year
count(thisTable.noNA[,1] > 0)
count(thisTable.noNA[,2] > 0)
count(thisTable.noNA[,3] > 0)
count(thisTable.noNA[,4] > 0)
count(thisTable.noNA[,5] > 0)
count(thisTable.noNA[,6] > 0)
count(thisTable.noNA[,7] > 0)