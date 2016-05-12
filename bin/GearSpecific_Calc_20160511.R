# Jennifer Selgrath Rauf
# Project Seahorse, UBC Institute for the Oceans and Fisheries
# May 11, 2016

# Goal: Estimate the mean number of days spent fishing IN EACH FISHING GROUND in 10 year intervals: 1960 - 2010
# This file: load, clean, and calc # gears each fisher used in a FG
#################################

library (plyr); library(dplyr);library(reshape2); library(magrittr); library (lazyeval) 
# load plyr before dplyr

remove(list=ls())

###########################
#LoadData
########################
dateToday<- "_20160511"
setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/GearDiv/data/")

FGfishing1<- read.csv(file = "FGdaysYear_FA_20160324.csv", header = T, stringsAsFactors = F, strip.white = TRUE, na.strings = c("NA","","na"))

FGgear1<- read.csv(file = "FGgeardaysYear_FA_20160324.csv", header = T, stringsAsFactors = F, strip.white = TRUE, na.strings = c("NA","","na"))

# Select years and FG fished in
FGfishing2<-filter(FGfishing1, year == 1960| year ==1970 | year == 1980|year == 1990| year == 2000|year == 2010)
FGgear2<-filter(FGgear1, (year == 1960| year ==1970 | year == 1980|year == 1990| year == 2000|year == 2010) & FGfishingb==1)

head(FGfishing2)
head(FGgear2)

################
# Time calculations
################

FGgear2$FishingGear<-as.factor(FGgear2$FishingGear); levels(FGgear2$FishingGear)
FGgear2$Eng.Name5<-as.factor(FGgear2$Eng.Name5); levels(FGgear2$Eng.Name5)

# #####################
# # clean illegal categories
# #####################
# # ASSUMPTIONS
# 
# # illegal location > all locations too close to land so make all illegal (checked in GIS)
# # assume used lights and illegal in 1998
# # assume boat mechanized gears are legal since can't tell if used illegally or not
# 
levels(as.factor(FGgear2$YrIllegal))
FGgear2$YrIllegal[FGgear2$YrIllegal=="legal"]<-"Legal" # fix different text
FGgear2$YrIllegal[FGgear2$YrIllegal=="boat - legal/1998"]<-"Legal"
FGgear2$YrIllegal[FGgear2$YrIllegal=="light - legal/1998"]<-"1998"
FGgear2$YrIllegal[FGgear2$YrIllegal=="location - legal/1998; lights"]<-"1998"
FGgear2$YrIllegal[FGgear2$YrIllegal=="location - legal/1998"]<-"1998"
FGgear2$YrIllegal[FGgear2$YrIllegal=="1986 (mesh size)"]<-"1986"


FGgear2b<-filter(FGgear2,YrIllegal != "NotRelevant")
levels(as.factor(FGgear2b$YrIllegal))

############
# cut unneeded data
names(FGgear2b)

# only years where gear was used in FG
FGgear2_No0<-filter(FGgear2b,fishingb>0)

head(FGgear2_No0)

levels(as.factor(FGgear2_No0$FGfishingb))

# 1 - notes on changes below
filter(FGgear2_No0,fishingb==0)

# 2 - notes on changes below
filter(FGgear2_No0, FGgearb  ==1 & gearb  == 0)

#1 errors (said used gear in FG, but not FG) in CBBCT01, CBBTC02, CBBGS01, NSBTC02, TMBTC08, CBBGS04,TMBTC08. 21CBBGS09 - had too short years. Fixed in wide FG tables

#2 effors - confirm these if possible from datasheets: BGGS04, BGGS06 (3,4) ,  BRBTC12 - changed FG1,2 to G2 based on years; CBBTC01; CBBTC02; CBGS08; CBBBTC10 - changed G3( lantern) to G2 (spear, day);CBB-BTC-08 (fixed years); CL-GS-10 mixed up G1 and G3; CLGS11 - kaykay for too many years; ls-btc-02 - removed G1 from FG2 - wrong years; PNBTC02 fixed G2dates; PN-BTC-05 fixed gear dates; pnbtc08 - fixed years for gear; TMBTC18 - FG11 switched G2 to G1; TMGS19 - fixed years for G2; TS-BTC-07 - did fish in 1990; CBB-BTC-10 2&3 to G5;

# Check for places where FGdaysYr=0, but gears are used there
filter(FGgear2_No0, FGdaysYr  ==0 & FGgearb  ==1)

#subset so only rows where gears are used in FG
FGgear3<-filter(FGgear2_No0, FGgearb  == 1) #no 0s here
names(FGgear3)
str(FGgear3)

###############################
# Select rows for gears only
FGgear4<-select(FGgear3,year,FisherID2,FisherID,FGID3,FGdaysYr,FishingGear,ID1)
head(FGgear4)

# get IDs for gears used in Focal Area
IDs<-data.frame(sort(unique(FGgear4$ID1)))
names(IDs) = "gearIDs"
length(IDs$gearIDs) #71

##########
# Save files
write.table(FGgear4,file=paste("gearDiv",dateToday,".csv",sep=""), col.names=TRUE, row.names=FALSE, sep=",")
write.table(IDs,    file=paste("gearIDs",dateToday,".csv", sep=""), col.names=TRUE, row.names=FALSE, sep=",")

