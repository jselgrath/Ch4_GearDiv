# Jennifer Selgrath
# Project Seahorse, UBC
# May 11, 2016

############################################################################################
####  OBJECTIVE 1: Simpson's Index of Diversity. Estimate SI values for different years, with days fished as the # sp. -- so the question then is how likely are you to be targeted by a different fishing gear?

library("ggplot2")
library('raster')
library('plyr')
library('rgdal')

################################################## 
### Task 1.1: Pull out .tif data only 
#             must be in floating format!!

rm(list=ls())
loc<-"C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/GearDiv/data/Ch3_DayYr_2cln_g5n_float_20160511/" 
setwd(loc) 

# list of files 
# be sure they are "float" format in ArcGIS
file_list1 = dir(loc)
file_list1

# list of just the TIFF files for all years
raster.list1= file_list1[  -c(grep('.xml', file_list1),grep('.tfw', file_list1),grep('.png', file_list1), grep('.dbf', file_list1),grep('.ovr', file_list1),grep('other', file_list1),grep('.vat', file_list1))  ]
raster.list1

# lists by year
rl.1960=raster.list1[grep('1960',raster.list1)]
rl.1970=raster.list1[grep('1970',raster.list1)]
rl.1980=raster.list1[grep('1980',raster.list1)]
rl.1990=raster.list1[grep('1990',raster.list1)]
rl.2000=raster.list1[grep('2000',raster.list1)]
rl.2010=raster.list1[grep('2010',raster.list1)]

##################################
# Task 1.2: stack the rasters by year

rs.1960 = stack(rl.1960); 
names(rs.1960)
dataType(rs.1960)

rs.1970 = stack(rl.1970)
rs.1980 = stack(rl.1980)
rs.1990 = stack(rl.1990)
rs.2000 = stack(rl.2000)
rs.2010 = stack(rl.2010)

#########################################
# Task 2.1 Calculate N
calc.N <- function (Rstack)
{
  N = Rstack[[1]]+Rstack[[2]]+Rstack[[3]]+Rstack[[4]]+Rstack[[5]]+Rstack[[6]]+Rstack[[7]]+Rstack[[8]]
  return(N)
}

N.60<- calc.N(Rstack=rs.1960)
dataType(N.60)
N.70<- calc.N(Rstack=rs.1970)
N.80<- calc.N(Rstack=rs.1980)
N.90<- calc.N(Rstack=rs.1990)
N.00<- calc.N(Rstack=rs.2000)
N.10<- calc.N(Rstack=rs.2010)

setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/GearDiv/results/Ch3_SI")
writeRaster(N.60,"N.1960.tif", datatype="FLT4S", format="GTiff", overwrite=TRUE)
writeRaster(N.70,"N.1970.tif", datatype="FLT4S", format="GTiff", overwrite=TRUE)
writeRaster(N.80,"N.1980.tif", datatype="FLT4S", format="GTiff", overwrite=TRUE)
writeRaster(N.90,"N.1990.tif", datatype="FLT4S", format="GTiff", overwrite=TRUE)
writeRaster(N.00,"N.2000.tif", datatype="FLT4S", format="GTiff", overwrite=TRUE)
writeRaster(N.10,"N.2010.tif", datatype="FLT4S", format="GTiff", overwrite=TRUE)

setwd(loc) 

###############
# Task 2.2 Calc Simp Div

calc.SI<-function(Rstack,N) #Rstack = stack of n values for each gear categeory
{
  rs2<-(Rstack/N) # n/N # returns raster stack
  rs3<-calc(rs2, fun=function(x) x^2, forceapply=T)  #n/N^2 # returns raster stack
  SI <-calc(rs3, sum, na.omit=T)
  # out=list(rs2,rs3,SI)
  return(SI)
}


SI.60<- calc.SI(Rstack=rs.1960,N=N.60)
SI.60
dataType(SI.60)

SI.70<- calc.SI(Rstack=rs.1970,N=N.70)
SI.80<- calc.SI(Rstack=rs.1980,N=N.80)
SI.90<- calc.SI(Rstack=rs.1990,N=N.90)
SI.00<- calc.SI(Rstack=rs.2000,N=N.00)
SI.10<- calc.SI(Rstack=rs.2010,N=N.10)
SI.10[[1]]

#####################
# Task 2.3 Save files

setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/GearDiv/results/Ch3_SI")
writeRaster(SI.60,"SI.1960.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(SI.70,"SI.1970.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(SI.80,"SI.1980.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(SI.90,"SI.1990.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(SI.00,"SI.2000.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(SI.10,"SI.2010.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
