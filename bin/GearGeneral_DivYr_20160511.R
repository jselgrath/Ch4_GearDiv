# Jennifer Selgrath
# Project Seahorse, UBC
# May 11, 2016

############################################################################################
####  OBJECTIVE 1: Calc maps of richness for general gear classes

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


####################################
# Task 2.1 make use data binary: reclass places where people fish as 1 and don't fish as 0 


# reclassify the values into two groups
# all values >= 0 become 0, etc.
m <- c(0, 0, 0,  0.01, 5000, 1)
rclmat <- matrix(m, ncol=3, byrow=TRUE)

b.60<- reclassify(rs.1960,rclmat)
b.60
b.70<- reclassify(rs.1970,rclmat)
b.80<- reclassify(rs.1980,rclmat)
b.90<- reclassify(rs.1990,rclmat)
b.00<- reclassify(rs.2000,rclmat)
b.10<- reclassify(rs.2010,rclmat)

###########################
# Task 2.2 calc richness for gear classes
r.60<- calc(b.60,sum, na.omit=T)
r.60  
r.70<- calc(b.70,sum, na.omit=T)
r.80<- calc(b.80,sum, na.omit=T)
r.90<- calc(b.90,sum, na.omit=T)
r.00<- calc(b.00,sum, na.omit=T)
r.10<- calc(b.10,sum, na.omit=T)
r.10


#####################
# Task 2.3 Save files

setwd("C:/Users/Jenny/Dropbox/1PhD/R.projects/Ch4/GearDiv/results/Ch3_Rich")
writeRaster(r.60,"GearGeneral_DivYr_1960.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(r.70,"GearGeneral_DivYr_1970.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(r.80,"GearGeneral_DivYr_1980.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(r.90,"GearGeneral_DivYr_1990.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(r.00,"GearGeneral_DivYr_2000.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
writeRaster(r.10,"GearGeneral_DivYr_2010.tif", datatype="FLT4S",format="GTiff", overwrite=TRUE)
