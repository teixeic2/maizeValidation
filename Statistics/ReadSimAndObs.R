# Purpose
# Reads observed data
# Reads simulated data
# Compare them and do statistics

# Set directory (chosse one option) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
setwd("C:\\Apsim_dev\\Projects\\CCII\\outFiles\\SowByGenotype\\")

# Region by Pixel table



regionFile = read.csv("C:\\Apsim_dev\\Projects\\CCII\\GIS_layers\\NZ_Regions_GIS\\RegionPerPixel.csv", header=TRUE)

head(regionFile)



# Gets file list

files = list.files(getwd(),pattern='.out', full.names=FALSE) # if true gets path

#Append files into one dataframe



info = file.info(files)

summary(info)



for (i in 1:length(files)) {
  
  
  
  if(i<10) {
    
    thisHeader = read.table(files[i], skip = 2, header = TRUE, comment.char = "(", blank.lines.skip = TRUE) # reads and skips the unit line   
    
  }
  
  
  
  if (file.info(files[i])["size"] == 0) {
    
    print(paste(files[i], " is empty "))
    
    next} 
  
  
  
  splitName = unlist(strsplit(files[i],"[_,.]"))
  
  
  
  r = as.numeric(splitName[1]) # FIXME: see if first number is indeed row
  
  c = as.numeric(splitName[2])
  
  
  
  # read data for that .out file
  
  thisOutFile = read.table(files[i], skip = 4, header = FALSE, blank.lines.skip = TRUE) # reads and skips the unit line
  
  
  
  # From the netCDF metadata (FIXME: should get this from printed field in .out file to minimise risks)
  
  # !!!!Atention!!!!! Lat/Long "cannot" be rounded otherwise it creates a mismatch of pixels during rasterisation
  
  lat = -34.375-0.050*(r-1)
  
  long = 166.425+0.050*(c-1)
  
  # FIXME: this is still not ideal as V3 and V4 depend on the arrangment in output, need to address the name and the rounding is not perfect
  
  #  if (lat!= min(thisOutFile$V3) | long != min(thisOutFile$V4)) {print(paste(c("Error: Latitude and/or longitude do not match in row/col: ", r, c)))}
  
  
  
  #colnames(thisOutFile)
  
  #  l = length(thisOutFile$Date)
  
  l = length(thisOutFile$V1) # fills the variable vector with a no of lines needed
  
  fileNo = rep(i, l)
  
  thisRow =  rep(r, l)
  
  thisCol =  rep(c, l)
  
  thisLat = rep(lat, l)
  
  thisLong = rep(long, l)
  
  #thisSowDate = rep(as.numeric(splitName[3]), l)
  
  # thisHybrid = rep(as.numeric(splitName[4]), l)
  
  thisHybrid = rep(splitName[3], l)
  
  thisSowDate = rep(splitName[4], l)
  
  
  
  # Get the region for this file FIXME: some pixels are not in the region file (?) so -9999
  
  region = NULL
  
  region = as.numeric(regionFile$region[regionFile$row == r & regionFile$col == c])
  
  if(length(region)==0) region = -9999
  
  thisRegion = rep(paste0("R",region), l)
  
  
  
  thisOutFile = data.frame(fileNo, thisRow, thisCol, thisLat, thisLong, thisRegion, thisHybrid, thisSowDate, thisOutFile)
  
  
  
  allData2 = rbind(allData2, thisOutFile)
  
  
  
  if(i == 1 | i %% 1000 == 0 | i == length(files)) {
    
    print(paste(fileNo[1], " " , Sys.time()))
    
    write.csv(allData2, file = paste("Out_",i,".csv", sep = ""))
    
    allData2 = NULL
    
  }
  
  
  
}



firstCols = colnames(thisOutFile) # terieves first columns

#colnames(allData2) = c(firstCols[1:5], colnames(thisHeader))



head(allData2)



summary(allData2)



# Read csvs

csv.files = list.files(getwd(),pattern='.csv', full.names=FALSE) # if true gets path

all.the.data <- lapply(csv.files, read.csv, header=TRUE)



DATA <- do.call("rbind", all.the.data)

summary(DATA)



colnames(DATA) = c("skip",firstCols[1:8], colnames(thisHeader))

head(DATA)



save(DATA,file="DATA.Rda")

summary(DATA)

head(DATA)





# Visualise

DATA_sub = DATA[ which(DATA$CurrentCrop!='wheat_exceed'),]

par(mfrow=c(2,1))

boxplot(DATA_sub$TotalBiomass~DATA_sub$thisSowDate*DATA_sub$thisRegion)

boxplot(DATA_sub$TotalBiomass~DATA_sub$thisHybrid*DATA_sub$thisRegion)



par(mfrow=c(1,1))

boxplot(DATA_sub$TotalBiomass~DATA_sub$thisRegion)



sp <- ggplot(DATA_sub, aes(x=thisSowDate, y=TotalBiomass)) + geom_boxplot(width=0.5, fill = "green") # geom_boxplot(width=0.2,fill = "grey")  + geom_violin(alpha = 0.2, fill = "green") # geom_point(shape=1)

sp

# Divide by levels of "sex", in the vertical direction

sp + facet_grid(thisRegion ~ .)



# Divide by levels of "sex", in the horizontal direction

sp + facet_grid(. ~ thisHybrid)



# Divide with "sex" vertical, "day" horizontal

sp + facet_grid(thisRegion ~ thisHybrid)



# Divide with "sex" vertical, "day" horizontal

sp + facet_grid(thisHybrid  ~ thisRegion)



# Risk index

x=c(1,2,3,4,5,6)

riskFunc = function(x) {
  
  q =  quantile(x, c(.25, .50, .75))
  
  r = (q[3]-q[1])/q[2] * 100
  
  return (round(r, digits = 1))
  
}



stDevData <-aggregate(DATA_sub$TotalBiomass, by=list(DATA_sub$thisRegion,DATA_sub$thisHybrid,DATA_sub$thisSowDate), FUN=sd, na.rm=TRUE)

medData <-aggregate(DATA_sub$TotalBiomass, by=list(DATA_sub$thisRegion,DATA_sub$thisHybrid,DATA_sub$thisSowDate), FUN=mean, na.rm=TRUE)



riskData = data.frame(reg = medData$Group.1, hyb =medData$Group.2, sow = medData$Group.3, value = stDevData$x/medData$x)

colnames(stDevData) = colnames(medData) = colnames(riskData)





sp <- ggplot(riskData, aes(x=hyb, y=value)) +  geom_point(width=0.5, fill = "green") # geom_boxplot(width=0.2,fill = "grey")  + geom_violin(alpha = 0.2, fill = "green") # geom_point(shape=1)

sp + facet_grid(reg ~ sow)



test = merge(riskData, medData, by = c("reg", "hyb", "sow"))

plot(test$value.x ~ test$value.y)

sp <- ggplot(test, aes(x=value.y, y=value.x)) +  geom_point(aes(colour = sow, shape = hyb, size = 3)) # + geom_text(aes(label=hyb),hjust=0, vjust=0)# geom_boxplot(width=0.2,fill = "grey")  + geom_violin(alpha = 0.2, fill = "green") # geom_point(shape=1)

sp + facet_grid(~reg)

surf3D(x = test$value.y,
       
       y = test$value.x,
       
       z = test$sow
       
)



library(plot3D)



sp <- ggplot(DATA_sub, aes(x=thisHybrid, y=HarvestIndex)) + geom_boxplot(width=0.5, fill = "green") # geom_boxplot(width=0.2,fill = "grey")  + geom_violin(alpha = 0.2, fill = "green") # geom_point(shape=1)

sp + facet_grid(thisRegion ~ thisSowDate)



# idea: stdev among hybrids gives an index of hybrid importance

stDevData <-aggregate(DATA_sub$IntRadSum, by=list(DATA_sub$thisRegion,DATA_sub$thisSowDate), FUN=sd, na.rm=TRUE)

medData <-aggregate(DATA_sub$IntRadSum, by=list(DATA_sub$thisRegion,DATA_sub$thisSowDate), FUN=mean, na.rm=TRUE)



riskData = data.frame(reg = medData$Group.1, sow =medData$Group.2, value = stDevData$x/medData$x)

colnames(stDevData) = colnames(medData) = colnames(riskData)





sp <- ggplot(riskData, aes(x=sow, y=value)) +  geom_point(width=0.5, fill = "green") # geom_boxplot(width=0.2,fill = "grey")  + geom_violin(alpha = 0.2, fill = "green") # geom_point(shape=1)

sp + facet_grid(~ reg)



# MJ/oCd is reduced as it warms so the phenology diffence needs to increase to make a difference

# HI is more affected by hyrid choice as you sow late

# Reads .out files from apsim (based on MapOutApsimFiles.R)

# Creates a dataframe with column row

# Transforms it into a raster

# displays the map



library(data.table)

library(plyr)

library(sp)

library(raster)



library (vioplot)



library(reshape)



library(ggplot2)



#Not sure which libraries are needed

library(rasterVis)

library(colorspace)

library(rgdal)



pixelSummary = NULL

allData2 = NULL

thisOutFile = NULL

