# Purpose
# Reads observed data
# Reads simulated data
# Compare them and do statistics

# Set directory (chosse one option) !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
setwd("C:\\Users\\Ed\\Documents\\LUCI4-MaizeData\\")
getwd()

obsFilePath = "C:\\Users\\Ed\\Documents\\LUCI4-MaizeData\\obsDataMaize_DB.txt"

# 1 Read observed data

obsData = read.table(obsFilePath, 
               sep="\t", 
               header = TRUE)
# Check obs data
head(obsData)
tail(obsData)
summary(obsData)

# create a DF from the file
obsDataDB = data.frame(obsData)
head(obsDataDB)
summary(obsDataDB)

# 2 Read observed data 

# Find files and inspect files
files = list.files(getwd(),pattern='.out', full.names=FALSE) # if true gets path
info = file.info(files)
summary(info)

# Loop through all files
allData = NULL
fileCount = 0
for (i in 1:length(files)) {
    
  # skip loop if file is empty
  if (file.info(files[i])["size"] == 0) {
    
    print(paste(files[i], " is empty "))
    
    next} 
  
  thisOutFile = NULL
  
  fileCount = fileCount + 1 # real number of files
 
  print(fileCount)
  
  # Standardise the header column names in the first file
    thisHeader = read.table(files[i], skip = 2, header = TRUE,
                            comment.char = "(", 
                            blank.lines.skip = TRUE) # reads and skips the unit line     
    outHeader = colnames(thisHeader)
    
  # read data for that .out file
    thisOutFile = read.table(files[i], skip = 4, header = FALSE, blank.lines.skip = TRUE) # reads and skips the unit line
  
  # Bring the header back
  colnames(thisOutFile) = outHeader
  
  # Creates a allData DF in the first file and appends down after that
  if (fileCount == 1) { 
    allData = data.frame(thisOutFile)
  } else {
    allData = rbind(allData, thisOutFile)
  }
      
  # Creates temporary CSVs to store data
  if(i == 1 | i %% 100 == 0 | i == length(files)) {  
    print(paste(fileCount, " " , Sys.time()))
    write.csv(allData, file = paste("Out_",i,".csv", sep = ""))
    
  }
}

# Read Back all CSVs and create a big DF with all simulated data
csv.files = list.files(getwd(),pattern='.csv', full.names=FALSE) # if true gets path
all.the.data <- lapply(csv.files, read.csv, header=TRUE)
simData <- do.call("rbind", all.the.data)

# inspect simulated data
summary(simData)
head(simData)
tail(simData)
write.table(simData, file = paste0("simData.txt"))


# 3 Look up and merge simulated and observed data

# example of melt function 
library(reshape)
simDataDB = melt(simData, id=c("X","ExpNo","TreatNo","Date"))
head(simDataDB)
summary(simDataDB)
colnames(simDataDB)[ncol(simDataDB)] = "simValue" # change column name to simulated
colnames(simDataDB)[ncol(simDataDB)-1] = "Variable" # change column name to simulated
head(simDataDB)
simDataDB = simDataDB[,!names(simDataDB)== "X"]
summary(simDataDB)
summary(obsDataDB)
head(obsDataDB)



# merge with observed data
obsSimDB = NULL
obsSimDB <- merge(simDataDB,obsDataDB,by=c("ExpNo","TreatNo", "Date","Variable"))
head(obsSimDB)
summary(obsSimDB)
obsSimDB[is.na(obsSimDB)] <- 0
write.table(obsSimDB, file = paste0("obsSimDB.txt"))


plot (obsSimDB$simValue,obsSimDB$obsValue)

if(length(obsSimDB$simValue) == length(obsSimDB$obsValue)) {print("All ok!")}


exps = unique(obsSimDB$ExpNo)

treats = unique(obsSimDB$TreatNo)

vars = unique(obsSimDB$Variable)

par(mfrow=c(2,3))

for(e in 1:length(exps)){
  for(t in 1:length(treats)){
    for(v in 1:length(vars)){
    #print(paste0(exps[e], treats[t], vars[v]))
    DFsub = NULL
    DFsub = obsSimDB[which (obsSimDB$Variable == vars[v] 
                            & obsSimDB$TreatNo == treats[t] 
                            & obsSimDB$ExpNo == exps[e]),]
    
    if(length(DFsub$simValue) == 0 | length(DFsub$obsValue)==0){
      next
    } else {
      plot(DFsub$simValue,DFsub$obsValue, 
           main = paste0(as.character(vars[v]),
                         " E",exps[e]," T",treats[t]))  
     }
    }
  }
}




########### - Finish here - below only code scraps

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

