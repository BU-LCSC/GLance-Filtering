######################################################
# GLanCE Training Data Analysis and Filtering Script #
######################################################

###################  PART 1  ######################### 
# Ingest data, screen some basic problematic cases   #
# Set up data for more involved filtering            #
######################################################

# First step in training data filtering.
# Ingest data, generate some basic plots;
# Screen out some basic problematic cases;
# Set up data for more involved filtering.

# Set up working dir
setwd("~/Dropbox (BOSTON UNIVERSITY)/Main/Rwork/GLanCETraining")

# load required libraries
library(randomForest)
library(ranger)

# Source script with function definitions
source('Rscripts_Operational/0.GlanceFunctionDefs.R')

# Read training data
glancedat.na <- read.csv('data/NA_Training_Master_V1_2021_11_23_predictors_final.csv',header=TRUE)[,-c(1,2)]
glancedat.af <- read.csv('data/Af_Training_Master_V1_2022_11_14_predictors_filtered.csv', header=TRUE)[,-1]
af.latlon <- get_lat_lon(glancedat.af[,'geo'])

# extract and add lat/lon to AF data set
colnames(af.latlon) <- c('lat','lon')
glancedat.af <- cbind(glancedat.af,af.latlon)

# compare colnames in AF vs NA data set
indx=colnames(glancedat.af) %in% colnames(glancedat.na)
colnames(glancedat.af)[!indx]

# Select continent
glancedat <- glancedat.af      # NA = North America, AF = Africa.....

# Extract bands and features
blue <- grep("BLUE_*", colnames(glancedat))
green <- grep("GREEN_*", colnames(glancedat))
red <- grep("RED_*", colnames(glancedat))
nir <- grep("NIR_*", colnames(glancedat))
swir1 <- grep("SWIR1_*", colnames(glancedat))
swir2 <- grep("SWIR2_*", colnames(glancedat))
lst <- grep("TEMP_.", colnames(glancedat))
topo <- c(grep("ASPECT", colnames(glancedat)), grep("DEM_SLOPE", colnames(glancedat)), 
          grep("ELEVATION", colnames(glancedat)),
          grep("MIN_LSZA", colnames(glancedat)), grep("MAX_LSZA", colnames(glancedat)))
years <- grep("*Year*", colnames(glancedat))
lc.class <- grep("*Class*", colnames(glancedat))
climate <- c(grep("*Temp", colnames(glancedat)), grep("precip", colnames(glancedat)), grep("waterDef", colnames(glancedat)))
Lat.Lon <- c(grep("lat", colnames(glancedat)), grep("lon", colnames(glancedat)))
aux_vars <- c(grep("DEVELOPED", colnames(glancedat)), grep("WATER_OCCURRENCE", colnames(glancedat)),
              grep("recentMag", colnames(glancedat)))

# assign row and col names to lat and lon data
lat.lon <- glancedat[,Lat.Lon]
rownames(lat.lon) <- rownames(glancedat)
colnames(lat.lon) <- c('Lat','Lon')

# Convert LC character string classes to integers for convenience
# LC1: 1=Bare; 2=Developed; 3=Forest; 4=Herbaceous; 
#      5=Shrub; 6=Snow/Ice; 7 = unfilled; 8 = Water
lc1.f <- factor(glancedat[,'LC_Class'])
table(lc1.f)  
lc1 <- as.integer(factor(glancedat[,'LC_Class']))

# extract surface reflectance data 
sr.data <- glancedat[,c(blue,green,red,nir,swir1,swir2)]

# identify and get rid of cases with NA's in sr features
sr.data <- na.omit(sr.data)

# and remove these cases from glancedat
glancedat <- glancedat[rownames(sr.data),]

# compute reflectances and NDVI from CCDC coefs 
blue.sr <- t(apply(glancedat[,blue],1,doPixSr))
green.sr <- t(apply(glancedat[,green],1,doPixSr))
red.sr <- t(apply(glancedat[,red],1,doPixSr))
nir.sr <- t(apply(glancedat[,nir],1,doPixSr))
swir1.sr <- t(apply(glancedat[,swir1],1,doPixSr))
swir2.sr <- t(apply(glancedat[,swir2],1,doPixSr))
lst.K <- t(apply(glancedat[,lst],1,doPixSr))*100
ndvi <- (nir.sr-red.sr)/(nir.sr+red.sr)

# filter cases where fitted SR data are <0 or >1, suggesting bad ccdc results
blue.oor <- getOor(blue.sr)
green.oor <- getOor(green.sr)
red.oor <- getOor(red.sr)
nir.oor <- getOor(nir.sr)
swir1.oor <- getOor(swir1.sr)
swir2.oor <- getOor(swir2.sr)

# put results all together in a single data frame, flag rows with no OOR values
oor.df <- data.frame(blue.oor,green.oor,red.oor,nir.oor,swir1.oor,swir2.oor)
n.oor.bypix <- rowSums(oor.df)
good.pix <- n.oor.bypix==0

# remove oor cases from glancedat and surface reflectances 
# (~472 cases in North America)
glancedat <- glancedat[good.pix,]

