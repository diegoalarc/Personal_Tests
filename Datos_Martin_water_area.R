
###useful###
library(RStoolbox)
library(raster)
library(rgdal)
library(sp)
library(tidyverse)
library(shiny)


setwd("c:/")
dir.create("Data")

setwd("c:/Data/")
#dir.create("NDWI")
#dir.create("Binary")
dir.create("Permanent_Water")
dir.create("Seasonal_Water")
dir.create("Total_Water")
dir.create("Zona_Study")
dir.create("Data_Bruto")

tempdl <- "c:/Data/Chile_all.zip"
setwd("c:/Data/Data_Bruto/")

fileURL <-
  "https://storage.googleapis.com/global-surface-water-stats/zips/Chile_all.zip"   
if (!file.exists(tempdl)) {
  download.file(fileURL ,tempdl, mode="wb") 
  unzip(tempdl,exdir = ".",overwrite = TRUE)
  } else {
unzip(tempdl,exdir = ".",overwrite = TRUE)
}
#identify the folders
fromFolder <- "c:/Data/Data_Bruto/"
toFolder <- "c:/Data/Zona_Study/"

# find the list of files to copy
list.of.files <- list.files(fromFolder, pattern=c("^Chile_classes_(.*)_0000000000-0000128000.tif$"))

# copy the files to the toFolder  - THIS DOES NOT WORK WHILE EVERYTHING PRIOR HAS WORKED

file.copy(file.path(fromFolder,list.of.files), toFolder, overwrite=TRUE)

##create the path where are all the images Landsat8 we will use.
Water_IMAGE_path <- "C:/Data/Zona_Study/"
##Load all the images Landsat8 in one list.
Water_all_IMAGE <- list.files(Water_IMAGE_path,
                        full.names = TRUE,
                        pattern = ".tif$")
##Create a List of Raster Files
temporal <- list()

for (i in 1:length(Water_all_IMAGE)){ 
  water_aculeo_raster <- raster(Water_all_IMAGE[i])
  temporal <- append(temporal,water_aculeo_raster)
}

x_coord <- c(-70.94622,  -70.87834)
y_coord <- c(-33.87002, -33.82135)
xym <- cbind(x_coord, y_coord)
p = Polygon(xym)
ps = Polygons(list(p),1)
sps = SpatialPolygons(list(ps))
proj4string(sps) = CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
aculeo_extent <- sps

##Create a List of the ROI.
temporal2 <- list()
crop_list <- list()

for (i in 1:length(temporal)){
  temporal2 <- crop(temporal[[i]],aculeo_extent)
  crop_list <- append(crop_list,temporal2)
}

temporal3 <- vector(mode="character")
names_file <- vector(mode="character")

for (i in 1:length(crop_list)){
  temporal3 <- names(crop_list[[i]])
  names_file <- append(names_file,temporal3)
}

brick_list <- list()
temporal4 <- list()

for (i in 1:length(crop_list)){
  temporal4 <- brick(crop_list[[i]])
  brick_list <- append(brick_list, temporal4)
}

t_Seasonal <- list()
t_Permanent <- list()
t_water <- list()
water <- list()

for (i in 1:length(brick_list)){
  Water <- brick(brick_list[i])  
  ####Water classfy####
  water_Seasonal <- reclassify(Water, c(0, 1, NA, 1, 2, 1, 2, 3, NA))
  Water_Permanent <- reclassify(Water, c(0, 2, NA, 2, 3, 1))
  total_water <- reclassify(Water, c(0, 1, NA, 1, 3, 1))
  t_Seasonal <- append(t_Seasonal,water_Seasonal)
  t_Permanent <- append(t_Permanent,Water_Permanent)
  t_water <- append(t_water,total_water)
}

temporal5 <- list()

setwd("C:/Data/Seasonal_Water/")

for (i in 1:length(t_Seasonal)){
  #Extract year
  Country <- substr(names_file[i], start=1, stop=6)
    #Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_Seasonal[[i]], filename=paste0(Country,"_Seasonal_",yr), format='GTiff', overwrite=T) 
  temporal5 <- append(temporal5,s_list)
  rm(Country,yr)
}

temporal6 <- list()

setwd("C:/Data/Permanent_Water/")

for (i in 1:length(t_Permanent)){
  #Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_Permanent[[i]], filename=paste0(Country,"_Permanent_",yr), format='GTiff', overwrite=T) 
  temporal6 <- append(temporal6,s_list)
  rm(Country,yr)
}


temporal7 <- list()

setwd("C:/Data/Total_Water/")

for (i in 1:length(t_water)){
  #Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_water[[i]], filename=paste0(Country,yr), format='GTiff', overwrite=T) 
  temporal7 <- append(temporal7,s_list)
  rm(Country,yr)
}

#####cambiar NDWI por Binary o viceversa####

tmp_Stack1 <- list()

IMAGE_path2 <- "C:/Data/Seasonal_Water/"
##Load all the images Landsat8 in one list.
all_IMAGE2 <- list.files(IMAGE_path2,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack1 <- stack(all_IMAGE2)

######################

tmp_Stack2 <- list()

IMAGE_path3 <- "C:/Data/Permanent_Water/"
##Load all the images Landsat8 in one list.
all_IMAGE3 <- list.files(IMAGE_path3,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack2 <- stack(all_IMAGE3)

###################

tmp_Stack3 <- list()

IMAGE_path4 <- "C:/Data/Total_Water/"
##Load all the images Landsat8 in one list.
all_IMAGE4 <- list.files(IMAGE_path4,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack3 <- stack(all_IMAGE4)

##################

# Define dataframe and fill it with the dates
my_years2 <- (names(tmp_Stack3))
my_mat2 <- matrix(data = NA, nrow = length(my_years2), ncol = 4)
my_mat2[,1] <- my_years2
my_df2 <- data.frame(my_mat2,stringsAsFactors=FALSE)
names(my_df2) <- c("Date", "Area_Seasonal_Water_Body", "Area_Permanent_Water_Body", "Area_Water_Body")

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(my_years2)){
  
  area_Seasonal <- cellStats(tmp_Stack1[[i]], 'sum')
  my_df2[i,2] <- ((area_Seasonal*9)/10000)
  area_Permanent <- cellStats(tmp_Stack2[[i]], 'sum')
  my_df2[i,3] <- ((area_Permanent*9)/10000)
  area_total <- (((area_Seasonal+area_Permanent)*9)/10000)
  my_df2[i,4] <- area_total
  rm(area_Seasonal,area_Permanent,area_total,i)
}


#agregar info de 2019 creando un mapa de permanente y seasonal water 


# my_df
setwd("c:/Data/")
# Plot resulting dataframe and perform a regression analysis to display a trend line
pdf("timeseries_Area_Water_Body.pdf",width=15,height=8)
ggplot(my_df2, aes(x=Date, y=as.numeric(Area_Water_Body), group = 1))+
  geom_point(size=2)+
  geom_line()+
  geom_smooth(method="loess", se=TRUE, formula= y ~ x)+
  labs(title="Time Series of Area Water Body in Aculeo Lake", 
       x="Date", y="Area Water Body (KM2)") +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "right")
dev.off()
