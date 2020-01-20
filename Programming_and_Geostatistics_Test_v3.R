install.packages("raster")
install.packages("rgdal")
install.packages("reshape")
install.packages("scales")

library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(reshape)
library(scales)
library(rgeos)
library(RStoolbox)

##create the path where are all the images Landsat8 we will use.
IMAGE_path <- "B:/Image_Landsat/TIF_Image"
##Load all the images Landsat8 in one list.
all_IMAGE <- list.files(IMAGE_path,
                        full.names = TRUE,
                        pattern = ".tif$")
##Create a List of Raster Files
tmp <- list()

for (i in 1:length(all_IMAGE)){ 
  aculeo_raster <- raster(all_IMAGE[i])
  tmp <- append(tmp,aculeo_raster)
}

##import the vector boundary in the study area.
STUDY_extent <- readOGR("C:/Users/JELG02/OneDrive/Uni-Wue/1er_Semestre/MB2_Introduction_to_Programming_and_Geostatistics/Final_Project/Study_Area.shp")
STUDY_extent <- spTransform(STUDY_extent, crs(tmp[[1]]))

##Create a List of the ROI.
tmp2 <- list()
roi_list <- list()

for (i in 1:length(tmp)){
  tmp2 <- crop(tmp[[i]],STUDY_extent)
  roi_list <- append(roi_list,tmp2)
}

tmp3 <- vector(mode="character")
names <- vector(mode="character")

for (i in 1:length(roi_list)){
  tmp3 <- names(roi_list[[i]])
  names <- append(names,tmp3)
}


sequence <- seq(1,length(tmp)-9,10)
brick_list <- list()
tmp4 <- list()


for (i in sequence){
  tmp4 <- brick(roi_list[[i]],roi_list[[i+1]],roi_list[[i+2]],roi_list[[i+3]],roi_list[[i+4]],roi_list[[i+5]],
                roi_list[[i+6]],roi_list[[i+7]],roi_list[[i+8]],roi_list[[i+9]])
  brick_list <- append(brick_list, tmp4)
}

ndwi <- list()
n_list <- list()
esp <- list()
RGB_img <- list()
t_img <- list()

for (i in 1:length(brick_list)){
esp <- brick(brick_list[i])  
ndwi <- ((esp[[6]]-esp[[8]])/(esp[[6]]+esp[[8]]))
ndwi[ndwi>1] <- 1; ndwi[ndwi< (-1)] <- (-1)


RGB_img <- brick((esp[[5]]),(esp[[6]]),(esp[[7]]))

n_list <- append(n_list, ndwi)

t_img <- append(t_img,RGB_img)
}

tmp9 <- sort(names[sequence])
names(n_list) <- names[sequence]
n_list = n_list[order(names(n_list))]


setwd("B:/Image_Landsat/NDWI/")

for (i in 1:length(n_list)){
  writeRaster(n_list[[i]],filename = names(n_list[i]),format="GTiff",overwrite=TRUE)
}


td <- readOGR("C:/Users/JELG02/OneDrive/Uni-Wue/1er_Semestre/MB2_Introduction_to_Programming_and_Geostatistics/Final_Project/Training_Area_utm.shp")
vd <- readOGR("C:/Users/JELG02/OneDrive/Uni-Wue/1er_Semestre/MB2_Introduction_to_Programming_and_Geostatistics/Final_Project/Val_data_utm.shp")

sc <- vector()
data_sc <- list()

for (i in 1:length(n_list)){

sc <- superClass(n_list[[i]], trainData = td,
                 responseCol = "Class_name",
                 model = "rf", valData = vd)
data_sc[[i]] <- sc$map
rm(sc)
}

plot(data_sc)


################# Unsupervised classification########
list <- vector()
list_2 <- list()

for(i in 1:length(n_list)){
  list <- unsuperClass(n_list[[i]],nClasses=6)
  list_2[[i]] <- list$map
  rm(list)
}

plot(list_2[[3]])


#############################

#######Test AREA#######

##################Histogram
library(ggplot2)
n_stack <- stack(n_list)

ndwi_df <- as.data.frame(n_stack, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

ggplot(ndwi_df) +
  geom_histogram(aes(value)) +
  facet_wrap(~variable)

###################### Plot

NDVI.stack <- stack(n_list)  # Representing your layerstack

# Plot Stack, select point and automaticly extract values
for (i in 1:length(n_list)){
plot(NDVI.stack[[i]])  # One exemplary layer for orientation
values <- click(NDVI.stack, n=1)
}
# Compose and plot dataframe
timeseries <- data.frame(year = c(2000, 2005, 2010, 2015),
                         values = values[1, ])
plot(timeseries, type="l")

######################## SLopeAspect

slope <- list()
n_layer <- list()

for (i in 1:length(n_list)){
n_layer <- brick(n_list[i])
slope_file <- terrain(n_layer[[1]], opt="slope", unit="tangent", neighbors=4)
slope <- append(slope,slope_file)
}
