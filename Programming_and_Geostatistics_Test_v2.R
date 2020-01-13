install.packages("raster")
install.packages("rgdal")
install.packages("reshape")
install.packages("scales")


library(raster)
library(rgdal)
library(reshape)
library(scales)

##create the path where are all the images Landsat8 we will use.
IMAGE_path <- "B:/Image_Landsat/TIF_Image"
##Load all the images Landsat8 in one list.
all_IMAGE <- list.files(IMAGE_path,
                            full.names = TRUE,
                            pattern = ".tif$")
##Activate the list in order to check if all the image are in.
all_IMAGE

##Create a List of Raster Files
aculeo_list <- list()
tmp <- list()

for (i in 1:length(all_IMAGE)){ 
aculeo_raster <- raster(all_IMAGE[i])
tmp <- append(tmp,aculeo_raster)
}
tmp

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


sequence <- seq(1,length(tmp)-9,10)
brick_list <- list()
tmp4 <- list()


for (i in sequence){
  tmp4 <- brick(roi_list[[i]],roi_list[[i+1]],roi_list[[i+2]],roi_list[[i+3]],roi_list[[i+4]],roi_list[[i+5]],
                roi_list[[i+6]],roi_list[[i+7]],roi_list[[i+8]],roi_list[[i+9]])
  
  brick_list <- append(brick_list, tmp4)
}

brick_list


# Name the brick list for sorting purposes

tmp5 <- sort(names[sequence])
names(brick_list) <- names[sequence]
brick_list = brick_list[order(names(brick_list))]

brick_list

setwd("B:/Image_Landsat/Landsat_raster/")

for (i in 1:length(brick_list)){
  final_raster <- writeRaster(brick_list[[i]], filename=n[i],format="GTiff", overwrite=TRUE)
}

Bandnames <- c("Pixel_QA","Rad_QA","Aerosols","B1","B2","B3","B4","B5","B6","B7")

for (i in 1:length(brick_list)){
  single_band <- raster(final_raster, layer = i)
  writeRaster(single_band, Bandnames[i])
}

single_band








