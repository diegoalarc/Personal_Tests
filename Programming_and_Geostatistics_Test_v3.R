install.packages("raster")
install.packages("rgdal")
install.packages("reshape")
install.packages("scales")
install.packages("lattice")



library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(reshape)
library(scales)
library(rgeos)
library(RStoolbox)
library(lattice)

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
water_img <- list()

for (i in 1:length(brick_list)){
esp <- brick(brick_list[i])  
ndwi <- ((esp[[6]]-esp[[8]])/(esp[[6]]+esp[[8]]))
ndwi[ndwi>1] <- 1; ndwi[ndwi< (-1)] <- (-1)
####Water classfy####
water_img <- reclassify(ndwi, cbind(-Inf, 0.2, NA))
###########
n_list <- append(n_list, ndwi)
####RGB List#####
t_img <- append(t_img,water_img)
}



tmp9 <- sort(names[sequence])
names(n_list) <- names[sequence]
n_list = n_list[order(names(n_list))]
names(t_img) <- names[sequence]
t_img = t_img[order(names(t_img))]

###############################

setwd("c:/Image_Landsat/test/")

for (i in 1:length(t_img)){
  #Sample image name
  nm <- t_img[i]
  #Extract year
  yr <- substr(names(nm), start=18, stop=21)
  #Extract month of year
  mt <- substr(names(nm), start=22, stop=23)
  #Extract day of year
  dy <- substr(names(nm), start=24, stop=25) 
  
  img_fn <-  writeRaster(t_img[[i]],filename=paste0('Water','_',yr,'-',mt,'-',dy),format="GTiff",overwrite=TRUE)
  rm(nm,yr,mt,dy)
}

tmp_Stack <- list()

IMAGE_path2 <- "C:/Image_Landsat/NDWI/"
##Load all the images Landsat8 in one list.
all_IMAGE2 <- list.files(IMAGE_path2,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack <- stack(all_IMAGE2)

# Define dataframe and fill it with the dates
my_years <- c(names(tmp_Stack))
my_mat <- matrix(data = NA, nrow = length(my_years), ncol = 4)
my_mat[,1] <- my_years
my_df <- data.frame(my_mat,stringsAsFactors=FALSE)
names(my_df) <- c("Year", "maxValue", "minValue", "Mean_NDWI")

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(my_years)){
  maxValue_layer <- c(maxValue(tmp_Stack[[i]]))
  my_df[i,2] <- maxValue_layer
  minValue_layer <- c(minValue(tmp_Stack[[i]]))
  my_df[i,3] <- minValue_layer
  mean_layer <- c((((maxValue(tmp_Stack[[i]]))+(minValue(tmp_Stack[[i]])))/2))
  my_df[i,4] <- mean_layer
  rm(maxValue_layer,minValue_layer,mean_layer,i)
}

###########################


tmp10 <- list()

setwd("C:/Image_Landsat/NDWI/")

for (i in 1:length(n_list)){
#Sample image name
nm <- n_list[i]
#Extract year
yr <- substr(names(nm), start=18, stop=21)
#Extract month of year
mt <- substr(names(nm), start=22, stop=23)
#Extract day of year
dy <- substr(names(nm), start=24, stop=25)
s_list <- writeRaster(n_list[[i]], filename=paste0('NDWI','_',yr,'-',mt,'-',dy), format='GTiff', overwrite=T) 
tmp10 <- append(tmp10,s_list)
rm(nm,yr,mt,dy)
}


#############

IMAGE_path2 <- "C:/Image_Landsat/NDWI/"
##Load all the images Landsat8 in one list.
all_IMAGE2 <- list.files(IMAGE_path2,
                        full.names = TRUE,
                        pattern = ".tif$")

tmp_Stack <- stack(all_IMAGE2)

df_tmp_stack <- as.data.frame(tmp_Stack,xy=TRUE)

plot(df_tmp_stack)




head(tmp_Stack)





##### GLCM Analysis ####

library(glcm)

for (i in 1:length(brick_list)){
  
textureresult <- glcm(n_list[[7]])


plot(textureresult$glcm_variance)
}

###############RGB DATA#######

###############  Supervised classification ###############

td <- readOGR("C:/Users/JELG02/OneDrive/Uni-Wue/1er_Semestre/MB2_Introduction_to_Programming_and_Geostatistics/Final_Project/Training_Area_utm.shp")
vd <- readOGR("C:/Users/JELG02/OneDrive/Uni-Wue/1er_Semestre/MB2_Introduction_to_Programming_and_Geostatistics/Final_Project/Val_data_utm.shp")

sc <- vector()
data_sc <- list()
classification_data <- list()
classification_data2 <- list()

for (i in 1:length(n_list)){

sc <- superClass(n_list[[i]], trainData = td, responseCol="id",
                 model = "rf", valData = vd)
data_sc[[i]] <- sc$map

classification_data <- brick(data_sc[[i]])
classification_data2 <- append(classification_data2,classification_data)

rm(sc)
}

setwd("B:/Image_Landsat/Test_Area/")

for (i in 1:length(n_list)){
  img_superclass <-  writeRaster(classification_data2[[i]],filename = names(n_list[i]),format="GTiff",overwrite=TRUE)
}

plot(classification_data2[[2]])


################# Unsupervised classification########
list <- vector()
list_2 <- list()

for(i in 1:length(n_list)){
  list <- unsuperClass(n_list[[i]],nClasses=6)
  list_2[[i]] <- list$map
  rm(list)
}

plot(list_2[[3]])

rasterEntropy(list_2[3])

#############################

#######Test AREA#######

##################Histogram
library(ggplot2)


setwd("B:/")
NDVI_HARV_path <- "Image_Landsat/NDWI/"

ndwi_list <- list()
ndwi_list <- list.files(NDVI_HARV_path,
                        full.names = TRUE,
                        pattern = ".tif$")


n_stack <- stack(ndwi_list)
n_stack[n_stack>1] <- 1; n_stack[n_stack< (-1)] <- (-1)


ndwi_df <- as.data.frame(n_stack, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

ggplot(ndwi_df) +
  geom_histogram(aes(value)) +
  facet_wrap(~variable)

