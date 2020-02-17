install.packages("raster")
install.packages("rgdal")
install.packages("reshape")
install.packages("scales")
install.packages("lattice")
install.packages("ggplot2")


library(tidyverse)
library(dplyr)
library(raster)
library(rgdal)
library(reshape)
library(scales)
library(rgeos)
library(RStoolbox)
library(lattice)
library(ggplot2)


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
water_img <- reclassify(ndwi, c(-Inf, 0, NA, 0, Inf, 1))
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
tmp10 <- list()

###############################
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
  
  s_list <- writeRaster(n_list[[i]], filename=paste0(yr,'-',mt,'-',dy), format='GTiff', overwrite=T, options = c('TFW=YES')) 
  tmp10 <- append(tmp10,s_list)
  rm(nm,yr,mt,dy)
}

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
  
  img_fn <-  writeRaster(t_img[[i]],filename=paste0(yr,'-',mt,'-',dy),format="GTiff",overwrite=TRUE, options = c('TFW=YES'))
  rm(nm,yr,mt,dy)
}

#####cambiar NDWI por test o viceversa####

tmp_Stack <- list()

IMAGE_path2 <- "C:/Image_Landsat/test/"
##Load all the images Landsat8 in one list.
all_IMAGE2 <- list.files(IMAGE_path2,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack <- stack(all_IMAGE2)

# Define dataframe and fill it with the dates
my_years <- (names(tmp_Stack))
my_mat <- matrix(data = NA, nrow = length(my_years), ncol = 2)
my_mat[,1] <- my_years
my_df <- data.frame(my_mat,stringsAsFactors=FALSE)
names(my_df) <- c("Date", "Mean_NDWI")

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(my_years)){

  mean_layer <- c((((maxValue(tmp_Stack[[i]]))+(minValue(tmp_Stack[[i]])))/2))
  my_df[i,2] <- mean_layer
  rm(maxValue_layer,minValue_layer,mean_layer,i)
}


#################sum pixel#####

# Define dataframe and fill it with the dates
my_years2 <- (names(tmp_Stack))
my_mat2 <- matrix(data = NA, nrow = length(my_years2), ncol = 2)
my_mat2[,1] <- my_years2
my_df2 <- data.frame(my_mat2,stringsAsFactors=FALSE)
names(my_df2) <- c("Date", "Area_Water_Body")

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(my_years2)){
  
  area_layer <- cellStats(tmp_Stack[[i]], 'sum')
  my_df2[i,2] <- ((area_layer*9)/10000)
  rm(area_layer,i)
}

###########################

# optional: check data frame
# my_df
setwd("c:/Image_Landsat/test/")
# Plot resulting dataframe and perform a regression analysis to display a trend line
pdf("timeseries_Area_Water_Body.pdf",width=15,height=8)
ggplot(my_df2, aes(x=Date, y=Area_Water_Body , group = 1))+
  geom_point(size=2)+
  geom_line()+
  geom_smooth(method="loess", se=TRUE, formula= y ~ x)+
  labs(title="Time Series of Area Water Body in Aculeo Lake", 
       x="Date", y="Area_Water_Body") +
  theme(plot.title = element_text(hjust = 0.5))
dev.off()


####################################




ndwi_df <- as.data.frame(n_stack, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

ggplot(my_df) +
  geom_histogram(aes(value)) +
  facet_wrap(~variable)


######################


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
  
textureresult <- glcm(n_list[[i]])


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


setwd("C:/")
NDVI_HARV_path <- "Image_Landsat/test/"

ndwi_list <- list()
ndwi_list <- list.files(NDVI_HARV_path,
                        full.names = TRUE,
                        pattern = ".tif$")


n_stack <- stack(ndwi_list[[5]])


ndwi_df <- as.data.frame(n_stack, xy = TRUE) %>%
  melt(id.vars = c('x','y'))

ggplot(ndwi_df) +
  geom_histogram(aes(value)) +
  facet_wrap(~variable)


