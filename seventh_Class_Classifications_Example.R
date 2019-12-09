#unsupervised classification

library(raster)
library(cluster)

raster_stack <- brick("C:/Users/JELG02/Documents/Clase_de_R/data/Nov_10m.grd")

raster_df <- raster_stack[]

kmeans_out <- kmeans(raster_df, 7, iter.max = 100, nstart = 10)

kmeans_raster <- raster(raster_stack)
kmeans_raster[] <- kmeans_out$cluster 
plot(kmeans_raster)


# Supervised classification

library(RStoolbox)
library(rgdal)

td <- readOGR("C:/Users/JELG02/Documents/Clase_de_R/data/Training_data.shp")
img <- brick("C:/Users/JELG02/Documents/Clase_de_R/data/Nov_10m.grd")

setwd("C:/Users/JELG02/Documents/Clase_de_R/data/")

sc <- superClass(img, trainData = td,
                 responseCol = "Class",
                 model = "mlc",
                 filename = "Classification.tif"
                 )

plot(sc$map)
