#unsupervised classification

library(raster)
library(cluster)

raster_stack <- brick("C:/Users/JELG02/Documents/Clase_de_R/data/Steigerwald/03_raster/01_landsat/01_indices/LC8_evi.grd")

raster_df <- raster_stack[]

kmeans_out <- kmeans(raster_df, 12, iter.max = 100, nstart = 10)

kmeans_raster <- raster(raster_stack)
kmeans_raster[] <- kmeans_out$cluster 
plot(kmeans_raster)


# Supervised classification

library(RStoolbox)
library(rgdal)

td <- readOGR("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/RSE_book_Brazil_deforestation/p224r63_1988_Training_data.shp")
img <- brick("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/RSE_book_Brazil_deforestation/p224r63_1988.grd")

setwd("C:/Users/JELG02/Documents/Clase_de_R/data/")

sc <- superClass(img, trainData = td,
                 responseCol = "Class_name",
                 model = "rf",
                 filename = "Classification.tif"
                 )

plot(sc$map,
     legend = FALSE,
     col = c("red", "green", "black"), axes = TRUE,
     main = "Classified Canopy LandCover Burn")

legend("bottomleft",
       legend = c("Deforestation", "Soil", "Trees"),
       fill = c("red", "green", "black"),
       border = FALSE,
       bty = "y") # turn off legend border
