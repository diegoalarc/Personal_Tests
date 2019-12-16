library(raster)
# Blue
b2 <- raster("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B2.TIF")
# Green
b3 <- raster("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B3.TIF")
# Red
b4 <- raster("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B4.TIF")
# Near Infrared (NIR)
b5 <- raster("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B5.TIF")

b2

s <- stack(b4, b3, b2)
plotRGB(s, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")



# first create a list of raster layers to use
filenames <- paste0("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B", 1:7, ".tif")
filenames
##  [1] "data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B1.TIF"
##  [2] "data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B2.TIF"
##  [3] "data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B3.TIF"
##  [4] "data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B4.TIF"
##  [5] "data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B5.TIF"
##  [6] "data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B6.TIF"
##  [7] "data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B7.TIF"

landsat <- stack(filenames)
landsat


par(mfrow = c(2,2))
plot(b2, main = "Blue", col = gray(0:100 / 100))
plot(b3, main = "Green", col = gray(0:100 / 100))
plot(b4, main = "Red", col = gray(0:100 / 100))
plot(b5, main = "NIR", col = gray(0:100 / 100))


##To make a "true (or natural) color" image, that is, something that looks like 
##a normal photograph (vegetation in green, water blue etc), we need bands in 
##the red, green and blue regions. For this Landsat image, band 4 (red), 3 (green), 
##and 2 (blue) can be used. The plotRGB method can be used to combine them into 
##a single composite. You can also supply additional arguments to plotRGB to improve 
##the visualization (e.g. a linear stretch of the values, using strecth = "lin").

landsatRGB <- stack(b4, b3, b2)
plotRGB(landsatRGB, axes = TRUE, stretch = "lin", main = "Landsat True Color Composite")


##The true-color composite reveals much more about the landscape than the earlier 
##gray images. Another popular image visualization method in remote sensing is 
##known "false color" image in which NIR, red, and green bands are combined.
##This representation is popular as it makes it easy to see the vegetation (in red).

par(mfrow = c(1,2))
plotRGB(landsatRGB, axes=TRUE, stretch="lin", main="Landsat True Color Composite")
landsatFCC <- stack(b5, b4, b3)
plotRGB(landsatFCC, axes=TRUE, stretch="lin", main="Landsat False Color Composite")


##You can select specific layers (bands) using subset function, or via indexing.

# select first 3 bands only
landsatsub1 <- subset(landsat, 1:3)
# same
landsatsub2 <- landsat[[1:3]]
# Number of bands in the original and new data
nlayers(landsat)
## [1] 7
nlayers(landsatsub1)
## [1] 3
nlayers(landsatsub2)
## [1] 3

##We won't use the last four bands in landsat. You can remove those using
##but for our case thereis only 7

landsat <- subset(landsat, 1:7)

names(landsat)
##[1] "LT52240632006164CUB03_B1" "LT52240632006164CUB03_B2" "LT52240632006164CUB03_B3"
##[4] "LT52240632006164CUB03_B4" "LT52240632006164CUB03_B5" "LT52240632006164CUB03_B6"
##[7] "LT52240632006164CUB03_B7"

names(landsat) <- c('ultra-blue', 'blue', 'green', 'red', 'NIR', 'SWIR1', 'SWIR2')
names(landsat)
## [1] "ultra.blue" "blue"       "green"      "red"        "NIR"
## [6] "SWIR1"      "SWIR2"


# Using extent
extent(landsat)
##class      : Extent 
##xmin       : 487485 
##xmax       : 723615 
##ymin       : -583215 
##ymax       : -375285 

##Interactive selection from the image is also possible.
##Use ``drawExtent`` and ``drawPoly`` to select an area of interest

drawExtent(show=TRUE, col="red")
drawPoly(sp= true, col = "red")

e <- extent(586289, 594793, -529714, -507035)
## crop landsat by the extent
landsatcrop <- crop(landsat, e)


##################################################################################


library(raster)
raslist <- paste0("C:/Users/JELG02/Documents/Clase_de_R/data/Landsat/p224r63/LT52240632006164CUB03.tar/LT52240632006164CUB03_B", 1:7, ".tif")
landsat <- stack(raslist)
landsatRGB <- landsat[[c(4,3,2)]]
landsatFCC <- landsat[[c(5,4,3)]]

##Vegetation indices
##Let's define a general function for a ratio based (vegetation) index.
##In the function below, img is a mutilayer Raster* object and i and k are 
##the indices of the layers (layer numbers) used to compute the vegetation index.

vi <- function(img, k, i) {
  bk <- img[[k]]
  bi <- img[[i]]
  vi <- (bk - bi) / (bk + bi)
  return(vi)
}
# For Landsat NIR = 5, red = 4.
ndvi <- vi(landsat, 5, 4)
plot(ndvi, col = rev(terrain.colors(10)), main = "Landsat-NDVI")


#An alternative way to accomplish this is like this

vi2 <- function(x, y) {
  (x - y) / (x + y)
}
ndvi2 <- overlay(landsat[[5]], landsat[[4]], fun=vi2)
plot(ndvi2, col=rev(terrain.colors(10)), main="Landsat-NDVI")

# view histogram of data
hist(ndvi,
     main = "Distribution of NDVI values",
     xlab = "NDVI",
     ylab= "Frequency",
     col = "wheat",
     xlim = c(-0.5, 1),
     breaks = 30,
     xaxt = 'n')
axis(side=1, at = seq(-0.5,1, 0.05), labels = seq(-0.5,1, 0.05))

##Cells with NDVI values greater than 0.4 are definitely vegetation.
##The following operation masks all cells that are perhaps not vegetation.

veg <- reclassify(ndvi, cbind(-Inf, 0.4, NA))
plot(veg, main='Vegetation')


##Let's map the area that corresponds to the peak between 0.25 and 0.3 in the NDVI histogram.

land <- reclassify(ndvi, c(-Inf, 0.25, NA,  0.25, 0.3, 1,  0.3, Inf, NA))
plot(land, main = 'Open Areas')

##Landsat False Color Composite

plotRGB(landsatRGB, r=1, g=2, b=3, axes=TRUE, stretch="lin", main="Landsat False Color Composite")
plot(land, add=TRUE, legend=FALSE)

##NDVI based thresholding

vegc <- reclassify(ndvi, c(-Inf,0.25,1, 0.25,0.3,2, 0.3,0.4,3, 0.4,0.5,4, 0.5,Inf, 5))
plot(vegc,col = rev(terrain.colors(4)), main = 'NDVI based thresholding')

##NIR-Red plot

set.seed(1)
sr <- sampleRandom(landsat, 10000)
plot(sr[,c(4,5)], main = "NIR-Red plot")


pca <- prcomp(sr, scale = TRUE)
pca
##Standard deviations (1, .., p=7):
##  [1] 2.3792629 0.9182083 0.5177622 0.3677326 0.2128427 0.1819575 0.1195248

##Rotation (n x k) = (7 x 7):
##  PC1        PC2        PC3         PC4        PC5          PC6
##LT52240632006164CUB03_B1 -0.3983124  0.1363253  0.4656166 -0.21984499  0.6620349 -0.002462757
##LT52240632006164CUB03_B2 -0.4010022  0.2446754  0.3039014 -0.16046772 -0.1555997  0.206369914
##LT52240632006164CUB03_B3 -0.3870579  0.3811562  0.1469298  0.07689757 -0.6512942  0.025844350
##LT52240632006164CUB03_B4 -0.3738636 -0.3438325 -0.3808615 -0.70129364 -0.1360289 -0.292775496
##LT52240632006164CUB03_B5 -0.3980728 -0.1091310 -0.5066585  0.21884640  0.1731926  0.699102054
##LT52240632006164CUB03_B6 -0.2765772 -0.7737445  0.3983438  0.36841262 -0.1621053 -0.064087007
##LT52240632006164CUB03_B7 -0.3949682  0.2178496 -0.3298797  0.49454087  0.1963077 -0.614954588
##PC7
##LT52240632006164CUB03_B1 -0.345450055
##LT52240632006164CUB03_B2  0.770989910
##LT52240632006164CUB03_B3 -0.502545966
##LT52240632006164CUB03_B4 -0.030258966
##LT52240632006164CUB03_B5 -0.079325616
##LT52240632006164CUB03_B6  0.005799488
##LT52240632006164CUB03_B7  0.162617890
screeplot(pca)


pci <- predict(landsat, pca, index = 1:2)
plot(pci[[1]])

pc2 <- reclassify(pci[[2]], c(-Inf,0,1,0,Inf,NA))
par(mfrow = c(1,2))
plotRGB(landsatFCC, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", main = "Landsat False Color Composite")
plotRGB(landsatFCC, r = 1, g = 2, b = 3, axes = TRUE, stretch = "lin", main = "Landsat False Color Composite")
plot(pc2, legend = FALSE, add = TRUE)
