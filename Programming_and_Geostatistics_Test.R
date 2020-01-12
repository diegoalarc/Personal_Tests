install.packages("raster")
install.packages("gdata")
install.packages("rgdal")

require(raster)
require(gdata)
require(rgdal)

# Reading the shapefile (mask to crop later)
Maskshp <- readOGR("C:/Users/JELG02/OneDrive/Uni-Wue/1er_Semestre/MB2_Introduction_to_Programming_and_Geostatistics/Final_Project/Study_Area.shp")

# Getting the spatial extent of the shapefile
e <- extent(Maskshp)

# for loop with (in this case) 60 runs
for (i in 1:620) {
  
  # Reading the raster to crop
  files <- list.files(path="B:/Image_Landsat/TIF_Image/",full.names=TRUE, pattern = "*.tif$")
  Landsat_raster <- raster(files[i])

    # Save the filename
  filename <- (paste(basename(files[i]), sep=""))
  
  # Crop the raster
  Landsat_raster.crop <- crop(Landsat_raster, e, snap="out")
  
  # Dummy raster with a spatial extension equal to the cropped raster,
  # but full of NA values
  crop <- setValues(Landsat_raster.crop, NA)
  
  #  Rasterize the catchment boundaries, with NA outside the catchment boundaries
  Maskshp.r <- rasterize(Maskshp, crop)
  
  # Putting NA values in all the raster cells outside the shapefile boundaries
  Maskshp.masked <- mask(x=Landsat_raster.crop, mask=Maskshp.r) 
  plot(Maskshp.masked)
  
  #Export file to working directory with original name as new name
  writeRaster(Maskshp.masked, filename)
}
