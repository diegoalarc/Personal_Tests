
library(shinythemes)
library(gganimate)
library(rasterVis)
library(animation)
library(RStoolbox)
library(tidyverse)
library(magrittr)
library(raster)
library(rgdal)
library(dplyr)
library(sp)




setwd("c:/")
dir.create("Data")

setwd("c:/Data/")
dir.create("GIF")
dir.create("Binary")
dir.create("Permanent_Water")
dir.create("Seasonal_Water")
dir.create("Total_Water")
dir.create("Zona_Study")
dir.create("Data_Bruto")
dir.create("Seasonal_Water_Color")
dir.create("Permanent_Water_Color")
dir.create("Total_Water_Color")




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

#create the path where are all the images Landsat8 we will use.
Water_IMAGE_path <- "C:/Data/Zona_Study/"
#Load all the images Landsat8 in one list.
Water_all_IMAGE <- list.files(Water_IMAGE_path,
                              full.names = TRUE,
                              pattern = ".tif$")
#Create a List of Raster Files
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

#Create a List of the ROI.
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
  #  Water classfy
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
  #  Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #    Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_Seasonal[[i]], filename=paste0(Country,"Seasonal_",yr), format='GTiff', overwrite=T) 
  temporal5 <- append(temporal5,s_list)
  rm(Country,yr)
}

temporal6 <- list()

setwd("C:/Data/Permanent_Water/")

for (i in 1:length(t_Permanent)){
  #  Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #  Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_Permanent[[i]], filename=paste0(Country,"Permanent_",yr), format='GTiff', overwrite=T) 
  temporal6 <- append(temporal6,s_list)
  rm(Country,yr)
}


temporal7 <- list()

setwd("C:/Data/Total_Water/")

for (i in 1:length(t_water)){
  #  Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #  Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_water[[i]], filename=paste0(Country,yr), format='GTiff', overwrite=T) 
  temporal7 <- append(temporal7,s_list)
  rm(Country,yr)
}

#cambiar NDWI por Binary o viceversa

tmp_Stack1 <- list()

IMAGE_path2 <- "C:/Data/Seasonal_Water/"
#Load all the images Landsat8 in one list.
all_IMAGE2 <- list.files(IMAGE_path2,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack1 <- stack(all_IMAGE2)



tmp_Stack2 <- list()

IMAGE_path3 <- "C:/Data/Permanent_Water/"
#Load all the images Landsat8 in one list.
all_IMAGE3 <- list.files(IMAGE_path3,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack2 <- stack(all_IMAGE3)



tmp_Stack3 <- list()

IMAGE_path4 <- "C:/Data/Total_Water/"
#Load all the images Landsat8 in one list.
all_IMAGE4 <- list.files(IMAGE_path4,
                         full.names = TRUE,
                         pattern = ".tif$")
tmp_Stack3 <- stack(all_IMAGE4)



# Define dataframe and fill it with the dates
my_years <- substr(names_file, start=15, stop=18)
my_mat <- matrix(data = "Seasonal", nrow = length(my_years), ncol = 3)
my_mat[,1] <- my_years
my_df <- data.frame(my_mat,stringsAsFactors=FALSE)

my_mat1 <- matrix(data = "Permanent", nrow = length(my_years), ncol = 3)
my_mat1[,1] <- my_years
my_df1 <- data.frame(my_mat1,stringsAsFactors=FALSE)

my_mat2 <- matrix(data = "Total", nrow = length(my_years), ncol = 3)
my_mat2[,1] <- my_years
my_df2 <- data.frame(my_mat2,stringsAsFactors=FALSE)

names(my_df) <- c("Year", "Type", "Area")
names(my_df1) <- c("Year", "Type", "Area")
names(my_df2) <- c("Year", "Type", "Area")

# For-loop calculating mean of each raster and save it in data.frame
for (i in 1:length(my_years)){
  area_Seasonal <- cellStats(tmp_Stack1[[i]], 'sum')
  my_df[i,3] <- ((area_Seasonal*9)/10000)
  area_Permanent <- cellStats(tmp_Stack2[[i]], 'sum')
  my_df1[i,3] <- ((area_Permanent*9)/10000)
  area_total <- (((area_Seasonal+area_Permanent)*9)/10000)
  my_df2[i,3] <- area_total
  rm(area_Seasonal,area_Permanent,area_total,i)
}


my_mat3 <- matrix(data = NA, nrow = (length(my_years)*3), ncol = 3)
my_df3 <- data.frame(my_mat3,stringsAsFactors=FALSE)
names(my_df3) <- c("Year", "Type", "Area")
my_df3 <- rbind.data.frame(my_df,my_df1,my_df2)

#######

if(!file.exists(paste0(reswd,"Seasonal.gif"))) {
setwd("c:/Data/Seasonal_Water_Color/")
for (i in 1:dim(tmp_Stack1)[3]){
  png(filename=paste0(names(tmp_Stack1)[i],".png"), width = 480, height = 400)
  
  
  plot(tmp_Stack1[[i]],
       main=names(tmp_Stack1)[i],
       legend=FALSE,
       col = c("green", "blue"),
       breaks=c(0,.000000000000000000001,1))
  
  dev.off()
}
setwd("c:/Data/GIF/")
list.files(path="c:/Data/Seasonal_Water_Color/", pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("Seasonal.gif") # write to current dir
}
####

if(!file.exists(paste0(reswd,"Permanent.gif"))) {
setwd("c:/Data/Permanent_Water_Color/")
for (i in 1:dim(tmp_Stack2)[3]){
  png(filename=paste0(names(tmp_Stack2)[i],".png"), width = 480, height = 400)
  
  
  plot(tmp_Stack2[[i]],
       main=names(tmp_Stack2)[i],
       legend=FALSE,
       col = c("green", "blue"),
       breaks=c(0,.000000000000000000001,1))
  
  dev.off()
}
setwd("c:/Data/GIF/")
list.files(path="c:/Data/Permanent_Water_Color/", pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("Permanent.gif") # write to current dir
}
#######

if(!file.exists(paste0(reswd,"Total.gif"))) {
setwd("c:/Data/Total_Water_Color/")
for (i in 1:dim(tmp_Stack3)[3]){
  png(filename=paste0(names(tmp_Stack3)[i],".png"), width = 480, height = 400)
  
  
  plot(tmp_Stack3[[i]],
       main=names(tmp_Stack3)[i],
       legend=FALSE,
       col = c("green", "blue"),
       breaks=c(0,.000000000000000000001,1))
  
  dev.off()
}
setwd("c:/Data/GIF/")
list.files(path="c:/Data/Total_Water_Color/", pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("Total.gif") # write to current dir
}

###########Shiny######
library(ggplot2)
library(shiny)
library(gganimate)
library(magick)

ui <- fluidPage(
  theme = shinythemes::shinytheme("superhero"),
  titlePanel("Time Series of Surface Water Body in Aculeo Lake"),
  sidebarLayout(position = "right",
                sidebarPanel(
                  sliderInput("yearsInput", "Choose Years between:", 2000, 2018, c(2000, 2001)),
                  radioButtons("typeInput", "Choose a type of water:",
                               choices = c("Permanent", "Seasonal", "Total"),
                               selected = "Total")
                  )
                ),
                mainPanel(position = "left",
                          plotOutput("coolplot"), 
                          br(), br(),
                          tableOutput("results"),
                          br(), br(),
                          plotOutput("myImage")
#uiOutput(outputId = "my_ui"))
)

server <- function(input, output, session) {
  
  
  output$coolplot <- renderPlot({
    filtered <-
      my_df3 %>%
      filter(Year >= input$yearsInput[1] & Year <= input$yearsInput[2],
             Type == input$typeInput,
      )
    
    ggplot(filtered, aes(Year, y=as.numeric(Area), group = input$typeInput)) +
      geom_line(aes(colour = Type), position = "stack", size = .5) +
      geom_point(aes(colour = Type), position = "stack", size = 2) +
      geom_smooth(method="loess", se=TRUE, formula= y ~ x)+
      labs(x="Year", y="Area"~Km^2) + 
      theme_minimal()
    
    
  })
  
  output$results <- renderTable({
    t(filtered<-
        my_df3 %>%
        filter(Year >= input$yearsInput[1],
               Year <= input$yearsInput[2],
               Type == input$typeInput
        ))
  },align = 'c', rownames = TRUE, digits = 2, colnames = FALSE, spacing = 'xs')
  
  
  
  
  output$myImage<-renderImage({
    
      filter(Type == input$typeInput)
    
    filename <- file.path("c:/Data/GIF",paste0(input$typeInput, '.gif', sep=''))

    list(src = filename)
    filetype = 'image/gif'
  }, deleteFile = T)
  
  }
  
  # initialize reactive values

  
#  output$image2 <- renderImage({
    

#      if (is.null(input$typeInput))
#        return(NULL)
#      
#      if (input$typeInput == "Permanent") {
#        return(list(
#          src = "c:/Data/GIF/",
#          contentType = "image/png",
#        ))
#      } else if (input$typeInput == "Seasonal") {
#        return(list(
#          src = "c:/Data/GIF/",
#          filetype = "image/jpeg",
#        ))
#      } else if (input$typeInput == "Total") {
#        return(list(
#          src = "c:/Data/GIF/",
#          filetype = "image/jpeg",
#        ))
#      }
#    }, deleteFile = FALSE)
#  }


shinyApp(ui = ui, server = server)








