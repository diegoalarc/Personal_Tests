# ###########
# download and display Area of water and look the diminish of the Aculeo Lake, Paine, Chile.
# created just for teaching purpose - not for scientific analysis! 100% accuracy not ensured
# learning goal: download data, convert them, analyse spatio-temporal data and display them in differents forms.
# ###########
# 
# idea triggered by these news, videos and personal experience:
# https://twitter.com/copernicusems/status/1178001302829375490
# https://www.youtube.com/watch?v=aEi-itbg4bs
# https://earthobservatory.nasa.gov/images/144836/lake-aculeo-dries-up
# https://www.straitstimes.com/world/americas/drought-wipes-chiles-popular-lake-aculeo-from-the-map
# https://chiletoday.cl/site/how-chile-should-prepare-for-a-future-without-water/
# 
# Originally written by Diego Alonso Alarcón Díaz in January 2020, latest Version: March 2020
# Code is good to go!



#It is necessary to check if the packages are install in  RStudio
if(!require(gganimate)){
  install.packages("gganimate")
  library(gganimate)
}
if(!require(rasterVis)){
  install.packages("rasterVis")
  library(rasterVis)
}
if(!require(animation)){
  install.packages("animation")
  library(animation)
}
if(!require(RStoolbox)){
  install.packages("RStoolbox")
  library(RStoolbox)
}
if(!require(tidyverse)){
  install.packages("tidyverse")
  library(tidyverse)
}
if(!require(magrittr)){
  install.packages("magrittr")
  library(magrittr)
}
if(!require(magick)){
  install.packages("magick")
  library(magick)
}
if(!require(rgdal)){
  install.packages("rgdal")
  library(rgdal)
}

#######################################################

#It is necessary to set and create the folders before hand to storage
#the data and also make the different file we will create
setwd("c:/")
dir.create("Data")

setwd("c:/Data/")
dir.create("GIF")
dir.create("Permanent_Water")
dir.create("Seasonal_Water")
dir.create("Total_Water")
dir.create("Zona_Study")
dir.create("Data_Bruto")
dir.create("Seasonal_Water_Color")
dir.create("Permanent_Water_Color")
dir.create("Total_Water_Color")

#######################################################

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
water_aculeo_raster <- list()

for (i in 1:length(Water_all_IMAGE)){ 
  water_aculeo_raster[[i]] <- raster(Water_all_IMAGE[i])
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
crop_list <- list()

for (i in 1:length(water_aculeo_raster)){
  crop_list[[i]] <- crop(water_aculeo_raster[[i]],aculeo_extent)
}

names_file <- vector(mode="character")

for (i in 1:length(crop_list)){
  names_file[[i]] <- names(crop_list[[i]])
}

brick_list <- list()

for (i in 1:length(crop_list)){
  brick_list[[i]] <- brick(crop_list[[i]])
}

t_Seasonal <- list()
t_Permanent <- list()
t_water <- list()
water <- list()

for (i in 1:length(brick_list)){
  Water <- brick(brick_list[i])  
  #  Water classfy
  t_Seasonal[[i]] <- reclassify(Water, c(0, 1, NA, 1, 2, 1, 2, 3, NA))
  t_Permanent[[i]] <- reclassify(Water, c(0, 2, NA, 2, 3, 1))
  t_water[[i]] <- reclassify(Water, c(0, 1, NA, 1, 3, 1))
}


setwd("C:/Data/Seasonal_Water/")

for (i in 1:length(t_Seasonal)){
  #  Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #    Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_Seasonal[[i]], filename=paste0(Country,"Seasonal_",yr), format='GTiff', overwrite=T) 
  rm(Country,yr)
}


setwd("C:/Data/Permanent_Water/")

for (i in 1:length(t_Permanent)){
  #  Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #  Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_Permanent[[i]], filename=paste0(Country,"Permanent_",yr), format='GTiff', overwrite=T) 
  rm(Country,yr)
}


setwd("C:/Data/Total_Water/")

for (i in 1:length(t_water)){
  #  Extract year
  Country <- substr(names_file[i], start=1, stop=6)
  #  Extract month of year
  yr <- substr(names_file[i], start=15, stop=18)
  
  s_list <- writeRaster(t_water[[i]], filename=paste0(Country,"Total_",yr), format='GTiff', overwrite=T) 
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

#######################################################
#It is necessary to check if the packages are install in  RStudio
if(!require(units)){
  install.packages("units")
  library(units)
}

# Define dataframe and fill it with the Year, Type and Area 
#for the difference types of water
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

for (i in 1:nrow(my_df3)){
my_df3[i, 3] <- (round(as.numeric(my_df3[i, 3]), digits = 2))
}

for (i in 1:nrow(my_df3)){
b <- data.frame(Area = set_units(as.numeric(my_df3[i, 3]), K^2))
my_df4[i,3] <- rbind.data.frame(b)
rm(b)
}
names(my_df4) <- c("Year", "Type", "Area Km^2")

#######
#It is necessary to check if the packages are install in  RStudio
if(!require(maps)){
  install.packages("maps")
  library(maps)
}
if(!require(GISTools)){
  install.packages("GISTools")
  library(GISTools)
}


reswd <- "c:/Data/GIF/"
if(!file.exists(paste0(reswd,"Seasonal.gif"))) {
setwd("c:/Data/Seasonal_Water_Color/")
for (i in 1:dim(tmp_Stack1)[3]){
  png(filename=paste0(names(tmp_Stack1)[i],".png"), width = 680, height = 600)
  
  
  plot(tmp_Stack1[[i]],
       main=names(tmp_Stack1)[i],
       legend=FALSE,
       col = c("green", "blue"),
       breaks=c(0,.000000000000000000001,1))
  maps::map.scale(x=-70.945, y=-33.865, relwidth=0.15, metric = TRUE, ratio=FALSE)  
  north.arrow(xb=-70.882, yb=-33.827, len=0.0009, lab="N") 
  
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
  png(filename=paste0(names(tmp_Stack2)[i],".png"), width = 680, height = 600)
  
  
  plot(tmp_Stack2[[i]],
       main=names(tmp_Stack2)[i],
       legend=FALSE,
       col = c("green", "blue"),
       breaks=c(0,.000000000000000000001,1))
  maps::map.scale(x=-70.945, y=-33.865, relwidth=0.15, metric = TRUE, ratio=FALSE)  
  north.arrow(xb=-70.882, yb=-33.827, len=0.0009, lab="N") 
  
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
  png(filename=paste0(names(tmp_Stack3)[i],".png"), width = 680, height = 600)
  
  
  plot(tmp_Stack3[[i]],
       main=names(tmp_Stack3)[i],
       legend=FALSE,
       col = c("green", "blue"),
       breaks=c(0,.000000000000000000001,1))
  maps::map.scale(x=-70.945, y=-33.865, relwidth=0.15, metric = TRUE, ratio=FALSE)  
  north.arrow(xb=-70.882, yb=-33.827, len=0.0009, lab="N") 
  
  dev.off()
}
  setwd("c:/Data/GIF/")
  list.files(path="c:/Data/Total_Water_Color/", pattern = '*.png', full.names = TRUE) %>% 
  image_read() %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=1) %>% # animates, can opt for number of loops
  image_write("Total.gif") # write to current dir
}



if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(DT)){
  install.packages("DT")
  library(DT)
}
if(!require(mgcv)){
  install.packages("mgcv")
  library(mgcv)
}
if(!require(ggpmisc)){
  install.packages("ggpmisc")
  library(ggpmisc)
}
if(!require(glue)){
  install.packages("glue")
  library(glue)
}

#add the CSS property white-space: nowrap to the cells in the columns, thus defining a CSS
#class and assigning it to the columns with className in the columnDefs option.
css <- "
.nowrap {
  white-space: nowrap;
}"

#Shiny app
ui <- fluidPage(
  
  navbarPage("Time Series of Surface Water Body in Aculeo Lake",
             tabPanel("Area v/s Years",
                      sidebarLayout(position = "left",
                                    sidebarPanel(
                                      sliderInput("yearsInput", "Choose Years between:", 2000, 2018, c(2000, 2001)),
                                      radioButtons("typeInput", "Choose a type of water:",
                                                   choices = c("Permanent", "Seasonal", "Total"),
                                                   selected = "Total")
                                    ),
                                    mainPanel(plotOutput("coolplot")
                                    ))
             ),
             
             tabPanel("Timeseries representation in a GIF",
                      sidebarLayout(position = "left",
                                    sidebarPanel(
                                      radioButtons("typeInput1", "Choose a type of water:",
                                                   choices = c("Permanent", "Seasonal", "Total"),
                                                   selected = "Total")
                                    ),
                                    mainPanel(plotOutput(outputId="preImage", width="480px",height="400px")))
             ),
             
             tabPanel("Image for the Year and Type",
                      sidebarLayout(position = "left",
                                    sidebarPanel(
                                      sliderInput("yearsInput2", "Choose Years between:", 2000, 2018, value = 2000),
                                      radioButtons("typeInput2", "Choose a type of water:",
                                                   choices = c("Permanent", "Seasonal", "Total"),
                                                   selected = "Total")
                                    ),
                                    mainPanel(plotOutput(outputId="Image", width="480px",height="400px")))
             ),
             
             tabPanel("Table of Data",
                      sidebarLayout(position = "left",
                                    sidebarPanel(
                                      sliderInput("yearsInput3", "Choose Years between:", 2000, 2018, c(2000, 2001)),
                                      selectInput("typeInput3", "Choose a type of water:",
                                                   choices = c("Permanent", "Seasonal", "Total")),
                                      downloadButton("download_data")),
                                    mainPanel(DT::dataTableOutput("table")))
             ))
)

server <- function(input, output, session) {
  
  output$coolplot <- renderPlot({
    (filtered <-
       my_df3 %>%
       filter(Year >= input$yearsInput[1] & Year <= input$yearsInput[2],
              Type == input$typeInput,
       ))
    
    my.formula <- y ~ x
    ggplot(filtered, aes(Year, y=as.numeric(Area), group = input$typeInput)) +
      geom_line(aes(colour = Type), position = "stack", size = .5) +
      geom_point(aes(colour = Type), position = "stack", size = 2) +
      geom_smooth(method="loess", se=TRUE, formula= my.formula) +
      stat_poly_eq(formula = my.formula, 
                  aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                  label.x = "left", label.y = "bottom",
                  parse = TRUE) +
      labs(title = "TimeSeries Aculeo Lake", subtitle = glue("All data here is produced under the Copernicus Programme, free of charge, without restriction of use."),
           caption = "Source: EC JRC/Google") +
      xlab("Year") + ylab("Area"~Km^2) + 
      theme_light()
    
  })
  
  ########################################
  
  output$preImage <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('c:/Data/GIF',
                                        paste(input$typeInput1, '.gif', sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$typeInput1))
    
    
  }, deleteFile = FALSE)
  
  #################################################
  
  output$Image <- renderImage({
    # When input$n is 3, filename is ./images/image3.jpeg
    filename <- normalizePath(file.path('c:/Data',
                                        paste(input$typeInput2,"_Water_Color/","Chile_",input$typeInput2,"_",input$yearsInput2, ".png", sep='')))
    
    # Return a list containing the filename and alt text
    list(src = filename,
         alt = paste(input$typeInput2))
    
  }, deleteFile = FALSE)
  
  ###############################  
  
  filtered_data <- reactive({
    data <- my_df4 %>%
        filter(Year >= input$yearsInput3[1] & Year <= input$yearsInput3[2],
               Type == input$typeInput3)
  })
  
  output$table <- DT::renderDataTable({
    data <- filtered_data()
    datatable(data,  options = list(pageLength = 19,
      columnDefs = list(
        list(className = "nowrap", targets = "_all")
      )
    ),rownames = FALSE) 
  })
  
  output$download_data <- downloadHandler(
    
    filename = paste("Aculeo_Lake_Data",".csv",sep=''),
      content = function(file) {
      data <- filtered_data()
      write.csv(data, file, row.names = FALSE)
    }
  )
}
shinyApp(ui = ui, server = server)

