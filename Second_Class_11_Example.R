prec_avg <??? c(56 ,46 ,50 ,53 ,69 ,83 ,83 ,80 ,62 ,55 ,60 ,63)
prec_avg
plot(prec_avg)
plot(prec_avg,pch=19,cex=2,col="#00ff0060")
lines(lowess(prec ,f =.2)

      
install.packages("raster")
library(raster)
germany <??? getData("GADM" ,country="DEU" , level=2)
plot(germany)
b<-read.csv("C:/Users/JELG02/Documents/Uni-Wue/MB2_Programming/Test_RStudio.csv")#abrir tablas
k<-edit(b)#editar tablas
k
