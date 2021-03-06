install.packages("tidyverse")
install.packages("gifski")
install.packages("png")
install.packages("RCurl")
install.packages("ggthemes")

library(ggthemes)
library(gifski)
library(tidyverse)
library(png)
library(RCurl)

d <- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vQE2hQGfupXld2Tft4hWB5cMF1QeIh9zxcyRWG12s1KrJEg37t9xAST1sO6tW2PCNX94WsYjPaPuJsc/pub?gid=424464857&single=true&output=csv")))
summary(death)
b <- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vQXON_R3kJ1C1FUiOaYHEm_viQp4MZJ83rB7Wg6dRSJIxD-7gFRjStxzOG0qj6VmSlC5JDWE2GOT5cl/pub?gid=2024840774&single=true&output=csv")))
summary(birth)

dt1 <- data.frame(d[,c(1,4,2,3)])
dt2 <- data.frame(b[,c(1,4,2,3)])

datos.names <- c("Country","Status","Persons","Year")

names(dt1) <- datos.names
names(dt2) <- datos.names

datos <- rbind(dt1,dt2)

summary(datos)

library(ggplot2)
library(gganimate)

k <- ggplot(data=datos, aes(y=Persons, x=Year, color=Country, size=Status))+
  geom_point(alpha =.5)

k

k + theme_stata(base_size = 15) + 
  theme(legend.position = "right", legend.key.size = unit(.5, 'lines')) +
  transition_time(Year) +
  ease_aes('linear') +
  labs(title = "Born vs Death for Chile - Germany - USA") +
  ylab(label = "Persons (Rate)") +
  xlab(label = "Years") +
  shadow_wake(wake_length = 1.5, alpha = FALSE, wrap = FALSE, size = TRUE)+
  enter_fade() +
  exit_fade()

anim_save("Transition_Born_vs_Death_persons.gif")
