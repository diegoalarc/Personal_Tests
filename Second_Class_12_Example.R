install.packages("fortunes")
library(fortunes)
fortune("memory")
install.packages("cowsay")
library(cowsay)
say("Hello world!")
someone_say_hello <- function(){animal<-sample(names(animals),1)
say(paste("Hello, I´m a",animal,".", collapse = ""), by = animal)}
someone_say_hello()
