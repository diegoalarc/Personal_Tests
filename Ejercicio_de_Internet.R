install.packages("ISwR")
library(ISwR)
energy
class(energy)
exp.lean <- energy$expend[energy$stature =="lean"]
exp.lean
exp.obese <- energy$expend[energy$stature =="obese"]
exp.obese
l <- split(energy$expend, energy$stature) #convierte objeto en una lista y genera 2 en funcion a la primera variable la cual es expend
l
class(l)
k <- split(energy$stature, energy$expend)
k
