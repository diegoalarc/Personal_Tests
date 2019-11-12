vector<-c(2,4,6,8,10,12)
vector
vector1<-c(1,3,5,7,9,11)
vector1
df<-data.frame(vector,vector1)
df
df[2,]
df[,3]
df[,2]
df
rownames(df)<-LETTERS[1:6]
df  
install.packages("ggplot2")
library(ggplot2)
x11()
x<-data.frame(x=1,y=1,label="ggplot instroduction \n@ EAGLE")
ggplot(data=x,aes(x=x,y=y))+geom_text(aes(label=label),size=15)


install.packages("ggplot2")
x1<-rnorm(1000,0,1)
x2<-rnorm(1000,5,10)
x3<-rep(c("catA","catB","catB","catC","catC","catC"),200)[1:1000]
x4<-factor(rep(c("yes","no"),500))
df<-data.frame(a=x1,b=x2,c=x3,d=x4)
library(ggplot2)
x11()
ggplot(df, aes(a,b))+geom_point()


x11()
ggplot(df, aes(a,b,color=c))+geom_point()

x11()
ggplot(df, aes(a,b,color=c))+geom_point(alpha =.5) #alpha = transparencia

x11()
ggplot(df, aes(a,b,color=c))+geom_point(alpha=.5)+labs(title = "first plot",x = "x axis \n and a new line")
  
x11()
ggplot(df, aes(a))+geom_histogram(color="white") #histograma...barras

x11()
ggplot(df, aes(c,color=c)) + geom_point(stat = "count" , size = 4)

ggplot(df) + geom_bar(aes(c)) + coord_flip()

ggplot(df, aes(d,fill=c))+
geom_bar(position="dodge")
+ scale_fill_grey()

ggplot(df, aes(d,a)) + geom_boxplot()

ggplot(df, aes(d,a)) + geom_boxplot()+ geom_jitter()

ggplot(df, aes(d,a)) + geom_boxplot()+ geom_jitter(alpha=.5, width = .3,color="blue")

ggplot(df, aes(d,a)) + geom_boxplot(aes(group=cut_width(a,0.5)), outliner.alpha=0.1)+ geom_jitter(alpha=.5, width =0.02, size=.7,color="blue")

ggplot(df, aes(c))+geom_bar()+facet_grid(d~.)

ggplot(df, aes(a,b))+geom_point(size=1)+ geom_density2d()

install.packages("hexbin")
library(hexbin)
ggplot(df, aes(a,b))+geom_hex(bins=30)

a<-ggplot()+geom_point(data=df,aes(x,y,colour=z))
a + theme_bw()

install.packages("Steigerwald")

