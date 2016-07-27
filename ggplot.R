#Load library
library(data.table)
library(ggplot2)
library(gridExtra)

#Load data
set.seed(20L)
DT <- data.table(iris)
attach(DT)

#Data exploration with qplot
str(DT)

#Sepal length of differnt species plants against Sepal width
qplot(x = Sepal.Length, y = Sepal.Width , data = DT, color = Species)

# Adding smooth geom
qplot(x = Sepal.Length, y = Sepal.Width , data = DT, geom = c("point","smooth"))

#Histogram of sepal length frequency 
qplot(Sepal.Length, data = DT, fill = Species)

#Scatterplots with facets
par(mfrow=c(2,2))
plot1 <- qplot(Sepal.Length,Sepal.Width, data = DT, facets = .~ Species,color = Species)
plot2 <- qplot(Petal.Length,Petal.Width, data = DT, facets = .~ Species,color = Species)
grid.arrange(plot1, plot2)

