#Load library
library(data.table)
library(ggplot2)
library(gridExtra)
library(ggrepel) # for repel
library(plyr) # for count

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

#Drop iris dataset
rm(DT)

#ggplot2
#Load data
setwd("C:/Users/Joni boy/Documents/Github/ggplot2")
df <- mtcars
  DT <- data.table(df)
attach(DT)
  
#5 min data exploration
str(DT)
head(DT)  
summary(DT)



p <- ggplot(DT, aes(x= hp, y= mpg))
#Scatterplot of hp vs mpg
p + geom_point()

p + geom_point(colour = "blue")

#boxplot overlayed on scatterplot of qsec vs disp
b <- ggplot(DT, aes(qsec,disp))
b +geom_boxplot() + geom_point()

#Changing Args
ggplot(DT, aes(mpg)) + geom_histogram(stat="bin", binwidth = 1)

p + geom_point() +geom_smooth(method = "lm")
p + geom_point() +geom_smooth(method = "auto")
p + geom_point() +geom_smooth(span = .4)

#Scales- For controlling aesthetic mapping
p3 <- ggplot(DT,
             aes(x = cyl,
                 y = mpg)) +
      theme(legend.position = "top",
            axis.text=element_text(size = 10))

p4 <- p3 +geom_point(aes(color = am),
                     alpha = 0.5,
                     size = 1.5,
                     position = position_jitter(width = 0.25, height = 0))

#modifying breaks and labels

p4 + scale_color_continuous(name = "Auto/Manual",
                         breaks = c(0.0,1.0),
                         labels = c(0,1))

#Change color to low and high value
p4 + scale_color_continuous(name = "Auto/Manual",
                            breaks = c(0.0,1.0),
                            labels = c(0,1),
                            low = "red",
                            high = "green")

#Muted did not work
p4 + scale_color_continuous (name = "Auto/Manual",
                            breaks = c(0.0,1.0),
                            labels = c(0,1),
                            low = muted("red"),
                            high = muted("green"))

# Generating housing price data
housing <- data.table(house.value = (25000 + cumsum(rep(c(30, 2, 50), 100)))[1:100], year = (1990:2010))
housing$state <- sample(c("FL","CA","TX","GA","NY","NJ"))
attach(housing)
#house.value <- (25000 + cumsum(rep(c(30, 2, 50), 100)))[1:100]

p5 <- ggplot(housing, aes(year,house.value)) 
p5 + geom_line(aes(color= state))


$Facetting 
p5 <- p5 + geom_line(aes(color= state)) + facet_wrap(~state,ncol = 5)

#Themes
p5 + theme_light()

p5 +theme_minimal() + theme(text = element_text(colour = "blue"))

theme_new <- theme_bw() + theme(plot.background = element_rect(size = 1,colour = "red" ,fill = "sepia"),
        text=element_text(size = 12, family = "Serif", color = "ivory"),
        axis.text.y = element_text(colour = "purple"),
        axis.text.x = element_text(colour = "red"),
        panel.background = element_rect(fill = "pink"),
        strip.background = element_rect(fill = "orange"))


#Creating the final graph

#Data prep
df <- read.csv("http://tutorials.iq.harvard.edu/R/Rgraphics/dataSets/EconomistData.csv")
data <- data.table(df)
attach(data)
g1 <- ggplot(data,aes(CPI,HDI,color = Region))
g1 + geom_point()

#Adding trendline
g2 <- g1 + geom_smooth(aes(group = 1),
              method = "lm",
              formula = y ~ log(x),
              se = FALSE,
              color = "red") +
  geom_point() 

#Change points to open points
g2 + geom_point(shape = 1, size = 4)

#Making point more dense
g3 <- g2 + geom_point(shape = 1, size = 3.5) +
  geom_point(shape = 1, size = 2) +
  geom_point(shape = 1, size = 1.5)

#Labelling points
pointsToLabel <- (c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                    "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                    "India", "Italy", "China", "South Africa", "Spane",
                    "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                    "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                    "New Zealand", "Singapore","United Arab Emirates"))

g4 <- g3 +
  geom_text(aes(label = Country),
            color = "gray20",
            data = subset(data, Country %in% pointsToLabel))

#Repel labels
g3 + geom_text_repel(aes(label = Country),
                      color = "gray20",
                      data = subset(data, Country %in% pointsToLabel),
                      force = 10)

#Changin Region labels and order
levels(data$Region)
data$Region <- factor(data$Region,
                      levels = c("EU W. Europe",
                                "Americas",
                                 "Asia Pacific",
                                 "East EU Cemt Asia",
                                 "MENA",
                                 "SSA"),
                      labels = c("OECD",
                                 "Americas",
                                 "Asia &\nOceania",
                                 "Central &\nEastern Europe",
                                 "Middle East &\nnorth Africa",
                                 "Sub-Saharan\nAfrica"))
g4$data <- data


#Adding titile and formatting axes
library(grid)
g5 <- g4 +
  scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                     limits = c(1,10),
                     breaks = c(1:10)) +
  scale_y_continuous(name = "Human development index, 2011 (1 = Best)",
                     limits = c(.1,1.0),
                     breaks = seq(.1,1, by = .1)) +
  scale_color_manual(name = "",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F"))+
    ggtitle("Corruption and Human development")

#Themes
g6<- g5 +
  theme_minimal() +
  theme(text = element_text(color = "gray20"),
        legend.position = c("top"),
        legend.direction = "horizontal",
        legend.justification = 0.1,
        legend.text = element_text(size = 11,color = "gray10"),
        axis.text = element_text(face = "italic"),
        axis.title.x = element_text(vjust = -1),
        axis.title.y = element_text(vjust = 2),
        axis.ticks.y = element_blank(),
        axis.line = element_line(color = "gray40", size = 0.5),
        axis.line.y = element_blank(),
        panel.grid.major = element_line(color = "gray50", size = 0.5),
        panel.grid.major.x = element_blank()
  )

