install.packages("patchwork")

library(tidyverse)
library(vroom)
library(DataExplorer)
library(GGally)
library(patchwork)

bike <- vroom("./train.csv")

glimpse(bike)

plot_correlation(bike)
#count and registered are highly correlated
#casual and count are also highly correlated
#use one of temp or atemp. Make sure that you don't use both

plot_histogram(bike)
#season is evenly categorical
#weather is categorical but is decreasing over category (almost monoe in four)
#atemp, humidity, and temp might be normal
#casual, count, widspeed, and registered are not (0 mode only positive)

plot_missing(bike)
#We have all of the data! (Thank goodness)

bike_ss <- bike %>% select(season, count)
ggpairs(bike_ss)

d <- ggplot() + geom_boxplot(data = bike, mapping = aes(y = count, x = as.factor(season)))
c <- ggplot() + geom_point(data = bike, mapping = aes(x = atemp, y = count))
ggplot() + geom_point(data = bike, mapping = aes(x = weather, y = atemp))
b <- ggplot() + geom_boxplot(data = bike, mapping = aes(y = count, x = as.factor(holiday)))

#final grouping of four                    
(plot_correlation(bike) + b) / (c + d)
