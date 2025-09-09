

getwd()
setwd("C:/Users/Digi/Documents/Custom Office Templates/heart")
hear<-read.csv("HeartDisease.csv", header = FALSE);hear





###plotting using ggplot
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

str(HeartDisease)
ggplot(aes(x = weight, y = shootarea), data = hear) +
  geom_point() # Adding a geom to display data as point data
ggplot( data = hear,aes(x = weight, y = shootarea)) +
  geom_point()
ggplot( data = hear,aes( weight, shootarea)) +
  geom_point()
ggplot(aes(x = weight, y = shootarea), data = hear) +
  geom_point() +
  geom_line() # Adding geom_line
ggplot(aes(x = weight, y = shootarea), data = hear) +
  geom_point() +
  geom_smooth()
ggplot(aes(x = weight, y = shootarea), data = hear) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # method and se
ggplot(aes(x = weight, y = shootarea, colour = nitrogen), data = hear) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  # Adding "block" to formula
  facet_wrap(~ treat + block)
#ggplot(aes(x = weight, y = shootarea, colour = nitrogen), data = flowers) +
geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~nitrogen)
# ggplot(aes(x = weight, y = shootarea, colour = nitrogen), data = flowers) +
geom_point() +
  geom_smooth(method = "lm", se = FALSE)+
  facet_wrap(~block)

library(patchwork)
ggplot(aes(x = weight, y = shootarea, colour = nitrogen), data = flowers) +
  geom_point(aes(shape = nitrogen), size = 2, alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~nitrogen)+
  # Adding layers for x and y labels
  xlab("Weight of flower (g)") +
  ylab("Area of shoot (cm^2)")
ggplot(flowers) +
  geom_boxplot(aes(y = flowers, x = nitrogen)) +
  labs(y = "Number of Flowers", x = "Nitrogen Concentration")
