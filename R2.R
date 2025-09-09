
getwd()
setwd("C:/Users/Digi/Documents/Custom Office Templates/flowers")


getwd()  #used to know the current location of your working directory or
#current file path of your working folder
setwd()  #used to set or change the working directory or file path, the new folder that
#you intend to use you can copy and paste it using forward slashes or double back slashes


#####Operators in R###
##operators can be implemented on variables or vectors
#Arithmetic operators= does computations  +,-, *, /,%%(mode which gives the remaider), %/%(Quotient), ^
##RElational operator= gives the comparison between two variables or
#vectors and will result to a boolean result ie either TRUE  FALSE    >,<, >=,<=,!=,
x= c(5,10,50)
y= c(20,40,15)

#X<y  TRUE, TRUE, FALSE
X=5
#  Y=6   X!=Y TRUE
##Logical opertors
# they include &,|,!,&&,||
#here non-zero values are considered to be TRUE and zero values to be FALSE
# &  if both are TRUE the result will be TRUE
#|  if both are FALSE the result will be FALSE 
# ! if TRUE it will return FALSE
#! if FALSE it will return TRUE
#&&, ||  are used in compound conditions for comparing conditions
#(a>b)&&(a>c)   if both are TRUE it will return TRUE
#  (a>b)   ||(a>c)        if both are FALSE it will return FALSE but if  one of the two augments 
Is TRUE the result will be TRUE

##Assignment operator    Used to assign values to a variable or a vector
# =,<-, ->, arrow,
# a=5, a<-5,....

##miscellaneous operator=  :, %in%, %*%
# : gives the range a= 4:10  4 5 6 7 8 9 10
# %in% search b=5  b %in% a   it will give you a boolean operator ie TRUE or FALSE
# %*% product of a matrix with its transpose or sum of the product in a vector
x= c(1,2,3)
y= c(4,5,6)
x*y  (4, 10, 18)
x%*%y (4+ 10+ 18)


#reading data

library(tidyverse)
library(readxl)
library(readxl)
dat1 <- read.table(file.choose(), header = TRUE)
setwd("C:/Users/Digi/Documents/Custom Office Templates/flowers")
flowers_N <- read.table(file = "flowers.csv", header = TRUE, sep = ",",stringsAsFactors = TRUE)

flowers_2 <- read.table(file = "flowers.txt", header = TRUE, sep = "\t",stringsAsFactors = TRUE)


dat <- read.csv(file.choose(), header = TRUE)
rm(list=ls())
?msleep
data()

setwd(C:/Users/Digi/Documents/Custom Office Templates/flowers")
getwd()

flowers<-read.csv("flower.csv", header = TRUE);flowers
flowers
View(flowers)
str(flowers)
head(flowers)
getwd()
#########Data wrangling#################
names(flowers)
flowers$height
f_height <- flowers$height
mean(f_height)
summary(flowers)
summary(flowers$height)
flowers[1, 4]
flowers[1:10, 1:4]
flowers[c(1, 5, 12, 30), c(1, 3, 6, 8)]
flowers[-(1:85), -c(4, 7, 8)]
big_flowers <- flowers[flowers$height > 12, ]
flowers[flowers$height >= 6, ]
flowers[flowers$height >= 6,c(1,4,5) ]
flowers[flowers$height >= 6,c("treat", "height", "block", "nitrogen") ]
# and boolean operator
flowers_1<- flowers[flowers$height >= 6 & flowers$nitrogen == "medium" &
                      flowers$treat == "notip", ]

# Or boolean operator
flowers_2<- flowers[flowers$height > 12.3 | flowers$height < 2.2, ]
# Ordered data
height_ord <- flowers[order(flowers$height), ]
leafarea_ord <- flowers[order(flowers$leafarea, decreasing = TRUE), ]
weight_ord <- flowers[order(flowers$weight) , ]
weight_ord1 <- flowers[order(flowers$weight,decreasing = TRUE) , ]
rm(list=ls())

# rbind for rows
df1 <- data.frame(id = 1:4, height = c(120, 150, 132, 122),weight = c(44, 56, 49, 45))
df2 <- data.frame(id = 5:6, height = c(119, 110),weight = c(39, 35))
df_rcomb <- rbind(df1, df2)



## creating variables ###

flowers_df2 <- flowers[order(flowers$nitrogen, flowers$height), ]
flowers_df2$flowers_sqrt <- sqrt(flowers_df2$flowers)
flowers_df2$log10_height <- log10(flowers_df2$height)
str(flowers_df2)
write.table(flowers_df2, file = 'flowers_04_12.txt', col.names = TRUE,
            row.names = FALSE, sep = "\t")
##################Data Visualiation###

plot(flowers$weight)
plot(x = flowers$weight, y = flowers$shootarea)


length(flowers$height)
summary(flowers$height)
hist(flowers$height)
brk <- seq(from = 0, to = 18, by = 1)
hist(flowers$height, main = "petunia height")
hist(flowers$height, breaks = brk, main = "petunia height")

hist(flowers$height, breaks = brk, main = "petunia height", freq=FALSE)

hist(flowers$height, breaks = brk, main = "petunia height", freq = FALSE)

boxplot(flowers$weight, ylab = "weight (g)")
boxplot(weight ~ nitrogen, data = flowers, ylab = "weight (g)", xlab = "nitrogen level")


###plotting using ggplot
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)

str(flowers)
ggplot(aes(x = weight, y = shootarea), data = flowers) +
  geom_point() # Adding a geom to display data as point data
ggplot( data = flowers,aes(x = weight, y = shootarea)) +
  geom_point()
ggplot( data = flowers,aes( weight, shootarea)) +
  geom_point()
ggplot(aes(x = weight, y = shootarea), data = flowers) +
  geom_point() +
  geom_line() # Adding geom_line
ggplot(aes(x = weight, y = shootarea), data = flowers) +
  geom_point() +
  geom_smooth()
ggplot(aes(x = weight, y = shootarea), data = flowers) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) # method and se
ggplot(aes(x = weight, y = shootarea, colour = nitrogen), data = flowers) +
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


names(flowers)

names(flowers_new)

flowers_new <- flowers %>% 
  dplyr::rename(TREAT = treat,
                NITROGEN =nitrogen,
                BLOCK = block) %>% 
  dplyr::select(TREAT, NITROGEN, BLOCK, height, weight) %>% 
  dplyr::filter(height > 5)

View(flowers)
str(flowers)

####STATISTICAL TESTS #####

##t.test
my_data<- flowers %>% 
  select(treat,height) 
##is the average height of flowers with tip more than the average height of flowers with notip   
#Ho: both categories have the same height
#H1: they have different heights
t.test(height~treat, data= my_data)

###using tidyverse
flowers %>% 
  select(height,treat) %>% 
  t.test(height~treat,data=.)

##ANOVA( analysis of variance)

##Q. is the average weight for each categories in nitrogen content the same
#H0  the av weight is the same
#H1   the average weight is different

my_data2 <- flowers %>% 
  select(weight, nitrogen)

mod_1 <- aov(weight~nitrogen, data= my_data2)
mod_1
summary(mod_1)
##using the tidyverse
flowers %>% 
  select(weight, nitrogen) %>% 
  aov(weight~nitrogen, data=.) %>% 
  summary()

###chi-squared test(the goodness of fit test)

#Q. is there a difference in the proportion of flowers which have high, medium and low nitrogen contents
#HO  they are all the same
#H1  there is a diffrence

my_data3 <- flowers %>% 
  select(nitrogen) %>% 
  filter( nitrogen %in% c("high", "medium","low"))

my_table<- table(my_data3)
view(my_table)
chisq.test(my_table)

##using the tidyverse

flowers %>% 
  select(nitrogen) %>% 
  filter(nitrogen %in% c("high", "medium", "low")) %>% 
  table() %>% 
  chisq.test()

###Correlaion test##
# linear relationship between two quantitative variables

cor(flowers$height,flowers$weight)
#weak possitive correlation


#######LINEAR REGRESSION########

#we imagine there is a linear relationship between the independent(explanatory variable) and the outcome(dependent variable)
#in this case we imagine that taller flowers has higher weights

View(flowers)
ggplot(data=flowers, aes(height,weight))+
  geom_point()+
  geom_smooth(method=lm, se=FALSE)+theme_bw()+
  labs(title="Weight explained by height",
       x= "Height of flowers",
       y="weight of flowers")

##create an object
##HO  there is no relationship(no slope/it is a flat line)

model_1 <- lm(weight~height, data=flowers)
summary(model_1)
attributes(model_1)

flowers %>% 
  lm(weight~height, data= .) %>% 
  summary()

new_height<- data.frame(height= c(100,90,80))

predict(model_1, new_height)
round(predict(model_1, new_height))

###using the tidyverse
flowers %>% 
  lm(weight~height, data=.) %>% 
  predict(data.frame(height= c(100,90,80)))

model_2<- lm(flowers~height, data=flowers)

summary(model_2)

###CO2 Dataset
?CO2
View(CO2)
names(CO2)

ggplot(data=CO2, aes(conc, uptake))+
  geom_point()+
  geom_smooth(method=lm, se=F)

model_5 <- lm(uptake~conc, data= CO2)
model_5

summary(model_5)

##trees dataset
view(trees)

ggplot(data=trees, aes(x=Height,y=Volume))+
  geom_point()+
  geom_smooth(method=lm,se=F)

model_3<- lm(Volume~Height, data=trees)
model_3
summary(model_3)

#predictive modelling

new_height2 <- data.frame(Height= 100)

predict(model_3, new_height2)
round(predict(model_3, new_height2))
##using the tidyverse

trees %>% 
  lm(Volume~Height, data=.) %>% 
  predict(data.frame(Height=c(100,90,80))) %>% 
  round(2)

##multiple linear regression

model_4 <- lm(Volume~Height+Girth, data=trees)

summary(model_4)

##More examples on data wrangling or data manipulation


# recoding data using tidyverse

starwars
View(starwars)
glimpse(starwars)

starwars %>% 
  select(name, gender) %>% 
  mutate(gender=recode(gender,
                       "masculine"= "M",
                       "feminine"="F") ) %>% 
  View()

stw2 <- starwars %>% 
  select(name, height, mass, gender,hair_color) %>% 
  na.omit() %>% 
  rename(WEIGHT= mass) %>% 
  mutate(height= height/ 100) %>% 
  filter(gender=="masculine" | gender== "feminine") %>%          # filter(gender %in% c("masculine", "feminine"))
  mutate(gender=recode(gender, masculine= "M", feminine="F")) %>% 
  mutate(size= height > 1 & WEIGHT> 80) %>% View()

stw2 <- starwars %>% 
  select(name, height, mass, gender,hair_color) %>% 
  na.omit() %>% 
  rename(WEIGHT= mass) %>% 
  mutate(height= height/ 100) %>% 
  filter(gender=="masculine" | gender== "feminine")  %>%         # filter(gender %in% c("masculine", "feminine"))
  mutate(gender=recode(gender, masculine= "M", feminine="F")) %>% 
  mutate(size= height > 1 & WEIGHT> 80,
         size= if_else(size== TRUE, "big", "small")) %>% 
  View()
#filter observations
unique(starwars$hair_color)
starwars %>% 
  select(name, height, ends_with("color")) %>%
  filter(hair_color %in% c("blond", "brown")& height< 170) %>% 
  View()

#select variables
starwars
View(starwars)

starwars %>% 
  select(name, height,ends_with("color")) %>%    
  names()
starwars %>% 
  select(name,gender,mass, starts_with("hair")) %>% 
  View()
starwars %>% 
  select(name, gender, species,everything()) %>% 
  View()


#missing data
?mean
mean(starwars$height)
mean(starwars$height, na.rm = TRUE)

starwars %>% 
  select(name,gender, hair_color, height) %>% 
  na.omit()
starwars %>% 
  select(name,species,gender, height) %>%
  filter(complete.cases(.)) %>% View()

starwars %>% 
  select(name,species,gender, height) %>%
  filter(!complete.cases(.)) %>% 
  View()

starwars %>%
  select(name,gender, hair_color, height) %>% 
  drop_na(height) %>%
  View()

starwars %>%
  select(name,gender, hair_color, height) %>% 
  na.omit(height) %>%
  View()



#duplicates

Names<- c("simon","john","stephen", "simon")
Age <- c( 40, 25, 44, 40)
friends <-data.frame(Names, Age)
duplicated(friends)
friends[duplicated(friends), ]
friends[!duplicated(friends), ]

friends %>% distinct() %>%  View() #using tidyverse


#########DATA VISUALIZATION#########
###ggplot for plots and graphs

library(tidyverse)
data()
?BOD   #BIOCHEMICAL OXYGEN DEMAND
BOD
View(BOD)
glimpse(BOD)
ggplot(data = BOD, mapping = aes(x= Time, y= demand))+ 
  geom_point(size = 5)+
  geom_line(color= "blue")

ggplot(BOD, aes(Time, demand))+
  geom_point(size= 5)+
  geom_line(colour= "red")

? CO2     #Carbon Dioxide Uptake in Grass Plants
CO2
View(CO2)
str(CO2)
glimpse(CO2)
names(CO2)

CO2 %>% 
  ggplot(aes(conc, uptake, colour= Treatment))+
  geom_point(size =5, alpha=0.6)+
  geom_smooth(method =lm, se= F)+ 
  facet_wrap(~Type)+
  labs(title = "concentration of CO2")+
  theme_bw()

CO2 %>% 
  ggplot(aes(Treatment, uptake))+
  geom_boxplot()+
  geom_point(aes(size= conc, colour=Plant), alpha=0.5)+
  facet_wrap(~Type)+
  theme_bw()+labs(title = "chilled vs nonchilled")

mpg
?mpg
View(mpg)

mpg %>% 
  # filter(cty <20) %>% 
  ggplot(aes(displ, cty))+
  geom_point(aes(colour= drv, size= trans),alpha= 0.5)+
  geom_smooth(method = "lm", se = F)+
  facet_wrap(~year)+
  labs(title = "Fuel efficiency", x= "Engine size",y="miles per gallon in the city")

##  create  scatter plots
View(mpg)
?mpg
names(mpg)
mpg %>%
  # filter(hwy <30) %>% 
  ggplot(aes(x= displ, y= hwy, colour= drv))+
  geom_point()+
  geom_smooth(method=lm, se= F)+
  #  facet_wrap(~drv)+
  labs(x="Engine displacement", y="MPG on the highway",
       title = "Fuel efficiency")+
  theme_bw()
mpg %>%
  filter(hwy <30) %>% 
  ggplot(aes(x= displ, y= hwy))+
  geom_point(aes(colour=drv))+    ####colour aesthetics to only be implicated in the points
  geom_smooth(method=lm, se= F)+
  #  facet_wrap(~drv)+
  labs(x="Engine displacement", y="MPG on the highway",
       title = "Fuel efficiency")+
  theme_bw()


##bar charts and Histograms 

msleep      ######mammals sleep dataset
?msleep
names(msleep)
##single categorical
msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(vore))+
  geom_bar(fill= "red")+     
  theme_bw()+
  labs(title= "Observation per order")

##single numeric

msleep %>% 
  ggplot(aes(awake))+
  geom_histogram(binwidth = 2, fill= "blue", colour="red")+
  theme_minimal()+
  labs(x= "Total sleep",
       y= "frequency",
       title="Histogram of totaltime spent awake ")

###create a scatter plot using R

?msleep

###two or more numeric variables
msleep %>% 
  filter(bodywt <1.8) %>% 
  ggplot(aes(bodywt, brainwt))+
  geom_point(aes(colour= sleep_total, 
                 size= awake))+
  geom_smooth(method = lm, se= F)+
  labs(x= "Body weight",
       y= "Brain weight",
       title="Brain and body weight")

###creating a boxplot using R programming
msleep
# A numeric and a categorical
msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(vore, sleep_total))+
  geom_boxplot(aes(colour=vore))+theme_bw()


msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(sleep_total))+
  geom_density(aes(colour=vore, fill= vore))+
  facet_wrap(~vore)+
  theme_bw()

msleep %>% 
  drop_na(vore) %>% 
  ggplot(aes(sleep_total))+
  geom_histogram(aes(colour=vore, fill= vore))+
  facet_wrap(~vore)+
  theme_bw()
