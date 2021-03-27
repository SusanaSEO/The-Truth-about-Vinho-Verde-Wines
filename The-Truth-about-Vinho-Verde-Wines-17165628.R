#SOURCES
#######################################################################################################
#######################################################################################################
#wine-quality-red https://www.openml.org/d/40691
#wine-quality-white https://www.openml.org/d/40498

#LIBRARIES NEEDED
#######################################################################################################
#######################################################################################################
library(tidyverse) #tidiversey contains other packages as
library(tidyr)
library(tibble)
library(ggplot2) #graphics
library(readr) #library to read csv
library(dplyr)
library(openintro)
library(tools)
library(GGally)
library(forcats)
library(ggpubr)
library(mvShapiroTest)
library(gridExtra)
library(moments)
library(cowplot) #add different plots together
library(nortest) # to be able to perform Anderson-Darling normality test
library(rstatix)
library(FSA) #Dunn Test
library(ggcorrplot) #correlation matrix
library(rpart)
library(rpart.plot)
library(caret)
library(e1071)
library(randomForest)
library(gdata)
library(DiagrammeR)

#IMPORTING DATA SETS
#######################################################################################################
#######################################################################################################
redwine <- read.csv("wine-quality-red.csv", head=TRUE, sep=",")
whitewine <- read.csv("wine-quality-white.csv", head=TRUE, sep=",")

#DATA SET EXPLORATION
#######################################################################################################
#######################################################################################################

#Check the number of rows of each data set
nrow(redwine)
nrow(whitewine)

#Check first rows of each of the data sets
head(redwine)
head(whitewine)

#Check the last rows of each of the data sets
tail(redwine)
tail(whitewine)

#Information: Number of observations. Number of valiables.
#Variables name and class, as well as some of the data for each of them.
str(redwine) 
str(whitewine)

#Check only names of variables for each of the data sets
names(redwine) #class variable will be more meaningful with the name quality
names(whitewine) #We will need to change the names so are the same as in the other data set

#Check if the data sets has any missing value
redwine[!complete.cases(redwine), ] #no missing rows
whitewine[!complete.cases(whitewine), ] #no missing rows



#CLEANING DATA / DATA TRANSFORMATION
#######################################################################################################
#######################################################################################################
#Renaming both files with same variable names
redwine_renamed <- redwine %>% rename("quality" = "class",
                                      "ph" = "pH")
whitewine_renamed <- whitewine %>% rename( 
  "fixed_acidity" = "V1",
  "volatile_acidity" = "V2",
  "citric_acid" = "V3",
  "residual_sugar" = "V4",
  "chlorides" = "V5",
  "free_sulfur_dioxide" = "V6",
  "total_sulfur_dioxide" = "V7",
  "density" = "V8",
  "ph" = "V9",
  "sulphates" = "V10",
  "alcohol" = "V11",
  "quality" = "Class")

#Check the names of variables again to ensure they were changed
names(redwine_renamed)
names(whitewine_renamed)



#Changing "Quality" from integer to factor with ordered levels 
#######################################################################################################
#Checking current class of variable
class(redwine_renamed$quality)
#Changing quality from character to factor
redwine_renamed <- mutate_at(redwine_renamed,vars(quality), as.factor)
#Adding levels to factor
redwine_renamed$quality <- factor(redwine_renamed$quality, levels = c("1","2","3","4","5","6","7","8","9","10"))
#Check if the variable is now a factor with levels
class(redwine_renamed$quality)
levels(redwine_renamed$quality)


#Checking current class of variable
class(whitewine_renamed$quality)
#Changing quality from character to factor
whitewine_renamed <- mutate_at(whitewine_renamed,vars(quality), as.factor)
#Adding levels to factor
whitewine_renamed$quality <- factor(whitewine_renamed$quality, levels = c("1","2","3","4","5","6","7","8","9","10"))
#Check if the variable is now a factor with levels
class(whitewine_renamed$quality)
levels(whitewine_renamed$quality)





#Creating new variable "Type" which will include if the wine is red or white and changing it from character to factor
#######################################################################################################
#New variable for red wine
redwine_renamed["type"] = "red"
#Checking class of new variable
class(redwine_renamed$type)
#Change from character to factor
redwine_renamed <- mutate_at(redwine_renamed, vars(type), as.factor)
#Check if the variable is now a factor
class(redwine_renamed$type)

#New variable for white wine
whitewine_renamed["type"] = "white"
#checking class of new variable
class(whitewine_renamed$type)
#Change from character to factor
whitewine_renamed <- mutate_at(whitewine_renamed, vars(type), as.factor)

#Check if the variable is now a factor
class(whitewine_renamed$type)


#Creating new valiable "Type by Sugar Level", which will help understanding the data set better
#######################################################################################################
#Following the guidance of https://en.wikipedia.org/wiki/Sweetness_of_wine
#Creating new variable Type by Sugar Level in red wine set
redwine_renamed <- redwine_renamed %>%
  add_column(type_by_sugar_level = ifelse (redwine_renamed$residual_sugar <= 4 ,"Dry",
                                           ifelse (redwine_renamed$residual_sugar >4 &  redwine_renamed$residual_sugar <=12,"Medium Dry",
                                                   ifelse (redwine_renamed$residual_sugar >12 &  redwine_renamed$residual_sugar <=45,"Medium","Sweet"))))
#Chaning new variable from character to factor
redwine_renamed <- mutate_at(redwine_renamed, vars(type_by_sugar_level), as.factor)
#Adding ordered levels to factor variable
redwine_renamed$type_by_sugar_level <- factor(redwine_renamed$type_by_sugar_level, levels = c("Dry","Medium Dry","Medium","Sweet"))
#Check if the variable is now a factor with levels
class(redwine_renamed$type_by_sugar_level)
levels(redwine_renamed$type_by_sugar_level)
#Checking number of samples inside each of the newest created variable
redwine_renamed %>% 
  group_by(redwine_renamed$type_by_sugar_level) %>%
  summarise(n = n())






#Creating new variable Type by Sugar Level in white wine set
whitewine_renamed <- whitewine_renamed %>%
  add_column(type_by_sugar_level =ifelse (whitewine_renamed$residual_sugar <=4,"Dry",
                                          ifelse (whitewine_renamed$residual_sugar >4 &  whitewine_renamed$residual_sugar <=12,"Medium Dry",
                                                  ifelse (whitewine_renamed$residual_sugar >12 &  whitewine_renamed$residual_sugar <=45,"Medium","Sweet"))))
#Chaning new variable from character to factor
whitewine_renamed <- mutate_at(whitewine_renamed, vars(type_by_sugar_level), as.factor)
#Adding ordered levels to factor variable
whitewine_renamed$type_by_sugar_level <- factor(whitewine_renamed$type_by_sugar_level, levels = c("Dry","Medium Dry","Medium","Sweet"))
#Check if the variable is now a factor with levels
class(whitewine_renamed$type_by_sugar_level)
levels(whitewine_renamed$type_by_sugar_level)
#Checking number of samples inside each of the newest created variable
whitewine_renamed %>% 
  group_by(whitewine_renamed$type_by_sugar_level) %>%
  summarise(n = n())





#Creating new valiable numerical variable "Total Acidity", which will help understanding the data set better
#######################################################################################################
#Creating new variable Total Acidity for red wine
redwine_renamed <- redwine_renamed %>%
  mutate(total_acitity = fixed_acidity + volatile_acidity)
#Checking the variable was created
head(redwine_renamed)
#Checking the class is numeric
class(redwine_renamed$total_acitity)


#Creating new variable Total Acidity for white wine
whitewine_renamed <- whitewine_renamed %>%
  mutate(total_acitity = fixed_acidity + volatile_acidity)
#Checking the variable was created
head(whitewine_renamed) 
#Checking the class is numeric
class(whitewine_renamed$total_acitity)



#Creating new variable "PH Level" as factor with levels
#######################################################################################################
#Creating new variable for red wine
redwine_renamed <- redwine_renamed %>%
  add_column(ph_level = ifelse (redwine_renamed$ph <= 2.8 ,"PH<=2.8",
                                ifelse (redwine_renamed$ph >2.8 &  redwine_renamed$ph <=3,"2.8>PH<=3",
                                        ifelse (redwine_renamed$ph >3 &  redwine_renamed$ph <=3.5,"3>PH<=3.5",
                                                ifelse (redwine_renamed$ph >3.5 &  redwine_renamed$ph <=4,"3>PH<=4", "PH>4")))))
#Checking class for new variable
class(redwine_renamed$ph_level)
#Change from character to factor
redwine_renamed <- mutate_at(redwine_renamed, vars(ph_level), as.factor)
#Add levels to the factor
redwine_renamed$ph_level <- factor(redwine_renamed$ph_level, levels = c("PH<=2.8","2.8>PH<=3","3>PH<=3.5","3>PH<=4","PH>4"))
#Check class again to confirm is a factor
class(redwine_renamed$ph_level)
#Check levels of factor
levels(redwine_renamed$ph_level)
#Check number of samples for each type of PH category
redwine_renamed %>% 
  group_by(redwine_renamed$ph_level) %>%
  summarise(n = n())






#Creating new variable for white wine
whitewine_renamed <- whitewine_renamed %>%
  add_column(ph_level = ifelse (whitewine_renamed$ph <= 2.8 ,"PH<=2.8",
                                ifelse (whitewine_renamed$ph >2.8 &  whitewine_renamed$ph <=3,"2.8>PH<=3",
                                        ifelse (whitewine_renamed$ph >3 &  whitewine_renamed$ph <=3.5,"3>PH<=3.5",
                                                ifelse (whitewine_renamed$ph >3.5 &  whitewine_renamed$ph <=4,"3>PH<=4", "PH>4")))))
#Checking class for new variable
class(whitewine_renamed$ph_level)
#Change from character to factor
whitewine_renamed <- mutate_at(whitewine_renamed, vars(ph_level), as.factor)
#Add levels to the factor
whitewine_renamed$ph_level <- factor(whitewine_renamed$ph_level, levels = c("PH<=2.8","2.8>PH<=3","3>PH<=3.5","3>PH<=4","PH>4"))
#Check class again to confirm is a factor
class(whitewine_renamed$ph_level)
#Check levels of factor
levels(whitewine_renamed$ph_level)
#Check number of samples for each type of PH category
whitewine_renamed %>% 
  group_by(whitewine_renamed$ph_level) %>%
  summarise(n = n())


#Creating new variable "Alcohol Level" as factor with levels
#######################################################################################################
#Following https://www.realsimple.com/holidays-entertaining/entertaining/food-drink/alcohol-content-wine
#Creating new variable for red wine
redwine_renamed <- redwine_renamed %>%
  add_column(alcohol_level = ifelse (redwine_renamed$alcohol <= 12.5 ,"Very Low",
                                ifelse (redwine_renamed$alcohol >12.5 &  redwine_renamed$alcohol <=13.5,"Moderately Low",
                                        ifelse (redwine_renamed$alcohol >13.5 &  redwine_renamed$alcohol <=14.5,"High", "Very High"))))
#Checking class for new variable
class(redwine_renamed$alcohol_level)
#Change from character to factor
redwine_renamed <- mutate_at(redwine_renamed, vars(alcohol_level), as.factor)
#Add levels to the factor
redwine_renamed$alcohol_level <- factor(redwine_renamed$alcohol_level, levels = c("Very Low","Moderately Low","High","Very High"))
#Check class again to confirm is a factor
class(redwine_renamed$alcohol_level)
#Check levels of factor
levels(redwine_renamed$alcohol_level)
#Check number of samples for each type of PH category
redwine_renamed %>% 
  group_by(redwine_renamed$alcohol_level) %>%
  summarise(n = n())






#Creating new variable for white wine
whitewine_renamed <- whitewine_renamed %>%
  add_column(alcohol_level = ifelse (whitewine_renamed$alcohol <= 12.5 ,"Very Low",
                                     ifelse (whitewine_renamed$alcohol >12.5 &  whitewine_renamed$alcohol <=13.5,"Moderately Low",
                                             ifelse (whitewine_renamed$alcohol >13.5 &  whitewine_renamed$alcohol <=14.5,"High", "Very High"))))
#Checking class for new variable
class(whitewine_renamed$alcohol_level)
#Change from character to factor
whitewine_renamed <- mutate_at(whitewine_renamed, vars(alcohol_level), as.factor)
#Add levels to the factor
whitewine_renamed$alcohol_level <- factor(whitewine_renamed$alcohol_level, levels = c("Very Low","Moderately Low","High","Very High"))
#Check class again to confirm is a factor
class(whitewine_renamed$alcohol_level)
#Check levels of factor
levels(whitewine_renamed$alcohol_level)
#Check number of samples for each type of PH category
whitewine_renamed %>% 
  group_by(whitewine_renamed$alcohol_level) %>%
  summarise(n = n())



#CLEAN DATA CHECK
#######################################################################################################
#######################################################################################################
str(redwine_renamed)
str(whitewine_renamed)

#Summary as all the variables are numerical, informs about Minimun Value, 1st Quartile, Median, Mean, 3rd Quartile and Maximum Value for each of the variables.
summary(redwine_renamed)
summary(whitewine_renamed)





#COMBINE DATA SETS
#######################################################################################################
#######################################################################################################
#Combine both data sets
head(redwine_renamed)
head(whitewine_renamed)
winequality <- rbind(redwine_renamed,whitewine_renamed)
as.data.frame(winequality)
head(winequality)
tail(winequality)




#ABOUT THE DATA
#######################################################################################################
#######################################################################################################

#Create bar plot for red and white wines based on "Quality"
data_quality <- ggplot(winequality, aes(x = quality)) +
  labs(title = "Quality of wines", x = "Quality",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))




#Create bar plot for red and white wines based on "Total Acidity"
data_total_acidity <- ggplot(winequality, aes(x = total_acitity)) +
  labs(title = "Total Acidity (g/l) in wines",x = "Total Acidity (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5, binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "Volatile Acidity"
data_volatile_acidity <- ggplot(winequality, aes(x = volatile_acidity)) +
  labs(title = "Volatile Acidity (g/l) in wines",x = "Volatile Acidity (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "Fixed Acidity"
data_fixed_acidity <- ggplot(winequality, aes(x = fixed_acidity)) +
  labs(title = "Fixed Acidity (g/l) in wines",x = "Fixed Acidity (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "PH Level"
data_ph_level <- ggplot(winequality, aes(x = ph_level)) +
  labs(title = "pH Level in wines", x="pH Level",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5)+
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))




#Create bar plot for red and white wines based on "Sulphates"
data_sulphates <- ggplot(winequality, aes(x = sulphates)) +
  labs(title = "Sulphates (g/l) in wines",x = "Sulphates (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "Total Sulfur Dioxide"
data_total_sulfur_dioxide <- ggplot(winequality, aes(x = total_sulfur_dioxide)) +
  labs(title = "Total Sulfur Dioxide in wines",x = "Total Sulfur Dioxide (mg/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "Free Sulfur Dioxide"
data_free_sulfur_dioxide <- ggplot(winequality, aes(x = free_sulfur_dioxide)) +
  labs(title = "Free Sulfur Dioxide (mg/l) in wines",x = "Free Sulfur Dioxide (mg/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))





#Create bar plot for red and white wines based on "Alcohol Level"
data_alcohol<- ggplot(winequality, aes(x = alcohol_level)) +
  labs(title = "Alcohol Level (%) in wines", x="Alcohol Level (%)",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5)+
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "Density"
data_density <- ggplot(winequality, aes(x = density)) +
  labs(title = "Density (g/ml) in wines", x = "Density (g/ml)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.001) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "Residual Sugar Level"
data_sugar_level <- ggplot(winequality, aes(x = type_by_sugar_level)) +
  labs(title = "Type of wine by Sugar Level", x = "Type by Sugar Level",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for red and white wines based on "Chlorides"
data_chlorides <- ggplot(winequality, aes(x = chlorides)) +
  labs(title = "Chlorides in wines", x = "Chlorides",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))




#After creating all the different bar plots for each of the variables I add some of them together for better visualisation
data_quality

plot_grid(data_total_acidity,data_ph_level, nrow = 1)

plot_grid(data_fixed_acidity, data_volatile_acidity, nrow = 1)

data_sulphates

plot_grid(data_total_sulfur_dioxide,data_free_sulfur_dioxide)

plot_grid(data_sugar_level,data_alcohol, nrow = 1)

plot_grid(data_density,data_chlorides, nrow = 1)





#CREATING SUBSET WITH ONLY DRY WINES
#######################################################################################################
#######################################################################################################
#Subset the combined data set and chose just dry wines
winequality_dry <- subset(winequality, winequality$type_by_sugar_level =="Dry")

#Subset the red wine data set
redwine_renamed_dry <- subset(redwine_renamed, redwine_renamed$type_by_sugar_level == "Dry")

#Subset the red wine data set
whitewine_renamed_dry <- subset(whitewine_renamed, whitewine_renamed$type_by_sugar_level == "Dry")


#information about new data sets
str(winequality_dry)
str(redwine_renamed_dry)
str(whitewine_renamed_dry)






#ABOUT DRY VINHO VERDE
#######################################################################################################
#######################################################################################################


#Create bar plot for dry red and white wines based on "Quality"
data_quality_dry <- ggplot(winequality_dry, aes(x = quality)) +
  labs(title = "Quality of dry wines", x = "Quality",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))




#Create bar plot for dry red and white wines based on "Total Acidity"
data_total_acidity_dry <- ggplot(winequality_dry, aes(x = total_acitity)) +
  labs(title = "Total Acidity (g/l) in dry wines",x = "Total Acidity (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5, binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "Volatile Acidity"
data_volatile_acidity_dry <- ggplot(winequality_dry, aes(x = volatile_acidity)) +
  labs(title = "Volatile Acidity (g/l) in dry wines",x = "Volatile Acidity (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "Fixed Acidity"
data_fixed_acidity_dry <- ggplot(winequality_dry, aes(x = fixed_acidity)) +
  labs(title = "Fixed Acidity (g/l) in dry wines",x = "Fixed Acidity (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "PH Level"
data_ph_level_dry <- ggplot(winequality_dry, aes(x = ph_level)) +
  labs(title = "pH Level in wines", x="pH Level",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5)+
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))




#Create bar plot for dry red and white wines based on "Sulphates"
data_sulphates_dry <- ggplot(winequality_dry, aes(x = sulphates)) +
  labs(title = "Sulphates (g/l) in dry wines",x = "Sulphates (g/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "Total Sulfur Dioxide"
data_total_sulfur_dioxide_dry <- ggplot(winequality_dry, aes(x = total_sulfur_dioxide)) +
  labs(title = "Total Sulfur Dioxide in dry wines",x = "Total Sulfur Dioxide (mg/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "Free Sulfur Dioxide"
data_free_sulfur_dioxide_dry <- ggplot(winequality_dry, aes(x = free_sulfur_dioxide)) +
  labs(title = "Free Sulfur Dioxide (mg/l) in dry wines",x = "Free Sulfur Dioxide (mg/l)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))





#Create bar plot for dry red and white wines based on "Alcohol Level"
data_alcohol_dry<- ggplot(winequality_dry, aes(x = alcohol_level)) +
  labs(title = "Alcohol Level (%) in wines", x="Alcohol Level (%)",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5)+
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "Density"
data_density_dry <- ggplot(winequality_dry, aes(x = density)) +
  labs(title = "Density (g/ml) in wines", x = "Density (g/ml)",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.001) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "Residual Sugar Level"
data_sugar_level_dry <- ggplot(winequality_dry, aes(x = type_by_sugar_level)) +
  labs(title = "Type of wine by Sugar Level", x = "Type by Sugar Level",y="Count",fill="Type")+
  geom_bar(aes(fill = type), alpha = 0.5) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))
#
#Create bar plot for dry red and white wines based on "Chlorides"
data_chlorides_dry <- ggplot(winequality_dry, aes(x = chlorides)) +
  labs(title = "Chlorides in wines", x = "Chlorides",y="Count",fill="Type")+
  geom_histogram(aes(fill = type), alpha = 0.5,binwidth = 0.1) +
  scale_color_manual(values = c("#990000","#CCCC00"))+
  scale_fill_manual(values = c("#990000","#CCCC00"))+
  theme(plot.title = element_text(hjust = 0.5))




#After creating all the different bar plots for each of the variables I add some of them together for better visualisation
data_quality_dry

plot_grid(data_total_acidity_dry,data_ph_level_dry, nrow = 1)

plot_grid(data_fixed_acidity_dry, data_volatile_acidity_dry, nrow = 1)

data_sulphates_dry

plot_grid(data_total_sulfur_dioxide_dry,data_free_sulfur_dioxide_dry)

plot_grid(data_sugar_level_dry,data_alcohol_dry, nrow = 1)

plot_grid(data_density_dry,data_chlorides_dry, nrow = 1)






#TESTING NORMALITY
#######################################################################################################
#######################################################################################################
#qq plots for numerical variables red wine
qq_red_fixed_acidity <- ggqqplot(redwine_renamed$fixed_acidity, color = "#990000")+
  labs(x="Theoretical Fixed Acidity", y="Sample Fixed Acidity")
qq_red_citric_acid <- ggqqplot(redwine_renamed$citric_acid, color = "#990000")+
  labs(x="Theoretical Citric Acid", y="Sample Citric Acid")
qq_red_volatile_acidity <- ggqqplot(redwine_renamed$volatile_acidity, color = "#990000")+
  labs(x="Theoretical Volatile Acidity", y="Sample Volatile Acidity")
qq_red_total_acidity <- ggqqplot(redwine_renamed$total_acitity, color = "#990000")+
  labs(x="Theoretical Total Acidity", y="Sample Total Acidity")
qq_red_ph <- ggqqplot(redwine_renamed$ph, color = "#990000")+
  labs(x="Theoretical PH", y="Sample PH")
qq_red_residual_sugar <- ggqqplot(redwine_renamed$residual_sugar, color = "#990000")+
  labs(x="Theoretical Residual Sugar", y="Sample Residual Sugar")
qq_red_alcohol <- ggqqplot(redwine_renamed$alcohol, color = "#990000")+
  labs(x="Theoretical Alcohol", y="Sample Alcohol")
qq_red_density <- ggqqplot(redwine_renamed$density, color = "#990000")+
  labs(x="Theoretical Density", y="Sample Density")
qq_red_chlorides <- ggqqplot(redwine_renamed$chlorides, color = "#990000")+
  labs(x="Theoretical Chlorides", y="Sample Chlorides")
qq_red_free_sulfur_dioxide <- ggqqplot(redwine_renamed$free_sulfur_dioxide, color = "#990000")+
  labs(x="Theoretical Free Sulphur Dioxide", y="Sample Free Sulphur Dioxide")
qq_red_total_sulfur_dioxide <- ggqqplot(redwine_renamed$total_sulfur_dioxide, color = "#990000")+
  labs(x="Theoretical Total Sulphur Dioxide", y="Sample Total Sulphur Dioxide")
qq_red_sulphates <- ggqqplot(redwine_renamed$sulphates, color = "#990000")+
  labs(x="Theoretical Sulphates", y="Sample Sulphates")
#After creating all the different qq plots for each of the variables we add them together for better visualisation
grid.arrange(qq_red_total_acidity, qq_red_fixed_acidity,qq_red_citric_acid, qq_red_volatile_acidity,
             qq_red_ph, qq_red_sulphates, qq_red_free_sulfur_dioxide, qq_red_total_sulfur_dioxide,
             qq_red_residual_sugar, qq_red_alcohol, qq_red_density, qq_red_chlorides)
              

#qq plots for numerical variables white wine
qq_white_fixed_acidity <- ggqqplot(whitewine_renamed$fixed_acidity, color = "#CCCC00")+
  labs(x="Theoretical Fixed Acidity", y="Sample Fixed Acidity")
qq_white_citric_acid <- ggqqplot(whitewine_renamed$citric_acid, color = "#CCCC00")+
  labs(x="Theoretical Citric Acid", y="Sample Citric Acid")
qq_white_volatile_acidity <- ggqqplot(whitewine_renamed$volatile_acidity, color = "#CCCC00")+
  labs(x="Theoretical Volatile Acidity", y="Sample Volatile Acidity")
qq_white_total_acidity <- ggqqplot(whitewine_renamed$total_acitity, color = "#CCCC00")+
  labs(x="Theoretical Total Acidity", y="Sample Total Acidity")
qq_white_ph <- ggqqplot(whitewine_renamed$ph, color = "#CCCC00")+
  labs(x="Theoretical PH", y="Sample PH")
qq_white_residual_sugar <- ggqqplot(whitewine_renamed$residual_sugar, color = "#CCCC00")+
  labs(x="Theoretical Residual Sugar", y="Sample Residual Sugar")
qq_white_alcohol <- ggqqplot(whitewine_renamed$alcohol, color = "#CCCC00")+
  labs(x="Theoretical Alcohol", y="Sample Alcohol")
qq_white_density <- ggqqplot(whitewine_renamed$density, color = "#CCCC00")+
  labs(x="Theoretical Density", y="Sample Density")
qq_white_chlorides <- ggqqplot(whitewine_renamed$chlorides, color = "#CCCC00")+
  labs(x="Theoretical Chlorides", y="Sample Chlorides")
qq_white_free_sulfur_dioxide <- ggqqplot(whitewine_renamed$free_sulfur_dioxide, color = "#CCCC00")+
  labs(x="Theoretical Free Sulphur Dioxide", y="Sample Free Sulphur Dioxide")
qq_white_total_sulfur_dioxide <- ggqqplot(whitewine_renamed$total_sulfur_dioxide, color = "#CCCC00")+
  labs(x="Theoretical Total Sulphur Dioxide", y="Sample Total Sulphur Dioxide")
qq_white_sulphates <- ggqqplot(whitewine_renamed$sulphates, color = "#CCCC00")+
  labs(x="Theoretical Sulphates", y="Sample Sulphates")
#After creating all the different qq plots for each of the variables we add them together for better visualisation
grid.arrange(qq_white_total_acidity, qq_white_fixed_acidity,qq_white_citric_acid, qq_white_volatile_acidity,
             qq_white_ph, qq_white_sulphates, qq_white_free_sulfur_dioxide, qq_white_total_sulfur_dioxide,
             qq_white_residual_sugar, qq_white_alcohol, qq_white_density, qq_white_chlorides)


str(redwine_renamed_dry)

#Shapiro-Wilk normality test 
#create variable so we can run the test just for the numerical values
x <- c(1:11,15)
#Red wine
for (i in x) { 
  print(shapiro.test(redwine_renamed_dry[, i]))
}
#White wine
for (i in x) { 
  print(shapiro.test(whitewine_renamed_dry[, i]))
}



#CORRELATIONS (https://www.dataquest.io/blog/statistical-learning-for-predictive-modeling-r/)
#######################################################################################################
#######################################################################################################
#Correlation dry red wines
#
x <- c(1:11,15)
#
#Spearman Correlations
corr_spearman_redwine_renamed_dry <- round(cor(redwine_renamed_dry[,x], method = "spearman"), 2)
corr_spearman_redwine_renamed_dry
#
#Kendall Correlations
corr_kendall_redwine_renamed_dry <- round(cor(redwine_renamed_dry[,x], method = "kendall"), 2)
corr_kendall_redwine_renamed_dry
#
#correlation matrix: Spearman Correlations
plot_corr_spearman_redwine_renamed_dry <- ggcorrplot(corr_spearman_redwine_renamed_dry, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           colors = c("#202020","white","#FF0000"))+
           labs (title = "Spearman Correlation Coeficients")+
           theme(plot.title = element_text(hjust = 0.5))
#
#correlation matrix: Kendall Correlations
plot_corr_kendall_redwine_renamed_dry <- ggcorrplot(corr_kendall_redwine_renamed_dry, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           colors = c("#202020","white","#FF0000"))+
           labs (title = "Kendall Correlation Coeficients")+
           theme(plot.title = element_text(hjust = 0.5))
#Combining both matrix
grid.arrange(plot_corr_spearman_redwine_renamed_dry, plot_corr_kendall_redwine_renamed_dry, nrow=1)



#Correlation dry white wines
#
x <- c(1:11,15)
#
#Spearman Correlations
corr_spearman_whitewine_renamed_dry <- round(cor(whitewine_renamed_dry[,x], method = "spearman"), 2)
corr_spearman_whitewine_renamed_dry
#
#Kendall Correlations
corr_kendall_whitewine_renamed_dry <- round(cor(whitewine_renamed_dry[,x], method = "kendall"), 2)
corr_kendall_whitewine_renamed_dry
#
#correlation matrix: Spearman Correlations
plot_corr_spearman_whitewine_renamed_dry <- ggcorrplot(corr_spearman_whitewine_renamed_dry, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           colors = c("#FFFF33","white","#CCCC00"))+
           labs (title = "Spearman Correlation Coeficients")+
           theme(plot.title = element_text(hjust = 0.5))
#
#correlation matrix: Kendall Correlations
plot_corr_kendall_whitewine_renamed_dry <- ggcorrplot(corr_kendall_whitewine_renamed_dry, 
           hc.order = TRUE, 
           type = "lower",
           lab = TRUE, 
           colors = c("#FFFF33","white","#CCCC00"))+
           labs (title = "Kendall Correlation Coeficients")+
           theme(plot.title = element_text(hjust = 0.5))
#Combining both matrix
grid.arrange(plot_corr_spearman_whitewine_renamed_dry, plot_corr_kendall_whitewine_renamed_dry, nrow=1)






#INSIGHT 1 - IS THERE ANY DIFFERENCE BETWEEN DRY RED AND WHITE VINHO VERDE WINES
#BASED ON THEIR CHEMICAL PROPERTIES?
#######################################################################################################
#######################################################################################################
#Subset by just dry wines with quality 5
winequality_dry_5 <- subset(winequality_dry, winequality_dry$quality == "5")
redwine_renamed_dry_5 <- subset(redwine_renamed_dry, redwine_renamed_dry$quality == "5")
whitewine_renamed_dry_5 <- subset(whitewine_renamed_dry, whitewine_renamed_dry$quality == "5")



#Descriptive Statistics
#Creationg of function which will give more information than summary function does
#Same as function created in class with kurtosis
Stats <- function(stats){
  newMatrix <- matrix(1:8, nrow=1) #creating a blank matrix
  colnames(newMatrix) <- c("Mean","Median","Variance","Standard Deviation","Minimum","Maximum","Skewness","Kurtosis") 
  rownames(newMatrix) <- "Stats" 
  newMatrix[1, ] <- c(mean(stats),median(stats),var(stats),sd(stats),min(stats),max(stats),skewness(stats),kurtosis(stats)) 
  newMatrix 
}


#Using stats for both subsets
#
x <- c(1:11,15)
#
for (i in x) { 
  print(Stats(redwine_renamed_dry_5[, i])) 
}
#
for (i in x) { 
  print(Stats(whitewine_renamed_dry_5[, i])) 
}




#Density plots
#
#Fixed Acidity
density_red_white_fixed_acidity <- ggplot(winequality_dry_5, aes(x = fixed_acidity, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(fixed_acidity)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Fixed Acidity (g/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Volatile Acidity
density_red_white_volatile_acidity <- ggplot(winequality_dry_5, aes(x = volatile_acidity, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(volatile_acidity)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Volatile Acidity (g/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Citric Acid
density_red_white_citric_acid <- ggplot(winequality_dry_5, aes(x = citric_acid, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(citric_acid)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Citric Acid (g/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Residual Sugar
density_red_white_residual_sugar <- ggplot(winequality_dry_5, aes(x = residual_sugar, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(residual_sugar)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Residual Sugar (g/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Chlorides
density_red_white_chlorides <- ggplot(winequality_dry_5, aes(x = chlorides, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(chlorides)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Chlorides", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Free Sulphur Dioxide
density_red_white_free_sulfur_dioxide <- ggplot(winequality_dry_5, aes(x = free_sulfur_dioxide, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(free_sulfur_dioxide)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Free Sulphur Dioxide (mg/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Total Sulphur Dioxide
density_red_white_total_sulfur_dioxide <- ggplot(winequality_dry_5, aes(x = total_sulfur_dioxide, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(total_sulfur_dioxide)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Total Sulphur Dioxide (mg/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Density
density_red_white_density <- ggplot(winequality_dry_5, aes(x = density, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(density)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Density (g/ml)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#PH
density_red_white_ph <- ggplot(winequality_dry_5, aes(x = ph, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(ph)),
             color="black", linetype="dashed", size=1)+
  labs (x = "PH", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Sulphates
density_red_white_sulphates <- ggplot(winequality_dry_5, aes(x = sulphates, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(sulphates)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Sulphates (g/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Alcohol
density_red_white_alcohol <- ggplot(winequality_dry_5, aes(x = alcohol, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(alcohol)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Alcohol (%)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))
#
#Total Acidity
density_red_white_total_acidity <- ggplot(winequality_dry_5, aes(x = total_acitity, color = type)) + 
  geom_density()+
  geom_vline(aes(xintercept=mean(total_acitity)),
             color="black", linetype="dashed", size=1)+
  labs (x = "Total Acidity (mg/l)", y = "Density", color = "Type")+
  scale_color_manual(values=c("#990000","#CCCC00"))


#After creating each of the density plots we add them all together for easier visualisation
plot_grid(density_red_white_fixed_acidity, density_red_white_citric_acid, density_red_white_volatile_acidity,
          density_red_white_total_acidity, labels = "AUTO", ncol = 2)
#
plot_grid(density_red_white_ph,density_red_white_residual_sugar, density_red_white_alcohol,
          density_red_white_density,labels = "AUTO", ncol = 2)
#
plot_grid(density_red_white_chlorides,density_red_white_free_sulfur_dioxide, density_red_white_total_sulfur_dioxide,
          density_red_white_sulphates,labels = "AUTO", ncol = 2)


#Creating box plot comparing both red and wine
#
#The boc plot contain the Mann-Whitney p-value. I have not added "alternative = "two.sided"" as is not recognised in the graph.
#However the results are the same as in the full test
#fixed acidity
red_white_fixed_acidity <- ggplot(winequality_dry_5, aes(x=type, y=fixed_acidity, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Fixed Acidity (g/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#citric acid
red_white_citric_acid <- ggplot(winequality_dry_5, aes(x=type, y=citric_acid, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Citric Acid (g/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#volatile acidity
red_white_volatile_acidity <- ggplot(winequality_dry_5, aes(x=type, y=volatile_acidity, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Volatile Acidity (g/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#total acidity
red_white_total_acidity <- ggplot(winequality_dry_5, aes(x=type, y=total_acitity, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Total Acitidy (g/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#ph
red_white_ph <- ggplot(winequality_dry_5, aes(x=type, y=ph, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "PH", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#residual sugar
red_white_residual_sugar <- ggplot(winequality_dry_5, aes(x=type, y=residual_sugar, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Residual Sugar (g/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#alcohol
red_white_alcohol <- ggplot(winequality_dry_5, aes(x=type, y=alcohol, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Alcohol (%)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#density
red_white_density <- ggplot(winequality_dry_5, aes(x=type, y=density, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Density (g/ml)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#chlorides
red_white_chlorides <- ggplot(winequality_dry_5, aes(x=type, y=chlorides, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Chlorides", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#free sulfur dioxide
red_white_free_sulfur_dioxide <- ggplot(winequality_dry_5, aes(x=type, y=free_sulfur_dioxide, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Free Sulphur Dioxide (mg/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#total sulfur dioxide
red_white_total_sulfur_dioxide <- ggplot(winequality_dry_5, aes(x=type, y=total_sulfur_dioxide, color = type)) +
  geom_jitter(alpha = 0.5) +
  stat_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Total Sulphur Dioxide (mg/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)
#
#sulphates
red_white_sulphates <- ggplot(winequality_dry_5, aes(x=type, y=sulphates, color = type)) +
  geom_jitter(alpha=0.5) +
  stat_boxplot(fill = NA, color = "Black") + 
  labs (x = element_blank(), y = "Sulphates (g/l)", color = "Type") + 
  scale_color_manual(values=c("#990000","#CCCC00"))+
  stat_compare_means(method = "wilcox.test", paired = FALSE)



#After creating each of the box plots we add them all together for easier visualisation
#I create a couple of them so that the graphs stay a bit bigger
#
plot_grid(red_white_fixed_acidity, red_white_citric_acid, red_white_volatile_acidity,
          red_white_total_acidity, labels = "AUTO", ncol = 2)
#

plot_grid(red_white_ph,red_white_residual_sugar,red_white_alcohol, red_white_density,
          labels = "AUTO", ncol = 2)
#
plot_grid(red_white_chlorides,red_white_free_sulfur_dioxide,red_white_total_sulfur_dioxide,
          red_white_sulphates, labels = "AUTO", ncol = 2)



#Full Mann-Whitney U test results appearing in index
#
str(redwine_renamed_dry_5)
#
x <- c(1:11,15)
#
for (i in x) {
print(wilcox.test(redwine_renamed_dry_5[,i],whitewine_renamed_dry_5[,i]),alternative = "two.sided", paired = FALSE)
}
#Doing the test for paired samples the same result appears, we reject the null hypothesis of populations being equal
for (i in x) {
  print(wilcox.test(redwine_renamed_dry_5[,i],whitewine_renamed_dry_5[,i]),alternative = "two.sided", paired = TRUE)
}










#INSIGHT 2 - IS THERE ANY DIFFERENCE BETWEEN DRY RED VINHO VERDE WINES' QUALITY BASED ON THEIR CHEMICAL PROPERTIES?
#######################################################################################################
#######################################################################################################
#Creating box plot comparing dry red wines by quality
#
#fixed acidity
quality_red_fixed_acidity <- ggplot(redwine_renamed_dry, aes(x=quality, y=fixed_acidity, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Fixed Acidity (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#citric acid
quality_red_citric_acid <- ggplot(redwine_renamed_dry, aes(x=quality, y=citric_acid, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Citric Acid (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#volatile acidity
quality_red_volatile_acidity <- ggplot(redwine_renamed_dry, aes(x=quality, y=volatile_acidity, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Volatile Acidity (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#total acidity
quality_red_total_acidity <- ggplot(redwine_renamed_dry, aes(x=quality, y=total_acitity, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Total Acidity (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#ph
quality_red_ph <- ggplot(redwine_renamed_dry, aes(x=quality, y=ph, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "pH", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#residual sugar
quality_red_residual_sugar <- ggplot(redwine_renamed_dry, aes(x=quality, y=residual_sugar, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Residual Sugar (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#alcohol
quality_red_alcohol <- ggplot(redwine_renamed_dry, aes(x=quality, y=alcohol, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Alcohol (%)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#density
quality_red_density <- ggplot(redwine_renamed_dry, aes(x=quality, y=density, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Density (g/ml)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#chlorides
quality_red_chlorides <- ggplot(redwine_renamed_dry, aes(x=quality, y=chlorides, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Chlorides", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#free sulfur dioxide
quality_red_free_sulfur_dioxide <- ggplot(redwine_renamed_dry, aes(x=quality, y=free_sulfur_dioxide, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Free Sulfur Dioxide (mg/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#total sulfur dioxide
quality_red_total_sulfur_dioxide <- ggplot(redwine_renamed_dry, aes(x=quality, y=total_sulfur_dioxide, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Total Surfur Dioxide (mg/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))
#
#sulphates
quality_red_sulphates <- ggplot(redwine_renamed_dry, aes(x=quality, y=sulphates, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Sulphates (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FF6666","#FF3333","#FF0000","#CC0000","#990000","#660000"))



#After creating each of the box plots we add them all together for easier visualisation
#I create a couple of them so that the graphs stay a bit bigger
#
plot_grid(quality_red_fixed_acidity, quality_red_citric_acid, quality_red_volatile_acidity,
          quality_red_total_acidity, labels = "AUTO", ncol = 2)
#

plot_grid(quality_red_ph,quality_red_residual_sugar,quality_red_alcohol, quality_red_density,
          labels = "AUTO", ncol = 2)
#
plot_grid(quality_red_chlorides,quality_red_free_sulfur_dioxide,quality_red_total_sulfur_dioxide,
          quality_red_sulphates, labels = "AUTO", ncol = 2)

#In order to run Kruskal Test:
#Assumptions: independent varaiables, dependant variable is continous, homogeanity of variances
#Checking homogeanity of variances with Fligner-Killeen test
#
#
x <- c(1:11,15)
#
for (i in x) {
  print(fligner.test(redwine_renamed_dry[, i] ~ redwine_renamed_dry$quality, data = redwine_renamed_dry))
}



#Kruskal-Wallis test 
#
x <- c(1:11,15)
#
for (i in x) {
  print(kruskal.test(redwine_renamed_dry[, i] ~ redwine_renamed_dry$quality, data = redwine_renamed_dry))
}


#Dunn’s test to identity which groups are different
#
x <- c(1:11,15)
#
for (i in x) {
  print(dunnTest(redwine_renamed_dry[, i] ~ redwine_renamed_dry$quality, data = redwine_renamed_dry,
                 method = "bonferroni"))
}






#INSIGHT 2 - IS THERE ANY DIFFERENCE BETWEEN DRY WHITE VINHO VERDE WINES' QUALITY BASED ON THEIR CHEMICAL PROPERTIES?
#######################################################################################################
#######################################################################################################
#Creating box plot comparing dry white wines by quality
#
#fixed acidity
quality_white_fixed_acidity <- ggplot(whitewine_renamed_dry, aes(x=quality, y=fixed_acidity, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Fixed Acidity (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#citric acid
quality_white_citric_acid <- ggplot(whitewine_renamed_dry, aes(x=quality, y=citric_acid, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Citric Acid (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#volatile acidity
quality_white_volatile_acidity <- ggplot(whitewine_renamed_dry, aes(x=quality, y=volatile_acidity, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Volatile Acidity (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#total acidity
quality_white_total_acidity <- ggplot(whitewine_renamed_dry, aes(x=quality, y=total_acitity, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Total Acidity (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#ph
quality_white_ph <- ggplot(whitewine_renamed_dry, aes(x=quality, y=ph, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "pH", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#residual sugar
quality_white_residual_sugar <- ggplot(whitewine_renamed_dry, aes(x=quality, y=residual_sugar, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Residual Sugar (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#alcohol
quality_white_alcohol <- ggplot(whitewine_renamed_dry, aes(x=quality, y=alcohol, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Alcohol (%)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#density
quality_white_density <- ggplot(whitewine_renamed_dry, aes(x=quality, y=density, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Density (g/ml)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#chlorides
quality_white_chlorides <- ggplot(whitewine_renamed_dry, aes(x=quality, y=chlorides, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Chlorides", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#free sulfur dioxide
quality_white_free_sulfur_dioxide <- ggplot(whitewine_renamed_dry, aes(x=quality, y=free_sulfur_dioxide, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Free Sulfur Dioxide (mg/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#total sulfur dioxide
quality_white_total_sulfur_dioxide <- ggplot(whitewine_renamed_dry, aes(x=quality, y=total_sulfur_dioxide, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Total Surfur Dioxide (mg/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))
#
#sulphates
quality_white_sulphates <- ggplot(whitewine_renamed_dry, aes(x=quality, y=sulphates, color = quality)) +
  geom_jitter(alpha = 0.5) +
  geom_boxplot(fill = NA,color = "Black") + 
  labs (x = element_blank(), y = "Sulphates (g/l)", color = "Quality") + 
  scale_color_manual(values=c("#FFFF99","#FFFF66","#FFFF33","#FFFF00","#CCCC00","#999900","666600"))



#After creating each of the box plots we add them all together for easier visualisation
#I create a couple of them so that the graphs stay a bit bigger
#
plot_grid(quality_white_fixed_acidity, quality_white_citric_acid, quality_white_volatile_acidity,
          quality_white_total_acidity, labels = "AUTO", ncol = 2)
#

plot_grid(quality_white_ph,quality_white_residual_sugar,quality_white_alcohol, quality_white_density,
          labels = "AUTO", ncol = 2)
#
plot_grid(quality_white_chlorides,quality_white_free_sulfur_dioxide,quality_white_total_sulfur_dioxide,
          quality_white_sulphates, labels = "AUTO", ncol = 2)


str(whitewine_renamed)



#In order to run Kruskal Test:
#Assumptions: independent varaiables, dependant variable is continous, homogeanity of variances
#Checking homogeanity of variances with Fligner-Killeen test
#
#
x <- c(1:11,15)
#
for (i in x) {
  print(fligner.test(whitewine_renamed_dry[, i] ~ whitewine_renamed_dry$quality, data = whitewine_renamed_dry))
}

#Kruskal-Wallis test 
#
x <- c(1:11,15)
#
for (i in x) {
  print(kruskal.test(whitewine_renamed_dry[, i] ~ whitewine_renamed_dry$quality, data = whitewine_renamed_dry))
}


#Dunn’s test to identity which groups are different
#
for (i in x) {
  print(dunnTest(whitewine_renamed_dry[, i] ~ whitewine_renamed_dry$quality, data = whitewine_renamed_dry,
                 method = "bonferroni"))
}




#INSIGHT 3 - IS IT POSSIBLE THE CREATION OF A PREDICTIVE SIMPLE LINEAR MODEL FOR SOME OF
#THE CHEMICAL PROPERTIES OF DRY RED VINHO WINE?
#######################################################################################################
#######################################################################################################
#Scatter Plots
scat_redwine_renamed_1 <- ggplot(redwine_renamed_dry, aes(x=fixed_acidity, y=total_acitity)) + 
  geom_point(color = "#990000", alpha = 0.5)+
  geom_smooth(method = lm, color = "black", fill="black")+
  labs (x = "Fixed Acidity (g/l)", y = "Total Acidity (g/l)")

scat_redwine_renamed_2 <- ggplot(redwine_renamed_dry, aes(x=free_sulfur_dioxide, y=total_sulfur_dioxide)) + 
  geom_point(color = "#990000", alpha = 0.5)+
  geom_smooth(method = lm, color = "black", fill="black")+
  labs (x = "Free Sulfur Dioxide (mg/l)", y = "Total Sulfur Dioxide (mg/l)")

scat_redwine_renamed_3 <- ggplot(redwine_renamed_dry, aes(x=total_acitity, y=ph)) + 
  geom_point(color = "#990000", alpha = 0.5)+
  geom_smooth(method = lm, color = "black", fill="black")+
  labs (x = "Total Acidity (g/l)", y = "pH")

scat_redwine_renamed_4 <- ggplot(redwine_renamed_dry, aes(x=fixed_acidity, y=ph)) + 
  geom_point(color = "#990000", alpha = 0.5)+
  geom_smooth(method = lm, color = "black", fill="black")+
  labs (x = "Fixed Acidity (g/l)", y = "pH")


plot_grid(scat_redwine_renamed_1,scat_redwine_renamed_2,scat_redwine_renamed_3,scat_redwine_renamed_4)




#Checking the range for the selected variables
range(redwine_renamed_dry$fixed_acidity)
range(redwine_renamed_dry$total_acitity)
range(redwine_renamed_dry$ph)
range(redwine_renamed_dry$free_sulfur_dioxide)
range(redwine_renamed_dry$total_sulfur_dioxide)
#Creating some linear models
model_1 <- lm(fixed_acidity ~ total_acitity, data = redwine_renamed_dry)
model_2 <- lm(free_sulfur_dioxide ~ total_sulfur_dioxide, data = redwine_renamed_dry)
model_3 <- lm(total_acitity ~ ph, data = redwine_renamed_dry)
model_4 <- lm(fixed_acidity ~ ph, data = redwine_renamed_dry)
#Having access to the beta and the alpha to form the equations
print(model_1)
print(model_2)
print(model_3)
print(model_4)
#Statistical summary of the models
summary(model_1)
summary(model_2)
summary(model_3)
summary(model_4)
#Predictions with the models
predict(model_1, data.frame(total_acitity = 7), level = 0.99, interval ="prediction")
predict(model_2, data.frame(total_sulfur_dioxide = 100), level = 0.99, interval ="prediction")
predict(model_3, data.frame(ph = 2.80), level = 0.99, interval ="prediction")
predict(model_4, data.frame(ph = 2.80), level = 0.99, interval ="prediction")






#INSIGHT 4 - IS IT POSSIBLE TO CLASSIFY DRY VINHO VERDE WINES BY TYPE (RED/WHITE)?
#######################################################################################################
#######################################################################################################
#Following different manuals:
#https://www.kaggle.com/vshelunts/wine-quality-decision-tree-and-random-forest
#https://dzone.com/articles/a-comprehensive-guide-to-random-forest-in-r
#https://machinelearningmastery.com/machine-learning-in-r-step-by-step/

#Model to predict if a wine is red
#Create data partition
validation_index_dry_5 <- createDataPartition(winequality_dry_5$type, p=0.80, list=FALSE)
# 80% of data for training purposes
training_dry_5 <- winequality_dry_5[validation_index_dry_5,]
# 20% of the data for validation
validation_dry_5 <- winequality_dry_5[-validation_index_dry_5,]
#checking number of rows for each partition
nrow(winequality_dry_5)
nrow(training_dry_5)
nrow(validation_dry_5)

#Creating model
model_classification_dry_5_type<- randomForest(type~., training_dry_5, ntree=50)
#Checking which variables contribute more to the model
varImpPlot(model_classification_dry_5_type, bg = "#990000", main="Contribution of variables to the Model")


#Testing the model - check acuracy in predicting the observations appearing in the validation subset
prediction_model_classification_dry_5_type <- predict(model_classification_dry_5_type, validation_dry_5)   
#Confusion Matrix to check the validity of the model
confusionMatrix(prediction_model_classification_dry_5_type, validation_dry_5$type)


importance(model_classification_dry_5_type)


 




#INSIGHT 4 - IS IT POSSIBLE TO CLASSIFY DRY VINHO VERDE WINES BY QUALITY?
#######################################################################################################
#######################################################################################################
#Model to classify red wine by quality
#Create data partition
validation_index_dry_red <- createDataPartition(redwine_renamed_dry$quality, p=0.80, list=FALSE)
# 80% of data for training purposes
training_dry_red <- redwine_renamed_dry[validation_index_dry_red,]
# 20% of the data for validation
validation_dry_red <- redwine_renamed_dry[-validation_index_dry_red,]

#Checking existing levels for quality variable
levels(training_dry_red$quality)
#Drop unused levels from traning subset
training_dry_red$quality <- drop.levels(training_dry_red$quality)
#Confirm the levels were droped
levels(validation_dry_red$quality)
#Drop unused levels from validation subset
validation_dry_red$quality <- drop.levels(validation_dry_red$quality)
#Confirm the levels were droped
levels(validation_dry_red$quality)

#checking number of rows for each partition
nrow(redwine_renamed_dry)
nrow(training_dry_red)
nrow(validation_dry_red)

#Creating model
model_classification_dry_red_quality<- randomForest(quality~., training_dry_red, ntree=50)
#Checking which variables contribute more to the model
varImpPlot(model_classification_dry_red_quality, bg = "#990000", main="Contribution of variables to the Model")

#Testing the model - check acuracy in predicting the observations appearing in the validation subset
prediction_model_classification_dry_red_quality <- predict(model_classification_dry_red_quality, validation_dry_red) 
#Confusion Matrix to check the validity of the model
confusionMatrix(prediction_model_classification_dry_red_quality, validation_dry_red$quality)





#Model to classify white wine by quality
#Creating a new subset from white dry wines where quality is 1 to 6
whitewine_renamed_dry_for_model <- subset(whitewine_renamed_dry, whitewine_renamed_dry$quality != 7 )
                                                                

#Visualising the observations for the new variable quality
whitewine_renamed_dry_for_model$quality

#Create data partition
validation_index_dry_white <- createDataPartition(whitewine_renamed_dry_for_model$quality, p=0.80, list=FALSE)
# 20% of the data for validation
validation_dry_white <- whitewine_renamed_dry_for_model[-validation_index_dry_white,]
# 80% of data for training purposes
training_dry_white <- whitewine_renamed_dry_for_model[validation_index_dry_white,]

#checking number of rows for each partition
nrow(whitewine_renamed_dry)
nrow(training_dry_white)
nrow(validation_dry_white)

#Checking existing levels for quality variable
levels(training_dry_white$quality)
#Drop unused levels from traning subset
training_dry_white$quality <- drop.levels(training_dry_white$quality)
#Confirm the levels were droped
levels(training_dry_white$quality)
#Confirm the levels were droped
levels(validation_dry_white$quality)
#Drop unused levels from validation subset
validation_dry_white$quality <- drop.levels(validation_dry_white$quality)
#Confirm the levels were droped
levels(validation_dry_white$quality)


#Creating model
model_classification_dry_white_quality<- randomForest(quality~., training_dry_white, ntree=50)
#Checking which variables contribute more to the model
varImpPlot(model_classification_dry_white_quality,bg = "#CCCC00", main="Contribution of variables to the Model")

#Testing the model - check acuracy in predicting the observations appearing in the validation subset
prediction_model_classification_dry_white_quality <- predict(model_classification_dry_white_quality, validation_dry_white)                   
#Confusion Matrix to check the validity of the model
confusionMatrix(prediction_model_classification_dry_white_quality, validation_dry_white$quality)

#######################################################################################################
#######################################################################################################

#THE END :)

