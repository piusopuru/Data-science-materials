library(haven)
library(dplyr)
library(readxl)
Kenya_agri_disease_spatial <- read_excel("Kenya_agri_disease_spatial.xls")

crop_recommendation <- read_excel("crop_recommendation.xlsx")

#filter
maize <- filter(crop_recommendation, Crop == "maize")
hum_above50 <- filter(crop_recommendation, humidity > 50)
hum_maizeabove50 <- filter(crop_recommendation, humidity > 50 & Crop == "maize") 

#select
#Example: Select Crop and yield_category then store them. 
data <- select(crop_recommendation, Crop, yield_category )
#Example Using pipe operator
data1 <- crop_recommendation  %>% 
  select(Crop, yield_category )
#deselect/remove columns from the data set
data2 <- select(crop_recommendation, -Crop, -yield_category )

#combine select and filter
data3 <- crop_recommendation  %>% 
  select( Crop, yield_category ) %>%
  filter(Crop == "rice")
         
#sort in ascending or descending order
data4 <- arrange(crop_recommendation, yield_kg_ha)  
#Sort yield in descending order
data5 <- arrange(crop_recommendation, -yield_kg_ha) 

#mutate
#Example: creates a new variable "new_yiled" by getting half the yield_kg_ha variable 
half_yield <- mutate(crop_recommendation , new_yield = yield_kg_ha/2)

#Alternative
half_yield1 <- crop_recommendation %>%
  mutate(new_yield = yield_kg_ha/2)
#rename
nit <- rename(crop_recommendation, N = Nitrogen)
#relocate
New_order <- relocate(crop_recommendation, rainfall, yield_category)


##frequency tables, use transform for vertical view
table(crop_recommendation$Crop)
transform(table(crop_recommendation$Crop))
#exercise using yield_category

#Summary and misingness
summary(crop_recommendation$Crop)
summary(crop_recommendation$Nitrogen)

#Exercise: what is the mean yield_kg_ha, rainfall

#missing values
table(is.na(crop_recommendation$Crop))
table(is.na(crop_recommendation$Nitrogen))

#Exercise: Are there any missing rainfall values?

#outlier detection using box plots
boxplot(crop_recommendation$Nitrogen)
boxplot(crop_recommendation$yield_kg_ha)

#Exercise: using a boxplot, are there any outliers in rainfall recordings

#data distributions using visual approach and statistical tests
hist(crop_recommendation$yield_kg_ha)
shapiro.test(crop_recommendation$yield_kg_ha)

#ggplot2
install.packages("ggplot2")
library(ggplot2)
#scatter plot
ggplot(crop_recommendation, aes(x=rainfall, y=Nitrogen)) + 
  geom_point() + 
  labs(title= "Scatter plot", x= "rainfall", y= "Nitrogen")

#Exercise: Draw a scatter plot of rainfall vs yield_kg_ha


#add a smooth line
ggplot(crop_recommendation, aes(x=rainfall, y=Nitrogen)) + 
  geom_point() + 
  geom_smooth() +
  labs(title= "Scatter plot", x= "rainfall", y= "Nitrogen")


#add a straight line
ggplot(crop_recommendation, aes(x=rainfall, y=Nitrogen)) + 
  geom_point() + 
  geom_smooth(method="lm")  +
  labs(title= "Scatter plot", x= "rainfall", y= "Nitrogen")

#add color
ggplot(crop_recommendation, aes(x=rainfall, y=Nitrogen, color = yield_category)) + 
  geom_point() + 
  labs(title= "Scatter plot", x= "rainfall", y= "Nitrogen")

#Exercise: add color by Crop on the scatter plot

#add straight line per category
ggplot(crop_recommendation, aes(x=rainfall, y=Nitrogen, color = yield_category)) + 
  geom_point() + 
  geom_smooth() + 
  labs(title= "Scatter plot", x= "rainfall", y= "Nitrogen")

#add straight line per category
ggplot(crop_recommendation, aes(x=rainfall, y=Nitrogen, color = yield_category)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(title= "Scatter plot", x= "rainfall", y= "Nitrogen")

#add facets by yield category
ggplot(crop_recommendation, aes(x=rainfall, y=Nitrogen, color = yield_category)) + 
  geom_point() + 
  geom_smooth(method="lm") + 
  labs(title= "Scatter plot", x= "rainfall", y= "Nitrogen") +
  facet_grid(rows = vars(yield_category))

#Exercise: add facets by Crop in the scatter plot above


#Box plots

ggplot(crop_recommendation, aes(x = rainfall, y = Crop, color = Crop)) + 
  geom_boxplot() +  
  labs(title = "Amount of rainfall", x = "rainfall") +
  theme(
    axis.text.y = element_blank(),   # remove crop names
    axis.ticks.y = element_blank()   # remove tick marks
  )


ggplot(crop_recommendation, aes(x = rainfall, y = Crop, color = Crop)) + 
  geom_boxplot() +  
  labs(title = "Amount of rainfall", x = "rainfall") +
  coord_flip() +
  theme(
    axis.text.x = element_blank(),   # remove crop names after flip
    axis.ticks.x = element_blank()   # remove tick marks after flip
  )



# Histograms
ggplot(crop_recommendation, aes(x=yield_kg_ha))+ 
         geom_histogram() +  
         labs(title= "Histogram of crop yield") 
       

ggplot(crop_recommendation, aes(x=yield_kg_ha))+ 
  geom_histogram(aes(y = ..density..), fill='lightgray', col='black') +  
  stat_function(fun = dnorm,args = list(mean = mean(crop_recommendation $ yield_kg_ha),sd = sd(crop_recommendation $ yield_kg_ha)))+
  labs(title= "Histogram of crop yield")  



#bar graph
ggplot(crop_recommendation, aes(x=yield_category)) + 
  geom_bar()+
  labs(title ="Yield category", x="Yield category")

#by crop
ggplot(crop_recommendation, aes(x=yield_category, fill = Crop)) + 
  geom_bar()+
  labs(title ="Yield category", x="Yield category")

#horizontzl bar graph
ggplot(crop_recommendation, aes(x=yield_category, fill =Crop)) + 
  geom_bar()+
  labs(title ="Yield category", x="Yield category")+
   coord_flip() 

#Diagnostics
#Normality test
shapiro.test(crop_recommendation$yield_kg_ha)
shapiro.test(Kenya_agri_disease_spatial$Pesticide_kg)
#Exercise: test if rainfall is normally distributed.

table(Kenya_agri_disease_spatial$year)
Kenya_agri_disease_spatial$year <- as.factor(Kenya_agri_disease_spatial$year)

#ttest
t.test(Pesticide_kg ~ year, data = Kenya_agri_disease_spatial)

##ANOVA using "aov" function
Kenya_agri_disease_spatial$SoilType <- as.factor(Kenya_agri_disease_spatial$SoilType)

ANOVA <- aov(Pesticide_kg~SoilType, data=Kenya_agri_disease_spatial)
summary(ANOVA)

#Exercise : Test whether mean pesticide is equal among LandCover categories

