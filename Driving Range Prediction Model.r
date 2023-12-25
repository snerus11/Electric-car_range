library(data.table)
library(ggplot2)

#Read The Input Data

setwd("C:\\Users\\lol51\\Downloads\\Driving_range-main")
df_ip <-fread('Electric_cars.csv')


#Checking the structure of Data
head(df_ip)
dim(df_ip)
str(df_ip)
#Checking for any Null Values.
#There are no Null values in the data
colSums(is.na(df_ip))

#Data Conversion into Factors
df_ip$city<-as.factor(df_ip$city)
df_ip$motor_way<-as.factor(df_ip$motor_way)
df_ip$country_roads<-as.factor(df_ip$country_roads)
df_ip$`A/C` <-as.factor(df_ip$`A/C`)
df_ip$park_heating<-as.factor(df_ip$park_heating)
df_ip<-as.data.frame(df_ip)

str(df_ip)
summary(df_ip)

df1<-df_ip

#EXPLORATORY DATA ANALYIS
#DEPENDENT VARIABLE
boxplot(df1$`trip_distance(km)`)
hist(df1$`trip_distance(km)`)
#BOX and Histogram of the target variable show that there are lot of outliers which have to be removed.  based on the plots, any value >150 can be removed.
df1<-df1[df1$`trip_distance(km)`<=100,]
#In the Quantiyt field, there seems to be some outliers.  Hence, for this model, anything less than 15 needs to be removed.
df1<-df1[df1$`quantity(kWh)` <=15,]

#UNI-VARIATE ANALYSIS
library(DescTools)
Desc(df1)

#BI-VARIATE ANALYSIS
library(dplyr)
library(DT)

#Average Driving Range by City
#From the summaries below we can see that City has high predictive power.  However we also have to check for classification imbalance
Range_City= df1 %>% 
  group_by(city) %>%
  summarise(avg_range = mean(`trip_distance(km)`)) %>%
  arrange(desc(avg_range))

DT::datatable(Range_City)


plt_city = df1 %>% group_by(city) %>% summarise(avg_range = mean(`trip_distance(km)`)) %>%
  ggplot(aes(x=city,y=avg_range)) + geom_bar(stat="identity") +
  labs(title = "Average Driving Range by City")
plot(plt_city)

#Average Driving Range by Tire Type
#As it can be seen, there is no much difference in the average driving Range with the Tire_Type.  This variable can be ignored in the model
Range_tiretype= df1 %>% 
  group_by(tire_type) %>%
  summarise(avg_range = mean(`trip_distance(km)`)) %>%
  arrange(desc(avg_range))

DT::datatable(Range_tiretype)

plt_tire = df1 %>% group_by(tire_type) %>% summarise(avg_range = mean(`trip_distance(km)`)) %>%
  ggplot(aes(x=tire_type,y=avg_range)) + geom_bar(stat="identity") +
  labs(title = "Average Driving Range by tire_type")
plot(plt_tire)

#Average Driving Range by Motor Way
#Motor way has a good predictive power for Driving Rage.  This variable will be used for the model
Range_MotorWay= df1 %>% 
  group_by(motor_way) %>%
  summarise(avg_range = mean(`trip_distance(km)`)) %>%
  arrange(desc(avg_range))

DT::datatable(Range_MotorWay)

plt_motor = df1 %>% group_by(motor_way) %>% summarise(avg_range = mean(`trip_distance(km)`)) %>%
  ggplot(aes(x=motor_way,y=avg_range)) + geom_bar(stat="identity") +
  labs(title = "Average Driving Range by motor_way")
plot(plt_motor)

#Average Driving Range by Country Road
#From the Summaries below, Country Road will be used in the model as it has significant results.
Range_CountryRoad= df1 %>% 
  group_by(country_roads) %>%
  summarise(avg_range = mean(`trip_distance(km)`)) %>%
  arrange(desc(avg_range))

DT::datatable(Range_CountryRoad)

plt_CountryRoad = df1 %>% group_by(country_roads) %>% summarise(avg_range = mean(`trip_distance(km)`)) %>%
  ggplot(aes(x=country_roads,y=avg_range)) + geom_bar(stat="identity") +
  labs(title = "Average Driving Range by country_roads")
plot(plt_CountryRoad)


#Average Driving Range by Driving Style
#as it can be seen, the average driving range reduces as the driving style moves from Fast, Moderate to Normal.  Hence, this variable should be used for the model.
Range_DrivingSytle= df1 %>% 
  group_by(driving_style) %>%
  summarise(avg_range = mean(`trip_distance(km)`)) %>%
  arrange(desc(avg_range))

DT::datatable(Range_DrivingSytle)

plt_dvngstyle = df1 %>% group_by(driving_style) %>% summarise(avg_range = mean(`trip_distance(km)`)) %>%
  ggplot(aes(x=driving_style,y=avg_range)) + geom_bar(stat="identity") +
  labs(title = "Average Driving Range by driving_style")
plot(plt_dvngstyle)

#Average Driving Range by A/C
#As seen below, A/C is an important variable.  However, as seen above from its univariate analysis, this has a hugh classification imbalance issue.  Hence this variable will NOT be considered.
Range_AC= df1 %>% 
  group_by(`A/C`) %>%
  summarise(avg_range = mean(`trip_distance(km)`)) %>%
  arrange(desc(avg_range))

DT::datatable(Range_AC)

plt_AC = df1 %>% group_by(`A/C`) %>% summarise(avg_range = mean(`trip_distance(km)`)) %>%
  ggplot(aes(x=`A/C`,y=avg_range)) + geom_bar(stat="identity") +
  labs(title = "Average Driving Range by `A/C`")
plot(plt_AC)

#Average Driving Range by Park Heating
#As it can seen that the Driving Range of the vehicle increases if there is a Park Heating.  Hence, this variable will be considered for the model.
Range_ParkHeating= df1 %>% 
  group_by(park_heating) %>%
  summarise(avg_range = mean(`trip_distance(km)`)) %>%
  arrange(desc(avg_range))

DT::datatable(Range_ParkHeating)

plt_parkheating = df1 %>% group_by(park_heating) %>% summarise(avg_range = mean(`trip_distance(km)`)) %>%
  ggplot(aes(x=park_heating,y=avg_range)) + geom_bar(stat="identity") +
  labs(title = "Average Driving Range by park_heating")
plot(plt_parkheating)


### Correlation overview-
# As seen in Correlation Chart and Scatter plots below, quantity(kWh) and avg_speed(km/h) seem to have too much correlation with the Target variable.  Hence, these 2 variables will be considered for the model.  The other 2 numerical variables such as "consumption(kWh/100km)" and "ecr_deviation" dont have any correlation, hence will be removed. 
library(correlation)
num_col<-sapply(df1, is.numeric)
df_num_cols<-df1[, num_col]
cor(df_num_cols)

# Correlation Heat Chart
cormat <- round(cor(df_num_cols),3)
melted_cormat <- melt(cormat)

ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile()

#Scatter Plots
plot(df1$`avg_speed(km/h)`,df1$`trip_distance(km)`)
plot(df1$`quantity(kWh)`,df1$`trip_distance(km)`)
plot(df1$ecr_deviation,df1$`trip_distance(km)`)
plot(df1$`consumption(kWh/100km)`,df1$`trip_distance(km)`)

##Hence, from the above Uni and Bi Variate Analysis, the following Variables will be Dropped from the model as they have very little or NO significance to predict the Dependent variable "trip_distance(km)"
#tire_type
#consumption(kWh/100km)
#A/C
#ecr_deviation

#Removing the 4 variables from the dataframe.
df_train<-within(df1, rm(tire_type, `A/C`, `consumption(kWh/100km)`, ecr_deviation))

#Data is ready to build a predictive Model.  

#LINEAR REGRESION
#From the Model Output, Multiple R-squared:  0.7508 meaning, it is only 75% accuracte.  Lets try the Random Forest Model
dr_range<-lm(`trip_distance(km)`~`quantity(kWh)` + city + motor_way + country_roads + driving_style + park_heating + `avg_speed(km/h)`, data = df_train)
summary(dr_range)

#RANDOM FOREST MODEL

library(randomForest)
library(caTools)
library(rpart)
library(rpart.plot)
# Splitting data in train and test data
split <- sample.split(df_train, SplitRatio = 0.7)
split

train <- subset(df_train, split == "TRUE")
test <- subset(df_train, split == "FALSE")

dim(train)
dim(test)


#RPART MODEL
fit <- rpart(`trip_distance(km)`~., data = train, method = 'anova')
fit
rpart.plot(fit, extra=101)
plotcp(fit)
predict_rpart <-predict(fit, test)
predict_rpart
summary(predict_rpart)
#Adding RPart Predicted values to the test dataframe
test$Rpart_Pred<-predict_rpart

# RANDOM FOREST MODEL
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[-1],
                             y = train$`trip_distance(km)`,
                             ntree = 500)

print(classifier_RF)
library(dplyr)
# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-1])

#Adding RF Predicted values to the test dataframe
test$RandomForest_Pred<-y_pred


# Plotting model
plot(classifier_RF)


# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)
#As seen from the above results, Random Forest model is performing better than LR or RPart models.  The RF model is giving an accuracy of 81.47% which is descent enough.  We can still increase the accuracy by looking at Deep Learning models or certain parameter tuning in the current RF Models.

#Exporting the Predicted output data into Excel
library("writexl")
write_xlsx(test,"prediction_output.xlsx")

