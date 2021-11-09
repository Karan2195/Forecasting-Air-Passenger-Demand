


################################################  FORECASTING AIR PASSENGERS ####################################################



################################################### DOMESTIC PASSENGERS #########################################################

# Libraries Import

library(rvest)
library(tidyverse)
library(zoo)
library(forecast)
library(dygraphs)
library(dplyr)

################################################## Data Preparation #############################################################

# Read the Data from the below url using rvest package

url = "https://www.transtats.bts.gov/Data_Elements.aspx?Qn6n=E"

data = url %>%read_html() %>% html_nodes(xpath = '//*[@id="GridView1"]') %>% html_table()
data

data = as.data.frame(data)
data

# Remove the speacial charactrs from the columns and convert them to Numeric

data$DOMESTIC = as.numeric(gsub(",","",data$DOMESTIC))
data$INTERNATIONAL = as.numeric(gsub(",","",data$INTERNATIONAL))
data

# Remove "Total" from Month Column:

data = subset(data, Month != "TOTAL")
data

# Use the Required Columns for furter Analysis

data1 = data %>% select(Year,Month,DOMESTIC,INTERNATIONAL)
data1

# Create a Date column for plotting Time Series

data1= transform(data1, Date = as.Date(paste(Year, Month,1, sep = "-")))
data1

############################################## Data Visualization #################################################################

# DOMESTIC

data1 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_point() + theme_bw()

# INTERNATIONAL

data1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_point() + theme_bw()

# Data Visualization 2: recreated the previous graphs and use 12 colors representing 12 months  to color all points

data1 %>%  ggplot(aes(x=Date,y=DOMESTIC)) + geom_line() + 
  geom_point(aes(color=factor(Month))) +  theme_bw()

data1 %>%  ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line() + 
  geom_point(aes(color=factor(Month))) +  theme_bw()

#  Plot both trajectories on the same graph: use 2 vertical axis: the left one for DOMESTIC and the right one for INTERNATIONAL

data2 = ts(data1[,c("DOMESTIC","INTERNATIONAL")], start= c(2002,10), frequency = 12)

dygraph(data2) %>% dySeries("INTERNATIONAL", axis = 'y2')
data2

#################################################  Data Modeling & Evaluation   #####################################################

## Checking Seasonality in the Data

M1 = lm(DOMESTIC ~ Month ,data=data1)
summary(M1)

# How well model fits the data?
data1$M1 = M1$fitted.values

data1 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_point() + geom_line()+
  geom_line(aes(x=Date,y=M1),color="red") + theme_bw()


## Checking Model trend:

dim(data1)
dim(data1)[1]
data1$Trend = 1:(dim(data1)[1])
data1 %>% head
data1 %>% tail
typeof(data1$Month)

M2 = lm(DOMESTIC ~ Trend, data=data1)
summary(M2)

data1$M2 = M2$fitted.values
data1 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_line(aes(x=Date,y=M2),color="red") + theme_bw()

# Checking both Trend and Seasonality

M3 = lm(DOMESTIC ~ Trend + Month , data=data1)
summary(M3)

data1$M3 = M3$fitted.values

data1 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_line(aes(x=Date,y=M3),color="red") + theme_bw()

## Check residuals
data1$M3.residuals = M3$residuals
tsdisplay(data1$M3.residuals)

## Looking at the ACF graph it indicates significant lags and the PACF indicates Lag1 & Lag2

data1$DOMESTICLag1 = lag(data1$DOMESTIC,1)
data1$DOMESTICLag2 = lag(data1$DOMESTIC,2)

M4 = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2 , data=data1) ### Best Model Scenario 1 ###
summary(M4)

data1$M4 = c(NA,NA,M4$fitted.values)

data1 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_line(aes(x=Date,y=M4),color="red") + theme_bw()

## Check residuals
data1$M4.residuals = c(NA,NA,M4$residuals)

tsdisplay(data1$M4.residuals)

data1 %>% ggplot(aes(x=Date,y=M4.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

####################################################### REGRESSION APPROACH ######################################################

#################################################  Scenario 1  ###################################################

#Training set: start = 10/2002, end=12/2007
#Testing set:   start = 01/2008, end=12/2008

# Separating Traning & Testing Data based on above scenarios

train = filter(data1, Date >= '2002-10-01' & Date <= '2007-12-01')
train %>% head
test = filter(data1, Date >= '2008-01-01' & Date <= '2008-12-01')

M5 = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2 , data=train) ### Best Model Scenario 1 ###
summary(M5)

train$M5 = c(NA,NA,M5$fitted.values)

# Plot data and overlay fitted values:
train %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_line(aes(x=Date,y=M5),color="red") + theme_bw()

train$M5.residuals = c(NA,NA,M5$residuals)

# Plot Residuals
train %>% ggplot(aes(x=Date,y=M5.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

# ACF & PACF Graphs of residuals
tsdisplay(train$M5.residuals)

#################################################  End Scenario 1  ###################################################

#################################################  Scenario 2  ###################################################

#Training set: start = 10/2002, end=12/2018
#Testing set:   start = 01/2019, end=12/2019

train1 = filter(data1, Date >= '2002-10-01' & Date <= '2018-12-01')
train1 %>% head
test1 = filter(data1, Date >= '2019-01-01' & Date <= '2019-12-01')


M6 = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2 , data=train1) ### Best Scenario 2 ###
summary(M6)

train1$M6 = c(NA,NA,M6$fitted.values)

# Plot data and overlay Fitted values:
train1 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_line(aes(x=Date,y=M6),color="red") + theme_bw()

train1$M6.residuals = c(NA,NA,M6$residuals)

# Plot Residuals
train1 %>% ggplot(aes(x=Date,y=M6.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

#################################################  End Scenario 2  ###################################################


#################################################  Scenario 3  ###################################################

#Training set: start = 10/2002, end=12/2019
#Testing set:   start = 01/2020, end=12/2020

train2 = filter(data1, Date >= '2002-10-01' & Date <= '2019-12-01')
train2 %>% head
test2 = filter(data1, Date >= '2020-01-01' & Date <= '2020-12-01')


M7 = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2 , data=train2) ### Best Scenario 3 ###
summary(M7)

train2$M7 = c(NA,NA,M7$fitted.values)

# Plot data and overlay Fitted values:
train2 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_line(aes(x=Date,y=M7),color="red") + theme_bw()

train2$M7.residuals = c(NA,NA,M7$residuals)

# Plot Residuals

train2 %>% ggplot(aes(x=Date,y=M7.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

#################################################  End Scenario 3  ###################################################

###################################################### Evaluation ################################################################

#Trend + Seasonality + lags used in Model:

#M5 is the Model from Scenario 1
#M6 is the Model from Secenario2
#M7 is the Model from Scenario3

#################################################  Scenario 1  ###################################################

test_pred = data.frame(Trend=64:(64+12-1),Month=paste(rep(1:12,1)),
                       DOMESTICLag1=NA,DOMESTICLag2=NA,Prediction_M5=NA)

test_pred[1,"DOMESTICLag1"] = train[63,"DOMESTIC"]
test_pred[1,"DOMESTICLag2"] = train[62,"DOMESTIC"]

head(test_pred)
tail(train)


test_pred[1,"Prediction_M5"] = predict(M5,newdata = test_pred[1,])


for(i in 2:12){
  test_pred[i,"DOMESTICLag1"] = test_pred[i-1,"Prediction_M5"]
  test_pred[i,"DOMESTICLag2"] = test_pred[i-1,"DOMESTICLag1"]
  test_pred[i,"Prediction_M5"] = predict(M5,newdata = test_pred[i,])
  
}

test$M5 = test_pred$Prediction_M5

test$M5.residuals = NA

# Add Train and Test as String to color the Train and Test part

test$TrainTest = "Test"
train$TrainTest = "Train"

train_test = rbind(train,test)
tail(train)

# Plot data and overlay fitted/predicted values and forecast on testing set

train_test %>% ggplot(aes(x=Date,y=DOMESTIC)) +geom_line() + 
  geom_line(aes(x=Date,y=M5,color= TrainTest))


#################################################  Scenario 2  ###################################################

test_pred1 = data.frame(Trend=196:(196+12-1),Month=paste(rep(1:12,1)),
                        DOMESTICLag1=NA,DOMESTICLag2=NA,Prediction_M6=NA)


test_pred1[1,"DOMESTICLag1"] = train1[195,"DOMESTIC"]
test_pred1[1,"DOMESTICLag2"] = train1[194,"DOMESTIC"]
head(test_pred1)
tail(train1)

test_pred1[1,"Prediction_M6"] = predict(M6,newdata = test_pred1[1,])


for(i in 2:12){
  test_pred1[i,"DOMESTICLag1"] = test_pred1[i-1,"Prediction_M6"]
  test_pred1[i,"DOMESTICLag2"] = test_pred1[i-1,"DOMESTICLag1"]
  test_pred1[i,"Prediction_M6"] = predict(M6,newdata = test_pred1[i,])
}


test1$M6 = test_pred1$Prediction_M6

test1$M6.residuals = NA

# Add Train and Test as String to color the Train and Test part
test1$TrainTest = "Test"
train1$TrainTest = "Train"


train_test1 = rbind(train1,test1)
tail(train_test1)

# Plot data and overlay fitted/predicted values and forecst on testing set

train_test1 %>% ggplot(aes(x=Date,y=DOMESTIC)) +geom_line() + 
  geom_line(aes(x=Date,y=M6,color= TrainTest))

#################################################  Scenario 3  ###################################################

test_pred2 = data.frame(Trend=208:(208+12-1),Month=paste(rep(1:12,1)),
                        DOMESTICLag1=NA,DOMESTICLag2=NA,Prediction_M7=NA, Prediction_M10=NA)


test_pred2[1,"DOMESTICLag1"] = train2[207,"DOMESTIC"]
test_pred2[1,"DOMESTICLag2"] = train2[206,"DOMESTIC"]
head(test_pred2)
tail(train2)

test_pred2[1,"Prediction_M7"] = predict(M7,newdata = test_pred2[1,])


for(i in 2:12){
  test_pred2[i,"DOMESTICLag1"] = test_pred2[i-1,"Prediction_M7"]
  test_pred2[i,"DOMESTICLag2"] = test_pred2[i-1,"DOMESTICLag1"]
  test_pred2[i,"Prediction_M7"] = predict(M7,newdata = test_pred2[i,])
}


test2$M7 = test_pred2$Prediction_M7

test2$M7.residuals = NA
test2$TrainTest = "Test"
train2$TrainTest = "Train"
train_test2 = rbind(train2,test2)
tail(train_test2)

# Plot data and overlay fitted/predicted values and forecast on testing set

train_test2 %>% ggplot(aes(x=Date,y=DOMESTIC)) +geom_line() + 
  geom_line(aes(x=Date,y=M7,color= TrainTest))

########################################################## End Regression Approach ##################################################


##########################################################  Auto Arima Approach  ###################################################

data3 = ts(data1[,"DOMESTIC"], start= c(2002,10), frequency = 12)
data3

#################################################  Scenario 1  ###################################################

data3.train = window(data3, end=c(2007,12))
data3.test = window(data3,start = c(2008,1),end=c(2008,12))



# Build an arima model
model = auto.arima(data3.train,lambda="auto")
# lambda="auto" = apple Box Cox transformation if it is needed
# when data has multiplicative seasonality 
# lambda = 0 -> log transformation is applied

model
# ARIMA(p=3,d=0,q=0)(P=0,D=1,Q=2)[12] with drift 
# This model includes teh following variables:
# p=3 -> yLag1, yLag2 and yLag3
# Q=2 -> residualLag12, residualLag24

# generate prediction
modelF= forecast(model,h=length(data3.test),level=95)
modelF


# is model adequate?
# Model is adequate if it explains all 
# systematic patterns in data
tsdisplay(model$residuals)
# Answer: the model pretty accurately captures 
# systematic patterns. There is one marginally 



# Plot data and overlat Fitted/Predicted Values:

autoplot(data3.train) + autolayer(modelF$fitted) +
  autolayer(data3.test)+autolayer(modelF$mean) + theme_bw()

################################################  End Scenario 1  ###################################################


################################################  Scenario 2  ###################################################
data3.train1 = window(data3, end=c(2018,12))
data3.test1 = window(data3,start = c(2019,1),end=c(2019,12))



# Build an arima model
model1 = auto.arima(data3.train1,lambda="auto")
# lambda="auto" = apple Box Cox transformation if it is needed
# when data has multiplicative seasonality 
# lambda = 0 -> log transformation is applied

model1
# ARIMA(p=3,d=0,q=0)(P=0,D=1,Q=2)[12] with drift 
# This model includes teh following variables:
# p=3 -> yLag1, yLag2 and yLag3
# Q=2 -> residualLag12, residualLag24

# generate prediction
model1F= forecast(model1,h=length(data3.test1),level=95)
model1F

# is model adequate?
# Model is adequate if it explains all 
# systematic patterns in data
tsdisplay(model1$residuals)
# Answer: the model pretty accurately captures 
# systematic patterns. There is one marginally 



# Plot data and overlat Fitted/Predicted Values:

autoplot(data3.train1) + autolayer(model1F$fitted) +
  autolayer(data3.test1)+autolayer(model1F$mean) + theme_bw()

################################################  End Scenario 2  ###################################################


################################################  Scenario 3  ###################################################

data3.train2 = window(data3, end=c(2019,12))
data3.test2 = window(data3,start = c(2020,1),end=c(2020,12))



# Build an arima model
model2 = auto.arima(data3.train2,lambda="auto")
# lambda="auto" = apple Box Cox transformation if it is needed
# when data has multiplicative seasonality 
# lambda = 0 -> log transformation is applied

model2
# ARIMA(p=3,d=0,q=0)(P=0,D=1,Q=2)[12] with drift 
# This model includes teh following variables:
# p=3 -> yLag1, yLag2 and yLag3
# Q=2 -> residualLag12, residualLag24

# generate prediction
model2F= forecast(model2,h=length(data3.test2),level=95)
model2F

# is model adequate?
# Model is adequate if it explains all 
# systematic patterns in data
tsdisplay(model2$residuals)
# Answer: the model pretty accurately captures 
# systematic patterns. There is one marginally 


# Plot data and overlay Fitted/Predicted Values:

autoplot(data3.train2) + autolayer(model2F$fitted) +
  autolayer(data3.test2)+autolayer(model2F$mean) + theme_bw()

################################################  End Scenario 3  ###################################################

############################################  End Auto Arima Approach  #################################################################

############# Regression #############

#M5 is the Model from Scenario 1
#M6 is the Model from Secenario2
#M7 is the Model from Scenario3

###### ARIMA ########################

#model is the Model from Scenario 1
#model1 is the Model from Secenario2
#model2 is the Model from Scenario3

#modelF is the Model forecast from Scenario 1
#model1F is the Model forecast from Secenario2
#model2F is the Model forecast from Scenario3

############################################ For the Training Set ############################################################

MAPE1 = c(mean(abs((train$DOMESTIC - train$M5)/train$DOMESTIC),na.rm = TRUE)*100,
          mean(abs((train1$DOMESTIC - train1$M6)/train1$DOMESTIC),na.rm = TRUE)*100,
          mean(abs((train2$DOMESTIC - train2$M7)/train2$DOMESTIC),na.rm = TRUE)*100)


RMSE1 = c(sqrt(mean((train$DOMESTIC - train$M5)^2,na.rm=TRUE)),
          sqrt(mean((train1$DOMESTIC - train1$M6)^2,na.rm=TRUE)),
          sqrt(mean((train2$DOMESTIC - train2$M7)^2,na.rm=TRUE)))


Avg_MAPE1 = mean(MAPE1)
Avg_MAPE1
Avg_MAPE2 = mean(MAPE2)
Avg_MAPE2
MAPE1

training_accuracy = data.frame(Model=NA,Scenario1_MAPE= NA, Scenario1_RMSE=NA,Scenario2_MAPE=NA,
                               Scenario2_RMSE=NA,Scenario3_MAPE=NA,Scenario3_RMSE=NA, Average_MAPE=NA)



training_accuracy[1,"Model"] = "Trend + Seasonality + Lags"
training_accuracy[1,"Scenario1_MAPE"] = MAPE1[1]
training_accuracy[1,"Scenario1_RMSE"] = RMSE1[1]
training_accuracy[1,"Scenario2_MAPE"] = MAPE1[2]
training_accuracy[1,"Scenario2_RMSE"] = RMSE1[2]
training_accuracy[1,"Scenario3_MAPE"] = MAPE1[3]
training_accuracy[1,"Scenario3_RMSE"] = RMSE1[3]
training_accuracy[1,"Average_MAPE"] = Avg_MAPE1
training_accuracy

############################################# For the Testing Set #############################################################

MAPE1 = c(mean(abs((test$DOMESTIC - test$M5)/test$DOMESTIC),na.rm = TRUE)*100,
          mean(abs((test1$DOMESTIC - test1$M6)/test1$DOMESTIC),na.rm = TRUE)*100,
          mean(abs((test2$DOMESTIC - test2$M7)/test2$DOMESTIC),na.rm = TRUE)*100)



RMSE1 = c(sqrt(mean((test$DOMESTIC - test$M5)^2,na.rm=TRUE)),
          sqrt(mean((test1$DOMESTIC - test1$M6)^2,na.rm=TRUE)),
          sqrt(mean((test2$DOMESTIC - test2$M7)^2,na.rm=TRUE)))


Avg_MAPE1 = mean(MAPE1)
Avg_MAPE1
Avg_MAPE2 = mean(MAPE2)
Avg_MAPE2

accuracy_metrics = rbind(round(accuracy(modelF,data3.test)[2,c("RMSE","MAPE")],2),
                         round(accuracy(model1F,data3.test1)[2,c("RMSE","MAPE")],2),
                         round(accuracy(model2F,data3.test2)[2,c("RMSE","MAPE")],2))

Avg_MAPE_Arima = mean(accuracy_metrics$MAPE)
Avg_MAPE_Arima


## Building data Frame to store Model's testing accuracy

testing_accuracy = data.frame(Model=NA,Scenario1_MAPE= NA, Scenario1_RMSE=NA,Scenario2_MAPE=NA,
                              Scenario2_RMSE=NA,Scenario3_MAPE=NA,Scenario3_RMSE=NA, Average_MAPE=NA)



testing_accuracy[1,"Model"] = "Regression"
testing_accuracy[1,"Scenario1_MAPE"] = MAPE1[1]
testing_accuracy[1,"Scenario1_RMSE"] = RMSE1[1]
testing_accuracy[1,"Scenario2_MAPE"] = MAPE1[2]
testing_accuracy[1,"Scenario2_RMSE"] = RMSE1[2]
testing_accuracy[1,"Scenario3_MAPE"] = MAPE1[3]
testing_accuracy[1,"Scenario3_RMSE"] = RMSE1[3]
testing_accuracy[1,"Average_MAPE"] = Avg_MAPE1
testing_accuracy[2,"Model"] = "ARIMA"
testing_accuracy[2,"Scenario1_MAPE"] = accuracy_metrics[1,2]
testing_accuracy[2,"Scenario1_RMSE"] = accuracy_metrics[1,1]
testing_accuracy[2,"Scenario2_MAPE"] = accuracy_metrics[2,2]
testing_accuracy[2,"Scenario2_RMSE"] = accuracy_metrics[2,1]
testing_accuracy[2,"Scenario3_MAPE"] = accuracy_metrics[3,2]
testing_accuracy[2,"Scenario3_RMSE"] = accuracy_metrics[2,1]
testing_accuracy[2,"Average_MAPE"] = Avg_MAPE_Arima

# Table containing the MAPE & RMSE for respective Models along with the Average Mape values

testing_accuracy


#####################################################  End Data Modeling ########################################################################

# Looking at the average MAPE across both regression and ARIMA we see that Regression has lowest avegarge MAPE across the 3 scenarios
# hence Regression model( M4 ) will be used as our champion model 

#######################################################  Deployment ##################################################################

# Reindexing data1 as per the row sequence

rownames(data1) = seq(length=nrow(data1))
data1 %>% tail

data1= data1 %>% select(Year,Month,DOMESTIC,INTERNATIONAL,Date,Trend,DOMESTICLag1,DOMESTICLag2,M4)
data1

# Creating the dummy 12 months data frame for Prediction

Year = c('2021','2021','2021','2021','2021','2022','2022','2022','2022','2022','2022','2022')
Month = c('8','9','10','11','12','1','2','3','4','5','6','7')
Trend = 227:(227+12-1)


df = data.frame(Year,Month,DOMESTIC,INTERNATIONAL,Date = transform(as.Date(paste(Year, Month,1, sep = "-"))),
                Trend,DOMESTICLag1=NA,DOMESTICLag2=NA,M4=NA)

colnames(df)[5]= "Date"

data1=rbind(data1,df)

data1 %>% tail

for(i in (227:dim(data1)[1])) {
  data1[i,"DOMESTICLag1"] = data1[i-1,"M4"]
  data1[i,"DOMESTICLag2"] = data1[i-1,"DOMESTICLag1"]
  data1[i,"M4"] = predict(M4,newdata = data1[i,])
  data1[i,"LowerBound"] = predict(M4,newdata = data1[i,], interval = "prediction")[2] 
  data1[i,"UpperBound"] = predict(M4,newdata  = data1[i,], interval = "prediction")[3] 
}

data1 %>% tail(15)

# Add Train and Test as String to color the Train and Test part

data1$TrainTest = "Test"
data1$TrainTest[data1$Date > '2021-07-01'] = "Train"

# Table containg the Predicted Values along with their 95% preiction interval (LowerBound & UpperBound)

Prediction_DOMESTIC = data1 %>% filter(Date > '2021-07-01') %>% select(Year,Month,Date, M4, LowerBound, UpperBound )
Prediction_DOMESTIC

# Plot data and overlay fitted/predicted values and forecast on testing set

data1 %>% ggplot(aes(x=Date,y=DOMESTIC)) +geom_line(size=1.25) + 
  geom_line(aes(x=Date,y=M4,color= TrainTest),size=1.25) + 
  geom_ribbon(aes(ymin = LowerBound , ymax = UpperBound),alpha=0.1,color='blue') + theme_bw()

####################################################### End Deployment ##################################################################


########################################################## COVID IMPACT #############################################################

## Assumption COVID-19 started in March 2020 hence we have split the data into a pre covid and post covid

train_covid = data1 %>% filter(Date < '2020-03-01' )
test_covid = data1 %>%  filter(Date >='2020-03-01')

## Fitting the regression model to the pre covid data

M = lm(DOMESTIC ~ Trend + Month + DOMESTICLag1 + DOMESTICLag2 , data=train_covid) 
summary(M)

train_covid

## Predicting the Post COVID data

for(i in 1:29){
  test_covid[i,"M"] = predict(M,newdata = test_covid[i,])
}

#### Calculating COVID-19 impact on Domestic Passengers

test_covid1 = test_covid %>% filter(Date <= '2021-07-01')

test_covid1$Errors = test_covid1$DOMESTIC- test_covid1$M

paste(round(100* sum(test_covid1$Errors) / sum(test_covid1$DOMESTIC),2),"%")

# As per the DOMESTIC Passengers data there is a 7.22 % decline in passengers due to COVID-19 Pandemic

####################################################### END COVID IMPACT #############################################################   


####################################################### END DOMESTIC PASSENGERS ##################################################


### INTERNATIONAL PASSENGERS

################################################  INTERNATIONAL PASSENGERS ################################################## 


################################################## Data Preparation #############################################################

url = "https://www.transtats.bts.gov/Data_Elements.aspx?Qn6n=E"

data = url %>%read_html() %>% html_nodes(xpath = '//*[@id="GridView1"]') %>% html_table()
data

data = as.data.frame(data)
data

data$DOMESTIC = as.numeric(gsub(",","",data$DOMESTIC))
data$INTERNATIONAL = as.numeric(gsub(",","",data$INTERNATIONAL))
data

# Remove"Total" from Month Column:

data = subset(data, Month != "TOTAL")
data

data1 = data %>% select(Year,Month,DOMESTIC,INTERNATIONAL)
data1

# Create a Date column for plotting Time Series

data1= transform(data1, Date = as.Date(paste(Year, Month,1, sep = "-")))
data1

############################### Data Visualization ###############################################

# DOMESTIC
data1 %>% ggplot(aes(x=Date,y=DOMESTIC)) + geom_line()+
  geom_point() + theme_bw()

# INTERNATIONAL
data1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_point() + theme_bw()

# Question 4:- 	Data Visualization 2: recreate previous graphs and use 12 colors representing 12 months  to color all points
data1 %>%  ggplot(aes(x=Date,y=DOMESTIC)) + geom_line() + 
  geom_point(aes(color=factor(Month))) +  theme_bw()

data1 %>%  ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line() + 
  geom_point(aes(color=factor(Month))) +  theme_bw()

# Question 5:- Plot both trajectories on the same graph: use 2 vertical axis: the left one for DOMESTIC and the right 
# one for INTERNATIONAL

data2 = ts(data1[,c("DOMESTIC","INTERNATIONAL")], start= c(2002,10), frequency = 12)

dygraph(data2) %>% dySeries("INTERNATIONAL", axis = 'y2')
data2


############################################################### Data Modeling ##################################################

## Checking Seasonality in the Data

M1 = lm(INTERNATIONAL ~ Month ,data=data1)
summary(M1)

# How well model fits the data?
data1$M1 = M1$fitted.values

data1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_point() + geom_line()+
  geom_line(aes(x=Date,y=M1),color="red") + theme_bw()


## Checking Model trend:

dim(data1)
dim(data1)[1]
data1$Trend = 1:(dim(data1)[1])
data1 %>% head
data1 %>% tail
typeof(data1$Month)

M2 = lm(INTERNATIONAL ~ Trend, data=data1)
summary(M2)

data1$M2 = M2$fitted.values
data1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_line(aes(x=Date,y=M2),color="red") + theme_bw()

# Checking both Trend and Seasonality

M3 = lm(INTERNATIONAL ~ Trend + Month , data=data1)
summary(M3)

data1$M3 = M3$fitted.values

data1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_line(aes(x=Date,y=M3),color="red") + theme_bw()

## Check residuals
data1$M3.residuals = M3$residuals
tsdisplay(data1$M3.residuals)

## Looking at the ACF graph it indicates significant lags and the PACF indicates Lag1 & Lag2

data1$INTERNATIONALLag1 = lag(data1$INTERNATIONAL,1)
data1$INTERNATIONALLag2 = lag(data1$INTERNATIONAL,2)

M4 = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2 , data=data1) ### Best Model Scenario 1 ###
summary(M4)

data1$M4 = c(NA,NA,M4$fitted.values)

data1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_line(aes(x=Date,y=M4),color="red") + theme_bw()

## Check residuals
data1$M4.residuals = c(NA,NA,M4$residuals)

tsdisplay(data1$M4.residuals)


data1 %>% ggplot(aes(x=Date,y=M4.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

########################################################## Regression Approach ######################################################

#################################################  Scenario 1  ###################################################

#Training set: start = 10/2002, end=12/2007
#Testing set:   start = 01/2008, end=12/2008

# Separating Traning & Testing Data based on above scenarios

train = filter(data1, Date >= '2002-10-01' & Date <= '2007-12-01')
train %>% head
test = filter(data1, Date >= '2008-01-01' & Date <= '2008-12-01')

M5 = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2 , data=train) ### Best Model Scenario 1 ###
summary(M5)

train$M5 = c(NA,NA,M5$fitted.values)

# Plot data and overlay fitted values:
train %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_line(aes(x=Date,y=M5),color="red") + theme_bw()

train$M5.residuals = c(NA,NA,M5$residuals)

# Plot Residuals
train %>% ggplot(aes(x=Date,y=M5.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

# ACF & PACF Graphs of residuals
tsdisplay(train$M5.residuals)

#################################################  End Scenario 1  ###################################################

#################################################  Scenario 2  ###################################################

#Training set: start = 10/2002, end=12/2018
#Testing set:   start = 01/2019, end=12/2019

train1 = filter(data1, Date >= '2002-10-01' & Date <= '2018-12-01')
train1 %>% head
test1 = filter(data1, Date >= '2019-01-01' & Date <= '2019-12-01')


M6 = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2 , data=train1) ### Best Scenario 2 ###
summary(M6)

train1$M6 = c(NA,NA,M6$fitted.values)

# Plot data and overlay Fitted values:
train1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_line(aes(x=Date,y=M6),color="red") + theme_bw()

train1$M6.residuals = c(NA,NA,M6$residuals)

# Plot Residuals
train1 %>% ggplot(aes(x=Date,y=M6.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

tsdisplay(train1$M6.residuals)


#################################################  End Scenario 2  ###################################################


#################################################  Scenario 3  ###################################################

#Training set: start = 10/2002, end=12/2019
#Testing set:   start = 01/2020, end=12/2020

train2 = filter(data1, Date >= '2002-10-01' & Date <= '2019-12-01')
train2 %>% head
test2 = filter(data1, Date >= '2020-01-01' & Date <= '2020-12-01')


M7 = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2 , data=train2)
summary(M7)

train2$M7 = c(NA,NA,M7$fitted.values)

# Plot data and overlay Fitted values:
train2 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) + geom_line()+
  geom_line(aes(x=Date,y=M7),color="red") + theme_bw()

train2$M7.residuals = c(NA,NA,M7$residuals)

# Plot Residuals

train2 %>% ggplot(aes(x=Date,y=M7.residuals)) + geom_line()+ 
  theme_bw()+  geom_hline(yintercept=0)

tsdisplay(train2$M7.residuals)

#################################################  End Scenario 3  ###################################################

###################################################### Evaluation ################################################################

#Trend + Seasonality + lags used in Model:

#M5 is the Model from Scenario 1
#M6 is the Model from Secenario2
#M7 is the Model from Scenario3

#################################################  Scenario 1  ###################################################

test_pred = data.frame(Trend=64:(64+12-1),Month=paste(rep(1:12,1)),
                       INTERNATIONALLag1=NA,INTERNATIONALLag2=NA,Prediction_M5=NA)

test_pred[1,"INTERNATIONALLag1"] = train[63,"INTERNATIONAL"]
test_pred[1,"INTERNATIONALLag2"] = train[62,"INTERNATIONAL"]

head(test_pred)
tail(train)


test_pred[1,"Prediction_M5"] = predict(M5,newdata = test_pred[1,])


for(i in 2:12){
  test_pred[i,"INTERNATIONALLag1"] = test_pred[i-1,"Prediction_M5"]
  test_pred[i,"INTERNATIONALLag2"] = test_pred[i-1,"INTERNATIONALLag1"]
  test_pred[i,"Prediction_M5"] = predict(M5,newdata = test_pred[i,])
  
}

test$M5 = test_pred$Prediction_M5

test$M5.residuals = NA

# Add Train and Test as String to color the Train and Test part
test$TrainTest = "Test"
train$TrainTest = "Train"

train_test = rbind(train,test)
tail(train)

# Plot data and overlay fitted/predicted values and forecast on testing set

train_test %>% ggplot(aes(x=Date,y=INTERNATIONAL)) +geom_line() + 
  geom_line(aes(x=Date,y=M5,color= TrainTest))


#################################################  Scenario 2  ###################################################

test_pred1 = data.frame(Trend=196:(196+12-1),Month=paste(rep(1:12,1)),
                        INTERNATIONALLag1=NA,INTERNATIONALLag2=NA,Prediction_M6=NA)


test_pred1[1,"INTERNATIONALLag1"] = train1[195,"INTERNATIONAL"]
test_pred1[1,"INTERNATIONALLag2"] = train1[194,"INTERNATIONAL"]
head(test_pred1)
tail(train1)

test_pred1[1,"Prediction_M6"] = predict(M6,newdata = test_pred1[1,])


for(i in 2:12){
  test_pred1[i,"INTERNATIONALLag1"] = test_pred1[i-1,"Prediction_M6"]
  test_pred1[i,"INTERNATIONALLag2"] = test_pred1[i-1,"INTERNATIONALLag1"]
  test_pred1[i,"Prediction_M6"] = predict(M6,newdata = test_pred1[i,])
}


test1$M6 = test_pred1$Prediction_M6

test1$M6.residuals = NA

# Add Train and Test as String to color the Train and Test part
test1$TrainTest = "Test"
train1$TrainTest = "Train"


train_test1 = rbind(train1,test1)
tail(train_test1)

# Plot data and overlay fitted/predicted values and forecst on testing set

train_test1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) +geom_line() + 
  geom_line(aes(x=Date,y=M6,color= TrainTest))

#################################################  Scenario 3  ###################################################

test_pred2 = data.frame(Trend=208:(208+12-1),Month=paste(rep(1:12,1)),
                        INTERNATIONALLag1=NA,INTERNATIONALLag2=NA,Prediction_M7=NA, Prediction_M10=NA)


test_pred2[1,"INTERNATIONALLag1"] = train2[207,"INTERNATIONAL"]
test_pred2[1,"INTERNATIONALLag2"] = train2[206,"INTERNATIONAL"]
head(test_pred2)
tail(train2)

test_pred2[1,"Prediction_M7"] = predict(M7,newdata = test_pred2[1,])


for(i in 2:12){
  test_pred2[i,"INTERNATIONALLag1"] = test_pred2[i-1,"Prediction_M7"]
  test_pred2[i,"INTERNATIONALLag2"] = test_pred2[i-1,"INTERNATIONALLag1"]
  test_pred2[i,"Prediction_M7"] = predict(M7,newdata = test_pred2[i,])
}


test2$M7 = test_pred2$Prediction_M7

test2$M7.residuals = NA
test2$TrainTest = "Test"
train2$TrainTest = "Train"
train_test2 = rbind(train2,test2)
tail(train_test2)

# Plot data and overlay fitted/predicted values and forecast on testing set

train_test2 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) +geom_line() + 
  geom_line(aes(x=Date,y=M7,color= TrainTest))

########################################################## End Regression Approach ##################################################


##########################################################  Auto Arima Approach  ###################################################

data3 = ts(data1[,"INTERNATIONAL"], start= c(2002,10), frequency = 12)
data3

#################################################  Scenario 1  ###################################################

data3.train = window(data3, end=c(2007,12))
data3.test = window(data3,start = c(2008,1),end=c(2008,12))



# Build an arima model
model = auto.arima(data3.train,lambda="auto")
# lambda="auto" = apple Box Cox transformation if it is needed
# when data has multiplicative seasonality 
# lambda = 0 -> log transformation is applied

model
# ARIMA(p=3,d=0,q=0)(P=0,D=1,Q=2)[12] with drift 
# This model includes teh following variables:
# p=3 -> yLag1, yLag2 and yLag3
# Q=2 -> residualLag12, residualLag24

# generate prediction
modelF= forecast(model,h=length(data3.test),level=95)
modelF
# Accuracy metrics 
#round(accuracy(M1,y.test)[,c("RMSE","MAPE")],2)
#round(accuracy(M2F,y.test)[,c("RMSE","MAPE")],2)
#round(accuracy(M3F,y.test)[,c("RMSE","MAPE")],2)

# is model adequate?
# Model is adequate if it explains all 
# systematic patterns in data
tsdisplay(model$residuals)
# Answer: the model pretty accurately captures 
# systematic patterns. There is one marginally 



# Plot data and overlat Fitted/Predicted Values:

autoplot(data3.train) + autolayer(modelF$fitted) +
  autolayer(data3.test)+autolayer(modelF$mean) + theme_bw()

################################################  End Scenario 1  ###################################################


################################################  Scenario 2  ###################################################
data3.train1 = window(data3, end=c(2018,12))
data3.test1 = window(data3,start = c(2019,1),end=c(2019,12))



# Build an arima model
model1 = auto.arima(data3.train1,lambda="auto")
# lambda="auto" = apple Box Cox transformation if it is needed
# when data has multiplicative seasonality 
# lambda = 0 -> log transformation is applied

model1
# ARIMA(p=3,d=0,q=0)(P=0,D=1,Q=2)[12] with drift 
# This model includes teh following variables:
# p=3 -> yLag1, yLag2 and yLag3
# Q=2 -> residualLag12, residualLag24

# generate prediction
model1F= forecast(model1,h=length(data3.test1),level=95)
model1F

# is model adequate?
# Model is adequate if it explains all 
# systematic patterns in data
tsdisplay(model1$residuals)
# Answer: the model pretty accurately captures 
# systematic patterns. There is one marginally 



# Plot data and overlat Fitted/Predicted Values:

autoplot(data3.train1) + autolayer(model1F$fitted) +
  autolayer(data3.test1)+autolayer(model1F$mean) + theme_bw()

################################################  End Scenario 2  ###################################################


################################################  Scenario 3  ###################################################

data3.train2 = window(data3, end=c(2019,12))
data3.test2 = window(data3,start = c(2020,1),end=c(2020,12))



# Build an arima model
model2 = auto.arima(data3.train2,lambda="auto")
# lambda="auto" = apple Box Cox transformation if it is needed
# when data has multiplicative seasonality 
# lambda = 0 -> log transformation is applied

model2
# ARIMA(p=3,d=0,q=0)(P=0,D=1,Q=2)[12] with drift 
# This model includes teh following variables:
# p=3 -> yLag1, yLag2 and yLag3
# Q=2 -> residualLag12, residualLag24

# generate prediction
model2F= forecast(model2,h=length(data3.test2),level=95)
model2F

# is model adequate?
# Model is adequate if it explains all 
# systematic patterns in data
tsdisplay(model2$residuals)
# Answer: the model pretty accurately captures 
# systematic patterns. There is one marginally 


# Plot data and overlay Fitted/Predicted Values:

autoplot(data3.train2) + autolayer(model2F$fitted) +
  autolayer(data3.test2)+autolayer(model2F$mean) + theme_bw()

################################################  End Scenario 3  ###################################################

############################################  End Auto Arima Approach  #################################################################

############# Regression #############

#M5 is the Model from Scenario 1
#M6 is the Model from Secenario2
#M7 is the Model from Scenario3

###### ARIMA ########################

#model is the Model from Scenario 1
#model1 is the Model from Secenario2
#model2 is the Model from Scenario3

#modelF is the Model forecast from Scenario 1
#model1F is the Model forecast from Secenario2
#model2F is the Model forecast from Scenario3

############################################ For the Training Set ############################################################

MAPE1 = c(mean(abs((train$INTERNATIONAL - train$M5)/train$INTERNATIONAL),na.rm = TRUE)*100,
          mean(abs((train1$INTERNATIONAL - train1$M6)/train1$INTERNATIONAL),na.rm = TRUE)*100,
          mean(abs((train2$INTERNATIONAL - train2$M7)/train2$INTERNATIONAL),na.rm = TRUE)*100)


RMSE1 = c(sqrt(mean((train$INTERNATIONAL - train$M5)^2,na.rm=TRUE)),
          sqrt(mean((train1$INTERNATIONAL - train1$M6)^2,na.rm=TRUE)),
          sqrt(mean((train2$INTERNATIONAL - train2$M7)^2,na.rm=TRUE)))


Avg_MAPE1 = mean(MAPE1)
Avg_MAPE1
Avg_MAPE2 = mean(MAPE2)
Avg_MAPE2
MAPE1

training_accuracy = data.frame(Model=NA,Scenario1_MAPE= NA, Scenario1_RMSE=NA,Scenario2_MAPE=NA,
                               Scenario2_RMSE=NA,Scenario3_MAPE=NA,Scenario3_RMSE=NA, Average_MAPE=NA)



training_accuracy[1,"Model"] = "Trend + Seasonality + Lags"
training_accuracy[1,"Scenario1_MAPE"] = MAPE1[1]
training_accuracy[1,"Scenario1_RMSE"] = RMSE1[1]
training_accuracy[1,"Scenario2_MAPE"] = MAPE1[2]
training_accuracy[1,"Scenario2_RMSE"] = RMSE1[2]
training_accuracy[1,"Scenario3_MAPE"] = MAPE1[3]
training_accuracy[1,"Scenario3_RMSE"] = RMSE1[3]
training_accuracy[1,"Average_MAPE"] = Avg_MAPE1
training_accuracy

############################################# For the Testing Set #############################################################

MAPE1 = c(mean(abs((test$INTERNATIONAL - test$M5)/test$INTERNATIONAL),na.rm = TRUE)*100,
          mean(abs((test1$INTERNATIONAL - test1$M6)/test1$INTERNATIONAL),na.rm = TRUE)*100,
          mean(abs((test2$INTERNATIONAL - test2$M7)/test2$INTERNATIONAL),na.rm = TRUE)*100)



RMSE1 = c(sqrt(mean((test$INTERNATIONAL - test$M5)^2,na.rm=TRUE)),
          sqrt(mean((test1$INTERNATIONAL - test1$M6)^2,na.rm=TRUE)),
          sqrt(mean((test2$INTERNATIONAL - test2$M7)^2,na.rm=TRUE)))


Avg_MAPE1 = mean(MAPE1)
Avg_MAPE1
Avg_MAPE2 = mean(MAPE2)
Avg_MAPE2

accuracy_metrics = rbind(round(accuracy(modelF,data3.test)[2,c("RMSE","MAPE")],2),
                         round(accuracy(model1F,data3.test1)[2,c("RMSE","MAPE")],2),
                         round(accuracy(model2F,data3.test2)[2,c("RMSE","MAPE")],2))

Avg_MAPE_Arima = mean(accuracy_metrics[,2])
Avg_MAPE_Arima


## Building data Frame to store Model's testing accuracy

testing_accuracy = data.frame(Model=NA,Scenario1_MAPE= NA, Scenario1_RMSE=NA,Scenario2_MAPE=NA,
                              Scenario2_RMSE=NA,Scenario3_MAPE=NA,Scenario3_RMSE=NA, Average_MAPE=NA)



testing_accuracy[1,"Model"] = "Regression"
testing_accuracy[1,"Scenario1_MAPE"] = MAPE1[1]
testing_accuracy[1,"Scenario1_RMSE"] = RMSE1[1]
testing_accuracy[1,"Scenario2_MAPE"] = MAPE1[2]
testing_accuracy[1,"Scenario2_RMSE"] = RMSE1[2]
testing_accuracy[1,"Scenario3_MAPE"] = MAPE1[3]
testing_accuracy[1,"Scenario3_RMSE"] = RMSE1[3]
testing_accuracy[1,"Average_MAPE"] = Avg_MAPE1
testing_accuracy[2,"Model"] = "ARIMA"
testing_accuracy[2,"Scenario1_MAPE"] = accuracy_metrics[1,2]
testing_accuracy[2,"Scenario1_RMSE"] = accuracy_metrics[1,1]
testing_accuracy[2,"Scenario2_MAPE"] = accuracy_metrics[2,2]
testing_accuracy[2,"Scenario2_RMSE"] = accuracy_metrics[2,1]
testing_accuracy[2,"Scenario3_MAPE"] = accuracy_metrics[3,2]
testing_accuracy[2,"Scenario3_RMSE"] = accuracy_metrics[2,1]
testing_accuracy[2,"Average_MAPE"] = Avg_MAPE_Arima

testing_accuracy



#####################################################  End Data Modeling ########################################################################

# Looking at the average MAPE across both regression and ARIMA we see that Regression has lowest avegarge MAPE across the 3 scenarios
# hence Regression model( M4 ) will be used as our champion model 

####################################################### Deployment ##################################################################

# Reindexing data1 as per the row sequence

rownames(data1) = seq(length=nrow(data1))
data1 %>% tail

data1= data1 %>% select(Year,Month,DOMESTIC,INTERNATIONAL,Date,Trend,INTERNATIONALLag1,INTERNATIONALLag2,M4)
data1

# Creating the dummy 12 months data frame for Prediction

Year = c('2021','2021','2021','2021','2021','2022','2022','2022','2022','2022','2022','2022')
Month = c('8','9','10','11','12','1','2','3','4','5','6','7')
Trend = 227:(227+12-1)


df = data.frame(Year,Month,DOMESTIC,INTERNATIONAL,Date = transform(as.Date(paste(Year, Month,1, sep = "-"))),
                Trend,INTERNATIONALLag1=NA,INTERNATIONALLag2=NA,M4=NA)

colnames(df)[5]= "Date"

data1=rbind(data1,df)

data1 %>% tail

for(i in (227:dim(data1)[1])) {
  data1[i,"INTERNATIONALLag1"] = data1[i-1,"M4"]
  data1[i,"INTERNATIONALLag2"] = data1[i-1,"INTERNATIONALLag1"]
  data1[i,"M4"] = predict(M4,newdata = data1[i,])
  data1[i,"LowerBound"] = predict(M4,newdata = data1[i,], interval = "prediction")[2] 
  data1[i,"UpperBound"] = predict(M4,newdata  = data1[i,], interval = "prediction")[3] 
}

data1 %>% tail(15)

# Add Train and Test as String to color the Train and Test part

data1$TrainTest = "Test"
data1$TrainTest[data1$Date > '2021-07-01'] = "Train"

Prediction_INTERNATIONAL = data1 %>% filter(Date > '2021-07-01') %>% select(Year,Month,Date, M4, LowerBound, UpperBound )
Prediction_INTERNATIONAL

# Plot data and overlay fitted/predicted values and forecast on testing set

data1 %>% ggplot(aes(x=Date,y=INTERNATIONAL)) +geom_line(size=1.25) + 
  geom_line(aes(x=Date,y=M4,color= TrainTest),size=1.25) + 
  geom_ribbon(aes(ymin = LowerBound , ymax = UpperBound),alpha=0.1,color='blue') + theme_bw()

####################################################### COVID IMPACT #############################################################

## Assumption COVID-19 started in March 2020 hence we have split the data into a pre covid and post covid

train_covid = data1 %>% filter(Date < '2020-03-01' )
test_covid = data1 %>%  filter(Date >='2020-03-01')

## Fitting the regression model to the pre covid data

M = lm(INTERNATIONAL ~ Trend + Month + INTERNATIONALLag1 + INTERNATIONALLag2 , data=train_covid) 
summary(M)

train_covid

## Predicting the Post COVID data

for(i in 1:29){
  test_covid[i,"M"] = predict(M,newdata = test_covid[i,])
}

#### Calculating COVID-19 impact on INTERNATIONAL Passengers

test_covid1 = test_covid %>% filter(Date <= '2021-07-01')

test_covid1$Errors = test_covid1$INTERNATIONAL- test_covid1$M

paste(round(100* sum(test_covid1$Errors) / sum(test_covid1$INTERNATIONAL),2),"%")

# As per the INTERNATIONAL Passengers data there is a 65.58 % decline in passengers due to COVID-19 Pandemic


################################################# END INTERNATIONAL PASSENGERS #####################################################


####################################################### ThOUGHTS & ANALYSIS  #############################################################


#The modelling approach gives a good picture but not an accurate picture of current and future demand. The model could be 
#improved by adding more models like Exponential smoothing and Naïve Bayes. Other factor which can make our model better:

# 1) Keeping track of important events and holidays in our estimation and including the analysis on the peak in passengers
#    in during these events and normal period.
# 2) Extensive data-bank - Having a large historical data which covers air passenger flows as well as economic and 
#    demographic data by country and regional aggregate.
# 3) Global trend analysis - factors affecting passenger flows such as travel liberalization, demographics, 
#    and energy costs, economic conditions


# The situations which could potentially distort predictions and cause challenges are below:-
#    
#  1)	Most forecasting methods are characterised by different methodological problems and are based on the assumption 
#     of continuation of current regulations and competitive conditions. Any change in these regulations and conditions can 
#     change our forecast.
#  2)	Changes in service quality, in the form of fares, exchange rates and aircraft type, frequencies, and schedules, 
#     among others. Expected changes in these factors must also be taken into consideration when traffic forecasts are prepared.
# 
# Adaptive Planning enables financial professionals to remain nimble and responsive to organizational, industry and global 
# economic changes. It provides capabilities to reforecast and maintain rolling forecasts - A rolling forecast incorporates actuals 
# data into a forecast to view several months of historical data combined with several months of forecast data.

#############################################################  End  ###############################################################

