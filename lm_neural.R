#library(janitor)
library(lubridate)
#library(hms)
library(tidyr)
#library(stringr)
library(readr)
#library(openxlsx)
library(forcats)
library(RcppRoll)
library(dplyr)
library(tibble)
library(neuralnet)
library(ggplot2)
library(Metrics)

trainee <- Train

#Data cleaning and imputation
trainee <- trainee %>% mutate(Item_Visibility = na_if(Item_Visibility, 0), years = 2013 - Outlet_Establishment_Year, 
                              Item_Identifier = substring(Item_Identifier, 1, 2), 
                              Item_Fat_Content = recode(Item_Fat_Content, 'LF' = "Low Fat", 'low fat' = "Low Fat", 'reg' = "Regular")) %>% 
  mutate_at(vars(Item_Visibility), ~ifelse(is.na(.), median(., na.rm = TRUE), .)) %>%
  mutate_at(vars(Item_Weight), ~ifelse(is.na(.), mean(., na.rm = TRUE),.)) %>%
  mutate_at(vars(Outlet_Size), ~ifelse(is.na(.), "Small",.))

#Normalising the sales distribution
#Treated the outliers
IQR(trainee$Item_Outlet_Sales)
trainee$Nor_Sales <- trainee$Item_Outlet_Sales 
qnt <- quantile(trainee$Nor_Sales, probs=c(.25, .75), na.rm = T)

## 25% 75% ## 834.2474 3101.2964

caps <- quantile(trainee$Nor_Sales, probs=c(.05, .95), na.rm = T)

## 5% 95% ## 188.4214 5522.8110

H <- 1.5 * IQR(trainee$Nor_Sales, na.rm = T) 
trainee$Nor_Sales[trainee$Nor_Sales < (qnt[1] - H)] <- caps[1]
trainee$Nor_Sales[trainee$Nor_Sales > (qnt[2] + H)] <- caps[2]
 
boxplot(trainee$Nor_Sales)

#Linear model
#lm1 <- lm(Item_Outlet_Sales ~ Item_Visibility + Item_Identifier + Item_Fat_Content 
#            + Outlet_Size + Item_MRP  +
 #            Outlet_Location_Type + Outlet_Type + years, data = trainee)
lm1 <- lm(Nor_Sales ~ Item_Visibility + Item_Identifier + Item_Fat_Content 
          + Outlet_Size + Item_MRP  +
            Outlet_Location_Type + Outlet_Type + years, data = trainee)
summary(lm1)
trainee$predict.lm1 <- round(predict(lm1),0)
trainee$residuals <- trainee$Item_Outlet_Sales - trainee$predict.lm1
plot(trainee$Item_Outlet_Sales , trainee$residuals)
# used library(Metrics)
MSE.lm <- rmse (trainee$Nor_Sales, trainee$predict.lm1)

##Neural Network
#Rcode for normalization before neural net
train <- trainee
train$Item_Identifier <- case_when(
  train$Item_Identifier == "FD" ~ 1,
  train$Item_Identifier == "DR" ~ 2,
  train$Item_Identifier == "NC" ~ 3
)

train$Item_Fat_Content <- case_when(
  train$Item_Fat_Content == "Low Fat" ~1,
  train$Item_Fat_Content == "Regular" ~2
)

train$Outlet_Identifier <- case_when(
  train$Outlet_Identifier == "OUT049"~1, 
  train$Outlet_Identifier =="OUT018" ~2,
  train$Outlet_Identifier =="OUT010" ~3,
  train$Outlet_Identifier =="OUT013" ~4,
  train$Outlet_Identifier =="OUT027" ~5,
  train$Outlet_Identifier =="OUT045" ~6,
  train$Outlet_Identifier =="OUT017" ~7,
  train$Outlet_Identifier =="OUT046" ~8,
  train$Outlet_Identifier =="OUT035"~9,
  train$Outlet_Identifier =="OUT019" ~10
)

train$Outlet_Size <- case_when(
  train$Outlet_Size == "Small" ~ 1,
  train$Outlet_Size == "Medium" ~2,
  train$Outlet_Size == "High" ~3
)

train$Outlet_Location_Type <- case_when(
  train$Outlet_Location_Type == "Tier 1" ~1,
  train$Outlet_Location_Type == "Tier 3" ~3,
  train$Outlet_Location_Type == "Tier 2" ~2
)

train$Outlet_Type <- case_when(
  train$Outlet_Type == "Supermarket Type1" ~1,
  train$Outlet_Type == "Supermarket Type2" ~2,
  train$Outlet_Type == "Grocery Store" ~3,
  train$Outlet_Type == "Supermarket Type3" ~4
)
train$Item_Type <- NULL
train$Outlet_Establishment_Year <- NULL
train$predict.lm1 <- NULL
train$Item_Outlet_Sales <- NULL
train$Outlet_Identifier <- NULL
train$residuals <- NULL

#Normalisation of data
maxs <- apply(train, 2, max)
mins <- apply(train, 2, min)
n <- as.data.frame(scale(train, center = mins, scale = maxs - mins))
#Used library(neuralnet)
set.seed(2)
net <- neuralnet(Nor_Sales ~ Item_Visibility + Item_Identifier + Item_Fat_Content 
                 + Item_Weight + Outlet_Size + Item_MRP +
                   Outlet_Location_Type + Outlet_Type + years, data = n, hidden
                 = 3, linear.output = T)
plot(net)

## Make test dataset with scaled data i.e. n
n.test <- n
n.test$Nor_Sales <- NULL
predict_n.test <- neuralnet::compute(net, n.test[, net$model.list$variables])

#summary(predict_n.test)
#Length Class  Mode   
#neurons       2   -none- list   
#net.result 8523   -none- numeric

# Denormalise the sales value
train$predicted <- (predict_n.test$net.result * (max(train$Nor_Sales)-min(train$Nor_Sales))) + min(train$Nor_Sales)
train$residuals <- (train$Nor_Sales - train$predicted)
plot(train$Nor_Sales, train$residuals)
MSE.net.2 <-  sum((n$Nor_Sales - predict_n.test$net.result)^2)/8523
MSE.net <- rmse (train$Nor_Sales, train$predicted)

percent_change <- ((MSE.lm - MSE.net)/MSE.lm)*100
round(percent_change,2)

plot(train$Item_Outlet_Sales, train$predicted)


## Plot
#Used library(ggplot2)
 
train$diff <- mutate(diff = trainee$predict.lm1 - predicted) # Difference between two models
train$predict.lm <- trainee$predict.lm1

ggplot(train, aes(x=Nor_Sales)) +
  geom_point(aes(y=predict.lm, color='Linear model')) +
  geom_point(aes(y=predicted,color='Neural net')) +
  geom_abline(intercept = 0, slope = 1) +
  xlab('Actual Sales') +
  ylab('Predicted Sales')

