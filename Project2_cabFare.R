#remove all the objects stored
rm(list=ls())

library('ggplot2')
library('DataExplorer')
install.packages('DataExplorer')
install.packages('ggExtra')
library('ggExtra')
#set current working directory
setwd("F:/backup F ddrive/anushka")

cabfare = read.csv('F:/train_cab.csv', header = T)

str(cabfare)

View(cabfare) 

###############Converting Data types######################
cabfare$fare_amount = as.numeric(cabfare$fare_amount)
cabfare$pickup_datetime = gsub( " UTC", "", as.character(cabfare$pickup_datetime))
cabfare$pickup_datetime <- as.numeric(as.POSIXct(cabfare$pickup_datetime,format="%Y-%m-%d %H:%M:%S"))



###############missing values analysis###########################
missing_val = data.frame(apply(cabfare,2,function(x){sum(is.na(x))}))
View(missing_val)

plot_missing(cabfare) #Visualization
cabfare$passenger_count[is.na(cabfare$passenger_count)] = mean(cabfare$passenger_count, na.rm = T)
cabfare$fare_amount[is.na(cabfare$fare_amount)] = mean(cabfare$fare_amount, na.rm = T)
cabfare$pickup_datetime[is.na(cabfare$pickup_datetime)] = mean(cabfare$pickup_datetime, na.rm = T)

###################################################################
#########BoxPLOT###################################
#####################################
cnames = cabfare[,]

for (i in 1:length(cnames))
{
  assign(paste0("gn",i), ggplot(aes_string(y = (cnames[i]), x = "responded"), data = subset(cabfare))+
           stat_boxplot(geom = "errorbar", width = 0.5) +
           geom_boxplot(outlier.colour="red", fill = "grey" ,outlier.shape=18,
                        outlier.size=1, notch=FALSE) +
           theme(legend.position="bottom")+
           labs(y=cnames[i],x="responded")+
           ggtitle(paste("Box plot of responded for",cnames[i])))
}

## Plotting plots together
gridExtra::grid.arrange(gn1,gn5,gn2,ncol=3)
gridExtra::grid.arrange(gn6,gn7,ncol=2)
gridExtra::grid.arrange(gn8,gn9,ncol=2)


################BOX PLOT###############
cnames = colnames(cabfare[,1:7])
for(i in cnames){
  val = cabfare[,i][cabfare[,i] %in% boxplot.stats(cabfare[,i])$out]
  #print(length(val))
  cabfare=  cabfare[which(!cabfare[,i] %in% val),] 
}


############Calculating Distance########
install.packages('geosphere')
library(geosphere)
library(dplyr)

#Haversine formula (hf) 
# gcd.hf = function(long1, lat1, long2, lat2) {   R = 6371 # Earth mean radius [km]   delta.long = (long2 - long1)   delta.lat = (lat2 - lat1) 
# a = sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2 
# c = 2 * asin(min(1,sqrt(a)))   d = R * c 
# return(d) # Distance in km 
# } 
# 
# # Now, I am going to apply the function, over all the rows to create a new variable - distance for (i in 1:nrow(train_cab)){ 
# train_cab$distance[i] = gcd.hf(train_cab$pickup_longitude[i], train_cab$pickup_latitude[i],                                   train_cab$dropoff_longitude[i], train_cab$dropoff_latitude[i]) } 




modified_distCosine <- function(Longitude1, Latitude1, Longitude2, Latitude2) {
  if (any(is.na(c(Longitude1, Latitude1, Longitude2, Latitude2)))) {
    NA
  } else {
    distCosine(c(Longitude1, Latitude1), c(Longitude2, Latitude2))
  }
}


cabfare = mutate(cabfare, distance = mapply(modified_distCosine, 
                         pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude))
#####dropping longitutde and latitude##############
cabfare <- cabfare[, -c(3:6)]

##################################Feature Selection################################################
## Correlation Plot
install.packages("corrgram")
library(corrgram)
corrgram(cabfare[1:12156,1:4], order = F,
         upper.panel=panel.pie, text.panel=panel.txt, main = "Correlation Plot")

#####Anova testing##########

anova_one_way = aov(distance~passenger_count, data =cabfare)
summary(anova_one_way)

anova_one_way_fare = aov(fare_amount~passenger_count, data =cabfare)
summary(anova_one_way)
cabfare <- cabfare[, -c(3:3)]

##Go through each row and determine if a value is zero
row_sub = apply(cabfare, 1, function(row) all(row !=0.000000 ))
##Subset as usual
cabfare[row_sub,]
x = cabfare$fare_amount !=0.0000000

##################################Feature Scaling################################################
#Normality check
qqnorm(cabfare$fare_amount)
qq_data = cabfare[, c("fare_amount","distance")]
plot_qq(qq_data, sampled_rows = 1000L)

#Normalisation
cnames = c("distance","fare_amount","pickup_datetime")

for(i in cnames){
  print(i)
  cabfare[,i] = (cabfare[,i] - min(cabfare[,i]))/
    (max(cabfare[,i] - min(cabfare[,i])))
}



######################## DECISSION TREE REGRESSION ##############################
library(rpart)
library(MASS)
library(caTools)
######trest and train data########################
train.index = sample(1:nrow(cabfare),0.8*nrow(cabfare))
train = cabfare[ train.index,]
test  = cabfare[-train.index,]

####r part for regression###
fit=rpart(fare_amount~.,data=cabfare,method="anova")

###prediction test
predictions=predict(fit,test[,-1])

############################ERROR MATRICS#################
library(DMwR)
mape=function (y,yhat){
  mean(abs((y-yhat)/y))*100
}
mape(test[,1],predictions)
test[,1]
library(DMwR)

regr.eval(test[,1],predictions,stats=c('mae','rmse'))

Rsquare = function(y,y1){ 
  cor(y,y1)^2 
} 

# Calculating Rsquam for Test Data 
DT = Rsquare(test[,1], predictions) 
DT

#########################################################

#Linear Regression
#check multicollearity
install.packages('usdm')
library(usdm)
vif(cabfare[,-1])

vifcor(cabfare[,-1], th = 0.9)

#run regression model
lm_model = lm(fare_amount ~., data = train)

#Summary of the model
summary(lm_model)

#Predict
predictions_LR = predict(lm_model, test[,2:3])

#Calculate rSq.
LR= Rsquare(test[,1], predictions_LR)
LR
##########################################################################
###Random Forest
install.packages(c('randomForest', 'mlr', 'ParamHelpers'))
library(randomForest)
library(mlr)
library(ParamHelpers)
#model 
rf_model = randomForest(x=train[,-1],y=train$fare_amount, importance = TRUE, ntree = 100)
#summary 
summary(rf_model)
#predictions
predict_rf=predict(rf_model,test[,-1])
#Error Matrics
rmse_rf=measureRMSE(as.numeric(test$fare_amount),as.numeric(predict_rf))
RF= Rsquare(test[,1], predict_rf)
RF

#####################################Test Data############


cab_test = read.csv('F:/test.csv', header = T)

str(cab_test)


###############Converting Data types######################

cab_test$pickup_datetime = gsub( " UTC", "", as.character(cab_test$pickup_datetime))
cab_test$pickup_datetime <- as.numeric(as.POSIXct(cab_test$pickup_datetime,format="%Y-%m-%d %H:%M:%S"))



###############missing values analysis###########################
missing_val = data.frame(apply(cab_test,2,function(x){sum(is.na(x))}))
View(missing_val)


###################################################################
#########BoxPLOT###################################
#####################################

################BOX PLOT###############
cnames = colnames(cab_test[,1:6])
for(i in cnames){
  val = cab_test[,i][cab_test[,i] %in% boxplot.stats(cab_test[,i])$out]
  #print(length(val))
  cab_test=  cab_test[which(!cab_test[,i] %in% val),] 
}


############Calculating Distance########

cab_test = mutate(cab_test, distance = mapply(modified_distCosine, 
                                            pickup_longitude, pickup_latitude, dropoff_longitude, dropoff_latitude))
#####dropping longitutde and latitude##############
View(cab_test)
cab_test <- cab_test[, -c(2:5)]
cab_test <- cab_test[, -c(2:2)]
#Normalisation
cnames = c("distance", "pickup_datetime")

for(i in cnames){
  print(i)
  cabfare[,i] = (cabfare[,i] - min(cabfare[,i]))/
    (max(cabfare[,i] - min(cabfare[,i])))
}

#Predicting Actual Test Data

predictions=predict(fit,cab_test[,])
predictions
