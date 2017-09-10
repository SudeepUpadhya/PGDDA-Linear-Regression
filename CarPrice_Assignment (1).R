install.packages("tidyr")
install.packages("stringr")
install.packages("dplyr")
install.packages("MASS")
install.packages("car")
library(tidyr)
library(stringr)
library(dplyr)
library(MASS)
library(car)

setwd("D:/Upgrad/R")
Carprice<- CarPrice_Assignment <- read.csv("CarPrice_Assignment.csv")
str(Carprice)


#check for Missing value
sum(is.na(Carprice))
#[1] 0

#remove duplicate values (if any)
Carprice2<-unique(Carprice)
#same 205 observations avaiable.Hence no duplicates

#Data Preparation and Data Cleaning
#Convert CarName to lower case
Carprice$CarName<- tolower(Carprice$CarName)
#format CarName with spaces
Carprice$CarName <- str_replace_all(Carprice$CarName, "-",  " ")
#Create separate column with Company name
Carprice<-separate(Carprice,CarName,into = c("Company","Brand"), sep= " ")
Carprice<-Carprice[,-4] #remove Brand column

# correct spelling mistakes in Company names
Carprice$Company <- str_replace_all(Carprice$Company, "maxda",  "mazda")
Carprice$Company <- str_replace_all(Carprice$Company, "porcshce",  "porsche")
Carprice$Company <- str_replace_all(Carprice$Company, "vokswagen","volkswagen")
Carprice$Company <- str_replace_all(Carprice$Company, "vw","volkswagen")
Carprice$Company <- str_replace_all(Carprice$Company, "toyouta",  "toyota")

Carprice$Company<-as.factor(Carprice$Company)


#Check outliers and standardize them
quantile(Carprice$curbweight,seq(0,1,0.01))
Carprice$curbweight[which(Carprice$curbweight<1819.72)]<-1819.72
quantile(Carprice$enginesize,seq(0,1,0.01))
Carprice$enginesize[which(Carprice$enginesize>209.00)]<-209.00
quantile(Carprice$boreratio,seq(0,1,0.01))
Carprice$boreratio[which(Carprice$boreratio<2.9100)]<-2.9100
quantile(Carprice$stroke,seq(0,1,0.01))
Carprice$stroke[which(Carprice$stroke>3.6400)]<-3.6400
quantile(Carprice$compressionratio,seq(0,1,0.01))
Carprice$compressionratio[which(Carprice$compressionratio>10.9400)]<-10.9400
quantile(Carprice$horsepower,seq(0,1,0.01))
Carprice$horsepower[which(Carprice$horsepower>207.00)]<-207.00
quantile(Carprice$highwaympg,seq(0,1,0.01))
Carprice$highwaympg[which(Carprice$highwaympg>46.92)]<-46.92


#Create 2 bins for each column and convert them into numeric
levels(Carprice$fueltype)<-c(0,1)
Carprice$fueltype<- as.numeric(Carprice$fueltype)

levels(Carprice$aspiration)<-c(0,1)
Carprice$aspiration<- as.numeric(Carprice$aspiration)

levels(Carprice$doornumber)<-c(0,1)
Carprice$doornumber<- as.numeric(Carprice$doornumber)

levels(Carprice$enginelocation)<-c(0,1)
Carprice$enginelocation<- as.numeric(Carprice$enginelocation)

#Creation of dummy variables for factors

#creation of levels/bins
levels(Carprice$Company)[1:5]<-"Car1"
levels(Carprice$Company)[2:6]<-"Car2"
levels(Carprice$Company)[3:7]<-"Car3"
levels(Carprice$Company)[4:8]<-"Car4"
levels(Carprice$Company)[5:9]<-"Car5"

dummy_1<-data.frame(model.matrix(~Company,data=Carprice))
dummy_1<-dummy_1[,-1] 
Carprice<-cbind(Carprice[,-3], dummy_1)

#creation of dummy variables for carbody
dummy2<-data.frame(model.matrix(~carbody, data=Carprice))
dummy2<-dummy2[,-1]
Carprice<-cbind(Carprice[,-6],dummy2)

#creation of dummy variables for drivewheel
dummy3<-data.frame(model.matrix(~drivewheel, data=Carprice))
dummy3<-dummy3[,-1]
Carprice<-cbind(Carprice[,-6],dummy3)

#Creation of level/bin for enginetype
levels(Carprice$enginetype)[1:3]<-"low"
levels(Carprice$enginetype)[2:3]<-"Meduim"
levels(Carprice$enginetype)[3:5]<-"High"

dummy4<-data.frame(model.matrix(~enginetype, data=Carprice))
dummy4<-dummy4[,-1]
Carprice<-cbind(Carprice[,-12],dummy4)

#Creation of level/bin for cylindernuber
levels(Carprice$cylindernumber)[1:3]<-"low"
levels(Carprice$cylindernumber)[2:3]<-"Powerful"
levels(Carprice$cylindernumber)[3:5]<-"Premuim"

dummy5<-data.frame(model.matrix(~cylindernumber, data=Carprice))
dummy5<-dummy5[,-1]
Carprice<-cbind(Carprice[,-12],dummy5)

#Creation of level/bin for fuelsystem
levels(Carprice$fuelsystem)[1:2]<-"system1"
levels(Carprice$fuelsystem)[2:3]<-"system2"
levels(Carprice$fuelsystem)[3:4]<-"system3"
levels(Carprice$fuelsystem)[4:5]<-"system4"

dummy6<-data.frame(model.matrix(~fuelsystem, data=Carprice))
dummy6<-dummy6[,-1]
Carprice<-cbind(Carprice[,-13],dummy6)

str(Carprice)
#All have been converted to numeric/int variables to start the model

#To convert the data in Test and Train
set.seed(100)
trainindices= sample(1:nrow(Carprice), 0.7*nrow(Carprice))
train = Carprice[trainindices,]

test = Carprice[-trainindices,]

model_1<-lm(price~., data=train)
summary(model_1)

stepAIC(model_1,direction="both")
model_2<-lm(price ~ fueltype + enginelocation + wheelbase + 
              curbweight + enginesize + boreratio + stroke + horsepower + 
              peakrpm + citympg + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
              CompanyCar5 + carbodyhardtop + carbodyhatchback + carbodysedan + 
              carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
              cylindernumberPremuim + fuelsystemsystem3, data = train)
  
summary(model_2)

vif(model_2)
#Multiple R-squared:  0.9456,	Adjusted R-squared:  0.9351 
#curweight & enginesize have high vif but cannot be removed since these are highly significant.
#vercarbodysedan has high Vif and has low significane in summary hence can be removed in model_3.


model_3<-lm(price ~ fueltype + enginelocation + wheelbase + 
                 curbweight + enginesize + boreratio + stroke + horsepower + 
                 peakrpm + citympg + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
                 CompanyCar5 + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
                 cylindernumberPremuim + fuelsystemsystem3, data = train)

vif(model_3)
summary(model_3)

#Multiple R-squared:  0.9441,	Adjusted R-squared:  0.9338
#check if there is correlation between curbweight & enginesize as they have high vif.
cor(train$curbweight,train$enginesize)
#[1] 0.8602367
# correlation is 86% between curbweight & enginesize.enginesize with lower significance can be removed in model_4.

model_4<-lm(price ~ fueltype + enginelocation + wheelbase + 
              curbweight + boreratio + stroke + horsepower + 
              peakrpm + citympg + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
              CompanyCar5 + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
              cylindernumberPremuim + fuelsystemsystem3, data = train)

vif(model_4)
summary(model_4)
#Multiple R-squared:  0.9349,	Adjusted R-squared:  0.9236 
# boreratio with high vif and is insignificant variable in summary can be removed in model_5.

model_5<-lm(price ~ fueltype + enginelocation + wheelbase + 
              curbweight + stroke + horsepower + 
              peakrpm + citympg + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
              CompanyCar5 + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
              cylindernumberPremuim + fuelsystemsystem3, data = train)

vif(model_5)
summary(model_5)
#Multiple R-squared:  0.9349,	Adjusted R-squared:  0.9242 
#check for correlation between curbweight & horsepower
cor(train$curbweight,train$horsepower)
#[1] 0.7478
# correlation is at 74% .Hence horsepower with lower significance can be removed in model_6

model_6<-lm(price ~ fueltype + enginelocation + wheelbase + 
              curbweight + stroke + 
              peakrpm + citympg + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
              CompanyCar5 + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
              cylindernumberPremuim + fuelsystemsystem3, data = train)
vif(model_6)
summary(model_6)
#Multiple R-squared:  0.9195,	Adjusted R-squared:  0.907 
# wheelbase has high vif and insignificant variable in summary hence removed in model_7

model_7<-lm(price ~ fueltype + enginelocation +
                 curbweight + stroke + 
                 peakrpm + citympg + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
                 CompanyCar5 + carbodyhardtop + carbodyhatchback + 
                 carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
                 cylindernumberPremuim + fuelsystemsystem3, data = train)
  
  vif(model_7)
summary(model_7)
#Multiple R-squared:  0.9177,	Adjusted R-squared:  0.9058
#citympg has hihg vif and is insignificant. can be removed in model_8.

model_8<-lm(price ~ fueltype + enginelocation +
              curbweight + stroke + 
              peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
              CompanyCar5 + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
              cylindernumberPremuim + fuelsystemsystem3, data = train)
  
vif(model_8)
summary(model_8)
#Multiple R-squared:  0.9159,	Adjusted R-squared:  0.9045 
#fuelsystemsystem3 has high vif and is insignificant variable in summary. can be removed in model_9

model_9<-lm(price ~ fueltype + enginelocation +
              curbweight + stroke + 
              peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
              CompanyCar5 + carbodyhardtop + carbodyhatchback + 
              carbodywagon + drivewheelfwd + enginetypeHigh + cylindernumberPowerful + 
              cylindernumberPremuim , data = train)
  
  vif(model_9)
summary(model_9)
#Multiple R-squared:  0.9143,	Adjusted R-squared:  0.9034
#Since variable having vif>2 have high significance, hence cannot be removed further. 
#We will remove variables based on higher P value which are not significant.cylindernumberPowerful has high P value and can be removed in model_10

model_10<-lm(price ~ fueltype + enginelocation +
               curbweight + stroke + 
               peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + carbodyhardtop + carbodyhatchback + 
               carbodywagon + drivewheelfwd + enginetypeHigh +
               cylindernumberPremuim , data = train)
  vif(model_10)
summary(model_10)
#Multiple R-squared:  0.914,	Adjusted R-squared:  0.9039
#cylindernumberPremuim has high P value. we remove cylindernumberPremuim from model_11 as its insignificant 

model_11<-lm(price ~ fueltype + enginelocation +
               curbweight + stroke + 
               peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + carbodyhardtop + carbodyhatchback + 
               carbodywagon + drivewheelfwd + enginetypeHigh 
               , data = train)
  
  vif(model_11)
summary(model_11)
#Multiple R-squared:  0.9137,	Adjusted R-squared:  0.9043
# carbodyhatchback has high P value and is insignificant. Hence can be removed in model_12

model_12<-lm(price ~ fueltype + enginelocation +
               curbweight + stroke + 
               peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + carbodyhardtop + 
               carbodywagon + drivewheelfwd + enginetypeHigh 
             , data = train)
  
  vif(model_12)
summary(model_12)
#Multiple R-squared:  0.913,	Adjusted R-squared:  0.9043 
#fueltype has high P value and become insignificant.Can be removed in model_13

model_13<-lm(price ~ enginelocation +
               curbweight + stroke + 
               peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + carbodyhardtop + 
               carbodywagon + drivewheelfwd + enginetypeHigh 
             , data = train)
vif(model_13)
summary(model_13)
#Multiple R-squared:  0.9124,	Adjusted R-squared:  0.9043
#check to seek if there is correlation between drivewheel & curbweight
cor(train$drivewheelfwd,train$curbweight)
#[1] -0.6995848
# there is 70% correlation between them. we can remove drivewheelfwd from model_14 which is in lower significance level 

model_14<-lm(price ~ enginelocation +
               curbweight + stroke + 
               peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + carbodyhardtop + 
               carbodywagon + enginetypeHigh 
             , data = train)
  
summary(model_14)
vif(model_14)
#Multiple R-squared:  0.9124,	Adjusted R-squared:  0.9043
# we can remove carbodyhardtop from model_15 as it has high P value 

model_15<-lm(price ~ enginelocation +
               curbweight + stroke + 
               peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + 
               carbodywagon + enginetypeHigh 
             , data = train)
summary(model_15)
vif(model_15)
#Multiple R-squared:  0.9032,	Adjusted R-squared:  0.8959
#stroke can be removed from model_16 since its insignificant and has high p value 
 
model_16<-lm(price ~ enginelocation +
               curbweight +
               peakrpm + CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + 
               carbodywagon + enginetypeHigh 
             , data = train)
summary(model_16)
vif(model_16)
#Multiple R-squared:  0.9016,	Adjusted R-squared:  0.8949 
#peakrpm can be removed from model_17 since its insignifcant 

model_17<-lm(price ~ enginelocation +
               curbweight +
               CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + 
               carbodywagon + enginetypeHigh 
             , data = train)
summary(model_17)
vif(model_17)
#Multiple R-squared:  0.8996,	Adjusted R-squared:  0.8936 
# can remove from model_18 enginetypeHigh which is significant 

model_18<-lm(price ~ enginelocation +
               curbweight +
               CompanyCar2 + CompanyCar3 + CompanyCar4 + 
               CompanyCar5 + 
               carbodywagon 
             , data = train)
summary(model_18)
vif(model_18)
#Multiple R-squared:  0.8926,	Adjusted R-squared:  0.887  
Predict1<-predict(model_18, test[, -20])
cor(test$price,Predict1)^2
#[1] 0.826131

#Adjusted R-Squared is 88% and test R-squared is 82%. There is a deviation of 6% between 
#the R-Square which is in the acceptable range.model_18 is the final model based on which car price can be Predicted.

#We can conclude following are the Driver variables by which price of the Car can be fixed.
#enginelocation
#curbweight
#CompanyCar
#carbodywagon
