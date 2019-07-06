##Reading dataset
cars<-read.csv("Car Mileage Dataset.csv")

##Normalizing the variable names
##install.packages("rattle")
library(rattle)
names(cars)
names(cars)<-normVarNames(names(cars))

##Understanding the datastructure for data preparation
str(cars)

##Converting variables into expected format 
cars$horsepower<-as.numeric(cars$horsepower)
cars$weight<-as.numeric(cars$weight)
cars$cylinders<-as.factor(cars$cylinders)
cars$origin<-as.factor(cars$origin)

##Data imputation
replace(cars$horsepower, cars$horsepower=='?', mean(cars$horsepower) )

##Splitting dataset for further analysis
cars_num<- subset(cars,select = c(mpg, 
                                  displacement, 
                                  horsepower, 
                                  acceleration,
                                  weight))

cars_date<-subset(cars, select=c(year_03_06, year_07_11, year_12_15))


##Data visualization 
##Displacement
par(mfrow=c(2,2), oma=c(0,0,1,0))
plot(cars_num$displacement,cars_num$mpg, pch=19,main="Scatter plot", col="orange")
boxplot(cars_num$displacement, main="Boxplot", col="blue")
hist(cars_num$displacement, main="Histogram", col="red")
qqnorm(cars_num$displacement, main="Q-Q Plot", col="green")
title("Displacement", outer=TRUE)

##Horsepower
plot(cars_num$horsepower,cars_num$mpg, pch=19,main="Scatter plot", col="orange")
boxplot(cars_num$horsepower, main="Boxplot", col="blue")
hist(cars_num$horsepower, main="Histogram", col="red")
qqnorm(cars_num$horsepower, main="Q-Q Plot", col="green")
title("Horsepower", outer=TRUE)

##Acceleration
plot(cars_num$acceleration,cars_num$mpg, pch=19,main="Scatter plot", col="orange")
boxplot(cars_num$acceleration, main="Boxplot", col="blue")
hist(cars_num$acceleration, main="Histogram", col="red")
qqnorm(cars_num$acceleration, main="Q-Q Plot", col="green")
title("Acceleration", outer=TRUE)

##Weight
plot(cars_num$weight,cars_num$mpg, pch=19,main="Scatter plot", col="orange")
boxplot(cars_num$weight, main="Boxplot", col="blue")
hist(cars_num$weight, main="Histogram", col="red")
qqnorm(cars_num$weight, main="Q-Q Plot", col="green")
title("Weight", outer=TRUE)

##Correlation plot
###install.packages("corrplot")
library(corrplot)
cor_cars<-cor(cars_num)
corrplot(cor_cars, method="number")

##Create dummy variable
###install.packages("caret")
library(caret)
dummy_cyl<-(predict(dummyVars(mpg~cylinders, data=cars), newdata=cars))
dummy_cyl<-dummy_cyl[,-1]

dummy_org<-(predict(dummyVars(mpg~origin, data=cars), newdata=cars))
dummy_org<-dummy_org[,-1]

##Arranging the required dataset in one dataframe
data<-cbind(cars_num, dummy_org,dummy_cyl, cars_date )
head(data)

##Creating training and testing data (70:30)
set.seed(100)
inTrain<-createDataPartition(y=data$mpg, p=0.7, list=FALSE)
train<-data[inTrain,]
test<-data[-inTrain,]

##Fitting regression model
model<-lm(mpg~.,data=train)
summary(model)


##Step wise regression 
#####install.packages("MASS")
library(MASS)
step<-stepAIC(model, direction="both")
step

##Final model after several trails 

model_F<-lm(formula = mpg ~ weight + origin.2 + origin.3 + 
              cylinders.6 + year_03_06 + year_07_11, 
            data = train)
summary(model_F)
##install.packages("car")
library(car)
vif(model_F)

##Prediction 
predTest<-predict(model_F, test)

##Model validation 
##MAPE Calculation
MAPE<-function(actual,predicted) {
  mean(abs(actual-predicted)/actual)
}

##Testing MAPE
MAPE(test$mpg,predTest)

##Standard residual
stdResidual = rstandard(model_F)

##Standard residual plot
plot(stdResidual, main="Model Standard Residual")

##Q-Q Plot
qqPlot(model_F$residuals, main="Residual QQ Plot")


##Density on Histogram
x <-model_F$residual 
h<-hist(x, breaks=10, col="red", xlab="Residuals", 
        main="Residual Curve") 
xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
yfit <- yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2)





