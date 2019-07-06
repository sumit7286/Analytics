##Reading the dataset
german_credit<-read.csv("german.csv", header=TRUE)
library(rattle)
names(german_credit)<-normVarNames(names(german_credit))

##Structure of dataframe
str(german_credit)

##Data exploration
summary(german_credit)

credit_integer<-subset(german_credit,select = c( default_status,
                                                 duration_in_month,
                                        credit_amount,
                                        installment_rate_in_percentage_of_disposable_income,
                                        present_residence_since,
                                        age_in_years,
                                        number_of_existing_credits_at_this_bank,
                                        number_of_people_being_liable_to_provide_maintenance_for
                                        ))


names<-names(credit_integer)
names<-as.list(names)

##Descriptive statistics
descriptive <- function(x,y)
{
  hist(x, col="red", main="Histogram ")
  boxplot(x, col="yellow", main="Boxplot")
  qqnorm(x, col="green", main= "QQ plot")
  plot(density(x), col="blue", main="Density plot")
  title(y, outer=TRUE)
}

par(mfrow=c(2,2), oma=c(0,0,1,0))
mapply(descriptive,credit_integer,names)


##Dummy variable
dummy_eca<-model.matrix(~status_of_existing_checking_account, german_credit)[,-1]
dummy_ch<-model.matrix(~credit_history, german_credit)[,-1]
dummy_p<-model.matrix(~purpose, german_credit)[,-1]
dummy_sab<-model.matrix(~savings_account_bonds, german_credit)[,-1]
dummy_pms<-model.matrix(~present_employment_since, german_credit)[,-1]
dummy_psas<-model.matrix(~personal_status_and_sex, german_credit)[,-1]
dummy_odg<-model.matrix(~other_debtors_guarantors, german_credit)[,-1]
dummy_ppt<-model.matrix(~property, german_credit)[,-1]
dummy_oip<-model.matrix(~other_installment_plans, german_credit)[,-1]
dummy_h<-model.matrix(~housing, german_credit)[,-1]
dummy_js<-model.matrix(~job_status, german_credit)[,-1]
dummy_fw<-model.matrix(~foreign_worker, german_credit)[,-1]
dummy_npbl<-model.matrix(~number_of_people_being_liable_to_provide_maintenance_for, german_credit)[,-1]

data<-cbind(credit_integer,dummy_eca,dummy_ch,
            dummy_p, dummy_sab,dummy_pms, dummy_psas,dummy_odg, dummy_ppt,   
            dummy_oip, dummy_h, dummy_js,dummy_fw, dummy_npbl )


##Data split
library(caret)
set.seed(100)

train<-createDataPartition(y=data$default_status, p = .70,list = FALSE)
training<-data[train,]
testing<-data[-train,]

##Model buiding
model<-glm(default_status ~ ., data=training ,family=binomial(link='logit'))
summary(model)

library(MASS)
library(car)
step<-stepAIC(model, direction="both")
step

model_F<-glm(formula = default_status ~ duration_in_month + credit_amount + 
               installment_rate_in_percentage_of_disposable_income + status_of_existing_checking_accountA12 + 
               status_of_existing_checking_accountA13 + status_of_existing_checking_accountA14 + 
               credit_historyA34 + purposeA43 + 
               purposeA46  + savings_account_bondsA65 + 
               present_employment_sinceA74 + personal_status_and_sexA93 + 
               other_debtors_guarantorsA102 + 
               other_debtors_guarantorsA103 + other_installment_plansA143, 
             family = binomial(link = "logit"), data = training)

summary(model_F)
vif(model_F)

##Predicting 

training$fit<-predict(model_F, training, type= 'response')
testing$fit<-predict(model_F, testing, type= 'response')

training$predscore<-ifelse(training$fit>0.5,1,0)
testing$predscore<-ifelse(testing$fit>0.5,1,0)

##Model Diagnostics
##HL test
##install.packages("Hmisc")
library(Hmisc)
omers2(fitted(model_F),training$default_status)
par(mfrow=c(1,1))
plot(residuals(model_F,type="pearson"), main="Pearson Residual Plot")

##install.packages("ResourceSelection")
library(ResourceSelection)
hoslem.test(training$default_status, fitted(model_F))

##Crosstable
##install.packages("gmodels")
library(gmodels)
##Training
CrossTable(training$default_status, training$predscore,prop.chisq = FALSE,
           prop.c=FALSE, prop.r=FALSE, dnn=c('actual', 'predected'))
##Testing
CrossTable(testing$default_status, testing$predscore,prop.chisq = FALSE,
           prop.c=FALSE, prop.r=FALSE, dnn=c('actual', 'predected'))

##ROC Curve
##install.packages("ROCR")
library(ROCR)
##Training
predTraining<-prediction(training$default_status, training$predscore)
perfTraining<-performance(predTraining,"tpr", "fpr")
plot(perfTraining, main="ROC Curve", col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucTraining<- performance(predTraining,"auc")
aucTraining

##Testing
predTesting<-prediction(testing$default_status, testing$predscore)
perfTesting<-performance(predTesting,"tpr", "fpr")
plot(perfTesting, main="ROC Curve", col = 2,lwd = 2)
abline(a = 0,b = 1,lwd = 2,lty = 3,col = "black")
aucTesting<- performance(predTesting,"auc")
aucTesting

