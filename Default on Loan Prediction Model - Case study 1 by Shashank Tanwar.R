
#============================Logistic regression Case Study 1 BY SHASHANK TANWAR==========================================================

#------------------------------Preparing the environment for Logistic Regression---------------------------------------#

list.of.packages <- c("caret", "ggplot2","MASS","car","mlogit","caTools","sqldf","Hmisc","aod","BaylorEdPsych","ResourceSelection","pROC","ROCR")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")
library(caret)
library(ggplot2)
library(MASS)
library(car)
library(mlogit)
library(sqldf)
library(Hmisc)
library(aod)
library(BaylorEdPsych)
library(ResourceSelection)
library(pROC)
library(ROCR)
library(caTools)


#Importing data

path<-"C:/Users/Stark/Documents/ivy files/R/Case studies/Case study logistic regression 1"

setwd(path)

data<- read.csv('Data_for_Logistic_Regression.csv')
data1<- data #backup file

str(data1)
summary(data1)

ncol(data1)
colnames(data1)
data2<- data1[,-c(1, 23)] # droping irrelevant columns

colnames(data2)
ncol(data2)

str(data2)
#data2$Inst_Rt_Income<- as.factor(data2$Inst_Rt_Income)
#data2$Current_Address_Yrs<- as.factor(data2$Current_Address_Yrs)
#data2$Num_CC<- as.factor(data2$Num_CC)
#data2$Dependents<- as.factor(data2$Dependents)

summary(data2)
#------------------------------------replacing null values----------------------------------------------------------------

colSums(is.na(data2)) # no null values found

#---------------------------------dividing numerical and categoracial data--------------------------------------------
colnames(data2)
str(data2)
num<- data2[,c(2,5,13,21)]
cat<- data2[,-c(2,3,13)]

#Information value for numerical variable

IVCal <- function(variable, target,data,groups)
{
  data[,"rank"] <- cut2(data[,variable],g=groups)
  tableOutput <-sqldf(sprintf("select rank, 
                              count(%s) n,
                              sum(%s) good
                              from data 
                              group by rank",target,target))
  tableOutput <- sqldf("select *,
                       (n - good) bad
                       from tableOutput")
  tableOutput$bad_rate<- tableOutput$bad/sum(tableOutput$bad)*100
  tableOutput$good_rate<- tableOutput$good/sum(tableOutput$good)*100
  tableOutput$WOE<- (log(tableOutput$good_rate/tableOutput$bad_rate))*100
  tableOutput$IV <- (log(tableOutput$good_rate/tableOutput$bad_rate))*(tableOutput$good_rate-tableOutput$bad_rate)/100
  IV <- sum(tableOutput$IV[is.finite(tableOutput$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

colnames(num)
a1<- IVCal("Duration_in_Months","Default_On_Payment",num,groups = 10)
a2<- IVCal("Credit_Amount","Default_On_Payment",num,groups = 10)
a3<- IVCal("Age","Default_On_Payment",num,groups = 10)

IV_num<- data.frame(rbind(a1,a2,a3))


#Information value for catogerical variable

CA <- function(target, variable, data) {
  A1<- fn$sqldf("select $variable,count($target)n, sum($target)good from data group by $variable")
  
  A1<- fn$sqldf("select *, (n-good) bad from A1")
  A1$bad_rate <- A1$bad/sum(A1$bad)*100
  
  A1$good_rate<- A1$good/sum(A1$good)*100
  A1$WOE<- (log(A1$good_rate/A1$bad_rate))*100
  A1$IV <- (log(A1$good_rate/A1$bad_rate))*(A1$good_rate-A1$bad_rate)/100
  IV <- sum(A1$IV[is.finite(A1$IV)])
  IV1 <- data.frame(cbind(variable,IV))
  return(IV1)
}

colnames(cat)

A<- CA("Default_On_Payment", "Status_Checking_Acc" , cat)
B<- CA("Default_On_Payment","Purposre_Credit_Taken"  , cat)
C<-CA("Default_On_Payment","Credit_Amount" , cat)
D<-CA("Default_On_Payment","Savings_Acc" , cat)
E<-CA("Default_On_Payment","Years_At_Present_Employment" , cat)
F<-CA("Default_On_Payment","Inst_Rt_Income" , cat)
G<-CA("Default_On_Payment","Marital_Status_Gender", cat)
H<-CA("Default_On_Payment","Other_Debtors_Guarantors", cat)
I<-CA("Default_On_Payment","Current_Address_Yrs", cat)
J<-CA("Default_On_Payment", "Property" , cat)
K<-CA("Default_On_Payment", "Other_Inst_Plans" , cat)
L<-CA("Default_On_Payment", "Housing", cat)
M<-CA("Default_On_Payment", "Num_CC" , cat)
N<-CA("Default_On_Payment", "Job"  , cat)
O<-CA("Default_On_Payment", "Dependents" , cat)
P<-CA("Default_On_Payment", "Telephone" , cat)
Q<-CA("Default_On_Payment","Foreign_Worker", cat)



IV_cat<- data.frame(rbind(A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q))
IV_cat

FinalIV<- rbind(IV_num,IV_cat)

write.csv(FinalIV,"FinalIV.CSV")
FinalIV<-read.csv("FinalIV.CSV")

Relevant_Variables<-FinalIV[FinalIV$IV>0.02,]

#----------------------------------Splitting Data into train and test dataset-------------------------------------------

set.seed(24)#This is used to produce reproducible results, everytime we run the model

spl = sample.split(data2$Default_On_Payment, 0.7)
data.train = subset(data2, spl == TRUE)
str(data.train)
dim(data.train)


data.test = subset(data2, spl == FALSE)
str(data.test)
dim(data.test)

#----------------------------------Creating Model on Train data--------------------------------------------------------

#Iteration 1
model1<- glm(Default_On_Payment~., data = data.train, family = binomial())
summary(model1)

colnames(data2)
#Iteration 2 Removing job,
model2<- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +Credit_History
             +Purposre_Credit_Taken +Credit_Amount +Savings_Acc +Years_At_Present_Employment 
             +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs 
             +Property + Age + Other_Inst_Plans + Housing +Num_CC+Dependents+ Telephone +Foreign_Worker, data = data.train, family = binomial())

summary(model2)

#iteration 3 removing I(Other_Inst_Plans=='A142')
model3<- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A33')+I(Credit_History=='A34')
                     +Purposre_Credit_Taken +Credit_Amount +Savings_Acc +Years_At_Present_Employment 
                     +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs 
                     +Property + Age + I(Other_Inst_Plans=='A143') + Housing +Num_CC+Dependents+ Telephone +Foreign_Worker, data = data.train, family = binomial())

summary(model3)

#Iteration 4 Removing I(Years_At_Present_Employment=='A73') and +I(Years_At_Present_Employment=='A75')
model4<- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A33')+I(Credit_History=='A34')
             +Purposre_Credit_Taken +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A72')+I(Years_At_Present_Employment=='A74') 
             +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs 
             +Property + Age + I(Other_Inst_Plans=='A143') + Housing +Num_CC+Dependents+ Telephone +Foreign_Worker, data = data.train, family = binomial())

summary(model4)
#Iteration 5 Removing 

model5<- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A33')+I(Credit_History=='A34')
             +I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A48')+I(Purposre_Credit_Taken=='A49')
             +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A72')+I(Years_At_Present_Employment=='A74') 
             +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors + Current_Address_Yrs 
             +Property + Age + I(Other_Inst_Plans=='A143') + Housing +Num_CC+Dependents+ Telephone +Foreign_Worker, data = data.train, family = binomial())

summary(model5)



#Iteration 6 Removing +I(Purposre_Credit_Taken=='A44')+I(Purposre_Credit_Taken=='A45')

model6<-glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A33')+I(Credit_History=='A34')
            +I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A48')+I(Purposre_Credit_Taken=='A49')
            +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A72')+I(Years_At_Present_Employment=='A74') 
            +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors 
            +Property + Age + I(Other_Inst_Plans=='A143') + Housing +Dependents+ Telephone +Foreign_Worker, data = data.train, family = binomial())

summary(model6)

#Final model

finalmodel<-glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A33')+I(Credit_History=='A34')
                +I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A48')+I(Purposre_Credit_Taken=='A49')
                +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A72')+I(Years_At_Present_Employment=='A74') 
                +Inst_Rt_Income +Marital_Status_Gender + Other_Debtors_Guarantors 
                +Property + Age + I(Other_Inst_Plans=='A143') + Housing +Dependents+ Telephone +Foreign_Worker, data = data.train, family = binomial())

summary(finalmodel)

#------------------------------CHECKING FITNESS OF THE MODEL---------------------------------------------------------

#MULTICOLINEARITY 
vif(finalmodel) #No evidence of multicolinearity found


wald.test(b=coef(finalmodel), Sigma= vcov(finalmodel), Terms=1:37)
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0



# Difference betweene null deviance and deviance
modelChi <- finalmodel$null.deviance - finalmodel$deviance
modelChi



#Finding the degree of freedom for Null model and model with variables
chidf <- finalmodel$df.null - finalmodel$df.residual
chidf



# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)

1-pchisq(deviance(finalmodel), df.residual(finalmodel))
#Thus, we accept the Null Hypthesis Ho thet Observed Frequencies = Expected Frequencies


#------------------------------------Predicting power of the model using R2----------------------------#


PseudoR2(finalmodel)



#getting predicted probabilities
prediction <- predict(finalmodel,newdata = data.train,type="response")
prediction
write.csv(prediction,"predtrain.csv")

# Coefficients (Odds)
finalmodel$coefficients
# Coefficients (Odds Ratio)
exp(finalmodel$coefficients)


# Variable Importance of the model
varImp(finalmodel)

#ROC curve
data.train$Default_On_Payment<- as.factor(data.train$Default_On_Payment)

roccurve<- roc(response = data.train$Default_On_Payment, predictor = prediction, 
               levels = rev(levels(data.train$Default_On_Payment)))

plot(roccurve)

#metrics
predclass <-ifelse(prediction>coords(roccurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.train$Default_On_Payment)
data.train$predclass<- predclass
write.csv(data.train, file = "TrainPredictions.csv")

AccuracyRate <- sum(diag(Confusion))/sum(Confusion)

Gini <-2*auc(roccurve)-1

AUCmetric <- data.frame(c(coords(roccurve,"best"),AUC=auc(roccurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(roccurve)


### KS statistics calculation
data.train$m1.yhat <- predict(finalmodel, data.train, type = "response")
m1.scores <- prediction(data.train$m1.yhat, data.train$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")


m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7


#---------------------------------testing on Test dataset------------------------------------------------------------

#iteration1 #Removing +I(Marital_Status_Gender=='A92')+I(Marital_Status_Gender=='A94')
modeltest<- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A33')+I(Credit_History=='A34')
                +I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A48')+I(Purposre_Credit_Taken=='A49')
                +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A72')+I(Years_At_Present_Employment=='A74') 
                +Inst_Rt_Income +I(Marital_Status_Gender=='A93') + Other_Debtors_Guarantors 
                +Property + Age + I(Other_Inst_Plans=='A143') + Housing +Dependents+ Telephone +Foreign_Worker, data = data.test, family = binomial())

summary(modeltest)

#iteration 2 Removing +I(Years_At_Present_Employment=='A74'), I(Purposre_Credit_Taken=='A48') I(Other_Debtors_Guarantors=='A102')

modeltest2 <- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A34')
                  +I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A49')
                  +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A72')+I(Years_At_Present_Employment=='A74') 
                  +Inst_Rt_Income +I(Marital_Status_Gender=='A93')+I(Marital_Status_Gender=='A94') + I(Other_Debtors_Guarantors=='A103') 
                  +Property + Age + I(Other_Inst_Plans=='A143') + Housing +Dependents+ Telephone +Foreign_Worker, data = data.test, family = binomial())

summary(modeltest2)

#Iteration 3 Removing +I(Years_At_Present_Employment=='A72'),+I(Property=='A122')+I(Property=='A123'), Dependents
modeltest3<- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A34')
            +I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A49')
            +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A74') 
            +Inst_Rt_Income +I(Marital_Status_Gender=='A93')+I(Marital_Status_Gender=='A94') + I(Other_Debtors_Guarantors=='A103') 
            +I(Property=='A124') + Age + I(Other_Inst_Plans=='A143') + Housing  +Telephone +Foreign_Worker, data = data.test, family = binomial())

summary(modeltest3)

#-------------------------Testing Model fitness--------------------------------------------------------------------=-


#Checking Multicolinearity

vif(modeltest3) #No multicolinearity found 





wald.test(b=coef(modeltest3), Sigma= vcov(modeltest3), Terms=1:28)
#Since, p-value is less then 0.001, hence we reject Ho that the all Bi=0


#------------------->Lagrange Multiplier or Score Test (Assess wether the current variable 
# Difference betweene null deviance and deviance
modelChi <- modeltest3$null.deviance - modeltest3$deviance
modelChi



#Finding the degree of freedom for Null model and model with variables
chidf <- modeltest3$df.null - modeltest3$df.residual
chidf



# If p value is less than .05 then we reject the null hypothesis that the model is no better than chance.
chisq.prob <- 1 - pchisq(modelChi, chidf)
format(round(chisq.prob, 2), nsmall = 5)


#--------------------Lackfit Deviance

1-pchisq(deviance(modeltest3), df.residual(modeltest3))
#Thus, we accept the Null Hypthesis Ho that Observed Frequencies = Expected Frequencies


#------------------------------------Predicting power of the model using R2----------------------------#


PseudoR2(modeltest3)



#getting predicted probabilities
prediction <- predict(modeltest3,newdata = data.test,type="response")
prediction
write.csv(prediction,"predtest.csv")

# Coefficients (Odds)
modeltest3$coefficients
# Coefficients (Odds Ratio)
exp(modeltest3$coefficients)


# Variable Importance of the model
varImp(modeltest3)

#ROC curve
data.test$Default_On_Payment<- as.factor(data.test$Default_On_Payment)

roccurve<- roc(response = data.test$Default_On_Payment, predictor = prediction, 
               levels = rev(levels(data.test$Default_On_Payment)))

plot(roccurve)

#metrics
predclass <-ifelse(prediction>coords(roccurve,"best")[1],1,0)
Confusion <- table(Predicted = predclass,Actual = data.test$Default_On_Payment)
data.test$predclass<- predclass
write.csv(data.test,file = "testPrediction.csv")

AccuracyRate <- sum(diag(Confusion))/sum(Confusion)

Gini <-2*auc(roccurve)-1

AUCmetric <- data.frame(c(coords(roccurve,"best"),AUC=auc(roccurve),AccuracyRate=AccuracyRate,Gini=Gini))
AUCmetric <- data.frame(rownames(AUCmetric),AUCmetric)
rownames(AUCmetric) <-NULL
names(AUCmetric) <- c("Metric","Values")
AUCmetric

Confusion 
plot(roccurve)


### KS statistics calculation
data.test$m1.yhat <- predict(modeltest3, data.test, type = "response")
m1.scores <- prediction(data.test$m1.yhat, data.test$Default_On_Payment)

plot(performance(m1.scores, "tpr", "fpr"), col = "red")
abline(0,1, lty = 8, col = "grey")


m1.perf <- performance(m1.scores, "tpr", "fpr")
ks1.logit <- max(attr(m1.perf, "y.values")[[1]] - (attr(m1.perf, "x.values")[[1]]))
ks1.logit # Thumb rule : should lie between 0.4 - 0.7

#Final model ready to apply on unseen data

FinalModel<- glm(Default_On_Payment~ Status_Checking_Acc +Duration_in_Months +I(Credit_History=='A32')+I(Credit_History=='A34')
                 +I(Purposre_Credit_Taken=='A41')+I(Purposre_Credit_Taken=='A410')+I(Purposre_Credit_Taken=='A42')+I(Purposre_Credit_Taken=='A43')+I(Purposre_Credit_Taken=='A49')
                 +Credit_Amount +Savings_Acc +I(Years_At_Present_Employment=='A74') 
                 +Inst_Rt_Income +I(Marital_Status_Gender=='A93')+I(Marital_Status_Gender=='A94') + I(Other_Debtors_Guarantors=='A103') 
                 +I(Property=='A124') + Age + I(Other_Inst_Plans=='A143') + Housing  +Telephone +Foreign_Worker, data = data.test, family = binomial())

summary(FinalModel)

FinalModel$coefficients

exp(FinalModel$coefficients)













