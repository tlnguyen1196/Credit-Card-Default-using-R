 # Change the file path to the file location.
setwd("/Users/tlnguyen/Documents/Study/Data 630/Module 3")
# Read the CSV file. 
credit <- read.csv("DefaultCreditCardClients.csv", head =TRUE, sep=",", as.is=FALSE)
options(scipen=999)
# Exploratory Analysis
str(credit) #check the data structure 
summary(credit) #check the descriptive statistic
colSums(is.na(credit)) #check missing value again
# Preprocessing 
credit$ID <- NULL #remove the unique identifier
#change numeric to factor 
credit$SEX <-factor(credit$SEX, level = 1:2, labels = c("male", "female"))
credit$EDUCATION <-factor(credit$EDUCATION, level = 1:4, labels = c("graduate school", "university", "high school", "others"))
credit$MARRIAGE <-factor(credit$MARRIAGE, level = 1:3, labels = c("married", "single", "others"))
credit$PAY_0 <-factor(credit$PAY_0, level = -1:8, labels = c("pay duly", "payment delay for one month", "payment delay for two months", "payment delay for three months", "payment delay for four months", "payment delay for five months", "payment delay for six months", "payment delay for seven months", "payment delay for eight months", "payment delay for nine months and above"))
credit$PAY_2 <-factor(credit$PAY_2, level = -1:8, labels = c("pay duly", "payment delay for one month", "payment delay for two months", "payment delay for three months", "payment delay for four months", "payment delay for five months", "payment delay for six months", "payment delay for seven months", "payment delay for eight months", "payment delay for nine months and above"))
credit$PAY_3 <-factor(credit$PAY_3, level = -1:8, labels = c("pay duly", "payment delay for one month", "payment delay for two months", "payment delay for three months", "payment delay for four months", "payment delay for five months", "payment delay for six months", "payment delay for seven months", "payment delay for eight months", "payment delay for nine months and above"))
credit$PAY_4 <-factor(credit$PAY_4, level = -1:8, labels = c("pay duly", "payment delay for one month", "payment delay for two months", "payment delay for three months", "payment delay for four months", "payment delay for five months", "payment delay for six months", "payment delay for seven months", "payment delay for eight months", "payment delay for nine months and above"))
credit$PAY_5 <-factor(credit$PAY_5, level = -1:8, labels = c("pay duly", "payment delay for one month", "payment delay for two months", "payment delay for three months", "payment delay for four months", "payment delay for five months", "payment delay for six months", "payment delay for seven months", "payment delay for eight months", "payment delay for nine months and above"))
credit$PAY_6 <-factor(credit$PAY_6, level = -1:8, labels = c("pay duly", "payment delay for one month", "payment delay for two months", "payment delay for three months", "payment delay for four months", "payment delay for five months", "payment delay for six months", "payment delay for seven months", "payment delay for eight months", "payment delay for nine months and above"))
# check for missing values
summary(as.factor(credit$MARRIAGE))
# treat missing values
newcredit <-na.omit(credit)
# Algorithm
#make sure that the result is reproducible
set.seed(1234)
#split the data into a training and test set
ind <- sample(2, nrow(newcredit), replace = TRUE, prob = c(0.7, 0.3))
train.data <- newcredit [ind == 1, ]
test.data <- newcredit [ind == 2, ]
#build the model and store in a variable model
model<-glm(default.payment.next.month~., family=binomial, data=train.data)
mypredictions <- round(predict (model, type="response"))
#output the coefficient, Residual Deviance, p value, and standard error for each independent variable and intercept
summary(model)
#output the coefficients and an intercept
exp(coef(model))
# Evaluate the model on training data
#confusion matrix for the training set; need to round the estimated values
table(mypredictions, train.data$default.payment.next.month, dnn=c("predicted","actual"))
#classification accuracy for train.data
mean(round(predict(model, train.data, type="response"))==train.data$default.payment.next.month)
#classification error for train.data
mean(round(predict(model,train.data,type="response"))!=train.data$default.payment.next.month)
# Evaluate model on test data
#store the estimated values in a variable mypredictions; need to round the values
mypredictions<-round(predict (model, test.data, type="response"))
#confusion matrix for the test data
table(mypredictions, test.data$default.payment.next.month, dnn=c("predicted","actual"))
#classification accuracy for test.data
mean(round(predict (model,test.data,type="response"))==test.data$default.payment.next.month)
#classification error for test.data
mean(round(predict (model,test.data,type="response"))!=test.data$default.payment.next.month)
# Build the ROC curve
install.packages("ROCR")
library(ROCR)
ROCRpred <- prediction(mypredictions, test.data$default.payment.next.month)
ROCRperF <- performance(ROCRpred, 'tpr','fpr')
plot(ROCRperF, colorize = TRUE, text.adj = c(-0.2,1.2), lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")
#plot the residuals
plot(predict(model),residuals(model), col=c("blue"))
lines(lowess(predict(model),residuals(model)), col=c("black"), lwd=2)
abline(h=0, col="grey")
# Effect displays
install.packages("effects")
library(effects)
plot(allEffects(model))
#minimal adequate model
model2 <- step(model)
summary(model2)
plot(allEffects(model2))
#classification accuracy and confusion matrix for reduced model
mypredictions <- round(predict (model2, test.data, type="response"))
table (mypredictions, test.data$default.payment.next.month, dnn=c("predicted", "actual"))
mean(round(predict (model2, test.data, type="response"))== test.data$default.payment.next.month)
# Naive Bayes method
mamodel<-glm(default.payment.next.month ~ LIMIT_BAL + SEX + EDUCATION + MARRIAGE + AGE + PAY_0 + PAY_2 + PAY_4 + PAY_5 + PAY_6 + BILL_AMT1 + BILL_AMT2 + BILL_AMT3 + PAY_AMT1 + PAY_AMT2 , family=binomial, train.data)
mamodel2 <-na.omit(mamodel)
summary(mamodel2)
mypredictions<-round(predict (mamodel2, test.data, type="response"))
table (mypredictions, test.data$class)
