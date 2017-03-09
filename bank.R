# Reading Data and take a look
bank <- read.csv("bank-additional-full.csv",stringsAsFactors=TRUE,header=TRUE,sep=";")
str(bank)
bank[0:15,]


# Check missing values
sapply(bank, function(x) sum(is.na(x))) # zero missing value


################ Exploratory data analysis ################

### Check categorical variable: job

# Returns the summary 
summary(bank$job) # 12 levels, 330 of unknown

# Plot number of success and failure by job 
barplot(table(bank$y,bank$job), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y))) 

# Create table of success and failure by job 
aggregate(y ~ job, summary, data=bank) # table with values
source("http://pcwww.liv.ac.uk/~william/R/crosstab.r") # Load function for crosstab
crosstab(bank, row.vars = "job", col.vars = "y", type = "r") # table with percentage


### Check categorical variable: marital

# Returns the summary 
summary(bank$marital)  # 4 levels, 80 of unknown

# Plot number of success and failure by marital status
barplot(table(bank$y,bank$marital), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y))) 

# Create table of success and failure by marital status 
aggregate(y ~ marital, summary, data=bank) # table with values
crosstab(bank, row.vars = "marital", col.vars = "y", type = "r") 
# Conclusion: Not much difference among classes


### Check categorical variable: education

# Returns the summary 
summary(bank$education)  # 8 levels, 1731 of unknown

# Plot number of success and failure by marital status
barplot(table(bank$y,bank$education), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by education 
aggregate(y ~ education, summary, data=bank) 
crosstab(bank, row.vars = "education", col.vars = "y", type = "r")
# Conclusion: success rate is higher for illiterate and unknown group 
# but it may be caused due to relatively smaller number of value


### Check categorical variable: default (has credit in default?)

# Returns the summary 
summary(bank$default)  # 3 levles, # 8597 of unknown

# Plot number of success and failure by default
barplot(table(bank$y,bank$default), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by default
aggregate(y ~ default, summary, data=bank) 
crosstab(bank, row.vars = "default", col.vars = "y", type = "r")

# Conclusion: Not valid to be used due to imbalanced class. majority is "no"



### Check categorical variable: housing (has housing loan?)

# Returns the summary 
summary(bank$housing)  # 3 levles, 990 of unknown

# Plot number of success and failure by housing
barplot(table(bank$y,bank$housing), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by housing
aggregate(y ~ housing, summary, data=bank) 
crosstab(bank, row.vars = "housing", col.vars = "y", type = "r")

# Conclusion: Not much difference among classes



### Check categorical variable: loan (has personal loan?)

# Returns the summary 
summary(bank$loan)  # 3 levles, 990 of unknown

# Plot number of success and failure by loan
barplot(table(bank$y,bank$loan), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by loan
aggregate(y ~ loan, summary, data=bank) 
crosstab(bank, row.vars = "loan", col.vars = "y", type = "r")

# Conclusion: Not much difference among classes



### Check categorical variable: contact (contact communication type)

# Returns the summary 
summary(bank$contact)  # 2 levels (celluar, telephone)

# Plot number of success and failure by contact 
barplot(table(bank$y,bank$contact), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by contact
aggregate(y ~ contact, summary, data=bank) 
crosstab(bank, row.vars = "contact", col.vars = "y", type = "r")

# Conclusion: Cellular brings 3 times more subscribers. 



### Check categorical variable: month (last contact month of year)

# Returns the summary 
summary(bank$month) # 12 levels

# Plot number of success and failure by month
barplot(table(bank$y,bank$month), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by month
aggregate(y ~ month, summary, data=bank) 
crosstab(bank, row.vars = "month", col.vars = "y", type = "r")

# months with less obervations (dec, mar, oct, sep) tend to have higher (around 50%) success rate



### Check categorical variable: day_of_week (last contact day of the week)

# Returns the summary 
summary(bank$day_of_week)  # 5 levles (fri, mon, thu, tue, wed)

# Plot number of success and failure by day_of_week
barplot(table(bank$y,bank$day_of_week), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by day_of week
aggregate(y ~ day_of_week, summary, data=bank) 
crosstab(bank, row.vars = "day_of_week", col.vars = "y", type = "r")

# Conclusion: Monday shows the lowest success rate (9.95%) whereas Thursday shows the highest. Needs to be validated. 



### Check categorical variable: poutcome (outcome of the previous marketing campaign)

# Returns the summary 
summary(bank$poutcome) # 3 levels

# Plot number of success and failure by poutcome
barplot(table(bank$y,bank$poutcome), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))

# Create table of success and failure by poutcome
aggregate(y ~ poutcome, summary, data=bank) 
crosstab(bank, row.vars = "poutcome", col.vars = "y", type = "r")

# Conclusion: Success of previous campaign results in success rate of 65.11%  



### Check numerical variables: age 

# Returns the summary 
summary(bank$age)  # from 17 to 98 with the mean of 40

# Returns the histogram with frequency
hist(bank$age)  

# Barplot number of success and failure by each age
barplot(table(bank$y,bank$age), col=c("darkblue","red"), legend = colnames(table(bank$age,bank$y)))  

# Boxplot age distribution for success and failure 
boxplot(bank$age~bank$y)

# Perform t-test to see whether success and failure differ significantly by age
t.test(bank$age~bank$y) # p-value = 1.805e-06

# Conclusion: age has an influence on success 



### Combine age and job
install.package("ggplot2")
library("ggplot2") 
gg <- ggplot(bank, aes(job, age))
gg + geom_point(aes(colour=y)) 

# Conclusion: Retired, housemaid, management are older than other job groups and more likely to subscribe


### Check numerical variables: duration (last contact duration, in seconds)

# Returns the summary 
summary(bank$duration) # from 0 to 4918 seconds (82 min), with the mean of 258 seconds (4 min)

# Returns the histogram with frequency
hist(bank$duration)  

# Boxplot duration distribution for success and failure 
boxplot(bank$duration~bank$y)

# Perform t-test to see whether success and failure differ significantly by duration
t.test(bank$duration~bank$y) # p-value < 2.2e-16

# Conclusion: shorter call is more likely to succeed 



### Check numerical variables: campaign (number of contacts performed during this campaign and for this client) 

# Returns the summary 
summary(bank$campaign) 

# Returns the histogram with frequency
hist(bank$campaign)  

# Boxplot campagin distribution for success and failure 
boxplot(bank$campaign~bank$y)

# Perform t-test to see whether success and failure differ significantly by campaign
t.test(bank$campaign~bank$y) # p-value < 2.2e-16

# Conclusion: The less the previous call during this campaign exists, the more likely to success this time it is



### Check numerical variables: pday 
# (number of days that passed by after the client was last contacted from a previous campaign)

# Returns the summary 
summary(bank$pdays) # from 0 to 999 days

# Returns the histogram with frequency
hist(bank$pdays) # Majority resides on 999 days meaning contact for previous contact doesn't exist or passed long

# Boxplot pday distribution for success and failure 
boxplot(bank$pdays~bank$y)

# Perform t-test to see whether success and failure differ significantly by pday
t.test(bank$pdays~bank$y) # p-value < 2.2e-16

# Perform chi-square test between pdays (999, the rest) and whether to succeed
library(car)
table_pdays <- ftable(xtabs(~recode(bank$pdays, "c('999')='999 days'; else='rest'")+y, data=bank))
table_pdays
chisq.test(table_pdays) # p-value < 2.2e-16. Having the pday of 999 and the rest are independent. 
# Therefore, we should exclude obervations with pday of 999 days for better accuracy. 

# Perform t-test again without obervations with pday of 999 days
subset_data <- bank[which(bank$pday<999),] 
t.test(subset_data$pdays~subset_data$y) # p-value = 0.1787

# Conclusion: previous contact leads to success 
# but number of days passed by after the last call for previous campaign are not correlated to success.



### Check numerical variables: previous (number of contacts performed before this campaign and for this client)

# Returns the summary 
summary(bank$previous) # from 0 to 7 with the mean of 0.173

# Returns the histogram with frequency
hist(bank$previous) # Majority of values resied on 0 meaning that there was no contact before this campaign for this client

# Boxplot previous distribution for success and failure 
boxplot(bank$previous~bank$y)

# Perform t-test to see whether success and failure differ significantly by previous
t.test(bank$previous~bank$y) # p-value < 2.2e-16

# Perform chi-square test between pdays (0, the rest) and whether to succeed
table_pre <- ftable(xtabs(~recode(bank$previous, "c('0')='no contact'; else='rest'")+y, data=bank))
table_pre
chisq.test(table_pre) # p-value < 2.2e-16. Having no previous contact and the rest are independent. 
# Therefore, we should exclude obervations with previous of 0 for better accuracy. 

# Perform t-test again without obervations with previous of 0
subset_data <- bank[which(bank$previous>0),] 
t.test(subset_data$previous~subset_data$y) # p-value < 2.2e-16

# Conclusion: previous contact leads to success and 
# more previous contacts before this campaign result in success.



### Check numerical variables: emp.var.rate (employment variation rate - quarterly indicator)

# Returns the summary 
summary(bank$emp.var.rate) # from -3.4 to 1.4 with mean of 0.08189 

# Returns the histogram with frequency
hist(bank$emp.var.rate)

# Boxplot emp.var.rate distribution for success and failure
boxplot(bank$emp.var.rate~bank$y) # most of observations reside in 1.1 and 1.4

# Perform t-test to see whether success and failure differ significantly by emp.var.rate
t.test(bank$emp.var.rate~bank$y) # p-value < 2.2e-16

# Conclusion: Low employment variation rate brings more subscribers



### Check numerical variables: cons.price.idx (consumer price index - monthly indicator)

# Returns the summary 
summary(bank$cons.price.idx) # from 92.2 to 94.77 with the mean of 93.58

# Returns the histogram with frequency
hist(bank$cons.price.idx)

# Boxplot cons.price.idx distribution for success and failure
boxplot(bank$cons.price.idx~bank$y)

# Perform t-test to see whether success and failure differ significantly by cons.price.idx
t.test(bank$cons.price.idx~bank$y) # p-value < 2.2e-16

# Conclusion: Low consumer price index brings more subscribers.



### Check numerical variables: cons.conf.idx (consumer confidence index - monthly indicator)

# Returns the summary 
summary(bank$cons.conf.idx) # from -50.8 to -26.9 with the mean of -40.5

# Returns the histogram with frequency
hist(bank$cons.conf.idx)

# Boxplot cons.price.idx distribution for success and failure
boxplot(bank$cons.conf.idx~bank$y)

# Perform t-test to see whether success and failure differ significantly by cons.conf.idx
t.test(bank$cons.conf.idx~bank$y) # p-value < 2.2e-16

# Conclusion: Higher consumer price index brings more subscribers.



### Check numerical variables: euribor3m (euribor 3 month rate)

# Returns the summary 
summary(bank$euribor3m) # from 0.634 to 5.045 with the mean of 3.621

# Returns the histogram with frequency
hist(bank$euribor3m)

# Boxplot euribor3m distribution for success and failure
boxplot(bank$euribor3m~bank$y)

# Perform t-test to see whether success and failure differ significantly by euribor3m
t.test(bank$euribor3m~bank$y) # p-value < 2.2e-16

# Conclusion: Low euribor 3 month rate brings more subscribers. 
#This is contradictory to the common sense.




### Check numerical variables: nr.employed (number of employees - quarterly indicator)

# Returns the summary 
summary(bank$nr.employed) # from 4964 to 5228 with the mean of 5167

# Returns the histogram with frequency
hist(bank$nr.employed)

# Boxplot nr.employed distribution for success and failure
boxplot(bank$nr.employed~bank$y)

# Perform t-test to see whether success and failure differ significantly by cons.conf.idx
t.test(bank$nr.employed~bank$y) # p-value < 2.2e-16

# Conclusion: Low number of employees brings more subscribers



### Chi-square test: jobs (unknown, the rest) + y (yes, no)
table_jobs <- ftable(xtabs(~recode(bank$job, "c('unknown')='unknown'; else='known'")+y, data=bank))
table_jobs
chisq.test(table_jobs) # p-value = 1


### Chi-square test: marital (unknown, the rest) + y (yes, no)
table_marital <- ftable(xtabs(~recode(bank$marital, "c('unknown')='unknown'; else='known'")+y, data=bank))
table_marital
chisq.test(table_marital) # p-value = 0.3786


### Chi-square test: education (unknown, the rest) + y (yes, no)
table_education <- ftable(xtabs(~recode(bank$education, "c('unknown')='unknown'; else='known'")+y, data=bank))
table_education
chisq.test(table_education) # p-value = 1.63e-05


### Chi-square test: default (unknown, the rest) + y (yes, no)
table_default <- ftable(xtabs(~recode(bank$default, "c('unknown')='unknown'; else='known'")+y, data=bank))
table_default
chisq.test(table_default) # p-value = < 2.2e-16


### Chi-square test: housing (unknown, the rest) + y (yes, no)
table_housing <- ftable(xtabs(~recode(bank$housing, "c('unknown')='unknown'; else='known'")+y, data=bank))
table_housing
chisq.test(table_housing) # p-value = 0.6819


### Chi-square test: loan (unknown, the rest) + y (yes, no)
table_loan <- ftable(xtabs(~recode(bank$loan, "c('unknown')='unknown'; else='known'")+y, data=bank))
table_loan
chisq.test(table_loan) # p-value = 0.6819 




# Reading Data and take a look
bank <- read.csv("bank-additional-full.csv",stringsAsFactors=TRUE,header=TRUE,sep=";")
str(bank)
bank[0:5,]


# Check missing values
sapply(bank, function(x) sum(is.na(x))) # zero missing value




################ Exploratory data analysis ################
# Please refer to seperate R file (bank_EDA.R)




################ Modeling ################

# Delete the observations with “unknown” in the variables including jobs, marital, housing, and loan and pday of 999
bank <- bank[!(bank$job=="unknown" | bank$marital=="unknown" | bank$housing=="unknown" | bank$loan=="unknown"| bank$pdays=="999"),]


# Randomly pick 30% based on the row number and split the data into training set and test set.
testrows <- sample(1:nrow(bank),size=0.3*nrow(bank))
bank_test <- bank[testrows,]
bank_train <- bank[-testrows,]


################ Random Forest w/ caret

library(randomForest) 
set.seed(100) 


##### Model training and tuning

customRF <- list(type = "Classification", library = "randomForest", loop = NULL)
customRF$parameters <- data.frame(parameter = c("mtry", "ntree"), class = rep("numeric", 2), label = c("mtry", "ntree"))
customRF$grid <- function(x, y, len = NULL, search = "grid") {}
customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...) {
  randomForest(x, y, mtry = param$mtry, ntree=param$ntree, ...)
}
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata)
customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL)
  predict(modelFit, newdata, type = "prob")
customRF$sort <- function(x) x[order(x[,1]),]
customRF$levels <- function(x) x$classes


# train model
library(caret)
control <- trainControl(method="repeatedcv", number=10, repeats=3, summaryFunction = twoClassSummary,classProbs = TRUE)
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(10, 20, 30, 40))
set.seed(100)
custom <- train(y~., data=bank_train, method=customRF, metric="ROC", tuneGrid=tunegrid, trControl=control)
summary(custom)
plot(custom)
print(custom)

# train model-Resample
control <- trainControl(method="repeatedcv", number=10, repeats=3, summaryFunction = twoClassSummary,classProbs = TRUE)
nmin <- sum(bank_train$y == "yes")
tunegrid <- expand.grid(.mtry=c(1:15), .ntree=c(10, 20, 30, 40))
set.seed(100)
custom_down <- train(y~., data=bank_train, method=customRF, metric="ROC", tuneGrid=tunegrid, trControl=control,
                     # Tell randomForest to sample by strata. Here, that means within each class
                     strata = bank_train$y,
                     # Now specify that the number of samples selected within each class should be the same
                     sampsize = rep(nmin, 2))
summary(custom_down)
plot(custom_down)
print(custom_down)

# Compare the model with unbalanced data and under-sampled data
install.packages("pROC")
library(pROC)
downProbs <- predict(custom_down, bank_test, type = "prob")[,1]
downsampledROC <- roc(response = bank_test$y, predictor = downProbs,levels = rev(levels(bank_test$y)))
unbalProbs <- predict(custom, bank_test, type = "prob")[,1]
unbalROC <- roc(response = bank_test$y, predictor = unbalProbs, levels = rev(levels(bank_test$y)))

plot(downsampledROC, col = rgb(1, 0, 0, .5), lwd = 2)
plot(unbalROC, col = rgb(0, 0, 1, .5), lwd = 2, add = TRUE)
legend(.4, .4,c("Down-Sampled", "Normal"),lwd = rep(2, 1),col = c(rgb(1, 0, 0, .5), rgb(0, 0, 1, .5)))

auc(downsampledROC) # AUC: 0.9381
auc(unbalROC) # AUC: 0.8068

# create the confusion matrix for test_data
predicted_values <- predict(custom_down, type="prob", bank_test)
threshold <- 0.5
pred <- factor(ifelse(predicted_values[,2]>threshold, "yes", "no"))
confusionMatrix(pred, bank_test$y, positive="yes") #Sensitivity : 0.9963 

# calculate AUC and create the ROC curve
install.packages("ROCR")
install.packages("ggplot2")
library(ROCR)
library(ggplot2)
predicted_values <- predict(custom_down, bank_test,type= "prob")[,2] 
pred <- prediction(predicted_values, bank_test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="RF")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc)) # AUC = 0.9332


# create the plot for variable importance (MeanDecreaseGini)
varImp(custom_down$finalModel)
varImpPlot(custom_down$finalModel)



################ Neural Network 
install.packages("nnet")
library(nnet)
library(doParallel)

# Conduct 5 cross-fold validation 
fitControl <- trainControl(method = "cv",number = 5, repeats = 5, 
                           allowParallel = TRUE,classProbs = TRUE,summaryFunction = twoClassSummary)

# Build a ANN classifier with parameter tuning 
set.seed(825)
nnetfit <- train(y ~ ., data = bank_train, 
                 method = "nnet", 
                 trControl = fitControl,metric = "auc",
                 verbose = FALSE, maxit=1000)

plot(nnetfit)
print(nnetfit)
summary(nnetfit) # a 53-3-1 network with 166 weights. options were - entropy fitting  decay=0.1

# create the confusion matrix for test_data
predicted_values <- predict(nnetfit,bank_test,type= "prob")
head(predicted_values)
threshold <- 0.5
pred <- factor(ifelse(predicted_values[,2]>threshold, "yes", "no"))
confusionMatrix(pred, bank_test$y, positive="yes") # Sensitivity : 0.8619

# calculate AUC and create the ROC curve
predicted_values <- predict(nnetfit,bank_test,type= "prob")[,2]
pred <- prediction(predicted_values, bank_test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="ANN")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc)) #AUC = 0.986783






################ Gradient Boosting Machines

# Conduct 10 cross-fold validation 
fitControl <- trainControl(method = "repeatedcv",number = 10,repeats = 10,
                           allowParallel = TRUE,classProbs = TRUE,summaryFunction = twoClassSummary)

# Build a GBM classifier with parameter tuning 
set.seed(825)
gbm_caret <- train(as.factor(y) ~ ., data = bank_train, method = "gbm", 
                   trControl = fitControl, metric = "auc", verbose = FALSE)

plot(gbm_caret)
print(gbm_caret)
summary(gbm_caret, 5)
varImp(gbm_caret)


# create the confusion matrix
predicted_values <- predict(gbm_caret, bank_test,type= "prob")
head(predicted_values) 
threshold <- 0.5 
pred <- factor( ifelse(predicted_values[,2] > threshold, "yes", "no") ) 
head(pred)
confusionMatrix(pred, bank_test$y, positive = "yes") #Sensitivity : 0.8881  

# calculate AUC and create the ROC curve
predicted_values <- predict(gbm_caret, bank_test,type= "prob")[,2]
pred <- prediction(predicted_values, bank_test$y)
perf <- performance(pred, measure = "tpr", x.measure = "fpr")
auc <- performance(pred, measure = "auc")
auc <- auc@y.values[[1]]
roc.data <- data.frame(fpr=unlist(perf@x.values),
                       tpr=unlist(perf@y.values),
                       model="GBM")
ggplot(roc.data, aes(x=fpr, ymin=0, ymax=tpr)) +
  geom_ribbon(alpha=0.2) +
  geom_line(aes(y=tpr)) +
  ggtitle(paste0("ROC Curve w/ AUC=", auc)) # AUC = 0.9432



################ Wrap-up

# Get the ROC of each model

downProbs <- predict(custom_down, bank_test, type = "prob")[,2]
roc_rf <- roc(response = bank_test$y, predictor = downProbs,levels = rev(levels(bank_test$y)))

predicted_values_gbm <- predict(gbm_caret, bank_test,type= "prob")[,2]
roc_gbm <- roc(response = bank_test$y, predictor = predicted_values_gbm,levels = rev(levels(bank_test$y)))

predicted_values_nn <- predict(nnetfit,bank_test,type= "prob")[,2]
roc_nn <- roc(response = bank_test$y, predictor = predicted_values_nn,levels = rev(levels(bank_test$y)))

#Plot up the models
plot(roc_rf, col='red')
plot(roc_gbm, add=TRUE, col='yellow')
plot(roc_nn, add=TRUE, col='blue')
legend(.4, .4,c("Random Forest", "GBM","Neural Network"),lwd = rep(2, 1),col = c('red','yellow','blue'))
