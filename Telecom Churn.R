library(ggplot2)
library(cowplot)
library(caret)
# Read datasets
train <- read.csv("Telecom_Train.csv")
test <- read.csv("Telecom_Test.csv")

# Removing X column
train_df <- train[,-1]
test_df <- test[,-1]

head(train_df)
dim(train_df)
str(train_df)
summary(train_df)
# No missing values in train
train_df[!complete.cases(train_df),]
sapply(train_df,function(x) sum(is.na(x)))
# No missing values in test
test_df[!complete.cases(test_df),]
sapply(test_df,function(x) sum(is.na(x)))
#........................................Univariate Analysis.............................................#

# Numerical variables

# account_length
p2 = ggplot(train_df,aes(x = 1, y = account_length))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3) # few outliers

# number_vmail_messages
p6 = ggplot(train_df,aes(x = 1, y = number_vmail_messages))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3) # many 0's & 1 outlier = 51

# total_day_minutes
p7 = ggplot(train_df,aes(x = 1, y = total_day_minutes))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3) # normally distributed

# total_day_calls
p8 = ggplot(train_df,aes(x = 1, y = total_day_calls))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_day_charge
p9 = ggplot(train_df,aes(x = 1, y = total_day_charge))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_eve_minutes
p10 = ggplot(train_df,aes(x = 1, y = total_eve_minutes))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_eve_calls
p11 = ggplot(train_df,aes(x = 1, y = total_eve_calls))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_eve_charge
p12 = ggplot(train_df,aes(x = 1, y = total_eve_charge))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_night_minutes
p13 = ggplot(train_df,aes(x = 1, y = total_night_minutes))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_night_calls
p14 = ggplot(train_df,aes(x = 1, y = total_night_calls))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_night_charge
p15 = ggplot(train_df,aes(x = 1, y = total_night_charge))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)

# total_intl_minutes
p16 = ggplot(train_df,aes(x = 1, y = total_intl_minutes))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)
ggplot(train_df,aes(x = total_intl_minutes))+geom_histogram(binwidth = 1,color = "black",fill = "orange")

# total_intl_calls
p17 = ggplot(train_df,aes(x = 1, y = total_intl_calls))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3) # positively skewed
ggplot(train_df,aes(x = total_intl_calls))+geom_histogram(binwidth = 1,color = "black",fill = "orange")

# total_intl_charge
p18 = ggplot(train_df,aes(x = 1, y = total_intl_charge))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3)
ggplot(train_df,aes(x = total_intl_charge))+geom_histogram(binwidth = 0.1,color = "black",fill = "orange")

# number_customer_service_calls
p19 = ggplot(train_df,aes(x = 1, y = number_customer_service_calls))+geom_boxplot(color = "blue") + geom_jitter(alpha = 0.1)+
  stat_summary(fun.y = mean,color = "darkred",geom = "point",shape = 10,size = 3) # 6 outliers


p <- plot_grid(p2,p6,p19)
p_day <- plot_grid(p7,p8,p9)
p_eve <- plot_grid(p10,p11,p12)
p_night <- plot_grid(p13,p14,p15)
p_intl <- plot_grid(p16,p17,p18) 

# Categorical variables

#Churn
p20 = ggplot(train_df,aes(x = churn))+geom_bar(color = "black",fill = "orange") # highly imbalanced data; 85% no & 15% yes

# State
p21 = ggplot(train_df,aes(x = state))+geom_bar(color = "black",fill = "orange")

# area_code
p22 = ggplot(train_df,aes(x = area_code))+geom_bar(color = "black",fill = "orange")# max 415

# international_plan
p23 = ggplot(train_df,aes(x = international_plan))+geom_bar(color = "black",fill = "orange")

# voice_mail_plan
p24 = ggplot(train_df,aes(x = voice_mail_plan))+geom_bar(color = "black",fill = "orange")

plot_grid(p20,p21,p22,p23,p24)

#.............................................Imputing Outliers...................................................#
boxplot(train_df$account_length)$out
quantile(train_df$account_length,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$account_length <- ifelse(train_df$account_length > quantile(train_df$account_length,0.99),
                                  quantile(train_df$account_length,0.99),train_df$account_length)



boxplot(train_df$number_vmail_messages)$out
quantile(train_df$number_vmail_messages,c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$number_vmail_messages <- ifelse(train_df$number_vmail_messages > quantile(train_df$number_vmail_messages,0.99),
                                  quantile(train_df$number_vmail_messages,0.99),train_df$number_vmail_messages)


sort(boxplot(train_df$total_day_minutes)$out)
quantile(train_df$total_day_minutes,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_day_minutes <- ifelse(train_df$total_day_minutes > quantile(train_df$total_day_minutes,0.99),
                                  quantile(train_df$total_day_minutes,0.99),
                                  ifelse(train_df$total_day_minutes < quantile(train_df$total_day_minutes,0.01),
                                  quantile(train_df$total_day_minutes,0.01),train_df$total_day_minutes))



sort(boxplot(train_df$total_day_calls)$out)
quantile(train_df$total_day_calls,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_day_calls <- ifelse(train_df$total_day_calls > quantile(train_df$total_day_calls,0.99),
                                  quantile(train_df$total_day_calls,0.99),
                                  ifelse(train_df$total_day_calls < quantile(train_df$total_day_calls,0.01),
                                  quantile(train_df$total_day_calls,0.01),train_df$total_day_calls))


sort(boxplot(train_df$total_day_charge)$out)
quantile(train_df$total_day_charge,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_day_charge <- ifelse(train_df$total_day_charge > quantile(train_df$total_day_charge,0.99),
                                  quantile(train_df$total_day_charge,0.99),
                                  ifelse(train_df$total_day_charge < quantile(train_df$total_day_charge,0.01),
                                  quantile(train_df$total_day_charge,0.01),train_df$total_day_charge))



sort(boxplot(train_df$total_eve_minutes)$out)
quantile(train_df$total_eve_minutes,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_eve_minutes <- ifelse(train_df$total_eve_minutes > quantile(train_df$total_eve_minutes,0.99),
                                  quantile(train_df$total_eve_minutes,0.99),
                                  ifelse(train_df$total_eve_minutes < quantile(train_df$total_eve_minutes,0.01),
                                  quantile(train_df$total_eve_minutes,0.01),train_df$total_eve_minutes))


sort(boxplot(train_df$total_eve_calls)$out)
quantile(train_df$total_eve_calls,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_eve_calls <- ifelse(train_df$total_eve_calls > quantile(train_df$total_eve_calls,0.99),
                                  quantile(train_df$total_eve_calls,0.99),
                                  ifelse(train_df$total_eve_calls < quantile(train_df$total_eve_calls,0.01),
                                  quantile(train_df$total_eve_calls,0.01),train_df$total_eve_calls))



sort(boxplot(train_df$total_eve_charge)$out)
quantile(train_df$total_eve_charge,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_eve_charge <- ifelse(train_df$total_eve_charge > quantile(train_df$total_eve_charge,0.99),
                                  quantile(train_df$total_eve_charge,0.99),
                                  ifelse(train_df$total_eve_charge < quantile(train_df$total_eve_charge,0.01),
                                  quantile(train_df$total_eve_charge,0.01),train_df$total_eve_charge))


sort(boxplot(train_df$total_night_minutes)$out)
quantile(train_df$total_night_minutes,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_night_minutes <- ifelse(train_df$total_night_minutes > quantile(train_df$total_night_minutes,0.99),
                                  quantile(train_df$total_night_minutes,0.99),
                                  ifelse(train_df$total_night_minutes < quantile(train_df$total_night_minutes,0.01),
                                  quantile(train_df$total_night_minutes,0.01),train_df$total_night_minutes))





sort(boxplot(train_df$total_night_calls)$out)
quantile(train_df$total_night_calls,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_night_calls <- ifelse(train_df$total_night_calls > quantile(train_df$total_night_calls,0.99),
                                  quantile(train_df$total_night_calls,0.99),
                                  ifelse(train_df$total_night_calls < quantile(train_df$total_night_calls,0.01),
                                  quantile(train_df$total_night_calls,0.01),train_df$total_night_calls))


sort(boxplot(train_df$total_night_charge)$out)
quantile(train_df$total_night_charge,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_night_charge <- ifelse(train_df$total_night_charge > quantile(train_df$total_night_charge,0.99),
                                  quantile(train_df$total_night_charge,0.99),
                                  ifelse(train_df$total_night_charge < quantile(train_df$total_night_charge,0.01),
                                  quantile(train_df$total_night_charge,0.01),train_df$total_night_charge))


sort(boxplot(train_df$total_intl_minutes)$out)
quantile(train_df$total_intl_minutes,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.99))
train_df$total_intl_minutes <- ifelse(train_df$total_intl_minutes > quantile(train_df$total_intl_minutes,0.99),
                                  quantile(train_df$total_intl_minutes,0.99),
                                  ifelse(train_df$total_intl_minutes < quantile(train_df$total_intl_minutes,0.01),
                                  quantile(train_df$total_intl_minutes,0.01),train_df$total_intl_minutes))




sort(boxplot(train_df$total_intl_calls)$out)
quantile(train_df$total_intl_calls,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.97,0.99))
train_df$total_intl_calls <- ifelse(train_df$total_intl_calls > quantile(train_df$total_intl_calls,0.97),
                                  quantile(train_df$total_intl_calls,0.97),
                                  ifelse(train_df$total_intl_calls < quantile(train_df$total_intl_calls,0.01),
                                  quantile(train_df$total_intl_calls,0.01),train_df$total_intl_calls))

sort(boxplot(train_df$total_intl_charge)$out)
quantile(train_df$total_intl_charge,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.97,0.99))
train_df$total_intl_charge <- ifelse(train_df$total_intl_charge > quantile(train_df$total_intl_charge,0.97),
                                  quantile(train_df$total_intl_charge,0.97),
                                  ifelse(train_df$total_intl_charge < quantile(train_df$total_intl_charge,0.01),
                                  quantile(train_df$total_intl_charge,0.01),train_df$total_intl_charge))

sort(boxplot(train_df$number_customer_service_calls)$out)
quantile(train_df$number_customer_service_calls,c(0.01,0.05,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95,0.96,0.99))
train_df$number_customer_service_calls <- ifelse(train_df$number_customer_service_calls > quantile(train_df$number_customer_service_calls,0.97),
                                  quantile(train_df$number_customer_service_calls,0.97),
                                  ifelse(train_df$number_customer_service_calls < quantile(train_df$number_customer_service_calls,0.01),
                                  quantile(train_df$number_customer_service_calls,0.01),train_df$number_customer_service_calls))

#.................................................................................................................#

# Dummify categorical variables (State and area code, rest factor variables are already in binary form)

dmy <- dummyVars("~state + area_code",data = train_df,fullRank = TRUE)
trsf <- data.frame(predict(dmy,newdata = train_df))
final_train <- data.frame(trsf,train_df[,c(-1,-3)])


dmy_test <- dummyVars("~state + area_code",data = test_df,fullRank = TRUE)
trsf_test <- data.frame(predict(dmy_test,newdata = test_df))
final_test <- data.frame(trsf_test,test_df[,c(-1,-3)])



# Basic Logistic regression model

basic_log <- glm(churn~.,family = binomial,data = final_train)
summary(basic_log)
basic_pred <- ifelse(predict(basic_log,final_train,type = 'response')>0.5,'yes','no')
confusionMatrix(final_train$churn,factor(basic_pred),positive = 'no')
prop.table(table(final_train$churn))
prop.table(table(basic_pred))


basic_pred_test <- ifelse(predict(basic_log,final_test,type = 'response')>0.5,'yes','no')
confusionMatrix(final_test$churn,factor(basic_pred_test))
prop.table(table(final_test$churn))
prop.table(table(basic_pred_test))

#............................................................................................
# Reducing threshold to get get the prediction in conformity with actual data

basic_pred <- ifelse(predict(basic_log,final_train,type = 'response')>0.3,'yes','no')
confusionMatrix(final_train$churn,factor(basic_pred),positive = 'no')
prop.table(table(final_train$churn))
prop.table(table(basic_pred))


basic_pred_test <- ifelse(predict(basic_log,final_test,type = 'response')>0.3,'yes','no')
confusionMatrix(final_test$churn,factor(basic_pred_test))
prop.table(table(final_test$churn))
prop.table(table(basic_pred_test))

# Accuracy : 0.8536 
# Kappa : 0.3915

# Cross Validation
my_control <- trainControl(method = 'cv', number = 3,verboseIter = TRUE)
log_model_cv <- train(churn~.,data = final_train,trControl = my_control,method = 'glm')
summary(log_model_cv)
log_cv_pred <- predict(log_model_cv,final_test)
table(final_test$churn,log_cv_pred)
confusionMatrix(final_test$churn,log_cv_pred)
varImp(log_model_cv)
# Accuracy : 0.8692          
# Kappa : 0.2699          
# Sensitivity : 0.8914          
# Specificity : 0.5294  

dt_model_cv<- train(churn~.,data = final_train,trControl = my_control,method = 'rpart')
summary(dt_model_cv)
dt_pred <- predict(dt_model_cv,newdata = final_test,type = 'prob')
confusionMatrix(factor(ifelse(dt_pred[,2]>0.5,'yes','no')),final_test$churn)
varImp(dt_model_cv)
# Accuracy : 0.8872         
# Kappa : 0.3413      
# Sensitivity : 0.9834         
# Specificity : 0.2679    



boost_tree <- train(churn~.,data = final_train,trControl = my_control,method = 'bstTree')
boost_pred <- predict(boost_tree,newdata = final_test)
boost_tree
confusionMatrix(boost_pred,final_test$churn)
# Accuracy : 0.9442         
# Kappa : 0.7182  
varImp(boost_tree)


# Random Forest

rf_model <- train(churn~.,data = final_train,
                  method = "ranger",
                  trControl = trainControl(method = 'cv',number = 5,classProbs = TRUE,verboseIter = TRUE))
plot(rf_model)
rf_pred <- predict(rf_model,newdata = final_test[,-70],'prob')
caTools::colAUC(rf_pred[,1],final_test$churn,plotROC = TRUE)#AUC 0.9234513

