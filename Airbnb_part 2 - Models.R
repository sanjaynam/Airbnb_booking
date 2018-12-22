install.packages("InformationValue")
install.packages("ROCR")
install.packages("plyr")
install.packages("rpart.plot")
install.packages("corrplot")

## Invoke required libraries
library(readr)
library(dplyr)
library(tidyverse)
library(descr)
library(data.table)
library(ROCR)
library(InformationValue)
library(caTools)
library(rpart)
library(rpart.plot)
library(corrplot)
library(fastcluster)
library(caret)
library(gam)
require(xgboost)

###############################################################################################
#
#                   Part Deliverable 2 - Analyze the Airbnb File 
#
###############################################################################################
#Import Session table
sessions <- read_csv("sessions.csv")

#Index data to provide indentification per record.
sessions$index<- rownames(sessions)

###Examine the first few rows of the dataset 
str(sessions)
names(sessions)
dim(sessions)
nrow(sessions)
ncol(sessions)

###Group that book
booking_session <- filter(sessions, sessions$action_detail == "booking")
plot(table(booking_session$device_type,booking_session$action_detail))
glimpse(booking_session)
summary(booking_session)
table(booking_session$action_detail,booking_session$device_type)

###Use of mobile device is higher with booking users than with all
barplot(table(booking_session$action_detail,booking_session$device_type),xlab = "Device Type",main = "Booking", col = "lightgreen")
barplot(table(sessions$action_detail,sessions$device_type),xlab = "Device Type", main = "All_Users",col = "lightblue" )

### group booking_session by user_id and return avg time elapsed
booking_id <- booking_session %>%
  select(user_id, action_detail, device_type, secs_elapsed) %>%
  group_by(user_id) %>%
  summarise(secs_elapsed = mean(na.omit(secs_elapsed)))
View(booking_id)

### group booking_session by device type and return avg time elapsed
booking_de <- booking_session %>%
  select(user_id, action_detail, device_type, secs_elapsed) %>%
  group_by(device_type) %>%
  summarise(secs_elapsed = mean(na.omit(secs_elapsed)))
View(booking_de)

plot(booking_de)
str(booking_de)
head(booking_de)

tableuniqueidall<- unique(sessions$user_id)
ggplot2::quickplot(booking_session$device_type,booking_session$action_detail)
ggplot2::quickplot(sessions$device_type,sessions$action_detail)

str(sessions)
summary(sessions)

###Transform all that do not state booking
table(sessions$action_detail[sessions$action_detail != "booking"])
sessions$action_detail[sessions$action_detail != "booking"] <- "non_booking"
View(sessions)

###Final table could be used for analysis
sessionsuser_id <- sessions %>%
  select(user_id, action_detail, device_type, secs_elapsed) %>%
  group_by(user_id,action_detail,device_type) %>%
  summarise(secs_elapsed = mean(na.omit(secs_elapsed)))
summary(sessionsuser_id)
View(sessionsuser_id)

###Save Table
write.csv(sessionsuser_id, file ="~/Documents/APANProject/sessionsuser_id.csv")

#Transform variables to create models
sessionsuser_id$user_id <- as.factor(sessionsuser_id$user_id)
sessionsuser_id$user_id <- as.numeric(sessionsuser_id$user_id)
sessionsuser_id$action_detail[sessionsuser_id$action_detail == "non_booking"] <- 0
sessionsuser_id$action_detail[sessionsuser_id$action_detail == "booking"] <- 1
sessionsuser_id$secs_elapsed[sessionsuser_id$secs_elapsed == "NaN"] <- 0

sessionsuser_id$index<- rownames(sessionsuser_id)
sessionsuser_id$index <- as.numeric(sessionsuser_id$index)
sessionsuser_id$action_detail <- as.numeric(sessionsuser_id$action_detail)
sessionsuser_id$device_type <- as.numeric(sessionsuser_id$device_type)
sessionsuser_id$secs_elapsed <- as.numeric(sessionsuser_id$secs_elapsed)
sessionsuser_id$user_id[sessionsuser_id$user_id == "NaN"] <- 0
sessionsuser_id$user_id[is.na(sessionsuser_id$user_id)] <- 0

write.csv(sessionsuser_id, file ="~/Documents/Del2/sessionsuser_id.csv")

View(sessionsuser_id)
summary(sessionsuser_id)
sapply(sessionsuser_id, mode)
head(sessionsuser_id)

#######################################################################################################################
#                                                                                                                     #
#                             Cluster Analysis- Sessions                                        
#
#######################################################################################################################

sessionsuser_id.stand <- scale(trainingData[-1])

### K-Means of k = 3 clusters
set.seed(1234)
k.means.fit <- kmeans(sessionsuser_id.stand, centers=3, nstart=20)

### Attributes of the 3 clusters
k.means.fit$centers
k.means.fit$cluster
k.means.fit$size

wssplot <- function(data, nc, seed){
  wss=numeric()
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(sessionsuser_id.stand, nc=6, seed=1234)
table(sessionsuser_id.stand[,1],k.means.fit$cluster)

d <- dist(sessionsuser_id.stand, method = "euclidean")
H.fit <- hclust(d, method="complete") 
plot(H.fit)

groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red") 
table(sessionsuser_id.stand[,1],groups)

H.fit <- hclust(d, method="average")
plot(H.fit)

groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red") 
table(sessionsuser_id.stand[,1],groups)

H.fit <- hclust(d, method="ward.D")  
plot(H.fit)

#######################################################################################################################
#                                                                                                                     #
#                             Logistic Regression - Sessions                                      
#
#######################################################################################################################
sessionsuser_id_ones <- sessionsuser_id[which(sessionsuser_id$action_detail == 1), ]  # all 1's
sessionsuser_id_zeros <- sessionsuser_id[which(sessionsuser_id$action_detail == 0), ]  # all 0's
set.seed(1234)  # for repeatability of samples
sessionsuser_id_ones_training_rows <- sample(1:nrow(sessionsuser_id_ones), 0.7*nrow(sessionsuser_id_ones))  # 1's for training
sessionsuser_id_zeros_training_rows <- sample(1:nrow(sessionsuser_id_zeros), 0.7*nrow(sessionsuser_id_ones))  # 0's for training. Pick as many 0's as 1's
training_ones <- sessionsuser_id_ones[sessionsuser_id_ones_training_rows, ]  
training_zeros <- sessionsuser_id_zeros[sessionsuser_id_zeros_training_rows, ]
trainingData <- rbind(training_ones, training_zeros)  # row bind the 1's and 0's 
trainingData$secs_elapsed[is.na(trainingData$secs_elapsed)] <- 0
trainingData$user_id[is.na(trainingData$user_id)] <- 0

### Create Test Data
test_ones <- sessionsuser_id_ones[-sessionsuser_id_ones_training_rows, ]
test_zeros <- sessionsuser_id_zeros[-sessionsuser_id_zeros_training_rows, ]
testData <- rbind(test_ones, test_zeros)  # row bind the 1's and 0's
testData$secs_elapsed[is.na(testData$secs_elapsed)] <- 0
testData$user_id[is.na(testData$user_id)] <- 0

########################################################################
#                                 Models
########################################################################
logitMod <- glm(sessionsuser_id$action_detail ~ sessionsuser_id$device_type + sessionsuser_id$secs_elapsed, data=trainingData, family=binomial(link="logit"))
predicted <- plogis(predict(logitMod, testData))  # predicted scores
# or
predicted <- predict(logitMod, testData, type="response")  # predicted scores
summary(logitMod)

#ROC Receiver Operating Characteristics Curve Greater the area under the ROC curve, better the predictive ability of the model.
plotROC(testData$action_detail, predicted)

#######################################################################################################################
#                                                                                                                     #
#                             Random Forest - Sessions                                      
#
#######################################################################################################################

View(trainingData)
head(trainingData)
sapply(trainingData, mode)
summary(trainingData)
View(testData)
head(testData)
sapply(testData, mode)
summary(testData)
set.seed(100)
forest = randomForest(factor(trainingData$action_detail)~.,data=trainingData,ntree=1000)

### Model performance as a factor of number of trees
plot(forest)
legend("top",colnames(forest$err.rate),col=1:4,cex=0.8,fill=1:4)

### Examine one of the trees (tree 17)
getTree(forest,17,labelVar = T)

### Trees vary in size, some small, some big
hist(treesize(forest))

### Relative importance of independent variables in predicting booking or non-booking
importance(forest) # or varImp(forest)
varImpPlot(forest)

### Predictions (on train data)
pred = predict(forest)
ct = table(trainingData$action_detail,pred); ct
accuracy = (ct[1,1]+ct[2,2])/nrow(trainingData); accuracy

## Predictions (on test data)
pred = predict(forest,newdata=testData)
ct = table(testData$action_detail,pred); ct
accuracy = (ct[1,1]+ct[2,2])/nrow(testData); accuracy

plotROC(testData$action_detail, accuracy)
plotROC(trainingData$action_detail, accuracy)
####xgBoost#########

### Create the response variable
y = trainingData$action_detail

### Create the predictor data set and encode categorical variables using caret library.
mtrain = trainingData[,-c(1,2)]
mtest = testData[,-c(1)]
dummies <- dummyVars(~ ., data = mtrain)
mtrain = predict(dummies, newdata = mtrain)
mtest = predict(dummies, newdata = mtest)

### Set necessary parameters and use parallel threads
param <- list("objective" = "reg:linear", "nthread" = 8, "verbose"=0)

### Fit the model
xgb.fit = xgboost(param=param, data = mtrain, label = y, nrounds=1500, eta = .01, max_depth = 7, 
                  min_child_weight = 5, scale_pos_weight = 1.0, subsample=0.8) 

### Predict booking for the test set
bookingpredicted <- data.frame(Id=testData$user_id)
testData$action_detail <- predict(xgb.fit, mtest)
write_csv(bookingpredicted , "xgbboost_r_booking.csv")

### Let us see what the model looks like.
model <- xgb.dump(xgb.fit, with.stats = T)
model[1:10]

# Bar graph representing each feature by a horizontal bar. 
# The longer the bar, the more important is the feature. 
# Features are classified by importance and clustered by importance.
names <- dimnames(mtrain)[[2]]
importance_matrix <- xgb.importance(names, model = xgb.fit)
xgb.plot.importance(importance_matrix[1:10,])


###########Regression Model on Train_users3 table################
logitMod1 <- glm(Train_users3$date_first_booking_tag ~ Train_users3$first_device_type_rev_tag + Train_users3$signup_method_tag + Train_users3$gender_rev_tag, data=Train_users3, family=binomial(link="logit"))
predictedTrainuser3a <- plogis(predict(logitMod1, Train_users3))  # predicted scores
plotROC(Train_users3$date_first_booking_tag, predictedTrainuser3a) ####AUROC 0.625


logitMod2 <- glm(Train_users3$book_rev_cnt_tag ~ Train_users3$first_device_type_rev_tag + Train_users3$gender_rev_tag, data=Train_users3, family=binomial(link="logit"))
predictedTrainuser3b <- plogis(predict(logitMod2, Train_users3))  # predicted scores
plotROC(Train_users3$book_rev_cnt_tag, predictedTrainuser3b) #####AUROC  0.5419

logitMod3 <- glm(Train_users3$date_first_booking_tag ~ Train_users3$first_device_type_rev_tag + Train_users3$signup_method_tag + Train_users3$gender_rev_tag, data=Train_users3, family=binomial(link="logit"))
predictedTrainuser3c <- plogis(predict(logitMod3, Train_users3))  # predicted scores
plotROC(Train_users3$date_first_booking_tag, predictedTrainuser3c) ####AUROC 0.625


#######Random Forest Train_user3 table#####
unique(is.na(Train_users3$book_rev_cnt_tag))
TrainUser3_id <- Train_users3 %>%
  select(id, date_first_booking_tag , signup_method_tag, session_book_Rev_tag, book_rev_cnt_tag, first_device_type_rev_tag, gender_rev_tag)
summary(TrainUser3_id)
View(TrainUser3_id)
sapply(TrainUser3_id, mode)
TrainUser3_id$id <- as.factor(TrainUser3_id$id)
TrainUser3_id$id <- as.numeric(TrainUser3_id$id)
sapply(TrainUser3_id, mode)

set.seed(100)
forest2 = randomForest(factor(TrainUser3_id$book_rev_cnt_tag)~.,data=TrainUser3_id,ntree=1000)
# Model performance as a factor of number of trees
plot(forest2)
legend("top",colnames(forest2$err.rate),col=1:4,cex=0.8,fill=1:4)

# Examine one of the trees (tree 17)
getTree(forest2,17,labelVar = T)

# Trees vary in size, some small, some big
hist(treesize(forest2))

# Relative importance of independent variables in predicting booking or non-booking
importance(forest2) # or varImp(forest)
varImpPlot(forest2)

## Predictions (on train_user3 table)
pred1 = predict(forest2)
ct1 = table(TrainUser3_id$book_rev_cnt_tag,pred1); ct1
accuracy1 = (ct1[1,1]+ct[2,2])/nrow(TrainUser3_id); accuracy1 ######0.906892
plotROC(TrainUser3_id$book_rev_cnt_tag, accuracy1) #####AUROC 0.5

#######Cluster Train_user3 table#####
td <- dist(TrainUser3_id, method = "euclidean")
tH.fit <- hclust(td, method="complete") 
plot(tH.fit)


#######################################################################################################################
#
#                     Treatment of Train_User file for Analysis
#
#                       1.Booking variable treatment
#                       2.Age variable treatment   
#
#######################################################################################################################

##Make first booking date to "1" and no booking date ="0" - Converting to Numeric
Train_Users$date_first_booking_rev <- Train_Users$date_first_booking
Train_Users$date_first_booking_rev <- as.numeric(Train_Users$date_first_booking_rev)

Train_Users$date_first_booking_rev[!is.na(Train_Users$date_first_booking_rev)] <- 1
Train_Users$date_first_booking_rev[is.na(Train_Users$date_first_booking_rev)] <- 0

print(Train_Users$date_first_booking_rev)

#Age varaible treatment - Categorize/group age variable in range of 20,25....90
agebreaks <- c(-1,0,20,25,30,35,40,45,50,55,60,65,70,75,80,85,90,500)
agelabels <- c("-1","0-19","20-24","25-29","30-34",
               "35-39","40-44","45-49","50-54","55-59","60-64","65-69",
               "70-74","75-79","80-84","85-90","90+")
setDT(Train_Users)[ ,age_group :=cut(age_rev, breaks = agebreaks, right = FALSE, labels = agelabels)]
Train_Users$age_group


#######################################################################################################################
#                                                                                                                     #
#                             Logistic Regression                                           
#                  
#                         1. Plot Relationship between variables
#                         2. Build Models
#                         3. Predict MOdels
#                         4. Classification tables
#                         5. ROCR Plot 
#
#######################################################################################################################

#Plot the relationship between variable for confirmed bookings.

tapply(Train_Users$date_first_booking_rev,Train_Users$gender_rev,sum)
ggplot(data=Train_Users,aes(x=factor(gender_rev), y=date_first_booking_rev, fill=(gender_rev)))+
  geom_bar(stat='summary', fun.y='sum')+ labs(x="Gender", y="Booking") # gender

ggplot(data=Train_Users,aes(x=factor(signup_app), y=date_first_booking_rev, fill=(signup_app)))+
  geom_bar(stat='summary', fun.y='sum') # Majority are webusers

ggplot(data=Train_Users,aes(x=factor(first_device_type_rev), y=date_first_booking_rev, fill=(first_device_type_rev)))+
  geom_bar(stat='summary', fun.y='sum') #Iphone and Mac desktop most used in successfull booking.

tapply(Train_Users$date_first_booking_rev,Train_Users$age_group,sum)
ggplot(data=Train_Users,aes(x=factor(age_group), y=date_first_booking_rev, fill=(age_group)))+
  geom_bar(stat='summary', fun.y='sum') + labs(x="Age Group", y="Booking") # Max booking between 25 to 45.

ggplot(data=Train_Users,aes(x=factor(age_group), y=date_first_booking_rev, fill=(gender_rev)))+
  geom_bar(stat='summary', fun.y='sum') + labs(x="Age Group", y="Booking") # Comparion b/w age group and gender - More female book

ggplot(data=Train_Users,aes(x=factor(affiliate_provider_rev), y=date_first_booking_rev, fill=(affiliate_provider_rev)))+
  geom_bar(stat='summary', fun.y='sum')+ labs(x="Affliate Provider", y="Booking")  # Most booking are via "Direct" affliate, then google

ggplot(data=Train_Users,aes(x=factor(affiliate_channel), y=date_first_booking_rev, fill=(affiliate_provider_rev)))+
  geom_bar(stat='summary', fun.y='sum')  # Interection between "Direct" for channel and provider.

tapply(Train_Users$date_first_booking_rev,Train_Users$language_rev,sum)
ggplot(data=Train_Users,aes(x=factor(language_rev), y=date_first_booking_rev, fill=(affiliate_provider_rev)))+
  geom_bar(stat='summary', fun.y='sum')  # Comparion b/w age group and gender - More female book

############################################################################
#    
#                       Models
#
############################################################################

#Gender
model1 = glm(date_first_booking_rev~gender_rev, data=Train_Users,family='binomial')
summary(model1)

#Signup app
model2 = glm(date_first_booking_rev~signup_app, data=Train_Users,family='binomial')
summary(model2)

#First device type
model3 = glm(date_first_booking_rev~first_device_type_rev, data=Train_Users,family='binomial')
summary(model3)

predict(model3,newdata=data.frame(first_device_type_rev="iPhone/iPad"),type='response')
predict(model3,newdata=data.frame(first_device_type_rev="Others"),type='response')
predict(model3,newdata=data.frame(first_device_type_rev="Windows Desktop"),type='response')
predict(model3,newdata=data.frame(first_device_type_rev="Android Phone/Tab"),type='response')

#First browser
model5 = glm(date_first_booking_rev~first_browser, data=Train_Users,family='binomial')
summary(model5)

#Age group
model6 = glm(date_first_booking_rev~age_group, data=Train_Users,family='binomial')
summary(model6)

#Combination of variables
model7 = glm(date_first_booking_rev~gender_rev+age_group+language_rev+
               affiliate_provider_rev, data=Train_Users,family='binomial')
summary(model7) # Significant interaction observed

#Language
model_lan = glm(date_first_booking_rev~language_rev, data=Train_Users,family='binomial')
summary(model_lan)

#Provider
model_pro = glm(date_first_booking_rev~affiliate_provider_rev, data=Train_Users,family='binomial')
summary(model_pro)

############################################################################
#    
#                      Predict Models
#
############################################################################

#gender - Better than other varaible
pred1= predict(model1,type='response')
table(as.numeric(pred1>.5))

#sign_up
pred2= predict(model2,type='response')
table(as.numeric(pred2>.5))

#first_device
pred3= predict(model3,type='response')
table(as.numeric(pred3>.5))

#first browser
pred5= predict(model5,type='response')
table(as.numeric(pred5>.5))

#age
pred6= predict(model6,type='response')
table(as.numeric(pred5>.5))

#lang - All are false - Cannot proceed.
pred_lan= predict(model_lan,type='response')
table(as.numeric(pred_lan>.5))

#provider
pred_pro= predict(model_pro,type='response')
table(as.numeric(pred_pro>.5))

#gender, age, language, affilate provider
pred7= predict(model7,type='response')
table(as.numeric(pred7>.5))


############################################################################
#    
#                      Classification Table
#
############################################################################


#classification table for probability greater than 0.5 - Gender (pred1)
ct1 = table(Train_Users$date_first_booking_rev, pred1>.5)
ct1

#classification table for probability greater than 0.5 - Age (pred6)
ct6 = table(Train_Users$date_first_booking_rev, pred6>.5)
ct6

#classification table for probability greater than 0.5 - language - Cannot proceed, all false
ct_lan = table(Train_Users$date_first_booking_rev, pred_lan>.5)
ct_lan


#classification table for probability greater than 0.5 - Combination (pred7)
ct7 = table(Train_Users$date_first_booking_rev, pred7>.5)
ct7


#Gender -0.59
accuracy1 = sum(ct1[1,1],ct1[2,2])/nrow(Train_Users)
accuracy1

specificity1 = ct1[1,1]/sum(ct1[1,1],ct1[1,2]);specificity1
sensitivity1 = ct1[2,2]/sum(ct1[2,1],ct1[2,2]);sensitivity1

#Age - 0.63
accuracy6 = sum(ct6[1,1],ct6[2,2])/nrow(Train_Users)
accuracy6

specificity6 = ct6[1,1]/sum(ct6[1,1],ct6[1,2]);specificity6 #0.61
sensitivity6 = ct6[2,2]/sum(ct6[2,1],ct6[2,2]);sensitivity6 #0.67

#Language - Cannot proceed
accuracy_lan = sum(ct_lan[1,1],ct_lan[2,2])/nrow(Train_Users)
accuracy_lan



##gender, age, language, affilate provider - 0.65 - So far most important
accuracy7 = sum(ct7[1,1],ct7[2,2])/nrow(Train_Users)
accuracy7

specificity7 = ct7[1,1]/sum(ct7[1,1],ct7[1,2]);specificity7 #0.64
sensitivity7 = ct7[2,2]/sum(ct7[2,1],ct7[2,2]);sensitivity7 #0.65


############################################################################
#    
#                      ROCR Plot
#
############################################################################

#Gender
ROCRpred1 = prediction(pred1,Train_Users$date_first_booking_rev)
as.numeric(performance(ROCRpred1,"auc")@y.values) #gives auc measure -0.60
ROCRperf1 = performance(ROCRpred1,"tpr","fpr")
plot(ROCRperf1)

#Age
ROCRpred6 = prediction(pred6,Train_Users$date_first_booking_rev)
as.numeric(performance(ROCRpred6,"auc")@y.values) #gives auc measure = 0.67
ROCRperf6 = performance(ROCRpred6,"tpr","fpr")
plot(ROCRperf6)

#Combination
ROCRpred7 = prediction(pred7,Train_Users$date_first_booking_rev)
as.numeric(performance(ROCRpred7,"auc")@y.values) #gives auc measure -0.685
ROCRperf7 = performance(ROCRpred7,"tpr","fpr")
plot(ROCRperf7)

plotROC(Train_Users$date_first_booking_rev, accuracy7)


#######################################################################################################################
#                                                                                                                     #
#                             Decision Tree                                           
#
#######################################################################################################################
str(Train_Users)

#####All variables###
tree = rpart(date_first_booking_rev~.,data=Train_Users,method = 'class',cp=0)
rpart.plot(tree)###Not useful result

#####Provider and gender###
tree1 = rpart(date_first_booking_rev~affiliate_provider_rev+gender_rev,data=Train_Users,method = 'class',cp=0)
rpart.plot(tree1)

####Most variables#### Most useful - 0.71


tree2 = rpart(date_first_booking_rev~gender_rev+age_rev+signup_method+
                language_rev+affiliate_channel+affiliate_provider_rev+signup_app+
                first_device_type_rev+first_browser,data=Train_Users,method = 'class',cp=0)
rpart.plot(tree2)
#Variable excluded - date_account_created+timestamp_first_active+signup_flow+first_affiliate_tracked++country_destination
pred_tree2 = predict(tree2, type='class')
ct_tree2 = table(Train_Users$date_first_booking_rev,pred_tree2); ct_tree2
accuracy_tree2 = (ct_tree2[1,1]+ct_tree2[2,2])/nrow(Train_Users); accuracy_tree2 # 0.71

ROCRpred_tree2 = prediction(pred_tree2,Train_Users$date_first_booking_rev)
as.numeric(performance(ROCRpred_tree2,"auc")@y.values)
ROCRperf7 = performance(ROCRpred7,"tpr","fpr")
plot(ROCRperf7)

ROCRpred7 = prediction(pred7,Train_Users$date_first_booking_rev)
as.numeric(performance(ROCRpred7,"auc")@y.values) #gives auc measure -0.685
ROCRperf7 = performance(ROCRpred7,"tpr","fpr")
plot(ROCRperf7)


#Similar as age - Age group instead of age
tree3 = rpart(date_first_booking_rev~gender_rev+age_group+signup_method+
                language_rev+affiliate_channel+affiliate_provider_rev+signup_app+
                first_device_type_rev+first_browser,data=Train_Users,method = 'class',cp=0)
rpart.plot(tree3) # similar tree as earlier

pred_tree3 = predict(tree3, type='class')
ct_tree3 = table(Train_Users$date_first_booking_rev,pred_tree3); ct_tree3
accuracy_tree3 = (ct_tree3[1,1]+ct_tree3[2,2])/nrow(Train_Users); accuracy_tree3 # 0.70

####Tree 4
tree4 = rpart(date_first_booking_rev~gender_rev+age_rev+
                language_rev+affiliate_provider_rev+signup_app+
                first_device_type_rev+first_browser,data=Train_Users,method = 'class',cp=0)
rpart.plot(tree4)

pred_tree4 = predict(tree4, type='class')
ct_tree4 = table(Train_Users$date_first_booking_rev,pred_tree4); ct_tree4
accuracy_tree4 = (ct_tree4[1,1]+ct_tree4[2,2])/nrow(Train_Users); accuracy_tree4 # 0.69

###Tree 5
tree5 = rpart(date_first_booking_rev~gender_rev+age_rev+first_browser+
                language_rev+affiliate_provider_rev+
                first_device_type_rev+first_browser,data=Train_Users,method = 'class',cp=0)
rpart.plot(tree5)

pred_tree5 = predict(tree5, type='class')
ct_tree4 = table(Train_Users$date_first_booking_rev,pred_tree4); ct_tree5
accuracy_tree5 = (ct_tree5[1,1]+ct_tree5[2,2])/nrow(Train_Users); accuracy_tree5 # 0.69

#######################################################################################################################
#                                                                                                                     #
#                             Random Forest                                           
#
#######################################################################################################################

set.seed(100)
str(Train_Users)

# Vairable Treatment
Train_Users$date_first_booking_rev <- as.factor(Train_Users$date_first_booking_rev)
Train_Users$gender_rev <- as.factor(Train_Users$gender_rev)
Train_Users$age_rev <- as.factor(Train_Users$age_rev)
Train_Users$age_group <- as.factor(Train_Users$age_group)
Train_Users$language_rev <- as.factor(Train_Users$language_rev)
Train_Users$first_device_type_rev <- as.factor(Train_Users$first_device_type_rev)
Train_Users$first_browser <- as.factor(Train_Users$first_browser)
Train_Users$affiliate_provider_rev <- factor(Train_Users$affiliate_provider_rev)

forest1 = randomForest(date_first_booking_rev~gender_rev+age_group+
                         language_rev+affiliate_provider_rev+
                         first_device_type_rev+first_browser,data=Train_Users,ntree=100)
plot(forest1)
hist(treesize(forest1))
importance(forest1)
varImpPlot(forest1)

pred_for1 = predict(forest1)
ct_for1 = table(Train_Users$date_first_booking_rev,pred_for1); ct_for1

accuracy_for1 = (ct_for1[1,1]+ct_for1[2,2])/nrow(Train_Users); accuracy_for1 ##0.657



#######################################################################################################################
#                                                                                                                     #
#                             Analysis of Model Dataset                                          
#
#######################################################################################################################
#Read files
Sessions <- read_csv("~/Columbia University/Spring Term/Frameworks/Project_AirBnB/Exp_Sessions.csv")
View(Sessions)
Exp_Test_Users <- read_csv("~/Columbia University/Spring Term/Frameworks/Project_AirBnB/Exp_Test_Users.csv")
View(Exp_Test_Users)
Exp_Test_Users <- read_csv("~/Columbia University/Spring Term/Frameworks/Project_AirBnB/Exp_Train_Users.csv")
View(Train_Users)


#Select unique ids from train user dataset
test_ids <- unique(Test_Users$id)   #62096
test_ids <- data.frame(test_ids)
nrow(test_ids)
train_ids <- unique(Train_Users$id) #213451
train_ids <- data.frame(train_ids)

rm("Exp_Sessions","Exp_Test_Users","Exp_Train_Users", "unique_ids")
names(train_ids) <- c("id")
names(test_ids) <- c("id")

#Merging the two datasets of unique user ids
all_ids <- rbind(train_ids, test_ids) 
#275547 records

#Duplicacy check
dedup_all_ids <- all_ids[!duplicated(all_ids), ]
#275547 records - no duplicates found


#Creating target variable
unique(Sessions$action_type_rev)
unique(Sessions$action_type)
a <- unique(Sessions$action_detail)
a

table(Sessions$action_detail) #booking - 54652
book_tag <- ifelse(Sessions$action_detail == "booking",1,0)
Sessions$action_detail_rev[Sessions$action_detail_rev == "set_default_payment_instrument"] <- "booking"

#######################################################################################################################
##          Correlation analysis on the final model dataset to determine correlated variables
#######################################################################################################################
Train_users3_Onlytag <- Train_users3[, -c(2:10)]
Train_users3_Onlytag <- Train_users3_Onlytag[, -c(2:10)]
Train_users3_Onlytag <- Train_users3_Onlytag[, -c(2:5)]
Train_users3_Onlytag <- Train_users3_Onlytag[, -3]
Train_users3_Onlytag <- Train_users3_Onlytag[, -3]
Train_users3_Onlytag <- Train_users3_Onlytag[, -c(3:7)]
Train_users3_Onlytag <- Train_users3_Onlytag[, -3]
Train_users3_Onlytag <- Train_users3_Onlytag[, -9]
Train_users3_Onlytag <- Train_users3_Onlytag[, -2]

#Train_users3_Onlytag <- Train_users3_Onlytag[, -16]


### Convert into numeric
Train_users3_Onlytag$id <- as.numeric(Train_users3_Onlytag$id)
Train_users3_Onlytag$signup_method_tag <- as.numeric(Train_users3_Onlytag$signup_method_tag)
Train_users3_Onlytag$affiliate_channel_tag <- as.numeric(Train_users3_Onlytag$affiliate_channel_tag)
Train_users3_Onlytag$affiliate_provider_rev_tag <- as.numeric(Train_users3_Onlytag$affiliate_provider_rev_tag)
Train_users3_Onlytag$gender_rev_tag <- as.numeric(Train_users3_Onlytag$gender_rev_tag)
Train_users3_Onlytag$language_rev_tag <- as.numeric(Train_users3_Onlytag$language_rev_tag)
Train_users3_Onlytag$user_type_tag <- as.numeric(Train_users3_Onlytag$user_type_tag)
Train_users3_Onlytag$first_device_type_rev_tag <- as.numeric(Train_users3_Onlytag$first_device_type_rev_tag)
Train_users3_Onlytag$age_rev_bin_tag <- as.numeric(Train_users3_Onlytag$age_rev_bin_tag)
Train_users3_Onlytag <- Train_users3_Onlytag[, -1] #Removed id col since it has all NA

cor(Train_users3_Onlytag, use="all.obs", method="pearson") 
# Correlation observed in affiliate_provider_rev_tag and affiliate_channel_tag 
summary(Train_users3_Onlytag)


#######################################################################################################################
#             Split data into 70:30 based on book_rev_cnt_tag
#######################################################################################################################
set.seed(100)
split = sample.split(Train_users3$book_rev_cnt_tag,SplitRatio = 0.7)
split
train_model = Train_users3[split,] #149416
test_model = Train_users3[!split,] #64035


########################################################################################################################
#             Logistic regression using book_rev_cnt_tag
#######################################################################################################################
logitMod1 <- glm(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                   train_model$android_cnt_tag +  train_model$date_first_booking_tag + train_model$first_device_type_rev_tag +
                   train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag + train_model$macdesk_cnt_tag +
                   train_model$review_cnt_tag + train_model$signup_method_tag + train_model$windesk_cnt_tag, data = train_model, family=binomial(link="logit"))  
summary(logitMod1)
pred_train_model <- plogis(predict(logitMod1, train_model))
plotROC(train_model$book_rev_cnt_tag, pred_train_model)  # AUC - 0.9138
table(as.numeric(pred_train_model>.5))
#     0      1 
#142166   7250 


#######################################################################################################################
#         Logistic regression using date_first_booking_tag
#######################################################################################################################
# Split data into 70:30 based on date_first_booking_tag
set.seed(100)
split1 = sample.split(Train_users3$date_first_booking_tag,SplitRatio = 0.7)
split1
train_model_dt = Train_users3[split1,] #149416
test_model_dt = Train_users3[!split1,] #64035

table(train_model_dt$book_rev_cnt_tag)
#     0      1 
# 133472  15944 

logitMod2 <- glm(train_model_dt$date_first_booking_tag ~ train_model$user_type_tag  + train_model_dt$affiliate_channel_tag + train_model_dt$affiliate_provider_rev_tag + train_model_dt$age_rev_bin_tag +
                   train_model_dt$android_cnt_tag + train_model_dt$book_rev_cnt_tag + train_model_dt$first_device_type_rev_tag +
                   train_model_dt$gender_rev_tag + train_model_dt$iphone_cnt_tag + train_model_dt$language_rev_tag + train_model_dt$macdesk_cnt_tag +
                   train_model_dt$review_cnt_tag + train_model_dt$signup_method_tag + train_model_dt$windesk_cnt_tag, data = train_model, family=binomial(link="logit"))  
summary(logitMod2)
pred_train_model2 <- plogis(predict(logitMod2, train_model))
plotROC(train_model$date_first_booking_tag, pred_train_model2)  # AUC - 0.2865

### Logistic regression using book_rev_cnt_tag : removing non-significant variables
logitMod3 <- glm(train_model$book_rev_cnt_tag ~ train_model$user_type_tag +  train_model$age_rev_bin_tag + train_model$android_cnt_tag + train_model$date_first_booking_tag +
                   train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$macdesk_cnt_tag + train_model$review_cnt_tag + train_model$signup_method_tag +
                   train_model$windesk_cnt_tag, data = train_model, family=binomial(link="logit"))  
summary(logitMod3)
pred_train_model3 <- plogis(predict(logitMod3, train_model))
plotROC(train_model$book_rev_cnt_tag, pred_train_model3) #0.9122
predicted <- predict(logitMod3, train_model, type="response")  # predicted scores


#######################################################################################################################
#        Testing above model on the test dataset
#######################################################################################################################
logitMod4 <- glm(test_model$book_rev_cnt_tag ~  test_model$user_type_tag + test_model$age_rev_bin_tag + test_model$android_cnt_tag + test_model$date_first_booking_tag +
                   test_model$gender_rev_tag + test_model$iphone_cnt_tag + test_model$macdesk_cnt_tag + test_model$review_cnt_tag + test_model$signup_method_tag +
                   test_model$windesk_cnt_tag, data = test_model, family=binomial(link="logit"))  
summary(logitMod4)
pred_train_model4 <- plogis(predict(logitMod4, test_model))
plotROC(test_model$book_rev_cnt_tag, pred_train_model4) #0.9145

# Testing on test model dataset
logitMod5 <- glm(test_model$date_first_booking_tag ~ test_model$first_device_type_rev_tag + test_model$signup_method_tag + test_model$gender_rev_tag
                 , data=test_model, family=binomial(link="logit"))
predictedTrainuser3a <- plogis(predict(logitMod5, test_model))  # predicted scores
plotROC(test_model$date_first_booking_tag, predictedTrainuser3a) ####AUROC 0.641

# Retaining above variables and adding the remaining significant ones
logitMod6 <-  glm(train_model$book_rev_cnt_tag ~  train_model$user_type_tag + train_model$first_device_type_rev_tag + train_model$age_rev_bin_tag + train_model$android_cnt_tag + train_model$date_first_booking_tag +
                    train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$macdesk_cnt_tag + train_model$review_cnt_tag + train_model$signup_method_tag +
                    train_model$windesk_cnt_tag, data = train_model, family=binomial(link="logit"))  
predictedTrainuser3a <- plogis(predict(logitMod6, train_model))  # predicted scores
plotROC(train_model$date_first_booking_tag, predictedTrainuser3a) ####AUROC 0.5962


#######################################################################################################################
##                                Random Forest
#######################################################################################################################
sapply(Train_users3_Onlytag, mode)
summary(trainingData)

set.seed(100)
#forest = randomForest(factor(Train_users3_Onlytag$book_rev_cnt_tag)~.,data=Train_users3_Onlytag,ntree=100)
forest2 = randomForest(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                         train_model$android_cnt_tag +  train_model$date_first_booking_tag + train_model$first_device_type_rev_tag +
                         train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag + train_model$macdesk_cnt_tag +
                         train_model$review_cnt_tag + train_model$signup_method_tag + train_model$windesk_cnt_tag, data = train_model)

# Model performance as a factor of number of trees
plot(forest2)
legend("top",colnames(forest2$err.rate),col=1:4,cex=0.8,fill=1:4)

# Examine one of the trees (tree 17)
getTree(forest2,98,labelVar = T)

# Trees vary in size, some small, some big
hist(treesize(forest2))

# Relative importance of independent variables in predicting booking or non-booking
importance(forest2) # or varImp(forest)
varImpPlot(forest2)

## Predictions (on train data)
pred = predict(forest2)
ct = table(train_model$book_rev_cnt_tag,pred); ct
accuracy = (ct[1,1]+ct[2,2])/nrow(train_model); accuracy
#[1] 6.692724e-06

## Predictions (on test data)
pred = predict(forest,newdata=testData)
ct = table(testData$action_detail,pred); ct
accuracy = (ct[1,1]+ct[2,2])/nrow(testData); accuracy

#######################################################################################################################
##                            Decision Tree
#######################################################################################################################
tree1 = rpart(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                train_model$android_cnt_tag 
              +  train_model$date_first_booking_tag 
              + train_model$first_device_type_rev_tag +
                train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag 
              + train_model$macdesk_cnt_tag 
              + train_model$review_cnt_tag + train_model$signup_method_tag 
              + train_model$windesk_cnt_tag
              , data = train_model,method='class', cp = 0.0005)
rpart.plot(tree1)

# Predictions and measuring performance
pred = predict(tree1,type='class')
ct = table(train_model$book_rev_cnt_tag,pred); ct
#        pred
#      0      1
# 0 132861    553
# 1  13871   2131
accuracy = (ct[1,1]+ct[2,2])/nrow(train_model); accuracy
# 0.9034642

tree2 = rpart(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                train_model$android_cnt_tag 
              +  train_model$date_first_booking_tag 
              + train_model$first_device_type_rev_tag +
                train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag 
              + train_model$macdesk_cnt_tag 
              + train_model$review_cnt_tag + train_model$signup_method_tag 
              + train_model$windesk_cnt_tag
              , data = train_model,method='class', cp = 0.05)
rpart.plot(tree2)

# Predictions and measuring performance
pred = predict(tree2,type='class')
ct = table(train_model$book_rev_cnt_tag,pred); ct
#        pred
#      0      1
# 0 133414    0
# 1  16002    0
accuracy = (ct[1,1]+ct[2,2])/nrow(train_model); accuracy
# 0.892903

tree3 = rpart(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                train_model$android_cnt_tag 
              +  train_model$date_first_booking_tag 
              + train_model$first_device_type_rev_tag +
                train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag 
              + train_model$macdesk_cnt_tag 
              + train_model$review_cnt_tag + train_model$signup_method_tag 
              + train_model$windesk_cnt_tag
              , data = train_model,method='class', cp = 0)
rpart.plot(tree3)
# Predictions and measuring performance
pred = predict(tree3,type='class')
ct = table(train_model$book_rev_cnt_tag,pred); ct
#        pred
#      0      1
# 0 131036   2378
# 1  10791   5211
accuracy = (ct[1,1]+ct[2,2])/nrow(train_model); accuracy
# 0.9118635

tree4 = rpart(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                train_model$android_cnt_tag 
              +  train_model$date_first_booking_tag 
              + train_model$first_device_type_rev_tag +
                train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag 
              + train_model$macdesk_cnt_tag 
              + train_model$review_cnt_tag + train_model$signup_method_tag 
              + train_model$windesk_cnt_tag
              , data = train_model,method='class', cp = 0.0001)
rpart.plot(tree4)
# Predictions and measuring performance
pred = predict(tree4,type='class')
ct = table(train_model$book_rev_cnt_tag,pred); ct
#        pred
#      0      1
# 0 132278   1136
# 1  12667   3335
accuracy = (ct[1,1]+ct[2,2])/nrow(train_model); accuracy
#  0.9076203

### Determine optimal cp
trControl = trainControl(method = 'cv',number = 10)
tuneGrid = expand.grid(.cp=seq(0,0.1,0.001))
set.seed(100)
trainCV = train(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                  train_model$android_cnt_tag 
                +  train_model$date_first_booking_tag 
                + train_model$first_device_type_rev_tag +
                  train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag 
                + train_model$macdesk_cnt_tag 
                + train_model$review_cnt_tag + train_model$signup_method_tag 
                + train_model$windesk_cnt_tag,train_model,method='rpart',trControl=trControl,tuneGrid=tuneGrid)
plot(trainCV)
trainCV$bestTune

### Estimate tree with optimal cp
tree_cv = rpart(train_model$book_rev_cnt_tag ~ train_model$user_type_tag + train_model$affiliate_channel_tag + train_model$affiliate_provider_rev_tag + train_model$age_rev_bin_tag +
                  train_model$android_cnt_tag 
                +  train_model$date_first_booking_tag 
                + train_model$first_device_type_rev_tag +
                  train_model$gender_rev_tag + train_model$iphone_cnt_tag + train_model$language_rev_tag 
                + train_model$macdesk_cnt_tag 
                + train_model$review_cnt_tag + train_model$signup_method_tag 
                + train_model$windesk_cnt_tag,data=train_model,method='class',cp=trainCV$bestTune,xval=0)
rpart.plot(tree_cv)

# Validation
pred = predict(tree4,newdata=Train_users3, type='class')
ct = table(Train_users3$book_rev_cnt_tag,pred); ct
accuracy = (ct[1,1]+ct[2,2])/nrow(Train_users3); accuracy

#######################################################################################################################
##                          Cluster Analysis
#######################################################################################################################

## K-means
# Standarize all variables and drop the variable Type
Train_users3_Onlytag.stand <- scale(Train_users3_Onlytag[-1])
summary(Train_users3_Onlytag.stand)

# K-Means of k = 3 clusters
set.seed(1234)
k.means.fit <- kmeans(Train_users3_Onlytag.stand, centers=3, nstart=20)

# Attributes of the 3 clusters
k.means.fit$centers
k.means.fit$cluster
k.means.fit$size

wssplot <- function(data, nc, seed){
  wss=numeric()
  for (i in 1:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(Train_users3_Onlytag.stand, nc=6, seed=1234)
table(Train_users3_Onlytag.stand[,1],k.means.fit$cluster)

d <- dist(Train_users3_Onlytag.stand, method = "euclidean")
H.fit <- hclust(d, method="complete") 
plot(H.fit)

groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red") 
table(Train_users3_Onlytag.stand[,1],groups)

H.fit <- hclust(d, method="average")
plot(H.fit)

groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red") 
table(Train_users3_Onlytag.stand[,1],groups)

H.fit <- hclust(d, method="ward.D")  
plot(H.fit)

groups <- cutree(H.fit, k=3)
rect.hclust(H.fit, k=3, border="red")
table(Train_users3_Onlytag.stand[,1],groups)

### Note - Cluster Analysis could not be performed due to hardware limitations.

###########################################################################################################################

