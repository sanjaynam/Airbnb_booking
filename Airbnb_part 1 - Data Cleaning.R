
## Invoke required libraries
library(readr)
library(dplyr)
library(tidyverse)
library(descr)

###################################################################################################################
#                                                                                                                 #
#         1.	Identified key data issues in the input dataset (Sessions, Test_User, Train_User).                  #
#         2.	Properly diagnosed the data issue using tools in R.                                                 #
#                                                                                                                 #
###################################################################################################################

###Read train_users, test_users and sessions dataset
Sessions <- read_csv("~/Documents/CU Getting Started/Spring Term/Frameworks - KC/Project Assignment/Frameworks_Project/sessions.csv")
Test_Users <- read_csv("~/Documents/CU Getting Started/Spring Term/Frameworks - KC/Project Assignment/Frameworks_Project/test_users.csv")
Train_Users <- read_csv("~/Documents/CU Getting Started/Spring Term/Frameworks - KC/Project Assignment/Frameworks_Project/train_users_2.csv")

###Verify the data has been properly imported
head(Sessions,10)
head(Test_Users,10)
head(Train_Users,10)

###Understand imported table structures
str(Sessions)
names(Sessions)
dim(Sessions)
nrow(Sessions) #10567737 Rows
ncol(Sessions) #6 Columns

str(Test_Users)
names(Test_Users)
dim(Test_Users)
nrow(Test_Users) #62096 Rows
ncol(Test_Users) #21 Columns

str(Train_Users)
names(Train_Users)
dim(Train_Users)
nrow(Train_Users) #213451 Rows
ncol(Train_Users) #22 Columns

## ---------------------------------------------------------------------------------
## Analysing the three datasets in detail
##
## ---------------------------------------------------------------------------------

#---------------------
##FILE : Test_Users
#---------------------

##Index data to provide indentification per record
Test_Users$index<- rownames(Test_Users)

test <- unique(Test_Users$id)
#Comments : 62096 unique users in Test_Useers
summary(Test_Users)
#Comments :  Users with account creation date between 2014-07-01 and 2014-09-30
#            Age variable contains 28876 missing values and some outliers

##Missing Value Analysis
which(is.na(Test_Users$id)) #No missing values
which(is.na(Test_Users$date_account_created)) #No missing values
which(is.na(Test_Users$timestamp_first_active)) #No missing values
#which(is.na(Test_Users$date_first_booking)) #61096 missing entries
which(is.na(Test_Users$gender)) #No missing values
which(is.na(Test_Users$signup_method)) #No missing values
which(is.na(Test_Users$signup_flow)) #No missing values
which(is.na(Test_Users$language)) #No missing values
which(is.na(Test_Users$affiliate_channel)) #No missing values
which(is.na(Test_Users$affiliate_provider)) #No missing values
which(is.na(Test_Users$first_affiliate_tracked)) #20 missing entries
which(is.na(Test_Users$signup_app)) #No missing values
which(is.na(Test_Users$first_device_type)) #No missing values
which(is.na(Test_Users$first_browser)) #No missing values

#---------------------
##FILE : Train_User
#---------------------

#Index data to provide indentification per record
Train_Users$index<- rownames(Train_Users)

train <- unique(Train_Users$id)
#Comment : 213451 unique users
summary(Train_Users)
#Comment : Users with account creation date between 2010-01-01 and 2014-06-30
#          Age variable contains 87990 missing values and some outliers
#          First booking date has 124543 missing entries

##Missing Value Analysis
which(is.na(Train_Users$id)) #No missing values
which(is.na(Train_Users$date_account_created)) #No missing values
which(is.na(Train_Users$timestamp_first_active)) #No missing values
#which(is.na(Train_Users$date_first_booking)) #124543 missing entries
which(is.na(Train_Users$gender)) #No missing values
which(is.na(Train_Users$signup_method)) #No missing values
which(is.na(Train_Users$signup_flow)) #No missing values
which(is.na(Train_Users$language)) #No missing values
which(is.na(Train_Users$affiliate_channel)) #No missing values
which(is.na(Train_Users$affiliate_provider)) #No missing values
#which(is.na(Train_Users$first_affiliate_tracked)) #6065 missing entries
which(is.na(Train_Users$signup_app)) #No missing values
which(is.na(Train_Users$first_device_type)) #No missing values
which(is.na(Train_Users$first_browser)) #No missing values

#-----------------------
## FILE : Sessions File
#-----------------------

#Index data to provide indentification per record
#sessions$index<- rownames(Sessions)

sessions <- unique(Sessions$user_id)
#Comments :  135484 unique users

summary(Sessions)
#Comment :  secs_elapsed variable contains 136031 missing values and some outliers

#Raw variable - action
unique(Sessions$action)
#COmment : Too many posibilities for action variable. Too much noise. Can be dropped from analysis

#Raw variable - action_type
unique(Sessions$action_type)
action_type <- table(Sessions$action_type)
b <- barplot(action_type, main="Action Type", xlab="action_type", col=c("darkblue"),beside=TRUE, horiz=FALSE)
#Comment : 11 different ways of describing action type. Possible dependent variable in booking request & booking response


#######################################################################################################################
#                                                                                                                     #
#         3.Demonstrated 'how to resolve the data issue using tools in R' (e.g. Missing, NA values etc.).             #
#         4.Before- and after-picture of each data issue.                                                             #
#         6.Cleaning/processing effort is aligned with the question                                                   #
#               a.Data issue addressed missing values in relevant categorical variables.                              #
#                 Frequency and barplots have been used to understand and validate the transformation                 #
#               b.Numeric variables have been checked for existing outliers and replaced by the                       #  
#                 1st and 98th percentile values.Box-plots have been used to understand                               #
#               c.validate the transformation                                                                         #
#                                                                                                                     #
#######################################################################################################################

#Analyse categorical variables and perform cleaning / transformation where required

#---------------------
## FILE : Test_User
#---------------------

#Before Transformation : Raw variable - Gender. 
unique(Test_Users$gender)
gender <- table(Test_Users$gender)
b <- barplot(gender, main="Gender Distribution", xlab="Gender", col=c("darkblue"), legend = rownames(gender),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - Gender_rev
Test_Users$gender_rev <- Test_Users$gender
Test_Users$gender_rev[which(Test_Users$gender_rev == "OTHER")] ="N/A"
Test_Users$gender_rev[which(Test_Users$gender_rev == "-unknown-")] ="N/A"
gender_rev <- table(Test_Users$gender_rev)
b <- barplot(gender_rev, main="Gender Distribution", xlab="Gender_Revised", col=c("darkblue"), beside=TRUE, horiz=FALSE)


#Before Transformation : Raw variable - First_device_type
unique(Test_Users$first_device_type)
table(Test_Users$first_device_type)
first_device_type <- table(Test_Users$first_device_type)
b <- barplot(first_device_type, main="Device Type Distribution", xlab="First_device_type", col=c("darkblue"), legend = rownames(first_device_type),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - First_device_type_rev
#     a.Merge phone and tablet to single device type
#     b.Merge unknown devices to 'Other' category
Test_Users$first_device_type_rev <- ifelse(Test_Users$first_device_type == "Android Phone","Android Phone/Tab", Test_Users$first_device_type)
Test_Users$first_device_type_rev <- ifelse(Test_Users$first_device_type_rev == "Android Tablet","Android Phone/Tab", Test_Users$first_device_type_rev)
Test_Users$first_device_type_rev <- ifelse(Test_Users$first_device_type_rev == "iPad","iPhone/iPad", Test_Users$first_device_type_rev)
Test_Users$first_device_type_rev <- ifelse(Test_Users$first_device_type_rev == "iPhone","iPhone/iPad", Test_Users$first_device_type_rev)
Test_Users$first_device_type_rev <- ifelse(Test_Users$first_device_type_rev == "SmartPhone (Other)","Others", Test_Users$first_device_type_rev)
Test_Users$first_device_type_rev <- ifelse(Test_Users$first_device_type_rev == "Desktop (Other)","Others", Test_Users$first_device_type_rev)
Test_Users$first_device_type_rev <- ifelse(Test_Users$first_device_type_rev == "Other/Unknown","Others", Test_Users$first_device_type_rev)
unique(Test_Users$first_device_type_rev)
first_device_type_rev <- table(Test_Users$first_device_type_rev)
b <- barplot(first_device_type_rev, main="Device Type Distribution", xlab="First_device_type_revised", col=c("darkblue"), legend = rownames(first_device_type_rev),beside=TRUE, horiz=FALSE)


#Before Transformation : Raw variable - Language
unique(Test_Users$language)
table(Test_Users$language)
language <- table(Test_Users$language)
b <- barplot(language, main="Language Distribution", xlab="Language", col=c("darkblue"), legend = rownames(language),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - Language_rev
Test_Users$language_rev <- Test_Users$language 
Test_Users$language_rev[which(Test_Users$language_rev == "-unknown-")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "ca")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "cs")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "da")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "el")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "fi")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "hu")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "id")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "it")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "ja")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "nl")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "no")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "pl")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "pt")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "sv")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "th")] ="Others"
Test_Users$language_rev[which(Test_Users$language_rev == "tr")] ="Others"

unique(Test_Users$language_rev)
language_rev <- table(Test_Users$language_rev)
b <- barplot(language_rev, main="Language Distribution", xlab="Language_revised", col=c("darkblue"),beside=TRUE, horiz=FALSE)


#Before Transformation : Raw variable - Affiliate_provider
unique(Test_Users$affiliate_provider)
affiliate_provider <- table(Test_Users$affiliate_provider)
b <- barplot(affiliate_provider, main="Affiliate Provider", xlab="affiliate_provider", col=c("darkblue"), legend = rownames(affiliate_provider),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - affiliate_provider_rev
Test_Users$affiliate_provider_rev <- Test_Users$affiliate_provider
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider == "baidu")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "craigslist")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "daum")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "email-marketing")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "facebook-open-graph")] ="facebook"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "gsp")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "meetup")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "naver")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "other")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "padmapper")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "vast")] ="Others"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "yahoo")] ="yahoo/bing"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "bing")] ="yahoo/bing"
Test_Users$affiliate_provider_rev[which(Test_Users$affiliate_provider_rev == "yandex")] ="Others"
unique(Test_Users$affiliate_provider_rev)
affiliate_provider_rev <- table(Test_Users$affiliate_provider_rev)
b <- barplot(affiliate_provider_rev, main="Affiliate Provider", xlab="affiliate_provider_revised", col=c("darkblue"), legend = rownames(affiliate_provider_rev),beside=FALSE, horiz=FALSE)


#Before Transformation : Outlier treatment for age
quantile(Test_Users$age, c(.01,.10,.5,.95,.98),na.rm=TRUE)
# 1% 10% 50% 95% 98% 
# 19  23  31  60  69 
# Age variable ranges from 19 - 69 years
boxplot(Test_Users$age)

#After Transformation : Transformed variable - age_rev
Test_Users$age_rev <- ifelse(Test_Users$age < 19,19, Test_Users$age)
Test_Users$age_rev <- ifelse(Test_Users$age_rev > 70,70, Test_Users$age_rev)
Test_Users$age_rev[which(is.na(Test_Users$age_rev))] = -1
summary(Test_Users$age_rev)
boxplot(Test_Users$age_rev)
#Comments : Post outlier treatment, age ranges from 19 to 69 years. All missing values have been replaced by -1


#---------------------
### FILE : Train_Users
#---------------------
## Analyse categorical variables and perform cleaning / transformation where required


#Before Transformation : Raw variable - Gender
unique(Train_Users$gender)
gender <- table(Train_Users$gender)
b <- barplot(gender, main="Gender Distribution", xlab="Gender", col=c("darkblue"), legend = rownames(gender),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - Gender_rev
Train_Users$gender_rev <- Train_Users$gender
Train_Users$gender_rev[which(Train_Users$gender_rev == "OTHER")] ="N/A"
Train_Users$gender_rev[which(Train_Users$gender_rev == "-unknown-")] ="N/A"
gender_rev <- table(Train_Users$gender_rev)
b <- barplot(gender_rev, main="Gender Distribution", xlab="Gender_Revised", col=c("darkblue"), legend = rownames(gender_rev),beside=TRUE, horiz=FALSE)


#Before Transformation : Raw variable - First_device_type
unique(Train_Users$first_device_type)
table(Train_Users$first_device_type)
first_device_type <- table(Train_Users$first_device_type)
b <- barplot(first_device_type, main="Device Type Distribution", xlab="First_device_type", col=c("darkblue"), legend = rownames(first_device_type),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - First_device_type_rev
Train_Users$first_device_type_rev <- ifelse(Train_Users$first_device_type == "Android Phone","Android Phone/Tab", Test_Users$first_device_type)
Train_Users$first_device_type_rev <- ifelse(Train_Users$first_device_type_rev == "Android Tablet","Android Phone/Tab", Test_Users$first_device_type_rev)
Train_Users$first_device_type_rev <- ifelse(Train_Users$first_device_type_rev == "iPad","iPhone/iPad", Test_Users$first_device_type_rev)
Train_Users$first_device_type_rev <- ifelse(Train_Users$first_device_type_rev == "iPhone","iPhone/iPad", Test_Users$first_device_type_rev)
Train_Users$first_device_type_rev <- ifelse(Train_Users$first_device_type_rev == "SmartPhone (Other)","Others", Test_Users$first_device_type_rev)
Train_Users$first_device_type_rev <- ifelse(Train_Users$first_device_type_rev == "Desktop (Other)","Others", Test_Users$first_device_type_rev)
Train_Users$first_device_type_rev <- ifelse(Train_Users$first_device_type_rev == "Other/Unknown","Others", Test_Users$first_device_type_rev)
unique(Train_Users$first_device_type_rev)
first_device_type_rev <- table(Train_Users$first_device_type_rev)
b <- barplot(first_device_type_rev, main="Device Type Distribution", xlab="First_device_type_revised", col=c("darkblue"), legend = rownames(first_device_type_rev),beside=TRUE, horiz=FALSE)


#Before Transformation : Raw variable - Language
unique(Train_Users$language)
table(Train_Users$language)
language <- table(Train_Users$language)
b <- barplot(language, main="Language Distribution", xlab="Language", col=c("darkblue"), legend = rownames(language),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - Language_rev
Train_Users$language_rev <- Train_Users$language 
Train_Users$language_rev[which(Train_Users$language_rev == "-unknown-")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "ca")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "cs")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "da")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "el")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "fi")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "hu")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "id")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "it")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "ja")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "nl")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "no")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "pl")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "pt")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "sv")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "th")] ="Others"
Train_Users$language_rev[which(Train_Users$language_rev == "tr")] ="Others"

unique(Train_Users$language_rev)
language_rev <- table(Train_Users$language_rev)
b <- barplot(language_rev, main="Language Distribution", xlab="Language_revised", col=c("darkblue"),beside=TRUE, horiz=FALSE)


#Before Transformation : Raw variable - Affiliate_provider
unique(Train_Users$affiliate_provider)
affiliate_provider <- table(Train_Users$affiliate_provider)
b <- barplot(affiliate_provider, main="Affiliate Provider", xlab="affiliate_provider", col=c("darkblue"),beside=TRUE, horiz=FALSE)

#After Transformation : Transformed variable - affiliate_provider_rev
Train_Users$affiliate_provider_rev <- Train_Users$affiliate_provider
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider == "baidu")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "craigslist")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "daum")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "email-marketing")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "facebook-open-graph")] ="facebook"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "gsp")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "meetup")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "naver")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "other")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "padmapper")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "vast")] ="Others"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "yahoo")] ="yahoo/bing"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "bing")] ="yahoo/bing"
Train_Users$affiliate_provider_rev[which(Train_Users$affiliate_provider_rev == "yandex")] ="Others"
unique(Train_Users$affiliate_provider_rev)
affiliate_provider_rev <- table(Train_Users$affiliate_provider_rev)
b <- barplot(affiliate_provider_rev, main="Affiliate Provider", xlab="affiliate_provider_revised", col=c("darkblue"), legend = rownames(affiliate_provider_rev),beside=FALSE, horiz=FALSE)


#Before Transformation : Outlier treatment for age
quantile(Train_Users$age, c(.01,.10,.50,.95,.98),na.rm=TRUE)
# 1% 10% 50% 95% 98% 
# 19  25  34  64  93 
# Age variable ranges from 19 - 93 years
boxplot(Train_Users$age)

#After Transformation : Transformed variable - age_rev
Train_Users$age_rev <- Train_Users$age
Train_Users$age_rev <- ifelse(Train_Users$age_rev < 19,19, Train_Users$age_rev)
Train_Users$age_rev <- ifelse(Train_Users$age_rev >= 90,90, Train_Users$age_rev)
Train_Users$age_rev[which(is.na(Train_Users$age_rev))] = -1
summary(Train_Users$age_rev)
boxplot(Train_Users$age_rev)
#Comments : Post outlier treatment, age ranges from 19 to 90 years. All missing values have been replaced by -1


#---------------------
##FILE : Sessions
#---------------------

sessions <- unique(Sessions$user_id)
#   135484 unique users
summary(Sessions)
#   secs_elapsed variable contains 136031 missing values and some outliers

#Raw variable - action
unique(Sessions$action)
#Comments : Too many posibilities for action variable. Too much noise. Can be dropped from analysis

#Before Transformation : Raw variable - action_type
unique(Sessions$action_type)
action_type <- table(Sessions$action_type)
b <- barplot(action_type, main="Action Type", xlab="action_type", col=c("darkblue"),beside=TRUE, horiz=FALSE)
#Comments : 11 different ways of describing action type. Possible dependent variable in booking request & booking response

#After Transformation : Transformed variable - action_type_rev
Sessions$action_type_rev <- Sessions$action_type 
Sessions$action_type_rev[which(Sessions$action_type_rev == "booking_response")] ="booking"
Sessions$action_type_rev[which(Sessions$action_type_rev == "booking_request")] ="booking"
Sessions$action_type_rev[which(Sessions$action_type_rev == "-unknown-")] ="others"
barplot(table(Sessions$action_type_rev))


#Before Transformation : Raw variable - device_type
unique(Sessions$device_type)
device_type <- table(Sessions$device_type)
b <- barplot(device_type, main="Device type ", xlab="device_type", col=c("darkblue"),beside=TRUE, horiz=FALSE)
#Comments : 14 different options of entering device type. Possible independent variable.

#After Transformation : Transformed variable - device_type_rev
#                       Change device type into three:mobile = 3 desktop = 4 and unknown=5
Sessions$device_type_rev <- ifelse(Sessions$device_type == "Android Phone","Android Phone/Tab", Sessions$device_type)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "Android App Unknown Phone/Tablet","Android Phone/Tab", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "iPad","iPhone/iPad", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "iPad Tablet","iPhone/iPad", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "-unknown-","Others", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "Linux Desktop","Others", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "Tablet","Others", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "Chromebook","Others", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "Blackberry","Others", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "iPodtouch","Others", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "Windows Phone","Others", Sessions$device_type_rev)
Sessions$device_type_rev <- ifelse(Sessions$device_type_rev == "Opera Phone","Others", Sessions$device_type_rev)
unique(Sessions$device_type_rev)
device_type_rev <- table(Sessions$device_type_rev)
b <- barplot(device_type_rev, main="Device Type Distribution", xlab="device_type_rev", col=c("darkblue"), legend = rownames(first_device_type_rev),beside=TRUE, horiz=FALSE)


#Before Transformation : Raw variable - action_detail
unique(Sessions$action_detail)
action_detail <- table(Sessions$action_detail)
b <- barplot(action_detail, main="Action Detail", xlab="action_detail", col=c("darkblue"),beside=TRUE, horiz=FALSE)
#Comments : Too many posibilities  for action detail. Too much noise
#           Action detail column containing booking related information can be used to create dependent variable 

#After Transformation : Transformed variable - action_detail_rev
Sessions$action_detail_rev <- Sessions$action_detail
Sessions$action_detail_rev[is.na(Sessions$action_detail_rev)] <- "other"
Sessions$action_detail_rev[Sessions$action_detail_rev == "book_it"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "instant_book"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "complete_booking"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "apply_coupon_click"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "reservations"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "apply_coupon_click_success"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "modify_reservations"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "account_payment_methods"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "create_payment_instrument"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "set_default_payment_instrument"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "booking"] <- "booking"
Sessions$action_detail_rev[Sessions$action_detail_rev == "delete_payment_instrument"] <- "delete_payment_instrument"

unique(Sessions$action_detail_rev)
action_detail_rev <- table(Sessions$action_detail_rev)
b <- barplot(action_detail_rev, main="Action Detail Revised", xlab="action_detail_revised", col=c("darkblue"),beside=TRUE, horiz=FALSE)


#Airbnb users group that book
booking_session <- filter(Sessions, Sessions$action_detail_rev == "booking")
#Comment : 54652 records found


#Group booking_session by user_id and return avg time elapsed
plot(table(booking_session$device_type,booking_session$action_detail_rev))
glimpse(booking_session)
summary(booking_session)
table(booking_session$action_detail_rev,booking_session$device_type)
barplot(table(booking_session$action_detail_rev,booking_session$device_type),xlab = "Device Type")


#Group booking_session by user_id and return avg time elapsed
booking_id <- booking_session %>%
  select(user_id, action_detail_rev, device_type, secs_elapsed) %>%
  group_by(user_id) %>%
  summarise(secs_elapsed = mean(na.omit(secs_elapsed)))
View(booking_id)
summary(booking_id$secs_elapsed)
quantile(booking_id$secs_elapsed, c(.01,.10,.95,.98),na.rm=TRUE)
#Comment : On an average, ~28000 secs elapse in any given active session and it ranges from as low as one second to 237322 seconds

#Outlier treatment for secs_elapsed
boxplot(booking_id$secs_elapsed)
booking_id$secs_elapsed_rev <- booking_id$secs_elapsed
booking_id$secs_elapsed_rev <- ifelse(booking_id$secs_elapsed_rev  < 1,1, booking_id$secs_elapsed_rev )
booking_id$secs_elapsed_rev <- ifelse(booking_id$secs_elapsed_rev  > 237322,237322,booking_id$secs_elapsed_rev )
booking_id$secs_elapsed_rev[which(is.na(booking_id$secs_elapsed_rev))]=-1
summary(booking_id$secs_elapsed_rev )
boxplot(booking_id$secs_elapsed_rev )


#Group booking_session by device type and return avg time elapsed
booking_de <- booking_session %>%
  select(user_id, action_detail_rev, device_type_rev, secs_elapsed) %>%
  group_by(device_type_rev) %>%
  summarise(secs_elapsed = mean(na.omit(secs_elapsed)))
View(booking_de)
#Comment : Ipad/Iphone and Android are the top two devices that are used for booking

#######################################################################################################################
#                                                                                                                     #
#       5.Issues that are not worth fixing (Identifies and justifies variable/values that were not fixed)             # 
#                                                                                                                     #
#######################################################################################################################

summary(Test_Users)
#   - Post analysing each field in the given dataset, the following variables were identified to be irrelevant 
#     for the given analysis : timestamp_first_active, signup_flow, date_first_booking
#   - Variables signup_method, first_affiliate_tracked,signup_app have limited category values with no typos
#     Hence, it did not require any further data cleaning procedure

summary(Train_Users)
#   - Post analysing each field in the given dataset, the following variables were identified to be irrelevant 
#     for the given analysis : timestamp_first_active, signup_flow, date_first_booking
#   - Variables signup_method, first_affiliate_tracked,signup_app have limited category values with no typos
#     Hence, it did not require any further data cleaning procedure

summary(Sessions)
#   - Variable action has been excluded from further data cleaning given its 359 different category values and 
#     we plan to use action_type and action_detail variables for further analysis
#   - Data exclusion : Records in the sessions dataset for user_ids that are not present in either Train_Users or Test_Users dataset have 
#     have been excluded from further analysis
#


#######################################################################################################################
#                                                                                                                     #
#                                          Data Prepartion for modelling                                              #
#                               The dataset(combin_all) would serve as input for creating model base                 #
#                                                                                                                     #
#######################################################################################################################

#Extracting unique user ids from both train and test data
train_id <- unique(Train_Users$id)
test_id <- unique(Test_Users$id)

train_id <- as.data.frame(train_id)
test_id <- as.data.frame(test_id)

names(train_id) <- c("id")
names(test_id) <- c("id")

#Merging the two datasets of unique user ids
all_ids <- rbind(train_id, test_id) 
#275547 records

#Duplicacy check
dedup_all_ids <- all_ids[!duplicated(all_ids), ]
#275547 records - no duplicates found

#Renaming user_id variable to id in Sessions
names(Sessions) = c("id","action","action_type","action_detail","device_type","time_secs_elapsed","action_type_rev","device_type_rev","action_detail_rev")

combin_all <- inner_join(all_ids, Sessions, by = c("id"))
#Comments : 10567737 useful records extracted from sessions dataset
#           34496 records removed from analysis since they do not match with the user ids present in either train and test datasets
