
library(ggplot2)
library(dplyr)
library(tidyverse)
library(readr)
library(magrittr)
library(psych)
library(skimr)
library(naniar)
library(corrplot)
library(plyr)
library(caret)
library(pROC)


# Import the dataset
hotels <- read.csv("/Users/summer/Downloads/hotel_bookings.csv")

# Find out the dimensions of the dataset and datatype of each column
str(hotels)
# This dataset consists of 119390 rows and 32 columns.

# First 10 rows of the dataset
head(hotels,10)

# Convert the character columns to factors
hotels <- hotels%>%
  mutate(is_canceled=as.factor(is_canceled))
hotels[sapply(hotels, is.character)] <- 
  lapply(hotels[sapply(hotels, is.character)], as.factor)

str(hotels)

# Get the feature statistics of dataset
skim(hotels)

# There are 14 categorical variables, 17 numerical variables and 1 date type 
# column. Also, there are 4 missing values in the children column.

# Replace the missing values and undefined factors in our data set

# As there are 4 missing values in the children column, we can replace the 
# missing values from the corresponding babies column to get total children 
# count.
hotels$children <- ifelse(is.na(hotels$children), hotels$babies, 
                          hotels$children)

summary(hotels)



# Check again if there are any missing values in our data set
vis_miss(hotels, warn_large_data = FALSE)
# There are no missing values in our data set.



# There is an undefined category in the meal column. This is same as the "SC"
# category which means no meal. So, we convert this class to SC.
hotels$meal <- replace(hotels$meal, hotels$meal=="Undefined", "SC")



# Convert meal column to factors
hotels$meal <- factor(hotels$meal)



# Check unique values in meal column
unique(hotels$meal)



# Replace the undefined factor with Online TA
hotels$market_segment <- replace(hotels$market_segment,
                                 hotels$market_segment=='Undefined',"Online TA")



# Convert the market segment to factors
hotels$market_segment <- factor(hotels$market_segment)



# Replace the undefined factor with TA/TO
hotels$distribution_channel <- replace(hotels$distribution_channel,
                                       hotels$distribution_channel=='Undefined',
                                       "TA/TO")



# Convert the market segment to factors
hotels$distribution_channel <- factor(hotels$distribution_channel)
str(hotels)



# Get the summary statistics to get the idea of distribution of data
describe(hotels)
summary(hotels)

# Exploratory Data Analysis

# Total number of booking for city and resort hotels
table(hotels$hotel)
# Total number of bookings in city hotel is 79330 and resort hotel is 40060.

# Bar plot to represent the same
ggplot(hotels,aes(x=factor(hotel))) +
  geom_bar(fill="dark blue",alpha=1) +
  scale_x_discrete("No. of bookings") +
  scale_y_continuous("Count")



# Bar plot to show percentage of cancellation and no-cancellations for each 
# hotel type
ggplot(data = hotels,aes(x = hotel,y = prop.table(stat(count)),
                         fill = factor(is_canceled),
                         label = scales::percent(prop.table(stat(count))))) +
  geom_bar(position = "dodge") +
  geom_text(stat = "count") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Cancellation Percent by Hotel Type",x = "Hotel Type",y = "Count")+
  scale_fill_manual(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Not Cancelled", "Cancelled"),
    values = c("0" = "dark green", "1"="black"))

# It can be be inferred that more percentage of city hotel bookings are likely 
# to get cancelled than resort hotels.



# Box plot to show percentage of cancellation and no-cancellations for each
# hotel type based on lead time
ggplot(data = hotels, aes(x = hotel,y = lead_time,
                          fill = factor(is_canceled))) +
  geom_boxplot(position = "dodge") +
  labs(title = "Cancellation By Hotel Type based on Lead time", x = "Hotel Type",
       y = "Lead time in days") +
  scale_fill_manual(name = "Booking Status", breaks = c("0", "1"),
                    labels = c("Not Cancelled", "Cancelled"), 
                    values = c("0" = "green", "1"="brown"))

# It can be inferred from this box plot that bookings which are made early are
# more likely to get cancelled.



# Bar plot to show percentage of cancellation and no-cancellations for each
# hotel type each year
ggplot(hotels,aes(x=factor(arrival_date_year),fill=factor(is_canceled))) +
  geom_bar(alpha=0.5) +
  scale_x_discrete("Arrival Year") +
  scale_y_continuous("Count")+
  scale_fill_manual(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Not Cancelled", "Cancelled"),
    values = c("0" = "purple", "1"="orange"))

# It can be inferred that there were highest number of hotel booking 
# cancellations in the year of 2016 and least in 2015.



# Bar plot to show the 
ggplot(hotels,aes(x=factor(arrival_date_month),fill=factor(is_canceled))) +
  geom_bar(alpha=0.5) +
  geom_text(stat = "count", aes(label = ..count..), hjust = 0.2,size=3) +
  scale_x_discrete("Arrival Month") +
  scale_y_continuous("Count")+
  scale_fill_manual(
    name = "Booking Status",
    breaks = c("0", "1"),
    labels = c("Not Cancelled", "Cancelled"),
    values = c("0" = "pink", "1"="dark blue"))

# It can be inferred that there were highest number of hotel booking 
# cancellations in the month of August aggregated for three consecutive years.



# Number of cancellations and no-cancellations based on room type
ggplot(hotels,aes(x=factor(assigned_room_type),fill=factor(is_canceled))) +
  geom_bar() +
  labs(title = "Cancellation based on Room Type", x = "Room Type",
       y = "No. of Bookings",size=15) +
  scale_fill_manual(name = "Booking Status",
                    breaks = c("0", "1"),
                    labels = c("Not Cancelled", "Cancelled"),
                    values = c("0" = "green", "1"="blue"))

# It can be inferred that room type A received the highest number of booking
# cancellations.



# Number of cancellations and no-cancellations based on distribution type
ggplot(hotels, aes(x=factor(distribution_channel),fill=factor(is_canceled))) +
  geom_bar() +
  labs(title = "Cancellation based on Distribution Type", x = "Distribution Type",
       y = "No. of Bookings",size=15) +
  scale_fill_manual(name = "Booking Status", breaks = c("0", "1"),
                    labels = c("Not Cancelled", "Cancelled"),
                    values = c("0" = "dark red", "1"="yellow"))
# It can be inferred that bookings made by travel were canceled the most.

# Data Preparation and Preprocessing

# Remove the reservation status date variable
hotels1 <- hotels %>%
  select(-c(32))
str(hotels1)



# Convert all factors to numeric 
hotels1 <- hotels1 %>%
  mutate_if(is.factor, as.numeric)

str(hotels1)

# Plot the correlation matrix

a <- cor(hotels1)
a
corrplot(a, type="lower", method="circle")

# Principal Component Analysis

#select all numerical variables
num_data <- hotels %>%
  select_if(is.numeric)

#principal components analysis
pc <- prcomp(num_data, scale. = TRUE, center = TRUE)
pca <- predict(pc, num_data)
pca_data <- as.data.frame(pca)

#select components with high variance 
pca_data <- pca_data[, 1:3]

#extract categorical data from dataset
cat_data <- select_if(hotels, is.factor)
#comine the categorical data with principal components
final <- cbind(cat_data, pca_data)

corrplot(cor(pca_data), type="lower")



set.seed(100)
#split the dataset into training data and validation in 6:4 ratio
train.index <- sample(c(1:dim(final)[1]), dim(final)[1]*0.6)  
train.df <- final[train.index, ]
valid.df <- final[-train.index, ]
#remove the variable with redundant data or column with too many missing value
train.df <- train.df[-c(5,11,12,14,15)]
valid.df <- valid.df[-c(5,11,12,14,15)]



#Random Forest Model

#build random forest model
rf <- randomForest::randomForest(is_canceled~., train.df, ntree = 500)
#predict the class
predict_train<- predict(rf,train.df)

predict_train_prob<- predict(rf,train.df,type = "prob")
confusionMatrix(predict_train, train.df$is_canceled)
#get accuracy as 91%

# Logistic Regression Model

library(caret)
#build logistic regression model
logit.reg <- glm(is_canceled~., family = "binomial", data = train.df)
summary(logit.reg)
#predict the class
logit.reg.pred <- predict(logit.reg, valid.df,type = "response")

confusionMatrix(as.factor(ifelse(logit.reg.pred>0.5,1,0)),valid.df$is_canceled)
#Logistic regression model with features predictors has accuracy of 0.7672

# Naive Bayes Classifier Model

library(e1071)

## run naive bayes
cancel.nb <- naiveBayes(is_canceled~., data = train.df)
cancel.nb

## predict probabilities
pred.prob <- predict(cancel.nb, newdata = valid.df, type = "raw")
## predict class 
pred.class <- predict(cancel.nb, newdata = valid.df)

confusionMatrix(pred.class, valid.df$is_canceled)
#Naive Bayes model with features predictors has accuracy of 0.7164

# Decision Tree Model

#package for decision tree
library(rpart)
library(rpart.plot)
library(caret)

# use rpart() to run a classification tree.
# define rpart.control() in rpart() to determine the depth of the tree.
class.tree <- rpart(is_canceled ~ ., data = train.df, 
                    control = rpart.control(cp=0.03), method = "class")
## plot tree
# use rpart.plot() to plot the tree, get tree depth of 3
rpart.plot(class.tree)

# use printcp() to print the table. 
printcp(class.tree)

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class 
class.tree.pred <- predict(class.tree,valid.df,type = "class")

# generate confusion matrix for validation data
#Accuracy = 0.75
confusionMatrix(class.tree.pred, valid.df$is_canceled)

set.seed(100)
#get classificatino tree with depth of 3
class.tree1<- rpart(is_canceled ~ ., data = train.df, 
                    control = rpart.control(cp=0,maxdepth = 3), method = "class")
rpart.plot(class.tree1,type = 5)

printcp(class.tree1)
#predict the class
class.tree.pred1 <- predict(class.tree1,valid.df,type = "class")
confusionMatrix(class.tree.pred1, valid.df$is_canceled)

#Decision tree model with accuracy of 0.7668, still getting lower accuracy than 
#logistic regression model

# prune by lower cp
pruned.ct <- prune(class.tree1, 
                   cp = class.tree1$cptable[which.min(class.tree1$cptable[,"xerror"]),"CP"])
#get the length 
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])
#plot classification tree
rpart.plot(pruned.ct,type=5)
printcp(pruned.ct)
#prunced tree
class.tree.pred2 <- predict(pruned.ct,valid.df,type = "class")
class.tree.pred_prob <- predict(pruned.ct,valid.df)

confusionMatrix(class.tree.pred2, valid.df$is_canceled)

#Still get the same accuracy after pruned the decision tree



#ROC plots for performance comparison


#get roc data of random forest model
rf_roc <- roc(valid.df$is_canceled,predict_train_prob[1:47756,2],auc=TRUE)
#random forest roc plot
plot(rf_roc,print.auc=TRUE,print.auc.y=.1,col="blue")

#roc data of logistic regression model
logistic_roc <- roc(valid.df$is_canceled,logit.reg.pred,auc=TRUE)
#logistic regression roc plot
plot(logistic_roc,print.auc=TRUE,print.auc.y=.2,col="black",add=TRUE)

#roc data of naive bayes model
naive_roc <- roc(valid.df$is_canceled,pred.prob[,2],auc=TRUE)
#naive bayes roc plot
plot(naive_roc,print.auc=TRUE,print.auc.y=.3,col="green",add=TRUE)

#roc data of decision tree model
tree_roc <- roc(valid.df$is_canceled,class.tree.pred_prob[,2],auc=TRUE)
#decision tree roc plot
plot(tree_roc,print.auc=TRUE,print.auc.y=.4,col="red",add=TRUE)


#add legend and label to the plot 
legend("right", legend = c("Random Forest", "Logistic Regression", "Naive Bayes", "Decision Tree"), col = c("blue", "black", "green", "red"),lwd =1,horiz=FALSE)


