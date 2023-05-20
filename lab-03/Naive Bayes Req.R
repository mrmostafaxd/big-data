# 1- cleaning the workspace and setting the working directory
rm(list=ls())
setwd("d:/College/SPRING 2023/CMPN451/labs/Lab 5 - Predictive Analysis/Requirement/")
cat("\014")  # clear console (CTRL+L)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) # clear plots and suppress errors

# Load required library for Naive Bayes Classifier
library("e1071")

# Import the dataset nbtrain.csv into a data frame
df = read.csv("nbtrain.csv")
class(df)

# Display the variables of the dataset
colnames(df)
str(df)

# Split the data into a training set containing the first 9000 rows and a test set containing the remaining rows
# The reason for splitting the data is to evaluate the model's performance on unseen data and avoid overfitting
train = df[1:9000,]
test = df[9001:10010,]

# Use the Naive Bayes classifier with Laplace smoothing to tackle the zero probability problem
# Laplace smoothing adds a value to all classes to make them closer to a uniform distribution

# income as target (dependent) and the rest as independent

# laplace to smooth probabilities and avoid zero probabilities
model <- naiveBayes(income ~., train, laplace=.01)

# Display the resulting model
model

# Predict the income class of the test data using the trained model
results <- predict(model, test)

# Install and load the required package for creating a confusion matrix
# A confusion matrix helps evaluate the accuracy of a classification model
# install.packages('caret')
library(caret)

# Convert income variable in the test set to a factor variable
test$income <- factor(test$income)


# Create a confusion matrix using predicted results and actual results
conf_mat = confusionMatrix(data = results, reference = test$income)

# Display the confusion matrix
tbl <- conf_mat$table
tbl

# Investigate the classification bias towards the first income class (10-50k)
# Evaluate the classification power across classes
trainY <- table(train$income)
c1 <- trainY/sum(trainY) * 100
c1

# The unbalanced training set leads to a bias towards the first class (10-50k)
# Display the overall accuracy of the classifier
conf_mat$overall["Accuracy"]

# The accuracy of nearly 80% is misleading as it is mainly driven by the first class (10-50k)
# Display the distribution of each class in the test set
testY <- table(test$income)
c1 <- testY/sum(testY) * 100
c1

# Precision and Recall can be seen to be very small (even zero) for class 2 and 3
conf_mat$byClass

# Calculate the misclassification rates for each income class
# Class 1: 10-50
tbl
m1 <- 1 - tbl[1,1] / sum(tbl[1:3, 1])
m1

# Class 2: 50-80
m2 <- 1 - tbl[2,2] / sum(tbl[1:3, 2])
m2

# Class 3: 80+
m3 <- 1 - tbl[3,3] / sum(tbl[1:3, 3])
m3
