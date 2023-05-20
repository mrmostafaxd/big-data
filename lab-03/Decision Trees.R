# 1- cleaning the workspace and setting the working directory
rm(list=ls())
setwd("d:/College/SPRING 2023/CMPN451/labs/Lab 5 - Predictive Analysis/Requirement/")
cat("\014")  # clear console (CTRL+L)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) # clear plots and suppress errors

#install.packages("rpart.plot")
library("rpart")
library("rpart.plot")
library("ROCR")

#Read the data
play_decision <- read.table("DTdata.csv",header=TRUE,sep=",")
play_decision
summary(play_decision)

#Build the tree to "fit" the model
fit <- rpart(Play ~ Outlook + Temperature + Humidity + Wind,
             method="class", 
             data=play_decision,
             control=rpart.control(minsplit=2, maxdepth = 3),
             parms=list(split='information'))
# split='information' : means split on "information gain" 
#plot the tree
rpart.plot(fit, type = 4, extra = 4)

summary(fit)
#######################################################################################
# Q1: what is the defult value for split?

# The default value for the split parameter in the rpart() function is "Gini" 
#    for classification trees and "deviance" for regression trees.



# Q2: what are the meanings of these control parameters?  
#          1- "minsplit=2"
#
#          2- "maxdepth=3" 
#
#          3- "minbucket=4" 
#
# Support your answers with graphs for different values of these parameters.

# minsplit: The minimum number of observations required in a node to be 
#    considered for splitting. The default value is 20.
# maxdepth: The maximum depth of the tree. The default value is 30.
# minbucket: The minimum number of observations required in a terminal node 
#    (i.e., a leaf) after a split. The default value is 7.
# Increasing minsplit or minbucket values can lead to smaller trees with fewer 
#    splits and higher bias but lower variance, while decreasing them can lead 
#    to larger trees with more splits and lower bias but higher variance.



#Q3: What will happen if only one of either minsplit or minbucket is specified
#    and not the other?

# If only one of either minsplit or minbucket is specified and not the other,
#    the rpart() function will use the default value for the parameter that is 
#    not specified. For example, if minsplit is specified as 5 and minbucket is 
#    not specified, the rpart() function will use the default value of minbucket 
#    (i.e., 7).

#Q4: What does 'type' and 'extra' parameters mean in the plot function?

# type
# The 'type' parameter in the rpart.plot() function specifies the type of label 
#   to be used for the tree nodes. It can take three values:

# "prob" displays the probabilities for each class at each node.
# "vector" displays the number of observations for each class at each node.
# "class" displays the predicted class for each node.


# extra
# 0: No extra information is added to the plot.
# 1: The percentage of observations in each node is added to the plot.
# 2: The variable names and values for each split are added to the plot.
# 4: Both 1 and 2 are added to the plot.


#Q5: Plot the tree with propabilities instead of number of observations in each node.
######################################################################################
 
#Predict if Play is possible for condition rainy, mild humidity, high temperature and no wind
newdata <- data.frame(Outlook="overcast",Temperature="mild",Humidity="high",Wind=FALSE)
newdata
predict(fit,newdata=newdata,type=c("class"))
# type can be class, prob or vector for classification trees.

######################################################################################
#Q6: What is the predicted class for this test case?

#Q7: State the sequence of tree node checks to reach this class (label).

## ================================= END ===================================== ##