# 1- cleaning the workspace and setting the working directory
rm(list=ls())
setwd("D:/College/SPRING 2023/CMPN451/labs/Lab 7 - Association Rules/")
list.files()
cat("\014")  # clear console (CTRL+L)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) # clear plots and suppress errors

# 2- Load the libraries arules and arulesViz
library("arules")
library("arulesViz")
# 3- Load the transactions in the file AssociationRules.csv using the function read.transactions.
df <- read.transactions("AssociationRules.csv",header = FALSE)

# 4- Display the transactions in a readable format using the function inspect
inspect(df[1:100])

# 5- What are the most frequent two items in the dataset? What are their frequencies?
summary(df) # item 13
itemFrequency(df)

# 6- Plot the 5 most frequent items of the transactions using the function itemFrequencyPlot
itemFrequencyPlot(df,topN=5)

# 7- Generate the association rules from the transactions using the apriori algorithm. Set
# the minimum support = 0.01, minimum confidence = 0.5, minimum cardinality 
# (number of items in the rule) = 2.

rules <- apriori(df, parameter = list(supp = 0.01, conf = 0.5, maxlen = 2))
rules
inspect(rules)

# 8- Now, sort the generated rules by support. Search the function sort found in the
# arules package. Show only the first 6 rules
h<-sort(rules, by = "support")
h
inspect(h)

sup <- inspect(head(rules, n = 6, by = "support"))
# they have lift of approx 1, so they are not important

# 9- Sort the generated rules by confidence. Show only the first 6 rules.
conf <- inspect(head(rules, n = 6, by = "confidence"))

# 10- Sort the generated rules by lift. Show only the first 6 rules
lif <- inspect(head(rules, n = 6, by = "lift"))

# 11- . Plot the generated rules with support as x-axis, confidence as y-axis and lift asshading
plot(rules, measure = c("support", "confidence"), shading = "lift")
