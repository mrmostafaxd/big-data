# 1- cleaning the workspace and setting the working directory
rm(list=ls())
setwd("D:/College/SPRING 2023/CMPN451/labs/Lab 1 - Introduction to R/Requirement_ TITANIC/")
list.files()
cat("\014")  # clear console (CTRL+L)
try(dev.off(dev.list()["RStudioGD"]), silent=TRUE) # clear plots and suppress errors

# 2- Import the dataset titanic.csv into a data frame.
titanic = read.csv("titanic.csv")

# 3-a Show the dimensions of the data frame
dim(titanic)

# 3-b Show the structure of the data frame
str(titanic)

# 3-c Get more insight into data by exploring the first and the last TEN rows in the dataset
head(titanic, 10)
tail(titanic, 10)

# 3-d Show summary of all variables in the data frame.
summary(titanic)

# 4-a Show a summary for the variable age only
summary(titanic$Age)

# 4-b What are the first and third quartile values for this variable? What do these values mean?

# Q1 = 20.12 and Q3 = 38.00
# The first quartile (Q1) is the value that separates the lower 25% of the data from the upper 75%, and the third quartile (Q3) is the value that separates the lower 75% of the data from the upper 25%.
# this shows a limited range of the age of the passengers between 20.12 and 38 as the median is 28 (low spread)

# 4-c Are there any missing values in the variable age?
anyNA(titanic$Age) 
# yes there are NA and it that info is available in 3-d (number of NAs)
# anyNA output 1 logical vector element denoting if there are any missing value or not, 
# is.na output a logical vector that tell for each element whether it is NA or not
# anyNA  implements any(is.na(x)) in a possibly faster way and the question just wanted
# to investigate if there are NA values or not in the Age values, so anyNA is better

# 4-d What is the type of the variable embarked? Show the levels of this variable. Is that what you were expecting?
class(titanic$Embarked)
# Embarked is character (also shown in str(titanic))
levels(factor(titanic$Embarked))
# 4 levels were displated (""  "C" "Q" "S") but I expected 3 ("C" "Q" "S")
anyNA(titanic$Embarked)
# FALSE, there are no NA values in Embarked
"" %in% titanic$Embarked 
# TRUE, there are empty string values in Embarked\

# 4-e Can you conclude what’s needed at this step in the data analysis cycle?
# we performed some exploratory data analysis on the Titanic dataset
# by exploring the data and generating insights or hypotheses based on what we find.
# used dim to get the number of passengers and the number of variables
# used std() to get a feel for the dataset variables and values
# Use summary statistics to understand the distribution of the dataset and the "Age" variable and the "embarked" and identify any outliers or missing values.

# 5-a Remove the rows containing <NA> in the age variable from the data frame
titanic <-titanic[!is.na(titanic$Age),]

# 5-b Remove the rows containing any unexpected value in the embarked variable from the dataset.
titanic <- titanic[titanic$Embarked != "",]

# 5-c Now, check that no NA values exist in the age variable. Also, factor the embarked variable and display its levels. Is that what you are expecting?
anyNA(titanic$Age)
# no NA values in age
levels(factor(titanic$Embarked))
# the levels are what are expected

# 5-d Some variables are not very interesting and provide no real indicative value. Remove columns Cabin and Ticket from the dataset.
names(titanic)
cols_to_remove <- c(which(names(titanic) == "Ticket"), which(names(titanic) == "Cabin"))
titanic <- titanic[,-cols_to_remove]
names(titanic)

# 6-a Show the number of males and females aboard the Titanic
table(titanic$Gender)

# 6-b Plot a pie chart showing the number of males and females aboard the Titanic. (Hint: use pie() function)
pie(as.integer(table(titanic$Gender)), names(table(titanic$Gender)), main="Males VS Females initial")

# 6-c Indicate males with a blue color and females with a red color in the above plot. (Hint: There is a color parameter in any plot function)
pie(as.integer(table(titanic$Gender)), names(table(titanic$Gender)), col=c("red","blue"), main="Males VS Females initial (COLORED)")

# 6-c Show the number of people who survived and didn’t survive from each gender.
survive_table <- table(titanic$Gender, titanic$Survived)
colnames(survive_table) <- c("didnt survive", "survived")
survive_table


# 6-d Plot a pie chart showing the number of males and females who survived only.
as.matrix(survive_table)[,2]
names(as.matrix(survive_table)[,2])
pie(as.matrix(survive_table)[,2], names(as.matrix(survive_table)[,2]), col=c("red","blue"), main="Male VS Females Survived (COLORED)")

# 6-e What do you conclude from that?
# From the analysis, we can see that there were more males (453) than females (259) 
# aboard the Titanic. However, the survival rate for females (74.2%) was much higher 
# than that of males (18.9%). In terms of absolute numbers, 195 females and 93 males 
# survived. This could be due to the "women and children first" policy that was 
# followed during the evacuation of the Titanic. Overall, the data suggests that
# gender played a significant role in determining the survival rate during 
# the Titanic disaster.

# 6-f Show the relationship between social class and survival i.e. show how many people survived and how many people didn’t survive from each class
survived_class <- table(titanic$Survived,titanic$Pclass)
survived_class
colnames(survived_class) <- c("upper", "middle", "lower")
rownames(survived_class) <- c("didn't survive", "survived")
survived_class

# 6-g Plot this relationship as a stacked bar plot. (Hint: use barplot() function)
barplot(survived_class, legend.text = TRUE, main="Passenger Survival VS Passenger Class")

# 6-h Indicate survived passengers with a blue color and un-survived passengers with a red color in the above plot.
barplot(survived_class, legend.text = TRUE, col = c("red", "blue"), main="Passenger Survival VS Passenger Class (COLORED)")

# 6-i What do you conclude from that?
# the passengers in the upper class had a higher survival rate compared to 
# those in the middle and lower classes. The lower class had the lowest survival 
# rate, with only 23% of passengers surviving. so it seems that social class 
# had a significant impact on survival chances during the Titanic disaster.

# 6-j Plot a box and whiskers plot for the variable age (Hint: use boxplot() function)
summary(titanic$Age)
boxplot(titanic$Age, horizontal = TRUE, main = "Box and Whiskers Plot of Age", ylab = "Age")

# 6-k What does this plot mean?
# Q1 = 20 Q2 = 28 Q3 = 38, means most of the passengers age lies 
# between 28 and 38, and there are few outliers above 65 (above the plot)
# (1.5 max interquartile range IQR)

# 6-l Plot a density distribution for the variable age
plot(density(titanic$Age), main="Age Density Distribution")

# 7 Remove all columns but passenger name and whether they survived or not. Export the new dataset to a file named “titanic_preprocessed.csv”
titanic <- titanic[,c("Name","Survived")]
write.csv(titanic, "titanic_preprocessed.csv", row.names=FALSE)
