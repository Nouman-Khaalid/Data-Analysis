# Installing the required packages

install.packages("tidyr")
install.packages("dplyr")
install.packages("stats")
install.packages("DT")
install.packages("tidytext")
install.packages("tidyverse")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("lubridate")
install.packages("scales")
install.packages("ggthemes")
install.packages("randomForest")
install.packages("mdsr")
install.packages("ggfortify")
install.packages("ROCR")
install.packages("pROC")
install.packages("caret")
install.packages("reshape2")
#Loading the libraries

library(reshape2)
library(ROCR)
library(pROC)
library(caret)
library(tidyr)
library(dplyr)
library(ggplot2)
library(lubridate)
library(scales)
library(ggthemes)
library(randomForest)
library(mdsr)
library(tidyverse)
library(tidytext)
library(DT)
library(ggfortify)

# Loading the dataset
Weather <- Weather_Training_Data
Weather

# 2: EXPLORING THE DATASET

# Displaying the dataset
str(Weather)

# to display few rows 
head(Weather)
view(Weather)

#  To see overview of the dataset along with the first few values of each variable 
glimpse(Weather)

# for the Summary statistics of our dataset
summary(Weather)

# Check the column names
colnames(Weather)


# 3: WRANGLING

#Filtering Rows: Select only the Bendigo's data.
Weather_w <- filter(Weather, Location == "Bendigo")
Weather_w

# Delete two columns
Weather_w <- subset(Weather_w, select = -c(Evaporation, Sunshine))
Weather_w

# Finding the missing values
missing_values <- sum(is.na(Weather_w))
missing_values

# Removing Missing Values
Weather_w <- na.omit(Weather_w)
View(Weather_w)


# 4: TIDY YOUR DATASET

# Re-nameing the column "WindGustDir" to a more comprehensive name

Weather_w <- rename(Weather_w, "WindGustDirection" = WindGustDir )
Weather_w


# 5: Choose a predictive algorithm to solve your problem

linerModel <- lm(Rainfall ~ Humidity9am, data = Weather_w)
summary(linerModel)

# scatter plot for linear regression
ggplot(Weather_w, aes(x = Humidity9am , y = Rainfall )) +
  geom_point() +
labs(x = "Humidity level", y = "Rainfall posibility", title = "Rainfall prediction")

ggplot(Weather_w, aes(x = Humidity9am , y = Rainfall )) +
  geom_point() +
  geom_line() +
labs(x = "Humidity level", y = "Rainfall posibility", title = "Rainfall prediction")

ggplot(Weather_w, aes(x = Humidity9am , y = Rainfall )) +
  geom_point() +
  geom_abline() +
  labs(x = "Humidity level", y = "Rainfall posibility", title = "Rainfall prediction")


predictions1 <- predict(linerModel, newdata = Weather_w)
predictions1

# Calculate the mean squared error for linear regression
mean_sqrd_error <- mean((Weather_w$Rainfall - predictions1)^2)
mean_sqrd_error


#logistic regression
logistic_model <- glm(RainTomorrow ~ MaxTemp, data = Weather_w, family = "binomial")
summary(logistic_model)


predictions2 <- predict(logistic_model, newdata = Weather_w)
predictions2

# Convert probabilities to class labels
predicted_classes <- ifelse(predictions2 > 0.5, 1, 0)

# Create the confusion matrix
confusion_matrix <- table(Weather_w$RainTomorrow, predicted_classes)

# Print the confusion matrix
confusion_matrix

# Finding the accuracy of logistic model
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
accuracy<- accuracy*100
accuracy


# scatter plot for logistic regression
#ggplot(Weather_w, aes(x = MaxTemp, y = RainTomorrow)) +
 # geom_point() +
  #geom_smooth(method = "glm", se = FALSE, color = "blue", method.args = list(family = "binomial")) +
  #geom_point(aes(y = predictions2), color = "red") +
  #labs(x = "Maximum temperature", y = "Rainfall possibility", title = "Logistic Regression Predictions") +
  #theme_minimal()






# 6: Visualize the predictions in multiple ways

#scatterplot
plot_scatter <- ggplot(Weather_w, aes(x= Humidity9am, y = Rainfall))+
  labs(x = "Humidity level", y = "Rainfall possibilty") +
  geom_point(size= 1, alpha= 0.5, color = "red") +
  geom_smooth(method = "lm", se = FALSE)
plot_scatter


#bar graph
#plot_bar <- ggplot(Weather_w, aes(x = Humidity9am, y = Rainfall)) + #, fill = categories
#geom_bar(stat= "identity", fill = "steelblue", color= "red") +
#theme(legend.position = "none")
#labs(x = "Humidity level", y = "Rainfall possibility", title = "Bar Graph") +
#theme_minimal()
#plot_bar

# Line plot
plot_line <- ggplot(data = Weather_w, aes(x = Humidity9am, y = Rainfall)) +
  geom_line(color = "cyan") +
  labs(x = "Humidity level", y = "Rainfall possibility", title = "line Graph") +
  theme_bw()
plot_line

#predicted values vs actual values


# Histogram 
#plot_histogram <- ggplot(data = Weather_w, aes(x = Humidity9am)) +
#geom_histogram(binwidth = 1) +
#labs(x = "Humidity level", y = "Rainfall possibility", title = "Histogram Graph") +
#theme_bw()
#plot_histogram

# Clustering with k-means
# We need to use numerical values, as K-means algorithm uses only numerical values
numeric_values<- Weather_w[ c("MinTemp", "MaxTemp", "Rainfall", "WindGustSpeed", "WindSpeed9am", "WindSpeed3pm", "Humidity9am", "Humidity3pm", "Pressure9am", "Pressure3pm", "Cloud9am", "Cloud3pm", "Temp9am", "Temp3pm")]

# Applying k-means for two clustering pairs
result<-kmeans(numeric_values,2)
result

# cluster plot for 2 pair
autoplot(result,numeric_values,frame=TRUE)

# Applying k-means for three clustering pairs
result<-kmeans(numeric_values,3)
result

# cluster plot for 3 pairs
autoplot(result,numeric_values,frame=TRUE)

# Applying k-means for four clustering pairs
result<-kmeans(numeric_values,5)
result

# cluster plot for 4 pairs
autoplot(result,numeric_values,frame=TRUE)


ggplot(Weather_w, aes(x = Humidity9am, y = Rainfall, color = factor(result$cluster))) +
  geom_point() +
  labs(x = "Humidity level", y = "Rainfall possibility", title = "K-means Clustering", resolution(12000)) +
  theme_minimal()



result$centers



view(Weather_w)


#box plot

comparison_df <- data.frame(Logistic_Regression = predicted_classes, KMeans_Cluster = result$cluster)

# Plotting a box plot to compare the results
comparison_melted <- melt(comparison_df)

boxplot_plot <- ggplot(comparison_melted, aes(x = variable, y = value)) +
  geom_boxplot(fill = "steelblue", color = "black") +
  labs(title = "Comparison of Logistic Regression and K-means Clustering",
       x = "Algorithm", y = "Result") +
  theme_minimal()
boxplot_plot
