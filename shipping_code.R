library(dplyr)
library(tidyverse)
library(ggplot2)
library (reshape2)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(xgboost)

path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


#Data Source: https://www.kaggle.com/datasets/prachi13/customer-analytics/data
shipping_data <- read.csv("Train.csv", stringsAsFactors = FALSE)

#data check
head(shipping_data)
tail(shipping_data)
colnames(shipping_data)
str(shipping_data)
summary(shipping_data)
dim(shipping_data)

sum(is.na(shipping_data)) 

#removing ID column
shipping_data$ID <- NULL

#changing categories of data
shipping_data$Warehouse_block <- as.factor(shipping_data$Warehouse_block)
shipping_data$Mode_of_Shipment <- as.factor(shipping_data$Mode_of_Shipment)
shipping_data$Product_importance <- as.factor(shipping_data$Product_importance)
shipping_data$Gender <- as.factor(shipping_data$Gender)

#####
#------data visualizations-----#
#####
shipping_data_summary <- shipping_data %>%
  group_by(Warehouse_block) %>%
  summarise(count = n())

ggplot(shipping_data_summary, aes(x = Warehouse_block, y = count)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = count), vjust = 1.5, color = "black", size = 3.5) +
  labs(title = "Warehouse Block") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="darkgreen", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="green4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="green4", size=12, hjust = 0.5, face="bold")) 
  

ggplot(shipping_data, aes(x = Mode_of_Shipment)) +
  geom_bar(fill = "skyblue") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1.5, color = "black", size = 3.5) +
  labs(title = "Using different shipping methods") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold")) 


ggplot(shipping_data, aes(x = Product_importance, fill = Product_importance)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust =1.5, color = "black", size = 3.5) +
  labs(title = "Product Importance") +
  theme_minimal() +
theme( 
  plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
  axis.title.x = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
  axis.title.y = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
  legend.position = "none") 



ggplot(shipping_data, aes(x = Customer_rating)) +
  geom_bar(fill = "pink", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1.5, color = "black", size = 3.5) +
  labs(title = "Customer Rating") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="pink4", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="pink4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="pink4", size=12, hjust = 0.5, face="bold")) 



ggplot(shipping_data, aes(x = factor(Customer_care_calls))) +
  geom_bar(fill = "dodgerblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1.5, color = "black", size = 3.5) +
  labs(
    title = "Number of Customer Care Calls",
    x = "Number of Customer Care Calls",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold")
  )

unique(shipping_data$Customer_care_calls)

#Average Rating for Gender
ggplot(shipping_data, aes(x = Gender, y = Customer_rating)) +
  geom_boxplot() +
  labs(title = "Customer Rating & Gender") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold")) 

average_ratings <- aggregate(Customer_rating ~ Gender, shipping_data, mean)
print(average_ratings)

#heatmap for numerical variables
numerical_vars <- shipping_data[sapply(shipping_data, is.numeric)]
cor_matrix <- cor(numerical_vars, use = "complete.obs")

cor_matrix_melted <- melt(cor_matrix)
ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +  
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), name = "Correlation"
  ) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  
  theme_minimal() +
  labs(title = "Heatmap for Variables") +
  theme(plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title.x = element_blank(),     
        axis.title.y = element_blank())
   
#charts showing the relationships between variables
ggplot(shipping_data, aes(x = Warehouse_block, y = Cost_of_the_Product)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = 1.5) +
  labs(
    title = "Average Product Cost by Warehouse Block",
    x = "Warehouse Block",
    y = "Average Cost of the Product"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold")
  )

ggplot(shipping_data, aes(x = Warehouse_block, y = Customer_care_calls)) +
  stat_summary(fun = mean, geom = "bar", fill = "lightgreen", color = "black") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = 1.5) +
  labs(
    title = "Average Customer Care Calls by Warehouse Block",
    x = "Warehouse Block",
    y = "Average Customer Care Calls"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "green4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "green4", size = 12, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(color = "green4", size = 12, hjust = 0.5, face = "bold")
  )

ggplot(shipping_data, aes(x = Gender, y = Cost_of_the_Product)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue", color = "black") +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., 2)), vjust = 1.5) +
  labs(
    title = "Average Product Cost by Gender",
    x = "Gender",
    y = "Average Cost of the Product"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold")
  )

ggplot(shipping_data, aes(x = Mode_of_Shipment, y = Weight_in_gms)) +
  geom_boxplot(fill = "lightgoldenrod2", color = "black") +
  geom_jitter(color = "darkorange2", width = 0.2, alpha = 0.6) +
  stat_summary(fun = median, geom = "point", shape = 23, size = 3, fill = "firebrick4") +
  stat_summary(fun = median, geom = "text", aes(label = round(..y.., 2)), vjust = -1.5, color = "firebrick4") +
  labs(
    title = "Weight by Mode of Shipment",
    x = "Mode of Shipment",
    y = "Weight (in grams)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "orange4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "orange4", size = 12, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(color = "orange4", size = 12, hjust = 0.5, face = "bold")
  )

ggplot(shipping_data, aes(x = factor(Customer_rating), fill = Reached.on.Time_Y.N)) +
  geom_bar(position = "dodge", stat = "count") +
  labs(
    title = "Customer Ratings by On-Time Delivery",
    x = "Customer Rating",
    y = "Count",
    fill = "Reached on Time"
  ) +
  scale_fill_manual(
    values = c("Not On Time" = "tomato2", "On Time" = "seagreen3"),
    labels = c("Not On Time" = "No", "On Time" = "Yes")
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold")
  )

###############################
#------predictions models - delivery time-----#
###############################


# Converting column: Reached.on.Time_Y.N on factor
shipping_data$Reached.on.Time_Y.N <- factor(shipping_data$Reached.on.Time_Y.N, 
                                            levels = c(0, 1), 
                                            labels = c("Not On Time", "On Time"))

#train and test data set
set.seed(123)
trainIndex <- createDataPartition(shipping_data$Reached.on.Time_Y.N, p = 0.8, list = FALSE)
trainData <- shipping_data[trainIndex, ]
testData <- shipping_data[-trainIndex, ]  

table(testData$Reached.on.Time_Y.N)

#####-------logistic regression model--------######

model_delivery <- glm(Reached.on.Time_Y.N ~ ., data = trainData, family = binomial)

#predictions
predictions <- predict(model_delivery, testData, type = "response")
predicted_classes <- ifelse(predictions > 0.5, "On Time", "Not On Time")
predicted_classes <- factor(predicted_classes, levels = c("Not On Time", "On Time"))

#accuracy
confusion_matrix <- confusionMatrix(predicted_classes, testData$Reached.on.Time_Y.N)
accuracy <- confusion_matrix$overall['Accuracy']
print(paste("Accuracy:", round(accuracy, 2)))


#####-------random forest--------######
model_rf <- randomForest(Reached.on.Time_Y.N ~ ., data = trainData, ntree = 100, mtry = 3, importance = TRUE)

# prediction
predictions_rf <- predict(model_rf, testData)

# accuracy
confusion_matrix_rf <- confusionMatrix(predictions_rf, testData$Reached.on.Time_Y.N)
accuracy_rf <- confusion_matrix_rf$overall['Accuracy']
print(paste("Accuracy:", round(accuracy_rf, 2))) #better: 0.65

importance(model_rf) #the most important: weight, discount, cost, prior purchases
varImpPlot(model_rf)


#####-------random forest with hypermetric optimization--------######
#setting the grid of params to search
tuneGrid <- expand.grid(.mtry = c(2, 3, 4, 5))

#control setting for model training
control <- trainControl(method = "cv", number = 5)

#Model training
set.seed(123)
rf_model <- train(Reached.on.Time_Y.N ~ ., data = trainData, method = "rf", 
                  tuneGrid = tuneGrid, trControl = control)

#best params
print(rf_model$bestTune)

#prediction
predictions_rf <- predict(rf_model, testData)

#accuracy
confusion_matrix_rf <- confusionMatrix(predictions_rf, testData$Reached.on.Time_Y.N)
accuracy_rf <- confusion_matrix_rf$overall['Accuracy']
print(paste("Accuracy:", round(accuracy_rf, 2)))

#####-------decision tree--------######
tree_model <- rpart(Reached.on.Time_Y.N ~ ., data = trainData, method = "class")

#prediction
predictions_tree <- predict(tree_model, testData, type = "class")

#accuracy
confusion_matrix_tree <- confusionMatrix(predictions_tree, testData$Reached.on.Time_Y.N)
accuracy_tree <- confusion_matrix_tree$overall['Accuracy']
print(paste("Accuracy:", round(accuracy_tree, 2))) #better: 0.68

#viisualization
rpart.plot(tree_model, type = 3, extra = 101, fallen.leaves = TRUE, 
           box.palette = "RdYlGn", shadow.col = "gray", nn = TRUE)

###############################
#------predictions models - clients rating-----#
###############################

#####-------regression model--------######


model_customer <- lm(Customer_rating ~ ., data = trainData)
predictions_customer <- predict(model_customer, testData)
rmse_lm <- sqrt(mean((predictions_customer- testData$Customer_rating)^2))
print(paste("RMSE:", round(rmse_lm, 2)))



#####-------random forest--------######

model_rf_customer <- randomForest(Customer_rating ~ ., data = trainData, ntree = 100, mtry = 3, importance = TRUE)

predictions_rf_customer <- predict(model_rf_customer, testData)

rmse_rf <- sqrt(mean((predictions_rf_customer - testData$Customer_rating)^2))
print(paste("RMSE:", round(rmse_rf, 2)))

importance(model_rf_customer) #the most important: customer care calls, weight in gms, cost of the product
varImpPlot(model_rf_customer)



#####-------decision tree--------######
model_tree_customer <- rpart(Customer_rating ~ ., data = trainData, method = "anova")

predictions_tree_customer <- predict(model_tree_customer, testData)

rmse_tree_customer <- sqrt(mean((predictions_tree_customer - testData$Customer_rating)^2))

print(paste("RMSE:", round(rmse_tree_customer, 2)))




