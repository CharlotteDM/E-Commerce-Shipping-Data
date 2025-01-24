library(dplyr)
library(tidyverse)
library(ggplot2)
library (reshape2)
library(caret)
library(randomForest)
library(rpart)
library(rpart.plot)
library(xgboost)
library(coefplot)
library(pROC)
library(e1071)
library(smotefamily)
install.packages("Cubist")
library(Cubist)
library(Boruta)


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
#items in every warehouse block
shipping_data_summary <- shipping_data %>%
  group_by(Warehouse_block) %>%
  summarise(count = n())

plot_1 <- ggplot(shipping_data_summary, aes(x = Warehouse_block, y = count)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  geom_text(aes(label = count), vjust = 1.5, color = "black", size = 3.5) +
  labs(title = "Warehouse Block") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="darkgreen", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="green4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="green4", size=12, hjust = 0.5, face="bold")) 
  
#different shipping methods
plot_2 <- ggplot(shipping_data, aes(x = Mode_of_Shipment)) +
  geom_bar(fill = "skyblue", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1.5, color = "black", size = 3.5) +
  labs(title = "Using Different Shipping Methods", x = "Shipping Way") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold")) 

#Product importance
plot_3 <- ggplot(shipping_data, aes(x = Product_importance, fill = Product_importance)) +
  geom_bar(color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust =1.5, color = "black", size = 3.5) +
  labs(title = "Product Importance") +
  theme_minimal() +
theme( 
  plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
  axis.title.x = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
  axis.title.y = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
  legend.position = "none") 


#customer ratings
plot_4 <- ggplot(shipping_data, aes(x = Customer_rating)) +
  geom_bar(fill = "pink", color = "black") +
  geom_text(stat = "count", aes(label = after_stat(count)), vjust = 1.5, color = "black", size = 3.5) +
  labs(title = "Customer Rating") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="pink4", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="pink4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="pink4", size=12, hjust = 0.5, face="bold")) 


#number of Custiomer Care Calls
plot_5 <- ggplot(shipping_data, aes(x = factor(Customer_care_calls))) +
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
plot_6 <- ggplot(shipping_data, aes(x = Gender, y = Customer_rating)) +
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

heatmap <- ggplot(cor_matrix_melted, aes(x = Var1, y = Var2, fill = value)) +
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
plot_7 <- ggplot(shipping_data, aes(x = Warehouse_block, y = Cost_of_the_Product)) +
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

#Average Customer Care Calls by Warehouse Block
plot_8 <- ggplot(shipping_data, aes(x = Warehouse_block, y = Customer_care_calls)) +
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

#Average Product Cost by Gender
plot_9 <- ggplot(shipping_data, aes(x = Gender, y = Cost_of_the_Product)) +
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

#weight and Mode of Shipment
plot_10 <- ggplot(shipping_data, aes(x = Mode_of_Shipment, y = Weight_in_gms)) +
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



##Question1: What was Customer Rating? And was the product delivered on time?
table(shipping_data$Customer_rating, shipping_data$Reached.on.Time_Y.N)

shipping_data$Reached.on.Time_Y.N <- factor(
  shipping_data$Reached.on.Time_Y.N,
  levels = c(0, 1), 
  labels = c("Not On Time", "On Time")
)

plot_11 <- ggplot(shipping_data, aes(x = factor(Customer_rating), fill = Reached.on.Time_Y.N, group = Reached.on.Time_Y.N)) +
  geom_bar(position = "dodge", stat = "count", color = "black") +
  geom_text(stat = "count", aes(label = ..count..), position = position_dodge(width = 0.9), vjust = -0.5) +
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
    axis.title.y = element_text(color = "blue4", size = 12, hjust = 0.5, face = "bold"),
    legend.position = "top"
  )




  
  
 #Question2: Is Customer query is being answered?
unique(shipping_data$Customer_care_calls)  
unique(shipping_data$Reached.on.Time_Y.N)

mean_calls <- mean(shipping_data$Customer_care_calls)
mean_calls<- round(mean_calls, 0)

# Create a contingency table
customer_care_table <- table(shipping_data$Customer_care_calls <= mean_calls, 
                             shipping_data$Reached.on.Time_Y.N)
colnames(customer_care_table) <- c("Not On Time", "On Time")
rownames(customer_care_table) <- c("> Mean Calls", "≤ Mean Calls")

# Display the table
customer_care_table

  
  #Question3:If Product importance is high. having higest rating or being delivered on time?
high_importance <- subset(shipping_data, Product_importance == "high")
high_ratings <- table(high_importance$Customer_rating == max(shipping_data$Customer_rating))
high_ratings #186 of products have the highest rating 
percentage_high_rating <- 186 / (762 + 186) * 100
percentage_high_rating #19.62%

on_time <- table(high_importance$Reached.on.Time_Y.N)
on_time #332 of high importance products didn't reach on time, 616 - on time
percentage_on_time <- 616 / (332 + 616) * 100
percentage_on_time  #64.97%

###############################
#------predictions models - delivery time-----#
###############################


# Converting column: Reached.on.Time_Y.N on factor
#shipping_data$Reached.on.Time_Y.N <- factor(shipping_data$Reached.on.Time_Y.N, 
                                         #   levels = c(0, 1), 
                                          #  labels = c("Not On Time", "On Time"))
  
  levels(shipping_data$Reached.on.Time_Y.N)
  table(shipping_data$Reached.on.Time_Y.N)
  
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
sensitivity <- confusion_matrix$byClass['Sensitivity']
specificity <- confusion_matrix$byClass['Specificity']
print(paste("Accuracy:", round(accuracy, 2))) #0.64
print(paste("Sensitivity:", round(sensitivity, 2))) #0.58
print(paste("Specificity:", round(specificity, 2)))  #0.68

coefplot(model_delivery)

#ROC Curve
roc_curve <- roc(testData$Reached.on.Time_Y.N, predictions)
plot(roc_curve, main = "ROC Curve", col = "blue")
auc(roc_curve) #area under the curve (the probability that the model will assign a higher probability of a positive class to a positive case than to a negative case): 0.7158

#histogram for predicted probabilities
predicted_df <- data.frame(TrueLabel = testData$Reached.on.Time_Y.N, PredictedProbability = predictions)
ggplot(predicted_df, aes(x = PredictedProbability, fill = TrueLabel)) +
  geom_histogram(binwidth = 0.1, position = "dodge", alpha = 0.7) +
  labs(title = "Histogram of Predicted Probabilities", x = "Predicted Probability", y = "Frequency") +
  scale_fill_manual(values = c("Not On Time" = "coral1", "On Time" = "aquamarine1"))

#heatmap for predictive data
confusion_matrix_data <- as.data.frame(confusion_matrix$table)
confusion_matrix_data <- expand.grid(
  Prediction = levels(testData$Reached.on.Time_Y.N),
  Reference = levels(testData$Reached.on.Time_Y.N))
confusion_matrix_data <- merge(
  confusion_matrix_data, as.data.frame(confusion_matrix$table), 
  by = c("Prediction", "Reference"), all.x = TRUE)
confusion_matrix_data$Freq[is.na(confusion_matrix_data$Freq)] <- 0

ggplot(confusion_matrix_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "gray80") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Heatmap (Regression)", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )



#####-------random forest--------######
model_rf <- randomForest(Reached.on.Time_Y.N ~ ., data = trainData, ntree = 100, mtry = 3, importance = TRUE)

# prediction
predictions_rf <- predict(model_rf, testData)

# accuracy
confusion_matrix_rf <- confusionMatrix(predictions_rf, testData$Reached.on.Time_Y.N)
accuracy_rf <- confusion_matrix_rf$overall['Accuracy']
sensitivity <- confusion_matrix_rf$byClass['Sensitivity']
specificity <- confusion_matrix_rf$byClass['Specificity']
print(paste("Accuracy:", round(accuracy_rf, 2))) #better: 0.66
print(paste("Sensitivity:", round(sensitivity, 2))) #0.7
print(paste("Specificity:", round(specificity, 2))) #0.63

importance(model_rf) #the most important: weight, discount, cost, prior purchases
varImpPlot(model_rf)


#ROC curve
predictions_prob <- predict(model_rf, testData, type = "prob")[,2]
roc_curve <- roc(testData$Reached.on.Time_Y.N, predictions_prob)
plot(roc_curve, main = "ROC Curve", col = "blue")
auc(roc_curve) #Area under the curve: 0.7337

#visualization of prediction
predicted_df <- data.frame(TrueLabel = testData$Reached.on.Time_Y.N, PredictedProbability = predictions_prob)
ggplot(predicted_df, aes(x = PredictedProbability, fill = TrueLabel)) +
  geom_histogram(binwidth = 0.1, position = "dodge", alpha = 0.7) +
  labs(title = "Histogram of Predicted Probabilities", x = "Predicted Probability", y = "Frequency") +
  scale_fill_manual(values = c("Not On Time" = "coral1", "On Time" = "aquamarine1"))

#heatmap for predictive data
confusion_matrix_data_rf <- as.data.frame(confusion_matrix_rf$table)
confusion_matrix_data_rf <- expand.grid(
  Prediction = levels(testData$Reached.on.Time_Y.N),
  Reference = levels(testData$Reached.on.Time_Y.N))
confusion_matrix_data_rf <- merge(
  confusion_matrix_data_rf, as.data.frame(confusion_matrix_rf$table), 
  by = c("Prediction", "Reference"), all.x = TRUE)
confusion_matrix_data_rf$Freq[is.na(confusion_matrix_data_rf$Freq)] <- 0

ggplot(confusion_matrix_data_rf, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "gray80") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )

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
sensitivity <- confusion_matrix_rf$byClass['Sensitivity']
specificity <- confusion_matrix_rf$byClass['Specificity']
print(paste("Accuracy:", round(accuracy_rf, 2))) #better: 0.67
print(paste("Sensitivity:", round(sensitivity, 2))) #better 0.77
print(paste("Specificity:", round(specificity, 2))) #better 0.6

#ROC curve
predictions_prob <- predict(rf_model, testData, type = "prob")[,2]
roc_curve <- roc(testData$Reached.on.Time_Y.N, predictions_prob)
plot(roc_curve, main = "ROC Curve", col = "blue")
auc(roc_curve) #Area under the curve: 0.7357

#visualization of prediction
predicted_df <- data.frame(TrueLabel = testData$Reached.on.Time_Y.N, PredictedProbability = predictions_prob)
ggplot(predicted_df, aes(x = PredictedProbability, fill = TrueLabel)) +
  geom_histogram(binwidth = 0.1, position = "dodge", alpha = 0.7) +
  labs(title = "Histogram of Predicted Probabilities", x = "Predicted Probability", y = "Frequency") +
  scale_fill_manual(values = c("Not On Time" = "coral1", "On Time" = "aquamarine1"))


#heatmap for predictive data
confusion_matrix_data <- as.data.frame(confusion_matrix$table)
confusion_matrix_data <- expand.grid(
  Prediction = levels(testData$Reached.on.Time_Y.N),
  Reference = levels(testData$Reached.on.Time_Y.N))
confusion_matrix_data <- merge(
  confusion_matrix_data, as.data.frame(confusion_matrix$table), 
  by = c("Prediction", "Reference"), all.x = TRUE)
confusion_matrix_data$Freq[is.na(confusion_matrix_data$Freq)] <- 0

ggplot(confusion_matrix_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "gray80") +
  geom_text(aes(label = Freq), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red") +
  labs(title = "Confusion Matrix Heatmap", x = "Actual", y = "Predicted") +
  theme_minimal()+
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )




#####-------decision tree--------######
tree_model <- rpart(Reached.on.Time_Y.N ~ ., data = trainData, method = "class")

#prediction
predictions_tree <- predict(tree_model, testData, type = "class")

#accuracy
confusion_matrix_tree <- confusionMatrix(predictions_tree, testData$Reached.on.Time_Y.N)
accuracy_dt <- confusion_matrix_tree$overall['Accuracy']
sensitivity_dt <- confusion_matrix_tree$byClass['Sensitivity']
specificity_dt <- confusion_matrix_tree$byClass['Specificity']
print(paste("Accuracy:", round(accuracy_dt, 2))) #0.68
print(paste("Sensitivity:", round(sensitivity_dt, 2))) #0.94
print(paste("Specificity:", round(specificity_dt, 2))) #0.5

#visualization
rpart.plot(tree_model, type = 3, extra = 101, fallen.leaves = TRUE, 
           box.palette = "RdYlGn", shadow.col = "gray", nn = TRUE)

# ROC curve
predictions_prob <- predict(tree_model, testData, type = "prob")[,2]
roc_curve_dt <- roc(testData$Reached.on.Time_Y.N, predictions_prob)
plot(roc_curve_dt, main = "ROC Curve", col = "blue")
auc(roc_curve_dt) #0.74

# Add AUC to the plot
auc_value_dt <- auc(roc_curve_dt); plot(roc_curve_dt); legend("bottomright", legend = paste("AUC =", round(auc_value_dt, 2)), col = "blue", lwd = 2)

#heatmap for predictive data

confusion_matrix_tree_data <- as.data.frame(confusion_matrix_tree$table)
confusion_matrix_tree_data$Freq[confusion_matrix_tree_data$Freq == 0] <- NA
heatmap_dt <- ggplot(confusion_matrix_tree_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "gray80") +
  geom_text(aes(label = ifelse(is.na(Freq), "", Freq)), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  labs(
    title = "Confusion Matrix Heatmap (Decision Tree)",
    x = "Actual",
    y = "Predicted",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )

#comparing models RF and DT

# Tuning hyperparameters
tree_model_tuned <- rpart(
  Reached.on.Time_Y.N ~ ., 
  data = trainData, 
  method = "class",
  control = rpart.control(cp = 0.01, maxdepth = 5, minsplit = 10)
)

# Prediction
predictions_tree_tuned <- predict(tree_model_tuned, testData, type = "class")

# Evaluate accuracy
confusion_matrix_tree_tuned <- confusionMatrix(predictions_tree_tuned, testData$Reached.on.Time_Y.N)
accuracy_tuned <- confusion_matrix_tree_tuned$overall['Accuracy']
print(paste("Tuned Decision Tree Accuracy:", round(accuracy_tuned, 2)))


# Random Forest Model
model_rf <- randomForest(
  Reached.on.Time_Y.N ~ ., 
  data = trainData, 
  ntree = 200, 
  mtry = sqrt(ncol(trainData) - 1), 
  importance = TRUE
)

# Prediction
predictions_rf <- predict(model_rf, testData)

# Evaluate accuracy
confusion_matrix_rf <- confusionMatrix(predictions_rf, testData$Reached.on.Time_Y.N)
accuracy_rf <- confusion_matrix_rf$overall['Accuracy']
print(paste("Random Forest Accuracy:", round(accuracy_rf, 2))) #0.66

# Variable Importance for Decision Tree
print(varImp(tree_model, scale = FALSE))

# Variable Importance for Random Forest
importance_rf <- importance(model_rf)
print(varImpPlot(model_rf, scale = F))


#improving models with cubist function
# Converting Reached.on.Time_Y.N to numeric for regression-like Cubist
trainData$Reached.on.Time_Y.N <- as.numeric(trainData$Reached.on.Time_Y.N) - 1
testData$Reached.on.Time_Y.N <- as.numeric(testData$Reached.on.Time_Y.N) - 1

# Train the Cubist model
cubist_model <- cubist(
  x = trainData[, -which(names(trainData) == "Reached.on.Time_Y.N")],
  y = trainData$Reached.on.Time_Y.N,
  committees = 10  # Number of committees (models)
)
print(summary(cubist_model))

# Predict on the test set
predictions_cubist <- predict(cubist_model, testData[, -which(names(testData) == "Reached.on.Time_Y.N")])

# Convert predictions to binary classes
predicted_classes_cubist <- ifelse(predictions_cubist > 0.5, "On Time", "Not On Time")
predicted_classes_cubist <- factor(predicted_classes_cubist, levels = c("Not On Time", "On Time"))

# Confusion matrix
confusion_matrix_cubist <- confusionMatrix(
  predicted_classes_cubist,
  factor(testData$Reached.on.Time_Y.N, levels = c(0, 1), labels = c("Not On Time", "On Time"))
)

# Extract metrics
accuracy_cubist <- confusion_matrix_cubist$overall["Accuracy"]
sensitivity_cubist <- confusion_matrix_cubist$byClass["Sensitivity"]
specificity_cubist <- confusion_matrix_cubist$byClass["Specificity"]

# Print metrics
print(paste("Accuracy:", round(accuracy_cubist, 2))) #0.68
print(paste("Sensitivity:", round(sensitivity_cubist, 2))) #0.94
print(paste("Specificity:", round(specificity_cubist, 2))) ##0.5

# ROC Curve
roc_curve_cubist <- roc(
  testData$Reached.on.Time_Y.N,
  predictions_cubist
)
plot(roc_curve_cubist, main = "ROC Curve (Cubist)", col = "blue")
auc_value_cubist <- auc(roc_curve_cubist) #0.73
legend("bottomright", legend = paste("AUC =", round(auc_value_cubist, 2)), col = "blue", lwd = 2)

# Heatmap for predictive data
confusion_matrix_cubist_data <- as.data.frame(confusion_matrix_cubist$table)
confusion_matrix_cubist_data$Freq[confusion_matrix_cubist_data$Freq == 0] <- NA
ggplot(confusion_matrix_cubist_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "gray80") +
  geom_text(aes(label = ifelse(is.na(Freq), "", Freq)), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  labs(
    title = "Confusion Matrix Heatmap (Cubist)",
    x = "Actual",
    y = "Predicted",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )


#####-------svm--------######
svm_model <- svm(Reached.on.Time_Y.N ~ ., data = trainData, probability = TRUE)

#prediction
predictions_svm <- predict(svm_model, testData, probability = TRUE)
predictions_prob_svm <- attr(predictions_svm, "probabilities")[,2]

#confusion matrix 
confusion_matrix_svm <- confusionMatrix(predictions_svm, testData$Reached.on.Time_Y.N)
accuracy_svm <- confusion_matrix_svm$overall['Accuracy']
sensitivity_svm <- confusion_matrix_svm$byClass['Sensitivity']
specificity_svm <- confusion_matrix_svm$byClass['Specificity']
print(paste("Accuracy:", round(accuracy_svm, 2)))
print(paste("Sensitivity:", round(sensitivity_svm, 2)))
print(paste("Specificity:", round(specificity_svm, 2)))

# ROC curve
roc_curve_svm <- roc(testData$Reached.on.Time_Y.N, predictions_prob_svm)
plot(roc_curve_svm, main = "ROC Curve (SVM)", col = "blue")
auc_value_svm <- auc(roc_curve_svm)
legend("bottomright", legend = paste("AUC =", round(auc_value_svm, 2)), col = "blue", lwd = 2)

# Heatmap for prediction
confusion_matrix_svm_data <- as.data.frame(confusion_matrix_svm$table)
confusion_matrix_svm_data$Freq[confusion_matrix_svm_data$Freq == 0] <- NA
heatmap_svm <- ggplot(confusion_matrix_svm_data, aes(x = Reference, y = Prediction, fill = Freq)) +
  geom_tile(color = "gray80") +
  geom_text(aes(label = ifelse(is.na(Freq), "", Freq)), color = "black", size = 4) +
  scale_fill_gradient(low = "white", high = "red", na.value = "white") +
  labs(
    title = "Confusion Matrix Heatmap (SVM)",
    x = "Actual",
    y = "Predicted",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(color = "blue4", size = 14, hjust = 0.5, face = "bold"),
    axis.title.x = element_text(color = "blue4", size = 12, face = "bold"),
    axis.title.y = element_text(color = "blue4", size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  )

print(heatmap_svm)


#########-----------------------------------------------##########
#selecting significant variables - Boruta#
#########---------------------------------------------##########


boruta_shipping <- Boruta(Reached.on.Time_Y.N ~ ., data = trainData, doTrace = 0)
rough_fix_mod <- TentativeRoughFix(boruta_shipping)
boruta_signif <- getSelectedAttributes(rough_fix_mod) #the most important factors
importances <- attStats(rough_fix_mod)
importances <- importances[importances$decision != "Rejected", 
                           c("meanImp", "decision")]
importances[order(-importances$meanImp), ] #the most important factors from the highest to the lowest importance
boruta_plot <- plot(boruta_shipping, ces.axis = 0.3, las = 2, xlab = "", 
                    main = "Feature importance")


