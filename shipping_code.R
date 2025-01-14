library(ggplot2)


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
  labs(title = "Warehouse Block") +
  theme_minimal()
  

ggplot(shipping_data, aes(x = Mode_of_Shipment)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Using different shipping methods") +
  theme_minimal()


ggplot(shipping_data, aes(x = Product_importance, fill = Product_importance)) +
  geom_bar() +
  labs(title = "Product Importance") +
  theme_minimal()

ggplot(shipping_data, aes(y = Customer_care_calls)) +
  geom_boxplot() +
  labs(title = "Customer Care Calls") +
  theme_minimal() #strange

ggplot(shipping_data, aes(x = Customer_rating)) +
  geom_histogram(binwidth = 1, fill = "lightpink", color = "black") +
  labs(title = "Customer Rating") +
  theme_minimal()

ggplot(shipping_data, aes(y = Cost_of_the_Product)) +
  geom_boxplot() +
  labs(title = "Cost of The Product") +
  theme_minimal(). #strange

ggplot(shipping_data, aes(x = Mode_of_Shipment, y = Customer_care_calls)) +
  geom_boxplot() +
  labs(title = "Customer Care Calls & Mode of Shipment") +
  theme_minimal()

#Average Rating for Gender
ggplot(shipping_data, aes(x = Gender, y = Customer_rating)) +
  geom_boxplot() +
  labs(title = "Customer Rating & Gender") +
  theme_minimal()

average_ratings <- aggregate(Customer_rating ~ Gender, shipping_data, mean)
print(average_ratings)
