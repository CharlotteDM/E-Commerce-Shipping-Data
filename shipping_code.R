library(ggplot2)
library (reshape2)

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
  theme_minimal() +
  theme( 
    plot.title = element_text(color="darkgreen", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="green4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="green4", size=12, hjust = 0.5, face="bold")) 
  

ggplot(shipping_data, aes(x = Mode_of_Shipment)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Using different shipping methods") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold")) 


ggplot(shipping_data, aes(x = Product_importance, fill = Product_importance)) +
  geom_bar() +
  labs(title = "Product Importance") +
  theme_minimal() +
theme( 
  plot.title = element_text(color="royalblue4", size=14, hjust = 0.5, face="bold"),
  axis.title.x = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
  axis.title.y = element_text(color="steelblue4", size=12, hjust = 0.5, face="bold"),
  legend.position = "none") 

ggplot(shipping_data, aes(y = Customer_care_calls)) +
  geom_boxplot() +
  labs(title = "Customer Care Calls") +
  theme_minimal() #strange

ggplot(shipping_data, aes(x = Customer_rating)) +
  geom_histogram(binwidth = 1, fill = "pink", color = "black") +
  labs(title = "Customer Rating") +
  theme_minimal() +
  theme( 
    plot.title = element_text(color="pink4", size=14, hjust = 0.5, face="bold"),
    axis.title.x = element_text(color="pink4", size=12, hjust = 0.5, face="bold"),
    axis.title.y = element_text(color="pink4", size=12, hjust = 0.5, face="bold")) 

ggplot(shipping_data, aes(y = Cost_of_the_Product)) +
  geom_boxplot() +
  labs(title = "Cost of The Product") +
  theme_minimal()#strange

ggplot(shipping_data, aes(x = Mode_of_Shipment, y = Customer_care_calls)) +
  geom_boxplot() +
  labs(title = "Customer Care Calls & Mode of Shipment") +
  theme_minimal()

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
  geom_tile(color = "white") +  # Kafelki z obwódką
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), name = "Korelacja"
  ) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 3) +  
  theme_minimal() +
  labs(title = "Heatmap Korelacji z Wartościami") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

