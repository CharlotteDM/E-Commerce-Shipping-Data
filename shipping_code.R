
path <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(path)


#Data Source: https://www.kaggle.com/datasets/prachi13/customer-analytics/data
shipping_data <- read.csv("Train.csv", stringsAsFactors = FALSE)
