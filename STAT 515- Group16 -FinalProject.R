

library(tidyverse)
library(dplyr)
library(tidyr)
library(GGally)
library(stats)
library(cluster)
library(FactoMineR)
library(factoextra)
library(glmnet)
library(randomForest)
library(randomForestExplainer)
library(Matrix)
library(cowplot)
library(caret)

#### Importing the data set
data_sales<- read.csv('sales_data_sample-1.csv')

### Printing the first few rows of the data set
head(data_sales)

### Structure of the data set
str(data_sales)

#### Summary statistics of the data set
summary(data_sales)

###Exploratory Analysis

##Barplot of the Status
ggplot(data_sales,aes(x=STATUS,fill=STATUS))+
  geom_bar(color='black')+
  labs(x='status of the order placed',
       title= "STATUS")+
  coord_flip()


### Bargraph of the status and Quantity ordered 
ggplot(data_sales,aes(x=STATUS,y=QUANTITYORDERED,fill=PRODUCTLINE))+
  geom_bar(stat="identity")+
  facet_grid(~PRODUCTLINE)+
  coord_flip()+
  theme(legend.position = "none")+
  labs(x="\n status of the orders",y="No of quantities orders",title="\n Relationship between status and quantities of orders\n")


#### Histogram of the Month the sales occurred
ggplot(data_sales,aes(x=MONTH_ID))+
  geom_histogram(color="black",fill="orange")+
  scale_x_continuous(limits = c(0.5, 12.5), breaks = 1:12)+
  labs(x="Months of orders",y="Frequency",title="Histogram of the ordered months")
  
 
### Relationship between Sales and status of the orders
ggplot(data_sales,aes(y=SALES,x=STATUS,fill=STATUS))+
  geom_bar(stat='identity')+
  facet_grid(~YEAR_ID)+theme(legend.position = "none")+
  labs(x="Status of the sales order",y="Sales in dollars",title="Relationship between Sales and Status of sales")


### Creating a bar plot between products and in which month they were ordered
ggplot(data_sales,aes(x=MONTH_ID,fill=PRODUCTLINE))+geom_bar(color="black")+
  scale_fill_manual(values=c("red","orange",rgb(0, 0.75, 0), "blue", "violet","lightpink","lightblue"))+
  facet_grid(~PRODUCTLINE)+
  scale_x_continuous(limits = c(0.5, 12.5), breaks = 1:12)+
  labs(x="Month in order",y="Frequency",title="Determing no.of sales for each month with respective productline")
  

####  Boxplot of the productline and the price of each quantity in dollars
ggplot(data_sales, aes(x = PRODUCTLINE, y = PRICEEACH, fill = PRODUCTLINE)) +
  geom_boxplot(fill = c("red", "orange", rgb(0, 0.75, 0), "blue", "violet", "lightpink", "lightblue")) +
  labs(title = "Boxplot of PRICEEACH by PRODUCTLINE", x = "PRODUCTLINE", y = "PRICEEACH in dollars") 


#### Boxplot of the Market Sale Retail Price(MSRP)

ggplot(data_sales,aes(x=MSRP,fill=STATUS))+
  geom_boxplot()+
  scale_fill_manual(values=c("red","orange",rgb(0, 0.75, 0), "blue", "violet","lightblue"))+
  labs(x="\n Market Retail Price in dollars",title="\n Boxplot of Market Retail price")+
  coord_flip()


#### Scatterplot of the sales with respective each country and dealsize
ggplot(data_sales,aes(x=SALES,y=COUNTRY))+
  geom_point(aes(color=DEALSIZE))+
  facet_wrap(~DEALSIZE)+
  theme(legend.position = "none")+
  labs(x="Sales in dollars",y="Country",title="Scatterplot of Sales for Country with respective Dealsize")

  coord_flip()
  
 
##### Histograms of the SALES, Market Sale Retail Price(MSRP) with respective country 
ggplot(data_sales,aes(x=SALES))+
  geom_histogram(binwidth=500, color="black",fill="cyan")+
  labs(x="\n Sales in dollars",y="Frequency",title="\n Histogram of Sales")

ggplot(data_sales,aes(x=MSRP))+
  geom_histogram(binwidth=50,color='black',fill='green')+
  labs(x="\n MSRP in dollars",y="Frequency",title="\n Histogram of Market Retail price")

ggplot(data_sales,aes(x=MSRP,fill=COUNTRY))+
  geom_bar()+
  facet_wrap(~PRODUCTLINE)+
  theme(legend.position="right")+
  labs(x="MSRP in dollars")






###### Statistical Analysis


#Linear Regression

# Create a scatter plot with a regression line
ggplot(data_sales, aes(x = QUANTITYORDERED, y = SALES)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Linear Regression: Sales vs Quantity Ordered",
       x = "Quantity Ordered",
       y = "Sales")



#### Multiple Linear Regression Model-1
model <- lm(SALES ~ MSRP + YEAR_ID, data = data_sales)

# Make predictions using the model
predictions <- predict(model, newdata = data_sales)

# Plot the predictions against the actual values, facetted by PRODUCTLINE
ggplot(data_sales, aes(x = SALES, y = predictions, color = PRODUCTLINE)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(title = "Multiple Regression: Actual vs Predicted",
       x = "Actual Sales",
       y = "Predicted Sales") +
  theme_minimal() +
  facet_wrap(~PRODUCTLINE, scales = "free")




##### PRINCIPAL COMPONENT ANLAYSIS

# Identify numeric columns
numeric_cols <- sapply(data_sales, is.numeric)

# Extract numeric columns
numeric_features <- data_sales[, numeric_cols]

# Standardize the features
scaled_features <- scale(numeric_features)

# Assuming your features start from column 2 onwards
# If your dataset structure is different, adjust accordingly
features <- data_sales[, 2:ncol(data_sales)]

# Get unique product lines
unique_products <- unique(data_sales$PRODUCTLINE)

# Create a directory to save individual plots
dir.create("PCA_Plots", showWarnings = FALSE)

# Define a vector of colors for titles
title_colors <- c("red", "blue", "green", "purple", "orange", "brown", "pink", "gray", "cyan", "magenta")

# Create an empty list to store plots
plot_list <- list()

# Iterate over each product and create a PCA plot
for (i in seq_along(unique_products)) {
  product <- unique_products[i]
  color <- title_colors[i]
  
  # Subset data for the current product
  subset_data <- data_sales[data_sales$PRODUCTLINE == product, ]
  
  # Fit PCA
  pca_result <- prcomp(subset_data[, numeric_cols], center = TRUE, scale. = TRUE)
  
  # Visualize PCA results
  pca_plot <- fviz_pca_ind(pca_result,
                           geom.ind = "point", # show points only (no labels)
                           col.ind = "blue", # set a single color
                           addEllipses = TRUE, # Concentration ellipses
                           title = paste("PCA of", product),
                           ggtheme = theme_minimal() + theme(text = element_text(color = color))
  )
  
  # Add the plot to the list
  plot_list[[i]] <- pca_plot
}





###### RANDOM FOREST

# Remove rows with missing values
data_sales <- na.omit(data_sales)

# Convert DEALSIZE to a factor
data_sales$DEALSIZE <- as.factor(data_sales$DEALSIZE)

train_indices <- sample(1:nrow(data_sales), 0.7 * nrow(data_sales))
train <- data_sales[train_indices, ]
test <- data_sales[-train_indices, ]

# Fit Random Forest model
rf_model <- randomForest(DEALSIZE ~ ., data = train)

# Summary of the model
print(rf_model)

# Assuming you have already fit the Random Forest model (rf_model)

# Get variable importance scores
importance_scores <- importance(rf_model)

# Print the variable importance
print(importance_scores)
# Plot variable importance
varImpPlot(rf_model)


pred_rf <- predict(rf_model, newdata = test)
pred_rf[1:10]

conf_matrix <- confusionMatrix(pred_rf, test$DEALSIZE)

# Print the confusion matrix
print(conf_matrix)


# Calculate test misclassification error
misclassification_error_rf <- mean(pred_rf != test$DEALSIZE)

cat("Test Misclassification Error (Random Forest):", misclassification_error_rf, "\n")




#### MULTIPLE LINEAR REGRESSION MODEL-2

# Example using linear regression with training set
linear_model <- lm(SALES ~ QUANTITYORDERED + PRICEEACH + MSRP, data = train)

# Predict on the testing set
predictions <- predict(linear_model, newdata = test)

# Assess model performance (e.g., mean squared error)
mse <- mean((test$SALES - predictions)^2)
print(paste("Mean Squared Error:", mse))



#### K-means Clustering


# Assuming your data is stored in a dataframe named 'data_sales' with numeric columns
# If needed, you might want to preprocess your data (e.g., convert categorical variables to numerical)

# Example: Numeric columns are selected using sapply
numeric_data <- data_sales[, sapply(data_sales, is.numeric)]

# Perform k-means clustering with, for example, 3 clusters
k <- 5
kmeans_result <- kmeans(numeric_data, centers = k, nstart = 10)



fviz_cluster(kmeans_result, data = numeric_data, geom = "point", stand = FALSE, main = "k-means Clustering")


# Check the cluster centers
kmeans_result$centers



#### Ridge Regression
x <- as.matrix(data_sales[, c("QUANTITYORDERED", "PRICEEACH", "MSRP")])
y <- data_sales$SALES

lambda_seq <- 10^seq(10, -2, length = 100)
ridge_model <- cv.glmnet(x, y, alpha = 0, lambda = lambda_seq)

# Find the optimal lambda
best_lambda_ridge <- ridge_model$lambda.min

# Display the optimal lambda
print(paste("Optimal Lambda for Ridge:", best_lambda_ridge))

# Predict on the testing set
ridge_predictions <- predict(ridge_model, newx = x)

# Assess model performance (e.g., mean squared error)
ridge_mse <- mean((y - ridge_predictions)^2)
print(paste("Mean Squared Error for Ridge:", ridge_mse))


plot(ridge_model)

## LASSO Regression

# Create a grid of lambda values
lambda_seq <- 10^seq(10, -2, length = 100)

# Fit Lasso regression model
lasso_model <- cv.glmnet(x, y, alpha = 1, lambda = lambda_seq)

# Find the optimal lambda
best_lambda <- lasso_model$lambda.min

# Display the optimal lambda
print(paste("Optimal Lambda for Lasso:", best_lambda))

# Predict on the testing set
lasso_predictions <- predict(lasso_model, newx = x)

# Assess model performance (e.g., mean squared error)
lasso_mse <- mean((y - lasso_predictions)^2)
print(paste("Mean Squared Error for Lasso:", lasso_mse))

plot(lasso_model)

