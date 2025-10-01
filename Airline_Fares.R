# Load necessary libraries

library(ggplot2)
library(dplyr)
library(car)
library(caret)
library(datarium)
library(readxl)
library(glmnet)

# Load the dataset
file_path <- "path to the dataset...\\airline_fares.xltx"
airline_fares <- read_excel(file_path)

#we did some initial visualizations to get a feeling:

#average fare prices by class
ggplot(airline_fares, aes(x = Class, y = Fare, fill = Class)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.6) +
  labs(title = "Average Fare by Class", 
       x = "Class", 
       y = "Average Fare") +
  theme_minimal()

#fare prices by class but boxplot
ggplot(airline_fares, aes(x = Class, y = Fare)) +
  geom_boxplot(aes(fill = Class), alpha = 0.6) + 
  labs(title = "Fare Price Distribution by Class", 
       x = "Class", 
       y = "Fare") +
  theme_minimal()

#flight duration vs. fare
ggplot(airline_fares, aes(x = Duration_in_hours, y = Fare)) +
  geom_point(shape = 1, color = "blue", alpha = 0.5) +
  labs(
    title = "Fare vs. Duration (Hours)",
    x = "Duration in Hours",
    y = "Fare"
  ) +
  theme_minimal()

#average fare per number of stops
ggplot(airline_fares, aes(x = Total_stops, y = Fare, fill = Total_stops)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.6) +
  labs(title = "Average Fare by Number of Stops", 
       x = "Number of Stops", 
       y = "Average Fare") +
  theme_minimal()

#fare by number of stops
ggplot(airline_fares, aes(x = Total_stops, y = Fare)) +
  geom_boxplot(aes(fill = Total_stops), alpha = 0.6) + 
  labs(title = "Fare Price Distribution by Number of Stops", 
       x = "Number of Stops", 
       y = "Fare") +
  theme_minimal()

#fare vs. kerosen - not very insightful
ggplot(airline_fares, aes(x = Fare, y = kerosen)) +
  geom_point(shape = 1, color = "darkgreen", alpha = 0.5) +
  labs(
    title = "Fare vs. Kerosene Price",
    x = "Kerosene Price",
    y = "Fare"
  ) +
  theme_minimal()

#days left vs. fare
ggplot(airline_fares, aes(x = Days_left, y = Fare)) +
  geom_point(shape = 1, color = "purple", alpha = 0.5) +
  labs(
    title = "Fare vs. Days Left",
    x = "Days Left to Journey",
    y = "Fare"
  ) +
  theme_minimal()


# We encode the "categorical variables"

airline_fares$Journey_day <- as.factor(airline_fares$Journey_day)
airline_fares$Airline <- as.factor(airline_fares$Airline)
airline_fares$Class <- as.factor(airline_fares$Class)
airline_fares$Source <- as.factor(airline_fares$Source)
airline_fares$Departure <- as.factor(airline_fares$Departure)
airline_fares$Arrival <- as.factor(airline_fares$Arrival)
airline_fares$Destination <- as.factor(airline_fares$Destination)
airline_fares$Total_stops <- as.factor(airline_fares$Total_stops)

#We standardize our numerical variables
airline_fares$kerosen <- scale(airline_fares$kerosen)
airline_fares$Duration_in_hours <- scale(airline_fares$Duration_in_hours)
airline_fares$Days_left <- scale(airline_fares$Days_left)

# Run linear regression with selected factors
full_model <- lm(Fare ~ kerosen + Journey_day + Airline + Class + Source + Departure + Total_stops + Arrival + Destination + Duration_in_hours + Days_left, data = airline_fares)

# Summarize the model
summary(full_model)

# Visualizing linear regression with confidence intervals:

# Prepare data for plotting
airline_fares$predicted <- predict(full_model)  # Predicted values
airline_fares$lower_conf <- predict(full_model, interval = "confidence")[, "lwr"]  # Lower confidence bound
airline_fares$upper_conf <- predict(full_model, interval = "confidence")[, "upr"]  # Upper confidence bound
airline_fares$lower_pred <- predict(full_model, interval = "prediction")[, "lwr"]  # Lower prediction bound
airline_fares$upper_pred <- predict(full_model, interval = "prediction")[, "upr"]  # Upper prediction bound

ggplot(data = airline_fares, aes(x = predicted, y = Fare)) +
  geom_point(color = "black", alpha = 0.6) +  # Actual data points
  geom_line(aes(y = predicted), color = "green", linewidth = 1) +  # Regression line
  geom_ribbon(aes(ymin = lower_conf, ymax = upper_conf, fill = "Confidence"), alpha = 0.3) +  # Confidence interval
  geom_ribbon(aes(ymin = lower_pred, ymax = upper_pred, fill = "Prediction"), alpha = 0.2) +  # Prediction interval
  scale_fill_manual(name = "Interval", values = c("Confidence" = "green", "Prediction" = "blue")) +
  labs(title = "Prediction and Confidence Intervals for Regression",
       x = "Predicted Fare",
       y = "Actual Fare") +
  theme_minimal()



# ANOVA test to see relevance of different variables:

  #we ommited the day of the journey
reduced_model_day <- lm(Fare ~ kerosen + Airline + Class + Source + Departure + Total_stops + Arrival + Destination + Duration_in_hours + Days_left, data = airline_fares)
anova_results <- anova(reduced_model_day, full_model)
print(anova_results)
  #we ommited the kerosen variable
reduced_model_kerosen <-lm(Fare ~ Journey_day + Airline + Class + Source + Departure + Total_stops + Arrival + Destination + Duration_in_hours + Days_left, data = airline_fares)
anova_results2 <- anova(reduced_model_kerosen, full_model)
print(anova_results2)


#AIC for model selection

AIC(full_model)
AIC(reduced_model_day)
AIC(reduced_model_kerosen)
  #we omitted departure
reduced_model_departure <- lm(Fare ~ kerosen + Journey_day + Airline + Class + Source + Arrival + Total_stops + Destination + Duration_in_hours + Days_left, data = airline_fares)
AIC(reduced_model_departure)
  #we omitted total number of stops
reduced_model_stops <- lm(Fare ~ kerosen + Journey_day + Airline + Class + Source + Departure + Arrival + Destination + Duration_in_hours + Days_left, data = airline_fares)
AIC(reduced_model_stops)
  #we omitted class
reduced_model_class <- lm(Fare ~ kerosen + Journey_day + Airline + Source + Departure + Total_stops + Arrival + Destination + Duration_in_hours + Days_left, data = airline_fares)
AIC(reduced_model_class)


#checking if requirements for linear regression are met - plotting residuals

residuals_full <- residuals(full_model)
summary(residuals_full)
hist(residuals_full, breaks = 50, main = "Histogram of Residuals", xlab = "Residuals")
plot(full_model, which = 5)
plot(full_model, which = 1)


# Perform the Kolmogorov-Smirnov test for normality

  #on the full normal model
ks_test <- ks.test(residuals_full, "pnorm")
print(ks_test)
  #taking the logarithm of Fare
airline_fares$Log_Fare <- log(airline_fares$Fare)
model_log <- lm(Log_Fare ~ kerosen + Journey_day + Airline + Class + Source + Departure + Total_stops + Arrival + Destination + Duration_in_hours + Days_left, data = airline_fares)
summary(model_log)
  #altering a little bit the residuals to break ties (in order to be able to perform the test)
residuals_log <- residuals(model_log)
residuals_log_jitter <- residuals_log + rnorm(length(residuals_log), mean = 0, sd = 1e-6)
ks_test_log <- ks.test(residuals_log_jitter, "pnorm")
print(ks_test_log)

#Cross-validation with 80%(traing set) and 20%(testing set)

set.seed(123)
random_sample <- createDataPartition(airline_fares $ Fare, p = 0.8, list = FALSE)
training_dataset <- airline_fares[random_sample, ]
testing_dataset <- airline_fares[-random_sample, ]
model_training <- lm(Fare ~ kerosen + Journey_day + Airline + Class + Source + Departure + Total_stops + Arrival + Destination + Duration_in_hours + Days_left, data = training_dataset)
predictions <- predict(model_training, testing_dataset)
data.frame( R2 = R2(predictions, testing_dataset $ Fare),RMSE = RMSE(predictions, testing_dataset $ Fare))



#LASSO:

# Prepare the model matrix for predictors (excluding intercept)
predictors <- model.matrix(Fare ~ kerosen + Airline + Class + Source + Departure + Total_stops + Arrival + Destination + Duration_in_hours + Days_left, 
                           data = airline_fares)[, -1]
scaled_predictors <- scale(predictors)

# prepare the target variable
y <- airline_fares$Fare

# apply LASSO with 10-fold cross-validation
lasso_model <- cv.glmnet(scaled_predictors, y, alpha = 1, family = "gaussian", nfolds = 10)
optimal_lambda <- lasso_model$lambda.min

# fitting the LASSO model with optimal lambda
final_lasso_model <- glmnet(scaled_predictors, y, alpha = 1, lambda = optimal_lambda)

# predict the values using the LASSO model
predicted_values <- predict(final_lasso_model, s = optimal_lambda, newx = scaled_predictors)

# convert predicted values to a vector (flatten the matrix)
predicted_values_vector <- as.vector(predicted_values)

# for visualization reasons - see report, we apply log transformation to both observed and predicted values
plot_data <- data.frame(Observed = log(y), Predicted = log(predicted_values_vector))

#scatter plot for Observed vs Predicted values using the log-transformed fares
ggplot(plot_data, aes(x = Observed, y = Predicted)) +
  geom_point(shape = 1,color = "blue", alpha = 0.5) +
  geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed") +
  labs(title = "Observed vs Predicted Fares (LASSO with Log Transformation for Visualization)", 
       x = "Log of Observed Fare", 
       y = "Log of Predicted Fare") +
  theme_minimal()

# plot the LASSO model results
plot(lasso_model)

# Extract the coefficients for the optimal lambda
coefficients <- coef(final_lasso_model, s = optimal_lambda)
coefficients_matrix <- as.matrix(coefficients)
coefficients_df <- data.frame(Variable = rownames(coefficients_matrix), 
                              Coefficient = coefficients_matrix[, 1])
# Generate residuals from the LASSO model
lasso_residuals <- residuals(lasso_model)

# Filter out zero coefficients (these variables were excluded by LASSO)
coefficients_df <- coefficients_df[coefficients_df$Coefficient != 0, ]

# Sorting coefficients by their absolute value to identify the most influential variables
coefficients_df <- coefficients_df[order(abs(coefficients_df$Coefficient), decreasing = TRUE), ]
print(coefficients_df)

# Calculate residuals (actual - predicted)
lasso_residuals <- y - predicted_values_vector

# Check for NA values and remove them
lasso_residuals <- na.omit(lasso_residuals)

# Calculate Mean Squared Error (MSE)
mse <- mean(lasso_residuals^2)
cat("Mean Squared Error (MSE):", mse, "\n")
# Calculate R-squared
rss <- sum(lasso_residuals^2)  # Residual sum of squares
tss <- sum((y - mean(y))^2)    # Total sum of squares
r_squared <- 1 - (rss / tss)
cat("R-squared:", r_squared, "\n")

# QQ plot for LASSO:
# Check if the residuals are non-empty
if(length(lasso_residuals) > 0) {
  # Plot the QQ plot
  qqnorm(lasso_residuals, main = "QQ Plot of LASSO Model Residuals")
  qqline(lasso_residuals, col = "red")  # Add a reference line
} else {
  print("Residuals are empty or contain only NA values.")
}

