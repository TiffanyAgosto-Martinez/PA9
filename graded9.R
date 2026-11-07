# graded9.R
# Tiffany Agosto-Martinez
# 11/6/2026
# Use R to create a model showing fuel efficiency and MPG using vehicle weight



# Plot the Data
plot(mtcars$wt, mtcars$mpg,
     main = "Fuel Efficiency vs. Vehicle Weight",
     xlab = "Weight (Thousands of lbs)",
     ylab = "Miles Per Gallon (MPG)",
     pch = 19,
     col = "blue")

# Fit a Linear Regression Line
car_model <- lm(mpg ~ wt, data = mtcars)

# Add a Fitted Regression Line
abline(car_model, col = "red", lwd = 2)

# Interpret the Slope by displaying the summary of model and analyze p-values
model_summary <- summary(car_model)
print(model_summary)

# Calculate the 95% confidence intervals
model_confint <- confint(car_model, level = 0.95)
print(model_confint)

slope_estimate <- model_summary$coefficients["wt", "Estimate"]
p_value <- model_summary$coefficients["wt","Pr(>|t|)"]

ci_lower <- model_confint["wt", "2.5 %"]
ci_upper <- model_confint["wt", "97.5 %"]

cat("Slope estimate:", slope_estimate, "\n")
cat("p-value:", p_value, "\n")
cat("95% Confidence Interval for Slope:", ci_lower, ci_upper, "\n")
cat("The slope is statistically significant at the 0.05 level, indicating
a significant relationship between weight and fuel efficiency.\n")

# Predict MPG for a given Vehicle Weight (Convert vehicle to thousands of lbs)
weight_in_thousands <- 6000 / 1000
car_weight <- data.frame(wt = weight_in_thousands)


prediction <- predict(car_model, newdata = car_weight,
                      interval = "prediction",
                      level = 0.95
                      )

# Print the Prediction
cat("Predicted MPG for 6,000-pound vehicle:", "\n")
print(prediction)

# Evaluate and discuss the Model's Prediction
cat("Note: The weight value is outside of the range of the dataset, which 
limits the accuracy of the prediction. Use caution when interpreting 
this result.")
