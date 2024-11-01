# Load your data (if not already loaded)
data <- read.csv("final.csv")

# Calculate the frequency of each HBM component
frequency_susceptibility <- sum(data$Susceptibility)
frequency_severity <- sum(data$Severity)
frequency_benefits <- sum(data$Benefits)
frequency_barriers <- sum(data$Barriers)
frequency_efficacy <- sum(data$Efficacy)
frequency_positive <- sum(data$Positive)
frequency_neutral <- sum(data$Neutral)
frequency_negative <- sum(data$Negative)

# Calculate the total number of tweets
total_tweets <- nrow(data)

# Calculate the percentage of tweets mentioning each HBM component
percentage_susceptibility <- (frequency_susceptibility / total_tweets) * 100
percentage_severity <- (frequency_severity / total_tweets) * 100
percentage_efficacy <- (frequency_efficacy / total_tweets) * 100
percentage_benefits <- (frequency_benefits / total_tweets) * 100
percentage_barriers <- (frequency_barriers / total_tweets) * 100
percentage_positive <- (frequency_positive / total_tweets) * 100
percentage_neutral <- (frequency_neutral / total_tweets) * 100
percentage_negative <- (frequency_negative / total_tweets) * 100

# Print the percentages
print(paste("Percentage mentioning Susceptibility:", round(percentage_susceptibility, 2), "%"))
print(paste("Percentage mentioning Severity:", round(percentage_severity, 2), "%"))
print(paste("Percentage mentioning Efficacy:", round(percentage_efficacy, 2), "%"))
print(paste("Percentage mentioning Benefits:", round(percentage_benefits, 2), "%"))
print(paste("Percentage mentioning Barriers:", round(percentage_barriers, 2), "%"))
print(paste("Percentage mentioning Positive:", round(percentage_positive, 2), "%"))
print(paste("Percentage mentioning Neutral:", round(percentage_neutral, 2), "%"))
print(paste("Percentage mentioning Negative:", round(percentage_negative, 2), "%"))

# List of all main variables
main_vars <- c("Susceptibility", "Severity", "Efficacy", "Benefits", "Barriers")

# Function to fit model for each variable against the others
fit_models <- function(data, vars) {
  models <- list()
  for (var in vars) {
    formula <- as.formula(paste(var, "~", paste(setdiff(vars, var), collapse = " + ")))
    models[[var]] <- lm(formula, data = data)
  }
  return(models)
}

# Fit the models
models <- fit_models(data, main_vars)

# Function to print summaries for all models
print_model_summaries <- function(models) {
  for (model_name in names(models)) {
    cat(paste("\nRegression results for", model_name, "as dependent variable:\n"))
    print(summary(models[[model_name]]))
  }
}

# Print summaries
print_model_summaries(models)


# Function to calculate means for subgroups
calculate_means <- function(data, main_vars, subgroup_vars) {
  results <- list()
  for (subgroup_var in subgroup_vars) {
    cat(paste("\nMeans for", subgroup_var, "groups:\n"))
    for (main_var in main_vars) {
      results[[paste(subgroup_var, main_var, sep = "_")]] <- tapply(data[[main_var]], data[[subgroup_var]], mean, na.rm = TRUE)
      cat(paste("Means of", main_var, "for", subgroup_var, "levels:\n"))
      print(results[[paste(subgroup_var, main_var, sep = "_")]])
    }
  }
  return(results)
}

# Calculate and print means
subgroup_vars <- c("Positive", "Neutral", "Negative")
mean_results <- calculate_means(data, main_vars, subgroup_vars)


# Function to fit models with interaction terms
fit_interaction_models <- function(data, main_vars, subgroup_vars) {
  interaction_models <- list()
  for (main_var in main_vars) {
    for (subgroup_var in subgroup_vars) {
      formula <- as.formula(paste(main_var, "~", subgroup_var, "*", paste(setdiff(main_vars, main_var), collapse = " + ")))
      interaction_models[[paste(main_var, subgroup_var, sep = "_")]] <- lm(formula, data = data)
    }
  }
  return(interaction_models)
}

# Fit interaction models
interaction_models <- fit_interaction_models(data, main_vars, subgroup_vars)

# Print summaries for interaction models
print_model_summaries(interaction_models)



# Load necessary libraries
library(vars)
library(lmtest)

# Load the dataset
data <- read.csv("ts.csv", stringsAsFactors = FALSE)
# Convert the 'Date' column to POSIXct format
data$Date <- as.POSIXct(data$Date, format = "%d-%b-%Y %I:%M%p")

# Check the result
head(data$Date)

# View the structure of the dataset
str(data)

# Load tseries package for the ADF test
install.packages("tseries")
library(tseries)

# Check for stationarity
adf.test(data$Positive)
adf.test(data$Neutral)
adf.test(data$Negative)

# Apply differencing if necessary (example for Positive)
data$diff_Positive <- diff(data$Positive, differences = 1)
data$diff_Neutral <- diff(data$Neutral, differences = 1)
data$diff_Negative <- diff(data$Negative, differences = 1)


# Fit a VAR model using all HBM-related variables and behavioral tendencies
# Adjust the lag based on the length of your time series data (e.g., lag = 2)
var_model <- VAR(data[,c("Susceptibility", "Severity", "Benefits", "Barriers", "Efficacy", "Positive", "Neutral", "Negative")], p = 2, type = "const")

# Summary of the model
summary(var_model)

# Granger causality test for Susceptibility causing Positive behavior
grangertest(Positive ~ Susceptibility, order = 2, data = data)
grangertest(Positive ~ Severity, order = 2, data = data)
grangertest(Positive ~ Efficacy, order = 2, data = data)
grangertest(Positive ~ Benefits, order = 2, data = data)
grangertest(Positive ~ Barriers, order = 2, data = data)
grangertest(Neutral ~ Susceptibility, order = 2, data = data)
grangertest(Neutral ~ Severity, order = 2, data = data)
grangertest(Neutral ~ Efficacy, order = 2, data = data)
grangertest(Neutral ~ Benefits, order = 2, data = data)
grangertest(Neutral ~ Barriers, order = 2, data = data)
grangertest(Negative ~ Susceptibility, order = 2, data = data)
grangertest(Negative ~ Severity, order = 2, data = data)
grangertest(Negative ~ Efficacy, order = 2, data = data)
grangertest(Negative ~ Benefits, order = 2, data = data)
grangertest(Negative ~ Barriers, order = 2, data = data)


# You can perform similar tests for other belief constructs and behavioral tendencies.
grangertest(Negative ~ Barriers, order = 2, data = data)
library(dplyr)
library(ggplot2)

# Convert 'Date' column to proper date format
data$Date <- as.Date(data$Date, format="%d-%b-%Y %I:%M%p")


# Create a new column for year-month
data <- data %>%
  mutate(Month = format(Date, "%Y-%m")) %>% 
  group_by(Month) %>%
  summarise(across(starts_with("Susceptibility"):starts_with("Negative"), sum, na.rm = TRUE)) # Adjust according to your columns
summarise(across(starts_with("Positive"):starts_with("Efficacy"), sum, na.rm = TRUE)) # Adjust according to your columns

# Convert Month to a Date format for proper plotting
data$Month <- as.Date(paste0(data$Month, "-01"))

# Melt the data if needed to create long format for ggplot
library(reshape2)
data_long <- melt(data, id.vars = "Month", variable.name = "Variable", value.name = "Count")

# Plot
ggplot(data_long, aes(x = Month, y = Count, color = Variable)) +
  geom_line(size = 1) + 
  labs(title = "Temporal Trend of HBM Constructs in Tweets",
       x = "Date (Monthly)",
       y = "Count of HBM-related Variables") +
  theme_minimal() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

