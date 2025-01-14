# Project Business Economic and Financial Data
# 2024/2025
# Author: Daniel Gutierrez & Fabio Pimentel
# Sales of DimSum Records, Asian-food restaurant in Medellin, Colombia

# Required Packages--------------------
rm(list = ls())
library(readxl)
library(ggplot2)
library(GGally)
library(dplyr)
library(lubridate)
library(corrplot)
library(feasts)
library(tsibble)
library(forecast)
library(tidyr)
library(ggthemes)
library(car)
library(DIMORA)
library(tseries)
library(lmtest)
# 1. Import Data--------------------------
# target variable
sales <- read_excel("data/sales/sales_dimsum_31102024.xlsx")

sales[is.na(sales)] <- 0

# economic variables
eco_growth <- read_excel("data/macroeconomic/economic_activity.xlsx")
fx <- read_excel("data/macroeconomic/fx.xlsx")
inflation <- read_excel("data/macroeconomic/inflation.xlsx")
unemployment <- read_excel("data/macroeconomic/unemployment.xlsx")

# other variables
google_trends <- read_excel("data/other/google_trends_restaurantes.xlsx")
rain <- read_excel("data/other/rain_proxy.xlsx")
temp <- read_excel("data/other/temperature_data.xlsx")
temp[is.na(temp)] <- 0
rain[is.na(rain)] <- 0
plot(temp$tavg) # no zeros in temp : OK
plot(temp$tmedian) # no zeros in temp : OK- looks better than mean

# Explore data structure
str(sales)
str(eco_growth)
str(fx)
str(inflation)
str(unemployment)
str(google_trends)
str(rain)
str(temp) # this has NaNs, must fill somehow

# create time variables

plot(sales$sales_cop)
plot(sales$bar)
plot(sales$food)
# 2. Wrangle Data--------------
## group data ---------

# sales
## sales monthly
df_sales_m <- sales %>%
  mutate(month = floor_date(date, "month")) %>% # Extract month
  group_by(month) %>%
  summarise(sales_m = sum(sales_cop), bar_m = sum(bar), food_m = sum(food)
            )     # Summing values

head(df_sales_m)

## sales weekly
df_sales_w <- sales %>%
  mutate(week = floor_date(date, "week")) %>% # Extract month
  group_by(week) %>%
  summarise(sales_w = sum(sales_cop), bar_w = sum(bar), food_w = sum(food))     # Summing values

head(df_sales_m)
head(df_sales_w)


# fx
df_fx_m <- fx %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(fx_m = mean(fx))

df_fx_w <- fx %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(fx_w = mean(fx))



head(df_fx_m)
head(df_fx_w)

# google trends

# montly
df_google_m <- google_trends %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(google_m = mean(google_trends))


# weekly
df_google_w <- google_trends %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(google_w = mean(google_trends))

head(df_google_m)
head(df_google_w)


## rain

df_rain_g = rain %>%
  group_by(date, region) %>%
  summarise(rain_sum=sum(contribution_m3s))

df_rain_g  <- df_rain_g[df_rain_g$region=="ANTIOQUIA",]

head(df_rain_g)

# montly
df_rain_m <- df_rain_g %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(rain_m = sum(rain_sum))


# weekly
df_rain_w <- df_rain_g %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(rain_w = sum(rain_sum))

head(df_rain_m)
head(df_rain_w)

# temperature
# montly
df_temp_m <- temp %>%
  mutate(month = floor_date(date, "month")) %>%
  group_by(month) %>%
  summarise(temp_m = mean(tmedian), prcp_m = sum(prcp))


# weekly
df_temp_w <- temp %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(temp_w = mean(tmedian), prcp_w = sum(prcp))

head(df_temp_m)
head(df_temp_w)




## Merge--------------
## daily data----------
#sales, rain, fx are the only ones daily
df_merged_d <- merge(sales, df_rain_g, by = "date", all = FALSE) # Inner join
df_merged_d <- merge(df_merged_d, fx, by = "date", all = FALSE) # Inner join
df_merged_d <- merge(df_merged_d, temp, by = "date", all = FALSE) # Inner join

head(df_merged_d)

### weekly data----------
df_merged_w <- merge(df_sales_w, df_rain_w, by="week", all=F)
df_merged_w <- merge(df_merged_w, df_google_w, by="week", all=F)
df_merged_w <- merge(df_merged_w, df_fx_w, by="week", all=F)
df_merged_w <- merge(df_merged_w, df_temp_w, by="week", all=F)

head(df_merged_w)

### monthly data----------
# change colnames
names(eco_growth) <- c("month", "ise")
names(inflation) <- c("month", "inflation")
names(unemployment) <- c("month", "unemployment") 

df_merged_m <- merge(df_sales_m, df_rain_m, by="month", all=F)
nrow(df_merged_m)
df_merged_m <- merge(df_merged_m, df_fx_m, by="month", all=F)
nrow(df_merged_m)
df_merged_m <- merge(df_merged_m, df_google_m, by="month", all=F)
nrow(df_merged_m)
df_merged_m <- merge(df_merged_m, eco_growth, by="month", all=F) # only has until aug 2024
nrow(df_merged_m)
df_merged_m <- merge(df_merged_m, inflation, by="month", all=F)
nrow(df_merged_m)
df_merged_m <- merge(df_merged_m, unemployment, by="month", all=F)
nrow(df_merged_m)

df_merged_m <- merge(df_merged_m, df_temp_m, by="month", all=F)
nrow(df_merged_m)

# # Export to excel

# library(openxlsx)
# write.xlsx(df_merged_m, file = "df_merged_m.xlsx")
# write.xlsx(df_merged_w, file = "df_merged_w.xlsx")
# write.xlsx(df_merged_d, file = "df_merged_d.xlsx")

# remove everything that is not df_merged_d, df_merged_w, df_merged_m
objects_to_keep <- c("df_merged_d", "df_merged_w", "df_merged_m")

# Remove all objects except those specified
rm(list = setdiff(ls(), objects_to_keep))

# 3. EDA--------------------

## 3.1 Sales----------------
# sales daily
ggplot(df_merged_d, aes(x=date, y=sales_cop)) +
  geom_line() + ggtitle("Daily Sales of Restaurant") +
  theme(plot.title = element_text(hjust=0.5))

# sales weekly
ggplot(df_merged_w, aes(x=week, y=sales_w)) +
  geom_line() + ggtitle("Weekly Sales of Restaurant") +
  theme(plot.title = element_text(hjust=0.5))

# sales montly
ggplot(df_merged_m, aes(x=month, y=sales_m)) +
  geom_line() + ggtitle("Monthly Sales of Restaurant") +
  theme(plot.title = element_text(hjust=0.5))

ggplot(df_merged_m, aes(x = month)) +
  geom_line(aes(y = sales_m, color = "Sales")) +
  geom_line(aes(y = bar_m, color = "Bar")) +
  geom_line(aes(y = food_m, color = "Food")) +
  labs(title = "Time Series of Sales, Bar, and Food",
       x = "Month", y = "Values") +
  scale_color_manual(values = c("Sales" = "blue", "Bar" = "green", "Food" = "red"))

## 3.2 Food-------------------

# sales daily
ggplot(df_merged_d, aes(x=date, y=food)) +
  geom_line() + ggtitle("Daily Sales of Restaurant - Food")
# sales weekly
ggplot(df_merged_w, aes(x=week, y=food_w)) +
  geom_line() + ggtitle("Weekly Sales of Restaurant - Food")

# sales montly
ggplot(df_merged_m, aes(x=month, y=food_m)) +
  geom_line() + ggtitle("Monthly Sales of Restaurant - Food")

## 3.3 Bar------------------------

# sales daily
ggplot(df_merged_d, aes(x=date, y=bar)) +
  geom_line() + ggtitle("Daily Sales of Restaurant - Bar")
# sales weekly
ggplot(df_merged_w, aes(x=week, y=bar_w)) +
  geom_line() + ggtitle("Weekly Sales of Restaurant - Bar")

# sales montly
ggplot(df_merged_m, aes(x=month, y=bar_m)) +
  geom_line() + ggtitle("Monthly Sales of Restaurant - Bar")

## 3.3 Combined Sales------------

#Monthly
# Reshape the data to a long format
df_sales_m_long <- df_merged_m %>%
  pivot_longer(cols = c(bar_m, food_m), names_to = "Category", values_to = "Value")

# Create the stacked bar plot
ggplot(df_sales_m_long, aes(x = month, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Monthly Sales of Restaurant") +
  labs(y = "Sales", x = "Month", fill = "Category") +
  theme_minimal() +
  theme(plot.title = element_text(hjust=0.5)) +

# Weekly
# Reshape the data to a long format
df_sales_w_long <- df_merged_w %>%
  pivot_longer(cols = c(bar_w, food_w), names_to = "Category", values_to = "Value")

# Create the stacked bar plot
ggplot(df_sales_w_long, aes(x = week, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Weekly Sales of Restaurant") +
  labs(y = "Sales", x = "Week", fill = "Category") +
  theme_minimal()


## 3.4 Seasonal plots-------------------------
df_sales_w_filtered <- df_merged_w %>%
  filter(week >= ymd("2021-12-31"))


tseries_w <- ts(df_sales_w_filtered$sales_w , start = c(2022, 1), frequency = 52)
tseries_w
seasonplot(tseries_w, col = rainbow(3), year.labels = TRUE, main = "Seasonal Plot")
text(x = 1, y = max(tseries_w) - 1.5e7, labels = "2024", col = "blue")

#seasonplot monthly
df_sales_m_filtered <- df_merged_m %>%
  filter(month >= ymd("2021-12-31"))

head(df_sales_m_filtered)

tseries_m <- ts(df_sales_m_filtered$sales_m , start = c(2022, 1), frequency = 12)
head(tseries_m)
seasonplot(tseries_m, col = rainbow(3), year.labels = TRUE, main = "Seasonal Plot")
text(x = 1, y = max(tseries_m) - 1e6, labels = "2024", col = "blue")

## 3.5 Density--------------
### 3.5.1 Monthly-------------
# Select the columns of interest
variables <- c("sales_m", "bar_m", "food_m", "rain_m", "fx_m", "google_m",
               "ise", "inflation", "unemployment", "temp_m", "prcp_m")


# Transform the data to long format for ggplot2
df_long_m <- df_merged_m %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value")

# Create the grid of density plots
ggplot(df_long_m, aes(x = Value)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Density Plots of Selected Variables",
       x = "Value", y = "Density") +
  theme_minimal()

### 3.5.2 Weekly--------------------
colnames(df_merged_w)
# Select the columns of interest
variables <- c("sales_w", "bar_w", "food_w", "rain_w", "fx_w", "google_w",
                "temp_w", "prcp_w")



df_long_w <- df_merged_w %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value")

# Create the grid of density plots
ggplot(df_long_w, aes(x = Value)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Density Plots of Selected Variables",
       x = "Value", y = "Density") +
  theme_minimal()

### 3.5.3 Daily ---------------------------
colnames(df_merged_d)
# Select the columns of interest
variables <- c("sales_cop", "bar", "food", "rain_sum", "fx", 
               "tmedian", "prcp")



df_long_d <- df_merged_d %>%
  pivot_longer(cols = all_of(variables), names_to = "Variable", values_to = "Value")

# Create the grid of density plots
ggplot(df_long_d, aes(x = Value)) +
  geom_density(fill = "blue", alpha = 0.4) +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Density Plots of Selected Variables",
       x = "Value", y = "Density") +
  theme_minimal()


## 3.5 Covariates ----------------------
### 3.5.1 economic variables-----------------------
# economic growth
ggplot(df_merged_m, aes(x=month, y=ise)) +
  geom_line() + ggtitle("Monthly activity in Colombia")
# clearly seasonal and trend

# fx
ggplot(df_merged_d, aes(x=date, y=fx)) +
  geom_line() + ggtitle("Daily COP/USD")
# trend but no clear seasonality

# inflation
ggplot(df_merged_m, aes(x=month, y=inflation)) +
  geom_line() + ggtitle("Monthly inflation National")
# business cycles, no tend or seasonality

# unemployment
ggplot(df_merged_m, aes(x=month, y=unemployment)) +
  geom_line() + ggtitle("Montly trailing unemployment Medellin")
# seasonal and trend downwards


### 3.5.2 Other variables

# google trends
ggplot(df_merged_w, aes(x=week, y=google_w)) +
  geom_line() + ggtitle("Weelkly Google trends 'Restaurantes'")
# no clear behaviour, drop in pandemic

# rain
ggplot(df_merged_d, aes(x=date, y=rain_sum)) +
  geom_line() + ggtitle("Daily rain approximated in Antioquia")
# no trend or seasonality clearly

# temperature
ggplot(df_merged_d, aes(x=date, y=tmedian)) +
  geom_line() + ggtitle("Daily Median temperature in Medellin")

# almost stationary

# temperature
ggplot(df_merged_d, aes(x=date, y=tavg)) +
  geom_line() + ggtitle("Daily Average temperature in Medellin")


# this one looks weird, better keep working on median

# precipitation from temp
ggplot(df_merged_d, aes(x=date, y=prcp)) +
  geom_line() + ggtitle("Daily  precipitation in Medellin")
# looks decent

## 3.6 Pairplot-----------------
df_merged_d <- subset(df_merged_d, select = -region)

# daily
ggpairs(df_merged_d, 
        columns = 2:8)
# sales have correl with fx and rain_sum
# weekly
ggpairs(df_merged_w, 
        columns = 2:9)
# sales have correl with rain, google, fx, temp
# bar has more correl with temp

# montly
ggpairs(df_merged_m, 
        columns = 2:12)

# sales correl negative with google, unemployment
# google impacts more food than bar
# temp does not seem to have effect in month level

## 3.7 Correlation -----------------

# Exclude 'date' column
numeric_df_d <- df_merged_d[, sapply(df_merged_d, is.numeric)]
cor_matrix_d <- cor(numeric_df_d, use = "complete.obs")  # Use only complete rows
cor_matrix_d

numeric_df_w <- df_merged_w[, sapply(df_merged_w, is.numeric)]
cor_matrix_w <- cor(numeric_df_w, use = "complete.obs")  # Use only complete rows
cor_matrix_w

numeric_df_m <- df_merged_m[, sapply(df_merged_m, is.numeric)]
cor_matrix_m <- cor(numeric_df_m, use = "complete.obs")  # Use only complete rows
cor_matrix_m

# Plot the Correlation Matrix
par(mfrow=c(1,1))
corrplot(cor_matrix_d, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_w, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_m, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# rain has stronger correl than prcp
# drop prcp beacuse they "are the same"
df_merged_m <- df_merged_m %>% select(-prcp_m)
df_merged_w <- df_merged_w %>% select(-prcp_w)
df_merged_d <- df_merged_d %>% select(-prcp)

# drop avg temp
df_merged_d <- df_merged_d %>% select(-tavg)
colnames(df_merged_d)

### drop everything not on use--------
objects_to_keep <- c("df_merged_d", "df_merged_w", "df_merged_m")
# Remove all objects except those specified
rm(list = setdiff(ls(), objects_to_keep))


# 4. Variable Transformation-------------
## 4.1 Datetime------------- 
# Vars for model
# Month
# Ensure the `month` column is in POSIXct format
df_merged_m$month <- as.POSIXct(df_merged_m$month)

# Create the numeric variable: an evenly increasing number
df_merged_m <- df_merged_m %>%
  arrange(month) %>%  # Ensure data is sorted by month
  mutate(numeric_month = row_number())  # Assign an increasing number

# Create the seasonal variable: the 12 different months as a factor
df_merged_m <- df_merged_m %>%
  mutate(seasonal_month = factor(format(month, "%B"), levels = month.name))  # Month names as ordered factors

# Week
# Ensure the `week` column is in POSIXct format
df_merged_w$week <- as.POSIXct(df_merged_w$week)

# Create the numeric variable: an evenly increasing number
df_merged_w <- df_merged_w %>%
  arrange(week) %>%  # Ensure data is sorted by week
  mutate(numeric_week = row_number())  # Assign an increasing number

# Create the seasonal variable: the 12 different months as a factor
df_merged_w <- df_merged_w %>%
  mutate(seasonal_month = factor(format(week, "%B"), levels = month.name))  # Month names as ordered factors

# Day
# Ensure the `day` column is in POSIXct format
df_merged_d$date <- as.POSIXct(df_merged_d$date)

# Create the numeric variable: an evenly increasing number
df_merged_d <- df_merged_d %>%
  arrange(date) %>%  # Ensure data is sorted by day
  mutate(numeric_day = row_number())  # Assign an increasing number

# Create the seasonal variable: the 12 different months as a factor
df_merged_d <- df_merged_d %>%
  mutate(seasonal_month = factor(format(date, "%B"), levels = month.name))  # Month names as ordered factors

# Create a column indicating the day of the week
df_merged_d <- df_merged_d %>%
  mutate(day_of_week = factor(weekdays(date), levels = c("Monday", "Tuesday", "Wednesday", 
                                                        "Thursday", "Friday", "Saturday", "Sunday")))  # Day of the week as ordered factor

## 4.2 Time series objects--------------
# convert to time series
sales_d_ts <- ts(df_merged_d$sales_cop)
sales_w_ts <- ts(df_merged_w$sales_w)
sales_m_ts <- ts(df_merged_m$sales_m)

par(mfrow=c(1,1))

# Daily
tsdisplay(sales_d_ts)
# is not stationary but has no clear trend
# and seasonality every 7 days

# Weekly
tsdisplay(sales_w_ts)
# not stationary: has trend

# Montly
tsdisplay(sales_m_ts)
# has clear trend, no seasonality

#df_merged_m = subset(df_merged_m, select = -c(month) )



## 4.3 Log transformation----------

# Monthly
df_merged_m <- df_merged_m %>%
  mutate(across(where(is.numeric) & !all_of(c("unemployment", "inflation")), ~ log(. + 1)))

# Weekly
df_merged_w <- df_merged_w %>%
  mutate(across(where(is.numeric), ~ log(. + 1)))

# Daily
# Weekly
df_merged_d <- df_merged_d %>%
  mutate(across(where(is.numeric), ~ log(. + 1)))

#5.  Models----------------

# 5. Models ---------------------------------------------------------------

## Function to create and summarize models------------------
run_model <- function(formula, data, model_name) {
  cat("\nRunning", model_name, "\n")
  model <- lm(formula, data = data)
  print(summary(model))
  par(mfrow = c(2, 2))
  plot(model)
  return(model)
}

# Function to compare models using ANOVA
compare_models <- function(model1, model2, name1, name2) {
  cat("\nComparing Models:", name1, "vs", name2, "\n")
  anova_result <- anova(model1, model2)
  print(anova_result)
  return(anova_result)
}

# Function to add predictions to the dataset
add_predictions <- function(model, data, pred_column) {
  data[[pred_column]] <- predict(model, newdata = data)
  return(data)
}

# Calculate RMSE
# Function to calculate RMSE
calculate_rmse <- function(observed, predicted) {
  rmse <- sqrt(mean((observed - predicted)^2, na.rm = TRUE))
  return(rmse)
}


# function that compares linear models
# Define the function to get R^2 and AIC
get_model_stats <- function(models) {
  # Initialize an empty data frame
  stats <- data.frame(
    Model = character(),
    R2 = numeric(),
    AIC = numeric(),
    stringsAsFactors = FALSE
  )
  
  # Loop through the list of models
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_name <- names(models)[i]
    # Extract R^2 and AIC
    r2 <- summary(model)$r.squared
    aic <- AIC(model)
    # Append to the data frame
    stats <- rbind(stats, data.frame(Model = model_name, R2 = r2, AIC = aic))
  }
  
  return(stats)
}

## 5.1 Linear Models Sales-------------------------------------------------------
### Monthly Models ----------------------------------------------------------

# View Dataframe
head(df_merged_m)

# Model 0: Trend only
ols0 <- run_model(sales_m ~ numeric_month, df_merged_m, "Model 0")
df_merged_m <- add_predictions(ols0, df_merged_m, "predicted_sales0")

# Model 1: Trend + Seasonality
ols1 <- run_model(sales_m ~ numeric_month + seasonal_month, df_merged_m, "Model 1")
df_merged_m <- add_predictions(ols1, df_merged_m, "predicted_sales1")


## Model 2: Backward Stepwise Regression 

# Start with the full model (excluding food and bar)
ols2_full <- lm(
  sales_m ~ numeric_month + seasonal_month + unemployment + ise + fx_m +
    google_m + temp_m + rain_m, 
  data = df_merged_m
)


# Perform backward stepwise regression
ols2_stepwise <- step(
  ols2_full, 
  direction = "backward",
  trace = 1 # Prints the stepwise regression process
)

# Summary of the final stepwise model
summary(ols2_stepwise)

# Add predictions from the final stepwise model
df_merged_m <- add_predictions(ols2_stepwise, df_merged_m, "predicted_sales2")

# Plot Actual vs Predicted Values
ggplot(df_merged_m, aes(x = month)) +
  geom_line(aes(y = exp(sales_m), color = "Actual Sales"), size = 1) +
  geom_line(aes(y = exp(predicted_sales0), color = "Model 0"), linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(predicted_sales1), color = "Model 1"), linetype = "dotted", size = 1) +
  geom_line(aes(y = exp(predicted_sales2), color = "Model 2 Stepwise"), linetype = "dotdash", size = 1) +
  labs(title = "Actual vs Predicted Monthly Sales for All Models",
       x = "Month", y = "Sales", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Models to compare
models <- list(
  "Model trend" = ols0,
  "Model trend + season" = ols1,
  "Model all covariates step" = ols2_stepwise
)

# Get R^2 and AIC for each model
model_stats <- get_model_stats(models)

# View the results
print(model_stats)


# Calculate RMSE for each model
rmse_stats <- data.frame(
  Model = character(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(models)) {
  model_name <- names(models)[i]
  predicted_column <- paste0("predicted_sales", i - 1) # Adjust index as per your data
  rmse <- calculate_rmse(df_merged_m$sales_m, df_merged_m[[predicted_column]])
  rmse_stats <- rbind(rmse_stats, data.frame(Model = model_name, RMSE = rmse))
}

# View RMSE statistics
print(rmse_stats)


### Weekly Models -----------------------------------------------------------
head(df_merged_w)
## Clean Data - Drop rows 1-2 because sales are 0 / was not open yet
df_merged_w <- df_merged_w %>% slice(-1, -2)

## Model 0A: Trend only
ols0w <- run_model(sales_w ~ numeric_week, df_merged_w, "Model 0A")
df_merged_w <- add_predictions(ols0w, df_merged_w, "predicted_sales0")

## Model 1A: Trend + Seasonality
ols1w <- run_model(sales_w ~ numeric_week + seasonal_month, df_merged_w, "Model 1A")
df_merged_w <- add_predictions(ols1w, df_merged_w, "predicted_sales1")


## Model 2A: Experimentation


# Start with the full model (excluding food and bar)
ols2_full_w <- lm(
  sales_w ~ numeric_week + seasonal_month + fx_w +
    google_w + temp_w + rain_w, 
  data = df_merged_w
)


# Perform backward stepwise regression
ols2_stepwise_w <- step(
  ols2_full_w, 
  direction = "backward",
  trace = 1 # Prints the stepwise regression process
)

# Summary of the final stepwise model
summary(ols2_stepwise_w)

# Add predictions from the final stepwise model
df_merged_w <- add_predictions(ols2_stepwise_w, df_merged_w, "predicted_sales2")

# Plot Actual vs Predicted Values
ggplot(df_merged_w, aes(x = week)) +
  geom_line(aes(y = exp(sales_w), color = "Actual Sales"), size = 1) +
  geom_line(aes(y = exp(predicted_sales0), color = "Model 0"), linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(predicted_sales1), color = "Model 1"), linetype = "dotted", size = 1) +
  geom_line(aes(y = exp(predicted_sales2), color = "Model 2 Stepwise"), linetype = "dotdash", size = 1) +
  labs(title = "Actual vs Predicted Weekly Sales for All Models",
       x = "Week", y = "Sales", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Models to compare
models_w <- list(
  "Model trend" = ols0w,
  "Model trend + season" = ols1w,
  "Model all covariates step" = ols2_stepwise_w
)

# Get R^2 and AIC for each model
model_stats_w <- get_model_stats(models_w)

# View the results
print(model_stats_w)


# Calculate RMSE for each model
rmse_stats_w <- data.frame(
  Model = character(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)


for (i in seq_along(models_w)) {
  model_name <- names(models_w)[i]
  predicted_column <- paste0("predicted_sales", i - 1) # Adjust index as per your data
  rmse <- calculate_rmse(df_merged_w$sales_w, df_merged_w[[predicted_column]])
  rmse_stats_w <- rbind(rmse_stats_w, data.frame(Model = model_name, RMSE = rmse)) # Corrected variable
}


# View RMSE statistics
print(rmse_stats_w)




### Daily Models------------------------------------------
head(df_merged_d,25)
# properly start in december
df_merged_d <-  df_merged_d %>%
  filter(date > "2021-11-30")
head(df_merged_d)

## Model 0: Trend only
ols0d <- run_model(sales_cop ~ numeric_day, df_merged_d, "Model 0A")
df_merged_d <- add_predictions(ols0d, df_merged_d, "predicted_sales0")

## Model 1: Trend + Seasonality
ols1d <- run_model(sales_cop ~ numeric_day + seasonal_month + day_of_week, df_merged_d, "Model 1A")
df_merged_d <- add_predictions(ols1d, df_merged_d, "predicted_sales1")

# Model 2: Backward
head(df_merged_d)

# Start with the full model (excluding food and bar)
ols2_full_d <- lm(
  sales_cop ~ numeric_day + seasonal_month + day_of_week + fx +
     tmedian + rain_sum, 
  data = df_merged_d
)
summary(ols2_full_d)

# Perform backward stepwise regression
ols2_stepwise_d <- step(
  ols2_full_d, 
  direction = "backward",
  trace = 1 # Prints the stepwise regression process
)

# Summary of the final stepwise model
summary(ols2_stepwise_d)

# Add predictions from the final stepwise model
df_merged_d <- add_predictions(ols2_stepwise_d, df_merged_d, "predicted_sales2")

# Plot Actual vs Predicted Values
ggplot(df_merged_d, aes(x = date)) +
  geom_line(aes(y = exp(sales_cop), color = "Actual Sales"), size = 1) +
  geom_line(aes(y = exp(predicted_sales0), color = "Model 0"), linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(predicted_sales1), color = "Model 1"), linetype = "dotted", size = 1) +
  geom_line(aes(y = exp(predicted_sales2), color = "Model 2 Stepwise"), linetype = "dotdash", size = 1) +
  labs(title = "Actual vs Predicted Sales for All Models",
       x = "date", y = "Sales", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Models to compare
models_d <- list(
  "Model trend" = ols0d,
  "Model trend + season" = ols1d,
  "Model all covariates step" = ols2_stepwise_d
)

# Get R^2 and AIC for each model
model_stats_d <- get_model_stats(models_d)

# View the results
print(model_stats_d)

# Calculate RMSE for each model on daily data
rmse_stats_d <- data.frame(
  Model = character(),
  RMSE = numeric(),
  stringsAsFactors = FALSE
)

for (i in seq_along(models_d)) {
  model_name <- names(models_d)[i]
  predicted_column <- paste0("predicted_sales", i - 1) # Adjust index as per your data
  rmse <- calculate_rmse(df_merged_d$sales_cop, df_merged_d[[predicted_column]])
  rmse_stats_d <- rbind(rmse_stats_d, data.frame(Model = model_name, RMSE = rmse))
}

# View RMSE statistics for daily data
print(rmse_stats_d)



# 6 Non Linear Models----------------

# re-declare time-series beacause we droped some rows:
# Ensure the 'date' columns are in Date format
df_merged_d$date <- as.Date(df_merged_d$date)
df_merged_w$date <- as.Date(df_merged_w$week)
df_merged_m$date <- as.Date(df_merged_m$month)

# Extract the start date and year for each dataframe
start_d <- min(df_merged_d$date)
start_w <- min(df_merged_w$date)
start_m <- min(df_merged_m$date)

# Extract components for daily, weekly, and monthly start times
start_d_year <- as.numeric(format(start_d, "%Y"))
start_d_day <- as.numeric(format(start_d, "%j")) # Day of the year

start_w_year <- as.numeric(format(start_w, "%Y"))
start_w_week <- as.numeric(format(start_w, "%U")) + 1 # Week number, adding 1 since R starts at week 0

start_m_year <- as.numeric(format(start_m, "%Y"))
start_m_month <- as.numeric(format(start_m, "%m"))

# Declare time series with appropriate frequencies
sales_d_ts <- ts(exp(df_merged_d$sales_cop), start = c(start_d_year, start_d_day), frequency = 365)
sales_w_ts <- ts(exp(df_merged_w$sales_w), start = c(start_w_year, start_w_week), frequency = 52)
sales_m_ts <- ts(exp(df_merged_m$sales_m), start = c(start_m_year, start_m_month), frequency = 12)

food_d_ts <- ts(exp(df_merged_d$food), start = c(start_d_year, start_d_day), frequency = 365)
food_w_ts <- ts(exp(df_merged_w$food_w), start = c(start_w_year, start_w_week), frequency = 52)
food_m_ts <- ts(exp(df_merged_m$food_m), start = c(start_m_year, start_m_month), frequency = 12)

bar_d_ts <- ts(exp(df_merged_d$bar), start = c(start_d_year, start_d_day), frequency = 365)
bar_w_ts <- ts(exp(df_merged_w$bar_w), start = c(start_w_year, start_w_week), frequency = 52)
bar_m_ts <- ts(exp(df_merged_m$bar_m), start = c(start_m_year, start_m_month), frequency = 12)

# Verify the created time series
par(mfrow=c(1,1))
plot(sales_d_ts)
plot(sales_w_ts)
plot(sales_m_ts)


plot(food_d_ts)
plot(food_w_ts)
plot(food_m_ts)

plot(bar_d_ts)
plot(bar_w_ts)
plot(bar_m_ts)


# Function to replace 1s with the mean of previous and next observations
fill_ones <- function(ts_data) {
  # Convert time series to numeric vector
  ts_vec <- as.numeric(ts_data)
  
  # Loop through and replace 1s
  for (i in seq_along(ts_vec)) {
    if (ts_vec[i] == 1) {
      # Check boundaries to avoid indexing issues
      prev_val <- ifelse(i > 1, ts_vec[i - 1], NA)
      next_val <- ifelse(i < length(ts_vec), ts_vec[i + 1], NA)
      
      # Replace with mean of previous and next, ignoring NA
      ts_vec[i] <- mean(c(prev_val, next_val), na.rm = TRUE)
    }
  }
  
  # Return as time series with original attributes
  ts(ts_vec, start = start(ts_data), frequency = frequency(ts_data))
}

# Apply the function 
sales_d_ts <- fill_ones(sales_d_ts)
sales_w_ts <- fill_ones(sales_w_ts)
sales_m_ts <- fill_ones(sales_m_ts)


food_d_ts <- fill_ones(food_d_ts)
food_w_ts <- fill_ones(food_w_ts)
food_m_ts <- fill_ones(food_m_ts)

bar_d_ts <- fill_ones(bar_d_ts)
bar_w_ts <- fill_ones(bar_w_ts)
bar_m_ts <- fill_ones(bar_m_ts)



## 6.1 Difussion Models----------------
## 6.1.1 BASS Model--------------
### Monthly---------
# simple Bass model
bm_m<-BM(sales_m_ts,display = T) # show graphical view of results / display = True

summary(bm_m)


bm_m$coefficients['m'] - sum(sales_m_ts)
# according to this, there are only 1m cop left to sell, this is less than a year / seems wrong

# Fits well but the 30- onward is wierd + sales might not be declining yet
# Still reflects the innovation and copying in some sense
# Also the restaurants rely in word of mouth to reach full stage
# m = 4.664.000.000 COP, i.e 1 mm EUR approx. / The restaurant has sold 3.515.788.885
# according to this only in 1 year it should extinguish sells
# p, innovation: 0.832% indicates that the adoption rate due to external 
# influence is relatively low, but not uncommon for many markets. - it is actually relativly innovative
# q: (8.96%) suggests that imitation plays a larger role than 
# innovation in driving adoption in this market




# Prediction
pred_bm_m<- predict(bm_m, newx=c(1:length(sales_m_ts)))
pred_bm_m <- ts(pred_bm_m, start = start(sales_m_ts), frequency = frequency(sales_m_ts))
pred.inst_bm_m <- make.instantaneous(pred_bm_m)
pred.inst_bm_m <- ts(pred.inst_bm_m, start = start(sales_m_ts), frequency = frequency(sales_m_ts))

# plot
plot(sales_m_ts, type = "p", col = "black", pch = 16, cex = 0.7,
     xlab = "Month", ylab = "Monthly Sales", main = "Actual vs Fitted Sales")

# Add the fitted values as a line
lines(pred.inst_bm_m, col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("Actual Values", "Fitted Values"),
       col = c("black", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))


# check residuals
res_bm_m <- sales_m_ts - pred.inst_bm_m
tsdisplay(res_bm_m)
# residuals have some structure and 2 lag has correl

# RMSE
# Calculate RMSE for Bass Model predictions
rmse_bm_m <- calculate_rmse(observed = sales_m_ts, predicted = pred.inst_bm_m)

# Print the RMSE
cat("RMSE for Bass Model Predictions:", rmse_bm_m, "\n")
### Weekly------------------------------------------------

bm_w<-BM(sales_w_ts,display = T) # show graphical view of results / display = True
summary(bm_w)
bm_w$coefficients['m'] - sum(sales_w_ts)
# results are similar in terms of m, p and w are in other scale 
#because they are in different time stamp
bm_m$coefficients['q'] / bm_w$coefficients['q'] # they are approx 4 times
bm_m$coefficients['p'] / bm_w$coefficients['p'] # they are approx 4 times
# which makes sense

# Prediction
pred_bm_w<- predict(bm_w, newx=c(1:length(sales_w_ts)))
pred_bm_w <- ts(pred_bm_w, start = start(sales_w_ts), frequency = frequency(sales_w_ts))
pred.inst_bm_w <- make.instantaneous(pred_bm_w)
pred.inst_bm_w <- ts(pred.inst_bm_w, start = start(sales_w_ts), frequency = frequency(sales_w_ts))

# plot
plot(sales_w_ts, type = "p", col = "black", pch = 16, cex = 0.7,
     xlab = "Week", ylab = "Weekly Sales", main = "Actual vs Fitted Sales")

# Add the fitted values as a line
lines(pred.inst_bm_w, col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("Actual Values", "Fitted Values"),
       col = c("black", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))


# check residuals
res_bm_w <- sales_w_ts - pred.inst_bm_w
tsdisplay(res_bm_w)
# residuals have some structure and 2 lag has correl
# clear trend and structure in the residuals

# RMSE
# Calculate RMSE for Bass Model predictions
rmse_bm_w <- calculate_rmse(observed = sales_w_ts, predicted = pred.inst_bm_w)

# Print the RMSE
cat("RMSE for Bass Model Predictions:", rmse_bm_w, "\n")



### Daily--------------------------------------

bm_d <- BM(
  sales_d_ts,
  prelimestimates = c(1.2 * sum(sales_d_ts), 0.005, 0.5), # Adjust these estimates
  display = TRUE
)


summary(bm_d)
bm_d$coefficients['m'] - sum(sales_d_ts)
# results are similar in terms of m, p and w are in other scale 
#because they are in different time stamp
bm_w$coefficients['q'] / bm_d$coefficients['q'] # they are approx 7 times
bm_w$coefficients['p'] / bm_d$coefficients['p'] # they are approx 7 times
# which makes sense

# Prediction
pred_bm_d <- predict(bm_d, newx = c(1:length(sales_d_ts)))
pred_bm_d <- ts(pred_bm_d, start = start(sales_d_ts), frequency = frequency(sales_d_ts))
pred.inst_bm_d <- make.instantaneous(pred_bm_d)
pred.inst_bm_d <- ts(pred.inst_bm_d, start = start(sales_d_ts), frequency = frequency(sales_d_ts))

# Plot actual vs fitted sales for daily data
plot(sales_d_ts, type = "p", col = "black", pch = 16, cex = 0.7,
     xlab = "Day", ylab = "Daily Sales", main = "Actual vs Fitted Sales (Daily)")

# Add the fitted values as a line
lines(pred.inst_bm_d, col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("Actual Values", "Fitted Values"),
       col = c("black", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

# Check residuals
res_bm_d <- sales_d_ts - pred.inst_bm_d
tsdisplay(res_bm_d)
# Note: Review residual plots for any trends or structures.

# Calculate RMSE for Bass Model predictions (daily data)
rmse_bm_d <- calculate_rmse(observed = sales_d_ts, predicted = pred.inst_bm_d)

# Print the RMSE
cat("RMSE for Daily Bass Model Predictions:", rmse_bm_d, "\n")



# overall the bass model fits a bell, if we are doing a generalized version
# lets try one that lets us define a more versatile curve

## 6.1.2 GGM-------------
# Runs on DIMORA
# documentation: https://cran.rstudio.com/web/packages/DIMORA/DIMORA.pdf
# bass model preliminary m, p, q for algorithm

# mt argument is the determination of market potential
### Monthly----------------------------------
ggm1 <- GGM(sales_m_ts, mt='base', display = T)
ggm2 <- GGM(sales_m_ts, mt= function(x) pchisq(x,10),display = T)
summary(ggm1)
summary(ggm2)
# try different functions for market potential

ggm3 <- GGM(sales_m_ts, mt= function(x) log(x),display = T)
ggm4 <- GGM(sales_m_ts, mt= function(x) (x)**(1/1.05),display = T)
summary(ggm3)
summary(ggm4)
# predictions

# Prediction using GGM model (example with ggm1)
pred_ggm_m <- predict(ggm1, newx = c(1:length(sales_m_ts)))
pred_ggm_m <- ts(pred_ggm_m, start = start(sales_m_ts), frequency = frequency(sales_m_ts))
pred.inst_ggm_m <- make.instantaneous(pred_ggm_m)
pred.inst_ggm_m <- ts(pred.inst_ggm_m, start = start(sales_m_ts), frequency = frequency(sales_m_ts))

# Plot actual vs fitted sales for monthly data
plot(sales_m_ts, type = "p", col = "black", pch = 16, cex = 0.7,
     xlab = "Month", ylab = "Monthly Sales", main = "Actual vs Fitted Sales (GGM Model)")

# Add the fitted values as a line
lines(pred.inst_ggm_m, col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("Actual Values", "Fitted Values (GGM)"),
       col = c("black", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))


###Analysis of residuals
res_GGM_m<- sales_m_ts - pred.inst_ggm_m
tsdisplay(res_GGM_m)


# Residuals somehow are kind of stationary
# check for stationarity of residuals
adf_test <- adf.test(res_GGM_m)
print(adf_test) # if p-val < alpha, series stationary
# so with this model we achieve stationary series

# check for autocorrelation in residuals
Box.test(res_GGM_m, lag = 10, type = "Ljung-Box") # h0 res indep
# p-val > alpha => fail to reject h0, so residuals seem indep

### Weekly----------------------------------
ggm1_w <- GGM(sales_w_ts, mt='base', display = T)
ggm2_w <- GGM(sales_w_ts, mt= function(x) pchisq(x,25),display = T)
summary(ggm1_w) # this one is better
summary(ggm2_w)
# try different functions for market potential

ggm3_w <- GGM(sales_w_ts, mt= function(x) log(x),display = T)
ggm4_w <- GGM(sales_w_ts, mt= function(x) (x)**(1/1.05),display = T)

summary(ggm3_w)
summary(ggm4_w) # better shaped but less significant

# predictions
pred_ggm_w <- predict(ggm1_w, newx = c(1:length(sales_w_ts)))
pred_ggm_w <- ts(pred_ggm_w, start = start(sales_w_ts), frequency = frequency(sales_w_ts))
pred.inst_ggm_w <- make.instantaneous(pred_ggm_w)
pred.inst_ggm_w <- ts(pred.inst_ggm_w, start = start(sales_w_ts), frequency = frequency(sales_w_ts))

# Plot actual vs fitted sales for weekly data
plot(sales_w_ts, type = "p", col = "black", pch = 16, cex = 0.7,
     xlab = "Week", ylab = "Weekly Sales", main = "Actual vs Fitted Sales (GGM Model)")

# Add the fitted values as a line
lines(pred.inst_ggm_w, col = "red", lwd = 2)

# Add a legend
legend("topleft", legend = c("Actual Values", "Fitted Values (GGM)"),
       col = c("black", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

# Analysis of residuals
res_GGM_w <- sales_w_ts - pred.inst_ggm_w
tsdisplay(res_GGM_w)


# Check for stationarity of residuals
adf_test_w <- adf.test(res_GGM_w)
print(adf_test_w) # if p-value < alpha, series is stationary

# Check for autocorrelation in residuals
box_test_w <- Box.test(res_GGM_w, lag = 10, type = "Ljung-Box")
print(box_test_w) # if p-value > alpha, residuals are independent

# series is stationary supposedely but has correlation

# Calculate RMSE for GGM model predictions (weekly data)
rmse_ggm_w <- calculate_rmse(observed = sales_w_ts, predicted = pred.inst_ggm_w)

# Print the RMSE
cat("RMSE for Weekly GGM Model Predictions:", rmse_ggm_w, "\n")

### Daily----------------------------------

# Scaling the sales data
sales_min <- min(sales_d_ts)
sales_max <- max(sales_d_ts)
sales_scaled <- (sales_d_ts - sales_min) / (sales_max - sales_min)

# View scaled data
summary(sales_scaled)
plot(sales_scaled, type = "l", main = "Scaled Daily Sales", xlab = "Day", ylab = "Scaled Sales")

# Fit GGM models using scaled data
ggm1_d <- GGM(sales_scaled, mt = 'base', display = T)
ggm2_d <- GGM(sales_scaled, mt = function(x) pchisq(x, 10), display = T)
ggm3_d <- GGM(sales_scaled, mt = function(x) log(x), display = T)
ggm4_d <- GGM(sales_scaled, mt = function(x) (x)^(1/1.05), display = T)

# Summarize models
summary(ggm1_d)  # Base model
summary(ggm2_d)  # Chi-squared
summary(ggm3_d)  # Log transformation
summary(ggm4_d)  # Power transformation

# Select the best model (example: ggm1_d)
# Prediction using GGM model
pred_ggm_d <- predict(ggm1_d, newx = c(1:length(sales_scaled)))
pred_ggm_d <- ts(pred_ggm_d, start = start(sales_scaled), frequency = frequency(sales_scaled))
pred.inst_ggm_d <- make.instantaneous(pred_ggm_d)
pred.inst_ggm_d <- ts(pred.inst_ggm_d, start = start(sales_scaled), frequency = frequency(sales_scaled))

# Re-scale predictions back to the original scale
pred_original_scale <- (pred.inst_ggm_d * (sales_max - sales_min)) + sales_min

# Plot actual vs fitted sales (original scale)
plot(sales_d_ts, type = "p", col = "black", pch = 16, cex = 0.7,
     xlab = "Day", ylab = "Daily Sales", main = "Actual vs Fitted Sales (Original Scale)")
lines(pred_original_scale, col = "red", lwd = 2)
legend("topleft", legend = c("Actual Values", "Fitted Values (GGM, Original Scale)"),
       col = c("black", "red"), pch = c(16, NA), lty = c(NA, 1), lwd = c(NA, 2))

# Analysis of residuals
res_GGM_d <- sales_d_ts - pred_original_scale
tsdisplay(res_GGM_d, main = "Residuals of GGM Model")
# residuals are not stationary probably

# Check for stationarity of residuals
adf_test_d <- adf.test(res_GGM_d)
print(adf_test_d)  # If p-value < alpha, series is stationary
# according to this, they are stationary

# Check for autocorrelation in residuals
box_test_d <- Box.test(res_GGM_d, lag = 10, type = "Ljung-Box")
print(box_test_d)  # If p-value > alpha, residuals are independent
# but they have serial correl

# Calculate RMSE for GGM model predictions (original scale)
rmse_original <- calculate_rmse(observed = sales_d_ts, predicted = pred_original_scale)

# Print the RMSE
cat("RMSE for Daily GGM Model Predictions (Original Scale):", rmse_original, "\n")






## 6.1.3 Holt-Winters---------------------
library(forecast)
# We try this model because fits trend and seasonality in a smooth way
#### Monthly------------------------------
autoplot(sales_m_ts)

# adjust timeseries:
sales_m_ts <- ts(sales_m_ts, frequency=12, start=c(2021, 11))

hw1_m<- hw(sales_m_ts, seasonal="additive")
hw2_m<- hw(sales_m_ts, seasonal="multiplicative")

# prediction
fitted_hw1 <- hw1_m$fitted
fitted_hw2 <- hw2_m$fitted

# plot

# Create a data frame for ggplot
plot_data <- data.frame(
  Time = time(sales_m_ts),
  Actual = as.numeric(sales_m_ts),
  Fitted_Additive = as.numeric(hw1_m$fitted),
  Fitted_Multiplicative = as.numeric(hw2_m$fitted)
)

# Melt data for easier ggplot usage
library(reshape2)
plot_data_melted <- melt(plot_data, id.vars = "Time", 
                         variable.name = "Series", 
                         value.name = "Value")

# Plot using ggplot2
ggplot(plot_data_melted, aes(x = Time, y = Value, color = Series)) +
  geom_point(data = subset(plot_data_melted, Series == "Actual"), size = 2) + # Actual values as dots
  geom_line(data = subset(plot_data_melted, Series != "Actual"), size = 1) +  # Fitted values as lines
  labs(
    title = "Actual vs Fitted Values",
    x = "Time",
    y = "Value",
    color = "Series"
  ) +
  scale_color_manual(
    values = c("Actual" = "black", "Fitted_Additive" = "blue", "Fitted_Multiplicative" = "red"),
    labels = c("Actual", "Fitted (Additive)", "Fitted (Multiplicative)")
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    legend.title = element_text(face = "bold")
  )

# residuals
residuals_hw1 <- residuals(hw1_m)  
residuals_hw2 <- residuals(hw2_m)  
tsdisplay(residuals_hw1)
tsdisplay(residuals_hw2)

# Stationarity and Correlation
# check for stationarity of residuals
# additive
adf_test <- adf.test(residuals_hw1) # H0: series is non-stationary
print(adf_test) # if p-val < alpha, series not stationary
# so with this model we achieve stationary series
# multiplicative
adf_test <- adf.test(residuals_hw2) # H0: series is non-stationary
print(adf_test) # if p-val < alpha, series not stationary
# so with this model we achieve stationary series

# additive
# check for autocorrelation in residuals
Box.test(residuals_hw1, lag = 10, type = "Ljung-Box") # h0 res indep
# p-val > alpha =>  Dont reject h0, so residuals are indep

# additive
# check for autocorrelation in residuals
Box.test(residuals_hw2, lag = 10, type = "Ljung-Box") # h0 res indep
# p-val > alpha =>  Dont reject h0, so residuals are indep


# Model Multiplicative follows the data better, 
# and residuals are slightly better

# forecast
# save the forecast of the second model
forecast_hw1 <- forecast(hw1_m, h=12)
forecast_hw2 <- forecast(hw2_m, h=12)

# Forecast plot
# Plot the time series with both forecasts
autoplot(sales_m_ts) +
  autolayer(forecast_hw1$mean, series="Additive Holt-Winters Forecast", PI=F) +
  autolayer(forecast_hw2$mean, series="Multiplicative Holt-Winters Forecast", PI=F) +
  ggtitle("Sales Forecast with Holt-Winters Models") +
  xlab("Time") +
  ylab("Sales") +
  scale_color_manual(
    values=c("Additive Holt-Winters Forecast" = "blue",
             "Multiplicative Holt-Winters Forecast" = "red")
  ) +
  theme_minimal() +
  theme(legend.position = "top", legend.title = element_blank())

# autoplot
autoplot(sales_m_ts)+
  autolayer(hw2_m, series="Holt-Winters' method", PI=F)


# RMSE
# RMSE Calculation for Holt-Winters models
rmse_hw1 <- calculate_rmse(observed = sales_m_ts, predicted = fitted_hw1)
rmse_hw2 <- calculate_rmse(observed = sales_m_ts, predicted = fitted_hw2)

# Print RMSE values
cat("RMSE for Additive Holt-Winters Model:", rmse_hw1, "\n")
cat("RMSE for Multiplicative Holt-Winters Model:", rmse_hw2, "\n")
# multiplicative is better

#### Weekly------------------------------
# Holt-winters packages is only useful until frequency 24 / so no can do for weekly and daily

tsdisplay(sales_m_ts)
autoplot(sales_w_ts)
head(df_merged_w)
head(sales_w_ts)
# adjust timeseries:
sales_w_ts <- ts(sales_w_ts, frequency=52, start=c(2021, 10,31))

# experiment to evaluate max frequency of holt winters method in R
# Find the grouping factor for 24 periods in a year (every 2.1667 weeks per period)
weeks_per_period <- 52 / 24
group <- floor((seq_along(sales_w_ts) - 1) / weeks_per_period)

# Aggregate by summing (or averaging)
sales_bimonthly <- tapply(sales_w_ts, group, sum)

# Create a new time series with frequency 24
sales_bimonthly_ts <- ts(sales_bimonthly, frequency=24, start=c(2021, 10))

autoplot(sales_w_ts)
autoplot(sales_bimonthly_ts)

#hw1_w<- hw(sales_bw_ts, seasonal="additive") # does not work
#hw2_w<- hw(sales_w_ts, seasonal="multiplicative")
hw2_w<- hw(sales_bimonthly_ts, seasonal="multiplicative") # Works
# Since we are particularly interested in weekly ts, 
# we dont continue with HW
autoplot(sales_bimonthly_ts)+
  autolayer(hw2_w, series="Holt-Winters' method", PI=F)

# 7. ARIMA Models----------------------------
## 7.1 Standard ARIMA---------------------------------
## Montly------------------

plot(sales_m_ts)
# see if series is stationary
adf.test(sales_m_ts) #H0, series is non-stationary
# p-val > 0.05 => dont reject, non stationary: series is not stationary
adf.test(diff(sales_m_ts)) #H0, series is non-stationary

# see the acf and pacf
tsdisplay(diff(sales_m_ts))
# PACF suggest AR-1 ?
# ACF suggests MA-1 ?

### Manual ARIMA------------
# ARIMA(p,d,q) = (1,1,0)
arima1_m<- Arima(sales_m_ts, order=c(1,1,0))
summary(arima1_m)

# study residual to see if is a good model
resid1_m<- residuals(arima1_m)
tsdisplay(resid1_m)

# Residuals seem stationary

### Auto-ARIMA------------
arima2_m <- auto.arima(sales_m_ts)

summary(arima2_m)
summary(arima1_m)
# AIC is almost the same, but keep the AR-1

# study residual to see if is a good model
resid2_m<- residuals(arima2_m)
tsdisplay(resid2_m)

## Weekly------------------
# see if series is stationary
adf.test(sales_w_ts) #H0, series is non-stationary
# p-val > 0.05 => dont reject, non stationary: series is not stationary
adf.test(diff(sales_w_ts)) # after diff is sationary

# see the acf and pacf
tsdisplay(diff(sales_w_ts))
# PACF suggest AR-1
#ACF suggest MA-1

### Manual ARIMA------------
# ARIMA(p,d,q) = (1,1,0)
arima1_w<- Arima(sales_w_ts, order=c(1,1,0))
summary(arima1_w)

# study residual to see if is a good model
resid1_w<- residuals(arima1_w)
tsdisplay(resid1_w)

# Residuals seem stationary

### Auto-ARIMA------------
arima2_w <- auto.arima(sales_w_ts)

summary(arima2_w)
summary(arima1_w)
# Autoarima is better

# study residual to see if is a good model
resid2_w<- residuals(arima2_w)
tsdisplay(resid2_w)

## Daily------------------
# see if series is stationary
adf.test(sales_d_ts) #H0, series is non-stationary
# p-val < 0.05 =>  reject non stationary: series might be stationary
# no need for differencing (?)

# see the acf and pacf
tsdisplay(sales_d_ts)
# But has correlation of great order

# ACF and PACF show a lot of seasonality
# try with 2 differences

### Manual ARIMA------------
# ARIMA(p,d,q) = (2,1,0)
arima1_d<- Arima(sales_d_ts, order=c(1,0,1))
summary(arima1_d)

# study residual to see if is a good model
resid1_d<- residuals(arima1_d)
tsdisplay(resid1_d)

# Residuals are not stationary - they have autocorrelation

### Auto-ARIMA------------
arima2_d <- auto.arima(sales_d_ts)

summary(arima2_d) # order (5,1,3)
summary(arima1_d)
# AIC is better in autoarima

# study residual to see if is a good model
resid2_d<- residuals(arima2_d)
tsdisplay(resid2_d)
# resids have autocorrelation still

## 7.2 SARIMA----------------------------
## Daily-------------------------
tsdisplay(sales_d_ts) # 
tsdisplay(diff(sales_d_ts))
sarima1_d<- Arima(sales_d_ts, order=c(0,1,1), seasonal=c(0,0,1))
summary(sarima1_d)

# study residual to see if is a good model
resid1_ds<- residuals(sarima1_d)
tsdisplay(resid1_ds)
# autocorrelation still present


# Fit auto.arima with seasonal components
sarima2_d <- auto.arima(sales_d_ts, seasonal=TRUE)
summary(sarima2_d)
# model 2 is better, lower AIC
resid2_ds<- residuals(sarima2_d)
tsdisplay(resid2_ds)

#still some autocorrelation at lags 5,9
# check for autocorrelation
Box.test(residuals(sarima2_d), lag=10, type="Ljung-Box")
# A low p-value (<0.05) suggests residual autocorrelation.
# Residuals have autocorrelation

# Need to adress by doing SARIMAX
## 7.3 SARIMAX---------------------
### Daily--------------------------

# readefine sales_d_ts
head(df_merged_d)
sales_d_ts <- ts(exp(df_merged_d$sales_cop), frequency=365, start=c(2021, 334))  # 334 is November 30
seasonal_sales_d_ts <- ts(exp(df_merged_d$sales_cop), frequency=7, start=c(2021, 334))  # 334 is November 30
plot(sales_d_ts)
tsdisplay(sales_d_ts,lag.max = 30)
tsdisplay(seasonal_sales_d_ts,lag.max = 30)
# define regresors
# Select specific columns by name
x_regressors_d <- df_merged_d %>% select(rain_sum, fx, tmedian)
# Apply the exponential function to each column
x_regressors_d <- as.data.frame(apply(x_regressors_d, 2, exp))
# Convert to a matrix for ARIMA modeling
x_regressors_d <- as.matrix(x_regressors_d)

# fit the model on sales
# Fit an auto.arima model with seasonal component and external regressors
sarimax_model_d <- auto.arima(
  sales_d_ts,
  seasonal = TRUE,               # Enable seasonal components
  xreg = x_regressors_d          # External regressors
)

# Display the summary of the fitted model
summary(sarimax_model_d)

# Validate residuals
checkresiduals(sarimax_model_d)

# fit the model on seasonal sales
# Fit an auto.arima model with seasonal component and external regressors
sarimax_model_d2 <- auto.arima(
  seasonal_sales_d_ts,
  seasonal = TRUE,               # Enable seasonal components
  xreg = x_regressors_d          # External regressors
)

# Display the summary of the fitted model
summary(sarimax_model_d2)

# Validate residuals
checkresiduals(sarimax_model_d2)
# they still escape the confidence intervals

resid_sarimax2_seasonal <- residuals(sarimax_model_d2)
adf.test(resid_sarimax2_seasonal)
Box.test(resid_sarimax2_seasonal, lag = 10, type = "Ljung-Box")
# Ljung Box indicates resids are white noise, at p-val 0.05
# But ADF Test says the resids are stationary
tsdisplay(resid_sarimax2_seasonal)

# set a search for best model
sarimax_model_d3 <- auto.arima(
  seasonal_sales_d_ts,
  seasonal = TRUE,
  xreg = x_regressors_d,
  max.p = 5, max.q = 5, max.P = 2, max.Q = 2,
  stepwise = FALSE, approximation = FALSE)

# get residuals
resid_sarimax3_seasonal <- residuals(sarimax_model_d3)

# ADF Test for stationarity
adf.test(resid_sarimax3_seasonal)
# are stationary according to adf test

# Ljung-Box Test for autocorrelation
Box.test(resid_sarimax3_seasonal, lag = 10, type = "Ljung-Box")
# serial correlation accoriding to this

# ACF and PACF plots
tsdisplay(resid_sarimax3_seasonal, lag.max = 30)
# we see lags with correlation

checkresiduals(resid_sarimax3_seasonal)

# 8. Model Mixture--------------
## 8.1 GGM + SARIMA----------------
### Weekly------------------------------
#### GGM-------------------------------

summary(ggm1_w) # this one is best model found


pred_GGM_w<- predict(ggm1_w, newx=matrix(1:length(sales_w_ts), ncol=1))
pred_GGM_w.inst<- make.instantaneous(pred_GGM_w)
pred_GGM_w.inst

# set same timeframe for GGM preds
start_time_w <- start(sales_w_ts)  # Get start time from sales_w_ts
frequency_w <- frequency(sales_w_ts)  # Get frequency from sales_w_ts

# Convert pred_GGM to a numeric vector
pred_GGM_w_vec <- unlist(pred_GGM_w.inst)  # Flatten the list to a numeric vector
# Create the time series for pred_GGM
pred_GGM_w_ts <- ts(pred_GGM_w_vec, start = start_time_w, frequency = frequency_w)


plot(sales_w_ts, type= "b",xlab="Week", ylab="Weekly Sales",  pch=16, lty=3, cex=0.6)
lines(pred_GGM_w_ts, col = "red", lty = 2)


#### SARMAX refinement------------------------


fit.sales_w <- fitted(ggm1_w)  # Predicted values from the GGM model

if (length(fit.sales_w) != length(sales_w_ts)) {
  stop("fit.sales_w and sales_w_ts lengths do not match")
}

summary(fit.sales_w) 
length(fit.sales_w) == length(cumsum(sales_w_ts))  # Should return TRUE

fit.sales_w <- scale(fit.sales_w) # scale regresor to make convergence

sales_w_ts_scaled <- scale(cumsum(sales_w_ts))  # Scale the time series because if not will not reach convergence

sarima_w <- Arima(
  sales_w_ts_scaled, 
  order = c(1, 0, 1), 
  seasonal = list(order = c(0, 0, 1), period = 52), 
  xreg = fit.sales_w # this is the GGM fitted values
)

summary(sarima_w)

# get fitted values
# Extract the fitted cumulative values from the SARIMA model
fitted_cumulative <- fitted(sarima_w)

# Reverse scaling transformation to get fitted cumulative values in the original scale
scaling_center <- attr(sales_w_ts_scaled, "scaled:center")
scaling_scale <- attr(sales_w_ts_scaled, "scaled:scale")

fitted_cumulative_original <- fitted_cumulative * scaling_scale + scaling_center

# Convert cumulative fitted values to instantaneous values
fitted_instantaneous <- diff(c(fitted_cumulative_original, NA))  # Add NA to align lengths

# Create a time series object for the fitted instantaneous values
fitted_instantaneous_ts <- ts(
  fitted_instantaneous, 
  start = start(sales_w_ts), 
  frequency = frequency(sales_w_ts)
)

# Check the fitted instantaneous values
plot(fitted_instantaneous_ts)


# plot

# Plot original instantaneous values vs fitted instantaneous values
plot(sales_w_ts, type = "p", col = "blue", pch = 16,
     main = "Original vs Fitted Instantaneous Values",
     xlab = "Time", ylab = "Instantaneous Values")

# Add the fitted instantaneous values as a line
lines(fitted_instantaneous_ts, col = "red", lwd = 3, lty = 1)

# Add legend
legend("topright", legend = c("Original Instantaneous", "Fitted Instantaneous"),
       col = c("blue", "red"), lty = c(NA, 1), pch = c(16, NA), lwd = c(NA, 3))

#### Residuals-----------------------
# Step 1: Extract residuals from the SARIMA model
resid_w <- residuals(sarima_w)

# Step 2: Visualize residuals
# Time series plot of residuals
tsdisplay(resid_w, main = "Residual Diagnostics for SARIMA Model")

# Step 3: Test residuals for stationarity
adf_test <- adf.test(resid_w)
cat("ADF Test p-value:", adf_test$p.value, "\n")

if (adf_test$p.value < 0.05) {
  cat("The residuals are stationary.\n")
} else {
  cat("The residuals are not stationary.\n")
}

# Step 4: Test residuals for white noise (no autocorrelation)

ljung_box_test <- Box.test(resid_w, lag = 20, type = "Ljung-Box")
cat("Ljung-Box Test p-value:", ljung_box_test$p.value, "\n")

if (ljung_box_test$p.value > 0.05) {
  cat("The residuals resemble white noise (uncorrelated).\n")
} else {
  cat("The residuals show significant autocorrelation.\n")
}


#### RMSE for SARIMAX Predictions ####
rmse_sarimax <- calculate_rmse(observed = sales_w_ts, predicted = fitted_instantaneous_ts)

# Print RMSE for SARIMAX
cat("RMSE for SARIMAX Predictions:", rmse_sarimax, "\n")

# 9. Gradient Boosting-----------------------------
# 10. Prophet--------------------------
# TO DO---------------------
# add residuals to best linear models
# get R2, RMSE of best linear models
# Do test on all residuals, DW, JB.....

