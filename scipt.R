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
  geom_line() + ggtitle("Daily Sales of Restaurant")

# sales weekly
ggplot(df_merged_w, aes(x=week, y=sales_w)) +
  geom_line() + ggtitle("Weekly Sales of Restaurant")

# sales montly
ggplot(df_merged_m, aes(x=month, y=sales_m)) +
  geom_line() + ggtitle("Monthly Sales of Restaurant")


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
  theme_minimal()

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
tseries_m
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



### Daily Models------------------------------------------
head(df_merged_d,25)
# properly start in december
df_merged_d <-  df_merged_d %>%
  filter(date > "2022-11-30")
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


## 5.2 Linear Model FOOD----------------

### Monthly Models ----------------------------------------------------------

# View Dataframe
head(df_merged_m)

# Model 0: Trend only
ols0 <- run_model(food_m ~ numeric_month, df_merged_m, "Model 0")
df_merged_m <- add_predictions(ols0, df_merged_m, "predicted_food0")

# Model 1: Trend + Seasonality
ols1 <- run_model(food_m ~ numeric_month + seasonal_month, df_merged_m, "Model 1")
df_merged_m <- add_predictions(ols1, df_merged_m, "predicted_food1")


## Model 2: Backward Stepwise Regression 

# Start with the full model
ols2_full <- lm(
  food_m ~ numeric_month + seasonal_month + unemployment + ise + fx_m +
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
df_merged_m <- add_predictions(ols2_stepwise, df_merged_m, "predicted_food2")

# Plot Actual vs Predicted Values
ggplot(df_merged_m, aes(x = month)) +
  geom_line(aes(y = exp(food_m), color = "Actual Food Sales"), size = 1) +
  geom_line(aes(y = exp(predicted_food0), color = "Model 0"), linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(predicted_food1), color = "Model 1"), linetype = "dotted", size = 1) +
  geom_line(aes(y = exp(predicted_food2), color = "Model 2 Stepwise"), linetype = "dotdash", size = 1) +
  labs(title = "Actual vs Predicted Monthly Food Sales for All Models",
       x = "Month", y = "Food Sales", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Weekly Models -----------------------------------------------------------
head(df_merged_w)

# Model 0A: Trend only
ols0w <- run_model(food_w ~ numeric_week, df_merged_w, "Model 0A")
df_merged_w <- add_predictions(ols0w, df_merged_w, "predicted_food0")

# Model 1A: Trend + Seasonality
ols1w <- run_model(food_w ~ numeric_week + seasonal_month, df_merged_w, "Model 1A")
df_merged_w <- add_predictions(ols1w, df_merged_w, "predicted_food1")

## Model 2A: Experimentation

# Start with the full model
ols2_full_w <- lm(
  food_w ~ numeric_week + seasonal_month + fx_w +
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
df_merged_w <- add_predictions(ols2_stepwise_w, df_merged_w, "predicted_food2")

# Plot Actual vs Predicted Values
ggplot(df_merged_w, aes(x = week)) +
  geom_line(aes(y = exp(food_w), color = "Actual Food Sales"), size = 1) +
  geom_line(aes(y = exp(predicted_food0), color = "Model 0"), linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(predicted_food1), color = "Model 1"), linetype = "dotted", size = 1) +
  geom_line(aes(y = exp(predicted_food2), color = "Model 2 Stepwise"), linetype = "dotdash", size = 1) +
  labs(title = "Actual vs Predicted Weekly Food Sales for All Models",
       x = "Week", y = "Food Sales", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

### Daily Models------------------------------------------

# Model 0A: Trend only
ols0d <- run_model(food ~ numeric_day, df_merged_d, "Model 0A")
df_merged_d <- add_predictions(ols0d, df_merged_d, "predicted_food0")

# Model 1A: Trend + Seasonality
ols1d <- run_model(food ~ numeric_day + seasonal_month + day_of_week, df_merged_d, "Model 1A")
df_merged_d <- add_predictions(ols1d, df_merged_d, "predicted_food1")

# Model 2: Backward
head(df_merged_d)

# Start with the full model
ols2_full_d <- lm(
  food ~ numeric_day + seasonal_month + day_of_week + fx +
    tmedian + rain_sum, 
  data = df_merged_d
)

# Perform backward stepwise regression
ols2_stepwise_d <- step(
  ols2_full_d, 
  direction = "backward",
  trace = 1 # Prints the stepwise regression process
)

# Summary of the final stepwise model
summary(ols2_stepwise_d)

# Add predictions from the final stepwise model
df_merged_d <- add_predictions(ols2_stepwise_d, df_merged_d, "predicted_food2")

# Plot Actual vs Predicted Values
ggplot(df_merged_d, aes(x = date)) +
  geom_line(aes(y = exp(food), color = "Actual Food Sales"), size = 1) +
  geom_line(aes(y = exp(predicted_food0), color = "Model 0"), linetype = "dashed", size = 1) +
  geom_line(aes(y = exp(predicted_food1), color = "Model 1"), linetype = "dotted", size = 1) +
  geom_line(aes(y = exp(predicted_food2), color = "Model 2 Stepwise"), linetype = "dotdash", size = 1) +
  labs(title = "Actual vs Predicted Food Sales for All Models",
       x = "Date", y = "Food Sales", color = "Legend") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 6 Non Linear Models----------------
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
pred.inst_bm_m<- make.instantaneous(pred_bm_m)


# Plot of fitted model 
plot(sales_m_ts, type= "b",xlab="month", ylab="Monthly sales",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred.inst_bm_m, lwd=2, col=2)

# check residuals
res_bm_m <- sales_m_ts - pred.inst_bm_m
tsdisplay(res_bm_m)
# clear trend still in residuals

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
pred.inst_bm_w<- make.instantaneous(pred_bm_w)


# Plot of fitted model 
plot(sales_w_ts, type= "b",xlab="month", ylab="Weekly sales",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred.inst_bm_w, lwd=2, col=2)

# check residuals
res_bm_w <- sales_w_ts - pred.inst_bm_w
tsdisplay(res_bm_w)

# clear trend and structure in the residuals

### Daily--------------

bm_d<-BM(sales_d_ts,display = T) # show graphical view of results / display = True
summary(bm_d)
bm_d$coefficients['m'] - sum(sales_d_ts)
# results are similar in terms of m, p and w are in other scale 
#because they are in different time stamp
bm_w$coefficients['q'] / bm_d$coefficients['q'] # they are approx 7 times
bm_w$coefficients['p'] / bm_d$coefficients['p'] # they are approx 7 times
# which makes sense

# Prediction
pred_bm_d<- predict(bm_d, newx=c(1:length(sales_d_ts)))
pred.inst_bm_d<- make.instantaneous(pred_bm_d)


# Plot of fitted model 
plot(sales_d_ts, type= "b",xlab="month", ylab="Daily sales",  pch=16, lty=3, xaxt="n", cex=0.6)
lines(pred.inst_bm_d, lwd=2, col=2)

# check residuals
res_bm_d <- sales_d_ts - pred.inst_bm_d
tsdisplay(res_bm_d)



## 6.1.2 Generalized Bass--------------
#### Mothly--------------------------
# Shock on 4 up
#GBM_m<- GBM(sales_m_ts,shock = "exp",nshock = 1,prelimestimates = c(4.463368e+04, 1.923560e-03, 9.142022e-02, 
#                                                                    ,38,-0.1))

## 6.1.3 GGM-------------
# Runs on DIMORA
# documentation: https://cran.rstudio.com/web/packages/DIMORA/DIMORA.pdf
# bass model preliminary m, p, q for algorithm

# mt argument is the determination of market potential
sp = c(1.69062e+06,2.60513e-03,3.20522e-02,1.00000e-03,1.00000e-01) 
ggm1 <- GGM(sales_m_ts, prelimestimates = sp, display = T)
ggm2 <- GGM(sales_m_ts, mt='base', display = T)
ggm3 <- GGM(sales_m_ts, mt= function(x) pchisq(x,10),display = T)
summary(ggm2)
summary(ggm3)
# try different functions for market potential

ggm4 <- GGM(sales_m_ts, mt= function(x) log(x),display = T)
ggm4 <- GGM(sales_m_ts, mt= function(x) (x)**(1/1.05),display = T)
summary(ggm4)
# predictions

pred_GGM_m<- predict(ggm2, newx=c(1:length(sales_m_ts)))
pred_GGM_m.inst<- make.instantaneous(pred_GGM_m)

plot(sales_m_ts, type= "b",xlab="Month", ylab="Monthly Sales",  pch=16, lty=3, cex=0.6)
lines(pred_GGM_m.inst, lwd=2, col=2)

###Analysis of residuals
res_GGM_m<- sales_m_ts - pred_GGM_m.inst
pred_GGM_m
tsdisplay(res_GGM_m)

plot(c(1:length(res_GGM_m)),res_GGM_m)

# Residuals somehow are kind of stationary
# check for stationarity of residuals
adf_test <- adf.test(res_GGM_m)
print(adf_test) # if p-val < alpha, series stationary
# so with this model we achieve stationary series

# check for autocorrelation in residuals
Box.test(res_GGM_m, lag = 10, type = "Ljung-Box") # h0 res indep
# p-val > alpha => fail to reject h0, so residuals seem indep

# TO DO---------------------
# add residuals to best linear models
# get R2, RMSE of best linear models
# Do test on all residuals, DW, JB.....

