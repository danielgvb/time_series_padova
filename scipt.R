# Project Business Economic and Financial Data
# 2024/2025
# Author: Daniel Gutierrez & Fabio Pimentel
# Sales of DimSum Records, Asian-food restaurant in Medellin, Colombia

# Required Packages--------------------
library(readxl)
library(ggplot2)
library(dplyr)
library(lubridate)
library(corrplot)
library(feasts)
library(tsibble)
library(forecast)
library(tidyr)
library(ggthemes)
setwd("C:/Users/danie/Documents/")
# 1. Import Data--------------------------
# target variable
sales <- read_excel("GitHub/time_series_padova/data/sales/sales_dimsum_31102024.xlsx")

sales[is.na(sales)] <- 0

# economic variables
eco_growth <- read_excel("GitHub/time_series_padova/data/macroeconomic/economic_activity.xlsx")
fx <- read_excel("GitHub/time_series_padova/data/macroeconomic/fx.xlsx")
inflation <- read_excel("GitHub/time_series_padova/data/macroeconomic/inflation.xlsx")
unemployment <- read_excel("GitHub/time_series_padova/data/macroeconomic/unemployment.xlsx")

# other variables
google_trends <- read_excel("GitHub/time_series_padova/data/other/google_trends_restaurantes.xlsx")
rain <- read_excel("GitHub/time_series_padova/data/other/rain_proxy.xlsx")
temp <- read_excel("GitHub/time_series_padova/data/other/temperature_data.xlsx")
temp[is.na(temp)] <- 0
plot(temp$tavg) # no zeros in temp : OK

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
  summarise(temp_m = mean(tavg), prcp_m = sum(prcp))


# weekly
df_temp_w <- temp %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(temp_w = mean(tavg), prcp_w = sum(prcp))

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
# install.packages("openxlsx")
# library(openxlsx)
# write.xlsx(df_merged_m, file = "df_merged_m.xlsx")
# write.xlsx(df_merged_w, file = "df_merged_w.xlsx")
# write.xlsx(df_merged_d, file = "df_merged_d.xlsx")

# 3. Plots----------------
# sales daily
ggplot(sales, aes(x=date, y=sales_cop)) +
  geom_line() + ggtitle("Daily Sales of Restaurant")
# sales weekly
ggplot(df_sales_w, aes(x=week, y=sales_w)) +
  geom_line() + ggtitle("Weekly Sales of Restaurant")

# sales montly
ggplot(df_sales_m, aes(x=month, y=sales_m)) +
  geom_line() + ggtitle("Monthly Sales of Restaurant")

#Stacked sales

#Monthly
# Reshape the data to a long format
df_sales_m_long <- df_sales_m %>%
  pivot_longer(cols = c(bar_m, food_m), names_to = "Category", values_to = "Value")

# Create the stacked bar plot
ggplot(df_sales_m_long, aes(x = month, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Monthly Sales of Restaurant") +
  labs(y = "Sales", x = "Month", fill = "Category") +
  theme_minimal()

# Weekly
# Reshape the data to a long format
df_sales_w_long <- df_sales_w %>%
  pivot_longer(cols = c(bar_w, food_w), names_to = "Category", values_to = "Value")

# Create the stacked bar plot
ggplot(df_sales_w_long, aes(x = week, y = Value, fill = Category)) +
  geom_bar(stat = "identity", position = "stack") +
  ggtitle("Weekly Sales of Restaurant") +
  labs(y = "Sales", x = "Week", fill = "Category") +
  theme_minimal()



# Seasonal plots
df_sales_w_filtered <- df_sales_w %>%
  filter(week >= ymd("2021-12-31"))



tseries_w <- ts(df_sales_w_filtered$sales_w , start = c(2022, 1), frequency = 52)
tseries_w
seasonplot(tseries_w, col = rainbow(3), year.labels = TRUE, main = "Seasonal Plot")
text(x = 1, y = max(tseries_w) - 1.5e7, labels = "2024", col = "blue")

#seasonplot monthly
head(df_sales_m)
df_sales_m_filtered <- df_sales_m %>%
  filter(month >= ymd("2021-12-31"))

head(df_sales_m_filtered)

tseries_m <- ts(df_sales_m_filtered$sales_m , start = c(2022, 1), frequency = 12)
tseries_m
seasonplot(tseries_m, col = rainbow(3), year.labels = TRUE, main = "Seasonal Plot")
text(x = 1, y = max(tseries_m) - 1e6, labels = "2024", col = "blue")

# economic growth
ggplot(eco_growth, aes(x=date, y=ise_original)) +
  geom_line() + ggtitle("Monthly activity in Colombia")

# fx
ggplot(fx, aes(x=date, y=fx)) +
  geom_line() + ggtitle("Daily COP/USD")

# inflation
ggplot(inflation, aes(x=date, y=inflation)) +
  geom_line() + ggtitle("Monthly inflation National")

# unemployment
ggplot(unemployment, aes(x=date, y=unemployment)) +
  geom_line() + ggtitle("Montly trailing unemployment Medellin")

# google trends
ggplot(google_trends, aes(x=date, y=google_trends)) +
  geom_line() + ggtitle("Weelkly Google trends 'Restaurantes'")

# rain
ggplot(df_rain_g, aes(x=date, y=rain_sum)) +
  geom_line() + ggtitle("Daily rain approximated in Antioquia")

# temperature
ggplot(temp, aes(x=date, y=tavg)) +
  geom_line() + ggtitle("Daily Average temperature in Medellin")

# precipitation from temp
ggplot(temp, aes(x=date, y=prcp)) +
  geom_line() + ggtitle("Daily  precipitation in Medellin")



# 4. EDA------------
## Correlation -------------

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


### Plots----------
# Plot the Correlation Matrix
corrplot(cor_matrix_d, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_w, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_m, method = "color", type = "upper", tl.col = "black", tl.srt = 45)



# Variable Transformation-------------
# Vars for model
# Ensure the `month` column is in POSIXct format
df_merged_m$month <- as.POSIXct(df_merged_m$month)

# Create the numeric variable: an evenly increasing number
df_merged_m <- df_merged_m %>%
  arrange(month) %>%  # Ensure data is sorted by month
  mutate(numeric_month = row_number())  # Assign an increasing number

# Create the seasonal variable: the 12 different months as a factor
df_merged_m <- df_merged_m %>%
  mutate(seasonal_month = factor(format(month, "%B"), levels = month.name))  # Month names as ordered factors

# View the updated dataframe
head(df_merged_m)


tail(df_merged_m)

#df_merged_m = subset(df_merged_m, select = -c(month) )
## Log transformation----------
df_merged_m <- df_merged_m %>%
  mutate(across(where(is.numeric) & !all_of(c("unemployment", "inflation")), ~ log(. + 1)))


## Autocorrelation--------------
# convert to time series
sales_d_ts <- ts(df_merged_d$sales_cop)
sales_w_ts <- ts(df_merged_w$sales_w)
sales_m_ts <- ts(df_merged_m$sales_m)

par(mfrow=c(1,1))
# Daily
tsdisplay(sales_d_ts)
# is not stationary but has no clear trend

# Weekly
tsdisplay(sales_w_ts)
# not stationary: has trend and seasonality maybe

# Montly
tsdisplay(sales_m_ts)
# has clear trend, no seasonality


#5.  Models----------------
## Linear models-----------
### Monthly----------------
# Model 0, just trend
ols0 <- lm(sales_m ~ month, data=df_merged_m)
summary(ols0)
par(mfrow = c(2,2))
plot(ols0)
df_merged_m$predicted_sales0 <- predict(ols0, newdata = df_merged_m)


# month is significant, but poor fitting

# Model 1: trend + season
colnames(df_merged_m)
ols1 <- lm(sales_m ~numeric_month + seasonal_month, data=df_merged_m)
summary(ols1)
# only time is relevant, no seasonality
plot(ols1)

df_merged_m$predicted_sales1 <- predict(ols1, newdata = df_merged_m)

# Plot actual vs predicted values for both models
ggplot(df_merged_m, aes(x = month)) +
  geom_line(aes(y = sales_m, color = "Actual Sales"), size = 1) +
  geom_line(aes(y = predicted_sales0, color = "Predicted Sales (Model 0)"), linetype = "dashed", size = 1) +
  geom_line(aes(y = predicted_sales1, color = "Predicted Sales (Model 1)"), linetype = "dotted", size = 1) +
  labs(title = "Actual vs Predicted Monthly Sales",
       x = "Month",
       y = "Sales",
       color = "Legend") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Model 2
# Initial model (ols2_full)
ols2_full <- lm(sales_m ~ numeric_month + seasonal_month + unemployment + ise + fx_m + google_m + temp_m + prcp_m, data = df_merged_m)
summary(ols2_full)

# Drop seasonal_month (rename model to ols2_no_seasonal_month)
ols2_no_seasonal_month <- lm(sales_m ~ numeric_month + unemployment + ise + fx_m + google_m + temp_m + prcp_m, data = df_merged_m)
summary(ols2_no_seasonal_month)

# Perform ANOVA test between ols2_full and ols2_no_seasonal_month
anova(ols2_full, ols2_no_seasonal_month)
# P-value > 0.05: can drop seasonal comp

# Drop ise (rename model to ols2_no_seasonal_month_no_ise)
ols2_no_seasonal_month_no_ise <- lm(sales_m ~ numeric_month + unemployment + fx_m + google_m + temp_m + prcp_m, data = df_merged_m)
summary(ols2_no_seasonal_month_no_ise)

# Perform ANOVA test between ols2_no_seasonal_month and ols2_no_seasonal_month_no_ise
anova(ols2_no_seasonal_month, ols2_no_seasonal_month_no_ise)
# p-value > 0.05: drop ISE

# Drop unemployment (rename model to ols2_no_seasonal_month_no_ise_no_unemployment)
ols2_no_seasonal_month_no_ise_no_unemployment <- lm(sales_m ~ numeric_month + fx_m + google_m + temp_m + prcp_m, data = df_merged_m)
summary(ols2_no_seasonal_month_no_ise_no_unemployment)

# Perform ANOVA test between ols2_no_seasonal_month_no_ise and ols2_no_seasonal_month_no_ise_no_unemployment
anova(ols2_no_seasonal_month_no_ise, ols2_no_seasonal_month_no_ise_no_unemployment)
# p-value > 0.05: drop unemployment

ols2_final <- ols2_no_seasonal_month_no_ise_no_unemployment
summary(ols2_final)
# residuals have some structure, there is something else going on
plot(ols2_final)
# predictions
df_merged_m$predicted_sales2 <- predict(ols2_final, newdata = df_merged_m)
# plot of actual vs predicted values
ggplot(df_merged_m, aes(x = month)) +
  geom_line(aes(y = sales_m, color = "Actual Sales"), size = 1) +
  geom_line(aes(y = predicted_sales2, color = "Predicted Sales (Model 2)"), linetype = "dashed", size = 1) +
  labs(title = "Actual vs Predicted Monthly Sales (Model 2)",
       x = "Month",
       y = "Sales",
       color = "Legend") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df_merged_m, aes(x = month)) +
  geom_line(aes(y = sales_m, color = "Actual Sales"), size = 1) +
  geom_line(aes(y = predicted_sales0, color = "Predicted Sales (Model 0)"), linetype = "dashed", size = 1) +
  geom_line(aes(y = predicted_sales1, color = "Predicted Sales (Model 1)"), linetype = "dotted", size = 1) +
  geom_line(aes(y = predicted_sales2, color = "Predicted Sales (Model 2)"), linetype = "dotdash", size = 1) +
  labs(title = "Actual vs Predicted Monthly Sales for Models 0, 1, and 2",
       x = "Month",
       y = "Sales",
       color = "Legend") +
  theme_economist() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


### Weekly----------------
colnames(df_merged_w)
# Create the seasonal_month column based on the week column
df_merged_w <- df_merged_w %>%
  mutate(seasonal_month = factor(format(week, "%m")))

# Optional: Convert the factor to have month names instead of numbers
df_merged_w <- df_merged_w %>%
  mutate(seasonal_month = factor(format(week, "%B"), levels = month.name))

# Model 1A
colnames(df_merged_w)
ols1w <- lm(sales_w ~week + seasonal_month, data=df_merged_w)
summary(ols1w)
plot(ols1w)

## Time Series Models--------------
# 
