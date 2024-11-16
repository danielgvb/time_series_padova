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

# Import Data--------------------------
# target variable
sales <- read_excel("GitHub/time_series_padova/data/sales/sales_dimsum_31102024.xlsx")
# economic variables
eco_growth <- read_excel("GitHub/time_series_padova/data/macroeconomic/economic_activity.xlsx")
fx <- read_excel("GitHub/time_series_padova/data/macroeconomic/fx.xlsx")
inflation <- read_excel("GitHub/time_series_padova/data/macroeconomic/inflation.xlsx")
unemployment <- read_excel("GitHub/time_series_padova/data/macroeconomic/unemployment.xlsx")

# other variables
google_trends <- read_excel("GitHub/time_series_padova/data/other/google_trends_restaurantes.xlsx")
rain <- read_excel("GitHub/time_series_padova/data/other/rain_proxy.xlsx")
temp <- read_excel("GitHub/time_series_padova/data/other/temperature_data.xlsx")

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


# group data ---------

# sales
## sales monthly
df_sales_m <- sales %>%
  mutate(month = floor_date(date, "month")) %>% # Extract month
  group_by(month) %>%
  summarise(sales_m = sum(sales_cop))     # Summing values

## sales weekly
df_sales_w <- sales %>%
  mutate(week = floor_date(date, "week")) %>% # Extract month
  group_by(week) %>%
  summarise(sales_w = sum(sales_cop))     # Summing values

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
  summarise(temp_m = mean(tavg))


# weekly
df_temp_w <- temp %>%
  mutate(week = floor_date(date, "week")) %>%
  group_by(week) %>%
  summarise(temp_w = mean(tavg))

head(df_temp_m)
head(df_temp_w)


# View data
# sales daily
ggplot(sales, aes(x=date, y=sales_cop)) +
  geom_line() + ggtitle("Daily Sales of Restaurant")
# sales weekly
ggplot(df_sales_w, aes(x=week, y=sales_w)) +
  geom_line() + ggtitle("Weekly Sales of Restaurant")

# sales montly
ggplot(df_sales_m, aes(x=month, y=sales_m)) +
  geom_line() + ggtitle("Monthly Sales of Restaurant")


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

# rain
ggplot(temp, aes(x=date, y=tavg)) +
  geom_line() + ggtitle("Daily Average temperature in Medellin")



# Merge--------------
## daily data----------
#sales, rain, fx are the only ones daily
df_merged_d <- merge(sales, df_rain_g, by = "date", all = FALSE) # Inner join
df_merged_d <- merge(df_merged_d, fx, by = "date", all = FALSE) # Inner join
head(df_merged_d)

## weekly data----------
df_merged_w <- merge(df_sales_w, df_rain_w, by="week", all=F)
df_merged_w <- merge(df_merged_w, df_google_w, by="week", all=F)
df_merged_w <- merge(df_merged_w, df_fx_w, by="week", all=F)

head(df_merged_w)

## monthly data----------
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

# Metrics -------------


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


# Plots----------


# Plot the Correlation Matrix
corrplot(cor_matrix_d, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_w, method = "color", type = "upper", tl.col = "black", tl.srt = 45)
corrplot(cor_matrix_m, method = "color", type = "upper", tl.col = "black", tl.srt = 45)

# Models--------------
## Linear model-----------
ols1 <- lm(sales_m ~., data=numeric_df_m)
ols2 <- lm(sales_m ~ unemployment, data=numeric_df_m)
ols2 <- lm(sales_m ~ unemployment + rain_m, data=numeric_df_m)
ols3 <- lm(sales_w ~ rain_w + google_w, data=numeric_df_w)

summary(ols1)
summary(ols2)
summary(ols3)
mflow(par = c(2,2))
