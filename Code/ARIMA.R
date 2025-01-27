# Getting started ----

library(excessmort) 
library(spatstat.utils)
library(spatstat.data)
library(ggplot2)
library(dplyr)
library(lubridate)
library(tseries)
library(vars)
library(forecast)
library(ggthemes)

# Loading and preparing data ----

# use totaldeaths_excessmort_prepandemic.csv or totaldeaths_excessmort.csv
total.deaths <- read.csv("/Users/.../totaldeaths_excessmort_prepandemic.csv")
head(total.deaths)

# Plotting monthly time series ----
deathsM= ddply(total.deaths, c("date"), summarise,
               deaths = sum(outcome))
deathsM$date <-  as.Date(deathsM$date, format = "%Y-%m-%d")
head(deathsM)


options(repr.plot.width=9, repr.plot.height=6)

ggplot(deathsM, aes(x = date, y = deaths, group = 1)) +
  geom_bar(stat="identity", width = 0.5, colour = "black") +
  stat_smooth(aes(y = deaths), method='auto', level=0.95) + 
  theme_economist()  +
  scale_x_date() +
  xlab("Year") +
  ylab("Cases")

# Decomposition of time series ----
# Change plot size to 8 x 3
options(repr.plot.width=9, repr.plot.height=6)
deaths_tsM <- ts(deathsM$deaths, start = c(2014, 11), frequency =12)
# Monthly ts dataset
Decompose_ts <- decompose(deaths_tsM)
#summary(Decompose_ts)
plot(Decompose_ts)

# Investigating seasonality ----
options(repr.plot.width=9, repr.plot.height=6)
deathsM$Month = month(deathsM$date, label=TRUE)
deathsM$Year = year(deathsM$date)
head(deathsM)
ggplot(deathsM, aes(y=deaths, x = Month)) + geom_boxplot(aes(group=Month), fill = 'grey90') + theme_economist()
ggplot(deathsM, aes(y=deaths, x = Year)) + geom_boxplot(aes(group=Year), fill = 'grey90') + theme_economist()

# Testing stationarity to describe autoregressivity and seasonality
## Tests for stationarity
# Weekly data
deaths_tsM %>% ggtsdisplay(theme = theme_economist())
ndiffs(deaths_tsM) # 1 difference estimated to induce stationarity
nsdiffs(deaths_tsM) # No differencing required to induce stationarity for seasonal component

tail(deaths_tsM)
deaths_tsM %>% diff() %>% ggtsdisplay(theme = theme_economist()) # Assess differenced ts and ACF and PACF plots.

# Fitting auto-ARIMA
Auto_Arima = auto.arima(deaths_tsM)
summary(Auto_Arima)


plot(forecast(Auto_Arima, 24))
print(summary(Auto_Arima)) 
print(confint(Auto_Arima)) 
print(checkresiduals(Auto_Arima))




