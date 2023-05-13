# load the dataset
library(readxl)
data <- read_excel("F:\\Users\\DHRUVI SHAH\\OneDrive\\Documents\\Research RBI\\Forecasted_Data.xlsx", sheet = "Rural1")
acf(ts_data$Nondurable)
pacf(ts_data$Nondurable)

library(tseries)
adf.test(ts_data$Nondurable)

# convert the Month column to a Date object
data$month_survey<- as.Date(data$month_survey)

install.packages("forecast")
library(forecast)
auto.arima(data$Nondurable)

# fit an ARIMA model to the data
model <- arima(data$Nondurable, order=c(2,1,1))

# forecast the next 5 months of NE with 95% confidence interval
forecast <- predict(model, n.ahead=5, level=0.95)

# create a vector of dates for the forecast period
forecast_dates <- seq(max(data$month_survey) + 1, length.out=5, by="months")

# combine the actual data and forecast into a single data frame
forecast_data <- data.frame(Month=c(data$month_survey, forecast_dates),
                            NE=c(data$Nondurable, forecast$pred),
                            Type=c(rep("Actual", length(data$Nondurable)), rep("Forecast", 5)))

forecast_data
# add lower and upper confidence interval to the forecast data
forecast_data$lower <- c(data$Nondurable, forecast$pred - 1.96 * forecast$se)
forecast_data$upper <- c(data$Nondurable, forecast$pred + 1.96 * forecast$se)

# plot the data and forecast with confidence interval
library(ggplot2)
ggplot(forecast_data, aes(x=Month, y=NE, color=Type)) +
  geom_line() +
  geom_ribbon(aes(ymin=lower, ymax=upper), alpha=0.2) +
  scale_x_date(date_labels = "%b-%y", date_breaks = "2 month") +
  labs(x="Month", y="NE", color="") +
  theme_bw() +
  annotate("text", x=as.Date("2023-04-01"), y=55, label=" ", color="black", size=5, fontface="bold")

