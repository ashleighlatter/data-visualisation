# Forecast visualisation - Air passengers
# Data from: https://www.kaggle.com/datasets/georgerocha/airpassengers?resource=download

# Load packages
library(readr)
library(dplyr)
library(fable)
library(feasts)
library(urca)
library(tsibble)
library(plotly)

# load and wrangle data
data <- read_csv("../../Data/AirPassengers.csv") %>% 
  rename(Passengers = `#Passengers`) %>% 
  mutate(Month = yearmonth(Month)) %>%  
  tsibble(index = Month)

# ARIMA forecast
forecast_fit <- data %>% 
  slice(1:132) %>% # remove final 12 months to gauge accuracy
  model(arima = ARIMA(Passengers))

# 12 month forecast
forecast_data <- forecast_fit %>% 
  forecast(h = 12) %>% 
  # Extract confidence intervals
  hilo(level = c(80, 95)) %>% 
  mutate(
    lo_80 = `80%`$lower,
    hi_80 = `80%`$upper,
    lo_95 = `95%`$lower,
    hi_95 = `95%`$upper
  ) %>% 
  select(Month, Forecast_Val = .mean, lo_80, hi_80, lo_95, hi_95)

# Forecast plot ----
plot_ly() %>% 
  # 80% confidence interval
  add_ribbons(
    data = forecast_data,
    x = ~as.Date(Month),
    ymin = ~lo_80,
    ymax = ~hi_80,
    name = "80% CI",
    type = "ribbon",
    fillcolor = "#BDBDBD",
    opacity = 0.75,
    line = list(color = "#BDBDBD", opacity = 0.75)
  ) %>% 
  # 95% confidence interval
  add_ribbons(
    data = forecast_data,
    x = ~as.Date(Month),
    ymin = ~lo_95,
    ymax = ~hi_95,
    name = "95% CI",
    type = "ribbon",
    fillcolor = "#BDBDBD",
    opacity = 0.5,
    line = list(color = "#BDBDBD", opacity = 0.5)
  ) %>%
  # Actuals
  add_trace(
    data = data %>% slice(1:132),
    x = ~as.Date(Month),
    y = ~Passengers,
    name = "Actuals",
    type = "scatter",
    mode = "lines",
    line = list(color = "#6699CC")
  ) %>% 
  # Predicted 
  add_trace(
    data = forecast_data,
    x = ~as.Date(Month),
    y = ~Forecast_Val,
    name = "Predicted",
    type = "scatter", 
    mode = "lines",
    line = list(color = "#FF715B", dash = "dot", opacity = 1)
  ) %>% 
  layout(
    title = "Air Passenger Forecast",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Number of Passengers")
  )

# Forecast plot - without the gap ----

# grab the final actuals value to paste onto the forecast
latest_val <- data %>% 
  slice(132) %>% 
  mutate(
    Forecast_Val = Passengers,
    lo_80 = Passengers,
    hi_80 = Passengers,
    lo_95 = Passengers,
    hi_95 = Passengers
  )

forecast_data <- bind_rows(
  latest_val,
  forecast_data
)

plot_ly() %>% 
  # 80% confidence interval
  add_ribbons(
    data = forecast_data,
    x = ~as.Date(Month),
    ymin = ~lo_80,
    ymax = ~hi_80,
    name = "80% CI",
    type = "ribbon",
    fillcolor = "#BDBDBD",
    opacity = 0.75,
    line = list(color = "#BDBDBD", opacity = 0.75)
  ) %>% 
  # 95% confidence interval
  add_ribbons(
    data = forecast_data,
    x = ~as.Date(Month),
    ymin = ~lo_95,
    ymax = ~hi_95,
    name = "95% CI",
    type = "ribbon",
    fillcolor = "#BDBDBD",
    opacity = 0.5,
    line = list(color = "#BDBDBD", opacity = 0.5)
  ) %>% 
  # Actuals
  add_trace(
    data = data %>% slice(1:132),
    x = ~as.Date(Month),
    y = ~Passengers,
    name = "Actuals",
    type = "scatter",
    mode = "lines",
    line = list(color = "#6699CC")
  ) %>% 
  # Predicted
  add_trace(
    data = forecast_data,
    x = ~as.Date(Month),
    y = ~Forecast_Val,
    name = "Predicted",
    type = "scatter",
    mode = "lines",
    line = list(color = "#FF715B", dash = "dot", opacity = 1)
  ) %>% 
  layout(
    title = "Air Passenger Forecast",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Number of Passengers")
  )

# Forecast & actuals plot ----
plot_ly() %>% 
  # 80% confidence interval
  add_ribbons(
    data = forecast_data,
    x = ~as.Date(Month),
    ymin = ~lo_80,
    ymax = ~hi_80,
    name = "80% CI",
    type = "ribbon",
    fillcolor = "#BDBDBD",
    opacity = 0.75,
    line = list(color = "#BDBDBD", opacity = 0.75)
  ) %>% 
  # 95% confidence interval
  add_ribbons(
    data = forecast_data,
    x = ~as.Date(Month),
    ymin = ~lo_95,
    ymax = ~hi_95,
    name = "95% CI",
    type = "ribbon",
    fillcolor = "#BDBDBD",
    opacity = 0.5,
    line = list(color = "#BDBDBD", opacity = 0.5)
  ) %>% 
  # Actuals
  add_trace(
    data = data,
    x = ~as.Date(Month),
    y = ~Passengers,
    name = "Actuals",
    type = "scatter",
    mode = "lines",
    line = list(color = "#6699CC")
  ) %>% 
  # Predicted
  add_trace(
    data = forecast_data,
    x = ~as.Date(Month),
    y = ~Forecast_Val,
    name = "Predicted",
    type = "scatter",
    mode = "lines",
    line = list(color = "#FF715B", dash = "dot", opacity = 1)
  ) %>% 
  layout(
    title = "Air Passenger Forecast",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Number of Passengers")
  )
