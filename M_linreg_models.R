library(tidyverse)
library(MASS)

(data <- read_csv('data_combined_with_dummies_weather.csv'))

# Getting M
M_model <- lm(avg_fare ~ avg_trip_distance, data = data, weights = trip_ct)
summary(M_model)

# Getting M_values 
intercept <- M_model$coefficients[1]
slope <- M_model$coefficients[2]

# Generating M's 
data <- data |> 
  mutate(
    base_price_airport = if_else(is_airport_origin == 1, 4.5, 
                                 if_else(is_airport_destination == 1, 4, 0)),
    M = (avg_fare - intercept - base_price_airport) / (slope * avg_trip_distance)
  )
