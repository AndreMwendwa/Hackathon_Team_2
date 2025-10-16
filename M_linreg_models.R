library(tidyverse)
library(MASS)

(data <- read_csv('data_combined_with_dummies_weather.csv'))

# Getting M
alpha_model <- lm(avg_fare ~ avg_trip_distance, data = data, weights = trip_ct)
summary(alpha_model)

# Getting M_values 
intercept <- alpha_model$coefficients[1]
slope <- alpha_model$coefficients[2]

# Generating M's 
data <- data |> 
  mutate(
    base_price_airport = if_else(is_airport_origin == 1, 4.5, 
                                 if_else(is_airport_destination == 1, 4, 0)),
    M = (avg_fare - intercept - base_price_airport) / (slope * avg_trip_distance),
    origin_ward_modified = as_factor(origin_ward_modified),
    dest_ward_modified = as_factor(dest_ward_modified),
    day_of_week = as_factor(day_of_week),
    hour_of_day = as_factor(hour_of_day),
    month_x = as_factor(month_x)
  )

data$origin_ward_modified <- relevel(data$origin_ward_modified, ref="13 - Toronto Centre")
data$dest_ward_modified <- relevel(data$dest_ward_modified, ref="13 - Toronto Centre")

# Linear model 
M_regression <- lm(M ~ avg_trip_distance + origin_demand + dest_demand + precip_amount_mm + origin_ward_modified
                   + dest_ward_modified + hour_of_day + day_of_week + year_trip + is_holiday + avg_waittime + month_x
                   , data=data, weights=trip_ct)
summary(M_regression)


# # Robust regression
# M_reg_robust <- rlm(M ~ avg_trip_distance + origin_demand + dest_demand + precip_amount_mm + origin_ward_modified
#                     + dest_ward_modified + hour_of_day + day_of_week + year_trip + is_holiday + avg_waittime
#                     , data=data, weights=trip_ct)
# summary(M_reg_robust)
