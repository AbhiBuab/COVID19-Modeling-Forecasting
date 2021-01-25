library(tidyverse)
library(dplyr)
library(ggplot2)
library(readr)
update.packages('covdata')
library(covdata)
library(covidregionaldata)
library(prophet)
################################################### Isolating Data
data_isolate <- function(df){
  final_df <- ungroup(df) %>%
    select(date, cases_total, deaths_total, recovered_total)
  final_df[is.na(final_df)] <- 0
  return(final_df)  
}
################################################### Fetch/Clean/Structure Mobility
fetch_mobility <- function(region, pred_length){
  mobility <- read_csv(file = "mobility.csv")
  mobility <- mobility %>%
    filter(country_region == "India", sub_region_1 == region) %>%
    select(date, retail_and_recreation_percent_change_from_baseline,
           workplaces_percent_change_from_baseline) %>%
    rename(retail_change = retail_and_recreation_percent_change_from_baseline,
           work_change = workplaces_percent_change_from_baseline)
  mobility <- mobility[68:(nrow(mobility)),]
  future_data <- as.data.frame(cbind(numeric(pred_length),numeric(pred_length),
                                     numeric(pred_length)))
  pattern_data <- tail(mobility,7)
  dates = seq.Date(mobility$date[nrow(mobility)], length.out=pred_length+1,
                   by="day")
  dates = as_tibble(dates)
  dates = dates[-c(1),]
  for(i in 1:pred_length) {
    if(i%%7 == 0) {
      future_data[i,2] <- pattern_data[7,2]
      future_data[i,3] <- pattern_data[7,3]
    }
    else {
      future_data[i,2] <- pattern_data[i%%7,2]
      future_data[i,3] <- pattern_data[i%%7,3]
    }
  }
  future_data[,1] = dates
  future_data <- future_data %>%
    rename(date = V1, retail_change = V2, work_change = V3)
  future_data <- rbind(mobility,future_data)
  return(list(mobility = select(mobility,date,retail_change,work_change),
              future = select(future_data,date,retail_change,work_change)))  
}
################################################### Main
region <- "Rajasthan"
pred_length <- 7
mob_data <- fetch_mobility(region, pred_length)
data <- get_regional_data(country = "India")
data <- data %>%
  filter(state == region)
main_data <- data[40:(nrow(data)-170),] ####5
trim_data <- data_isolate(main_data) %>%
  select(date,cases_total) %>%
  rename(ds = date,
         y = cases_total)

##N = nrow(trim_data)
##data_70 = round(N *0.7, digits = 0)
##test_data  = trim_data[(data_70+1):N, ]
##trim_data = trim_data[1:data_70, ] #70/30 training/testing split


trim_data$retail_change <- as.double(mob_data$mobility$retail_change)
trim_data$work_change <- as.double(mob_data$mobility$work_change)
model <- prophet(changepoint.prior.scale = 0.75, growth = 'linear',
                 changepoint.range = 1, holidays.prior.scale = 0.05)
model <- add_country_holidays(model, country_name = 'India')
model <- add_regressor(model, "retail_change")
model <- add_regressor(model, "work_change")
model <- fit.prophet(model,trim_data)
forecast_param <- make_future_dataframe(model, periods = pred_length)
forecast_param$retail_change <- as.double(mob_data$future$retail_change)
forecast_param$work_change <- as.double(mob_data$future$work_change)
modelfc <- predict(model, forecast_param)
plot(model, modelfc, xlab = 'Months', ylab = 'Case Count') + add_changepoints_to_plot(model)