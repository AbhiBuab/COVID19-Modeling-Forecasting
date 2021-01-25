library(tidyverse)
library(dplyr)
library(ggplot2)
update.packages('covdata')
library(covdata)
library(covidregionaldata)
library(forecast)
################################################### Isolating Data
data_isolate <- function(df){
    final_df <- ungroup(df) %>%
        select(date, cases_total, deaths_total, recovered_total)
    final_df[is.na(final_df)] <- 0
    return(final_df)  
}
################################################### Computing ES forecast
compute_ARIMA <- function(trim_data){
    cases <- ungroup(trim_data) %>%
        select(cases_total)
    cases_ts <- ts(cases, frequency=1)
    case_forecast <- forecast(HoltWinters(cases_ts, gamma = FALSE),h=7)
    
    deaths <- ungroup(trim_data) %>%
        select(deaths_total)
    deaths_ts <- ts(deaths, frequency=1)
    death_forecast <- forecast(HoltWinters(deaths_ts, gamma = FALSE),h=7)
    return(case_forecast)
}
################################################### Main
region = "Assam"
data <- get_regional_data(country = "India")
data <- data %>%
    filter(state == region)
main_data <- data[40:(nrow(data)-9),]
trim_data <- data_isolate(main_data) %>%
    mutate(days_elapsed = as.numeric(date - min(date), units="days"))

forecasts <- compute_ARIMA(trim_data)


par(mfrow=c(1,1))

plot(forecasts,main = "Case Count + ES Projection",
     xlab = "Days since 1st Measurement", ylab = "Case Count")
legend("topleft", inset=.05,c("Cases","Forecast","80% Confidence",
                              "95% Confidence"),fill=c("black","cornflowerblue",
                                                       "darkgray","gray87"))