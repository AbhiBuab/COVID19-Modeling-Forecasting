library(tidyverse)
library(dplyr)
library(ggplot2)
library(covdata)
library(forecast)
library(tensorflow)
library(keras)
library(covidregionaldata)
library(forecast)
################################################### Lagging Function
lag_transform <- function(x, k= 1){
  
  lagged =  c(rep(NA, k), x[1:(length(x)-k)])
  DF = as.data.frame(cbind(lagged, x))
  colnames(DF) <- c( paste0('x-', k), 'x')
  DF[is.na(DF)] <- 0
  return(DF)
}
################################################### Scaling Function
scale_data = function(train, feature_range = c(0, 1)) {
  x = train
  fr_min = feature_range[1]
  fr_max = feature_range[2]
  std_train = ((x - min(x) ) / (max(x) - min(x)  ))
  
  scaled_train = std_train *(fr_max -fr_min) + fr_min
  
  return( list(scaled_train = as.vector(scaled_train), scaled_test = as.vector(scaled_train) ,scaler= c(min =min(x), max = max(x))) )
}
################################################### Inverse Scaler
invert_scaling = function(scaled, scaler, feature_range = c(0, 1)){
  min = scaler[1]
  max = scaler[2]
  t = length(scaled)
  mins = feature_range[1]
  maxs = feature_range[2]
  inverted_dfs = numeric(t)
  
  for( i in 1:t){
    X = (scaled[i]- mins)/(maxs - mins)
    rawValues = X *(max - min) + min
    inverted_dfs[i] <- rawValues
  }
  return(inverted_dfs)
}
################################################### Isolating Data
data_isolate <- function(df){
  final_df <- ungroup(df) %>%
    select(date, cases_total, deaths_total, recovered_total)
  final_df[is.na(final_df)] <- 0
  return(final_df)  
}
################################################### Grab Data
region <- "Tamil Nadu"
data <- get_regional_data(country = "India")
data <- data %>%
  filter(state == region)
main_data <- data[40:(nrow(data)-12),]
trim_data <- ungroup(data_isolate(main_data)) %>%
  mutate(days_elapsed = as.numeric(date - min(date), units="days")) %>%
  select(cases_total)


################################################### Differencing/Lagging Data
data_ts <- ts(trim_data, frequency=1)
diffed_data = diff(data_ts, differences = 1)
lagged_data = lag_transform(diffed_data, 3)

################################################### Splitting/Normalizing
train = lagged_data

Scaled = scale_data(lagged_data, c(-1, 1))

y_train = Scaled$scaled_train[, 2]
x_train = Scaled$scaled_train[, 1]
################################################### Assemble/Compile Model
dim(x_train) <- c(length(x_train), 1, 1)

X_shape2 = dim(x_train)[2]
X_shape3 = dim(x_train)[3]
batch_size = 1                
units = 32            

model <- keras_model_sequential()
model%>%
  layer_lstm(units, batch_input_shape = c(batch_size, X_shape2, X_shape3), return_sequences = TRUE,stateful= TRUE)%>%
  layer_lstm(units) %>%
  layer_dense(units = 1)

model %>% compile(
  loss = 'mean_squared_error',
  optimizer = optimizer_adam( lr= 0.02, decay = 1e-6 ),  
  metrics = c('accuracy')
)
################################################### Train/Predict
Epochs = 12   
for(i in 1:Epochs ){
  model %>% fit(x_train, y_train, epochs=1, batch_size=batch_size, verbose=1, shuffle=FALSE)
  model %>% reset_states()
}
L = 7
scaler = Scaled$scaler
predictions = numeric(L)
prev_val = numeric(L)
prev_val[1] = x_train[length(x_train)]


for(i in 1:L){
  X = prev_val[i]
  dim(X) = c(1,1,1)
  yhat = model %>% predict(X, batch_size=batch_size)
  prev_val[i+1] <- yhat
  # invert scaling
  yhat = invert_scaling(yhat, scaler,  c(-1, 1))
  # invert differencing
  if (i==1) {
    yhat  = yhat + data_ts[length(data_ts)]
  } else {
    yhat  = yhat + predictions[i-1]
  }
  # store
  predictions[i] <- yhat
}
################################################### Visualization
predictions <- as.data.frame(predictions)
pred_forecast <- list(x = data_ts,
                      mean = ts(predictions, frequency=1, start=length(data_ts)+1))
pred_forecast <- structure(pred_forecast, class = 'forecast')
plot(pred_forecast, main = "Case Counts and LSTM Forecast", 
     xlab = "Days since 1st Measurement", ylab = "Case Count")