# COVID19-Modeling-Forecasting
Companion code repository for a COVID-19 data project. Project revolves around the modeling, forecasting, and analysis of the COVID-19 outbreak in India

Code is included for 5 different types of models and is provided in R--> Exponential Smoothing, ARIMA, LSTM, SIR, and Prophet Library
All 5 models are used for forecasting, and the SIR and Prophet models are used for deeper analysis

All 5 models utilize the covidregionaldata R package (CRAN/github) as the base data set --> set contains case count, death count, and recovery count data. The SIR model additionally 
uses annual population data. The Prophet model uses the google human mobility dataset as an additional regressor. It should be noted that the google human mobility report is released
monthly and must be downloaded as a local file in the same directory as the model script. It cannot be imported via CRAN or github+drat.

NOTE: The current CRAN version of covidregionaldata is unpatched and bugged, the package should be updated using github for the time being.

The models were run through the dates April 22, 2020 to August 8, 2020, yielding 108 data points. The models were used to generate a 7-day forecast for real time predicting and a 
70%/30% training set validation forecast.

Exponential Smoothing and ARIMA models are generated/run using the stats and forecast packages
2 versions exist for each model type, one that uses the entire available data set to generate a 7-day forecast and one that uses 70% of the training data to generate a forcast
over the other 30% (for validation). All forecasts are generated with and 80% and 95% confidence interval.

LSTM model is generated/run using the keras package. It features two 30 unit LSTM layers and one dense layer. The model is trained over 10-20 epochs. The model utilizes available
cross validation features and can generate a 7-day forecast. One script is provided. No confidence interval is provided.

SIR model is run off of a custom discrete time script. The model relies on 3 differential equations to track three population compartments (generated off of case data set and population
data). The model generates both forecasts (any time length) and equation rate constants (for analysis). One script is provided. A 95% confidence interval is provided.

Prophet model is run using the facebook prophet library. The model is set up as an additive linear model using an additive additional regressor (mobility data). The model was used to 
generate forecasts, and perform a changepoint analysis. A 95% confidence interval is provided.

Each model has lines that allow the user to choose a timeframe to train the model on as well as a forecast length (in days). The Prophet model can also be altered to display various
outputs such as changepoints. The SIR can be altered to output a raw case count trend + forecast or charts for individual population compartments and rate constants.

Outputs are generated using ggplot or ggplot based utilities.
