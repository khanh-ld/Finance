
library(quantmod)
library(forecast)
library(tidyquant)

# a vector of stock tickers to look at
s <- tq_index("SP500", use_fallback = TRUE)[,1] %>% as.list() %>% unlist()
e <- new.env()
lapply(s, function(x){
  try(
    getSymbols(
      x,
      env=e),
    silent=TRUE)
}) # download stock prices

e <- eapply(e, Ad)
e <- lapply(e, function(x) x[!is.na(x)]) #omit NA values from e

# lapply loops over every object in the list and applies a function to it.
# our function calculate the forecast of a stock over the next 60 days and compare the last actual price with the furthest lower 80th percentile
# Then it returns TRUE or FALSE depending on whether the last actual stock price is above the furthest lower 80th percentile
# eapply returns a list, which we can `unlist` into a named vector
eighty <- unlist(lapply(e, function(x) {
              forecast_result <- x %>% auto.arima() %>% forecast(h = 60)
              last(x) < last(forecast_result$lower[,1])
              }))
            
  
#total number of stocks scanned
length(eighty)

# get the names where the value is TRUE
names(eighty)[eighty]
# get the names where the value is FALSE
names(eighty)[!eighty]


#code below is only meant to detect data error

lapply(e, function(x) {
  forecast_result <- x %>% auto.arima() %>% forecast(h = 60)
  print(names(x))
})


