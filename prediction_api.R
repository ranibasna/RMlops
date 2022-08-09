#* @apiTitle stock prediction
#* @param dates
#* @post /stock prediction
#* @get /endpoint_name


function(){
  # get the data on daily basis
  new_data_input <- tq_get("AAPL", from = Sys.Date() - 1300, to = Sys.Date())
  # convert to weekly data prices
  new_data_input <- new_data_input %>%  tq_transmute(select= open:adjusted, mutate_fun = to.period, period = "weeks")
  # calculate the weekly returens
  new_data_input <- new_data_input %>% tq_mutate(select = adjusted, mutate_fun = periodReturn, period = "weekly", type = "arithmetic")
  # add the indicators MACD, RSI, and Simple moving average 20,50,10
  new_data_input <- new_data_input %>% tq_mutate(select= close, mutate_fun = MACD, nFast= 12, nSlow= 26, nSig= 9, maType= SMA) %>% mutate(diff = macd - signal) %>% tq_mutate(select = close, mutate_fun = RSI, n =14) %>% tq_mutate(select = close, mutate_fun = SMA, n=20) %>% tq_mutate(select = close, mutate_fun = SMA, n=50) %>% tq_mutate(select = close, mutate_fun = SMA, n=150) %>% dplyr::mutate(direction = ifelse(weekly.returns > 0, 1, 0)) %>% tidyr::drop_na()
  
  # data processing
  new_data_input <- new_data_input %>% select(-c(open, high, low))
  # lagg the data using lead
  new_data_input <- new_data_input %>% mutate(close_lead = lead(close), date_lead = lead(date)) %>% select(-c(date, close)) %>% tidyr::drop_na()
  
  # filter last week data
  
  new_data_input_laged <- new_data_input %>% filter(date_lead > Sys.Date() - 14)
  
  predictions = xgboost_model_final %>% fit(formula = weekly.returns ~ ., data    = training_df) %>% predict(new_data = new_data_input_laged)
  
  # predictions$prediction_date = Sys.Date()
  
  return(predictions)
}