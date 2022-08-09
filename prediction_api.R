libs = c('yaml', 'tidyverse','tidymodels','tidyquant')

sapply(libs[!libs %in% installed.packages()], install.packages)
sapply(libs, require, character.only = T)


# Load Variables
predict_data_info =  read_yaml('configuration/parameters.yaml')[['predict']]
model_path = predict_data_info$model_path
training_data_path = predict_data_info$training_data_path
n_trees = predict_data_info$n_trees

#* @apiTitle stock prediction for the next week Apple stock direction
#* Charting Apple stock 
#* @get /plot
#* @serializer contentType list(type='image/png')


function(){
  AAPL <- tq_get("AAPL", from = Sys.Date() - 1300, to = Sys.Date())
  plot <- ggplot(AAPL,aes(x=date, y=close,
                          open = open, high = high, low = low, close = close))+ geom_line(size=1) +
    geom_bbands(ma_fun = SMA, sd = 2, n = 30, linetype = 5)
  file <- "plot.png"
  ggsave(file, plot)
  readBin(file, "raw", n = file.info(file)$size)
}


#* Returns most recent week apple stock direction
#* @get /AppleSharePriice

function(){
  # get the data on daily basis
  new_data_input <- tq_get("AAPL", from = Sys.Date() - 1300, to = Sys.Date())
  # convert to weekly data prices
  new_data_input <- new_data_input %>%  tq_transmute(select= open:adjusted, mutate_fun = to.period, period = "weeks")
  # calculate the weekly returns
  new_data_input <- new_data_input %>% tq_mutate(select = adjusted, mutate_fun = periodReturn, period = "weekly", type = "arithmetic")
  # add the indicators MACD, RSI, and Simple moving average 20,50,10
  new_data_input <- new_data_input %>% tq_mutate(select= close, mutate_fun = MACD, nFast= 12, nSlow= 26, nSig= 9, maType= SMA) %>% mutate(diff = macd - signal) %>% tq_mutate(select = close, mutate_fun = RSI, n =14) %>% tq_mutate(select = close, mutate_fun = SMA, n=20) %>% tq_mutate(select = close, mutate_fun = SMA, n=50) %>% tq_mutate(select = close, mutate_fun = SMA, n=150) %>% dplyr::mutate(direction = ifelse(weekly.returns > 0, 1, 0)) %>% tidyr::drop_na()
  
  # data processing
  new_data_input <- new_data_input %>% select(-c(open, high, low))
  # lag the data using lead
  new_data_input <- new_data_input %>% mutate(close_lead = lead(close), date_lead = lead(date)) %>% select(-c(date, close)) %>% tidyr::drop_na()
  
  # filter last week data
  new_data_input_laged <- new_data_input %>% filter(date_lead > Sys.Date() - 14)
  
  # Load model
  xgboost_model_final = readRDS(model_path)
  # load training data for the model
  training_df = readRDS(training_data_path)
  
  # predictions
  predictions = xgboost_model_final %>% fit(formula = weekly.returns ~ ., data    = training_df) %>% 
                predict(new_data = new_data_input_laged)
  # results
  result <- list(predictions, new_data_input_laged$date_lead)
  return(result)
}

