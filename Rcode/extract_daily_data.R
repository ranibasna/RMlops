libs = c('tidyquant', 'quantmod', 'lubridate', 'dplyr', 'yaml', 'glue')

sapply(libs[!libs %in% installed.packages()], install.packages)
sapply(libs, require, character.only = T)

# Read parameters file
parameters = read_yaml('configuration/parameters.yaml')[['data_extraction']]

first_date = parameters$first_date
output_path = parameters$output_path
output_filename_data = parameters$output_filename_data


# Create full paths
output_data = glue("{output_path}/{output_filename_data}")

# # get the data, add the RSI indicator, calculate, the daily return, get the direction and remove the NA. 
# AAPL_stock_data <- tq_get("AAPL", from = first_date, to = Sys.Date())
# AAPL_stock_data$rsi <- RSI(AAPL_stock_data$close, n = 14)
# AAPL_stock_data <-  AAPL_stock_data %>% tq_mutate(select = adjusted, mutate_fun = periodReturn, period = "daily", type = "arithmetic" ) %>% 
#               dplyr::mutate(direction = ifelse(daily.returns > 0, 1, 0)) %>% tidyr::drop_na()



# get the data on daily basis
mystock <- tq_get("AAPL", from = first_date, to = Sys.Date())
# convert to weekly data prices
mystock_weeks <- mystock %>%  tq_transmute(select= open:adjusted, mutate_fun = to.period, period = "weeks")
# calculate the weekly returens
mystock_weeks_ret <- mystock_weeks %>% tq_mutate(select = adjusted, mutate_fun = periodReturn, period = "weekly", type = "arithmetic")
# add the indicators MACD, RSI, and Simple moving average 20,50,10
mystock_weeks_ret_ind <- mystock_weeks_ret %>% tq_mutate(select= close, mutate_fun = MACD, nFast= 12, nSlow= 26, nSig= 9, maType= SMA) %>% mutate(diff = macd - signal) %>% tq_mutate(select = close, mutate_fun = RSI, n =14) %>% tq_mutate(select = close, mutate_fun = SMA, n=20) %>% tq_mutate(select = close, mutate_fun = SMA, n=50) %>% tq_mutate(select = close, mutate_fun = SMA, n=150) %>% dplyr::mutate(direction = ifelse(weekly.returns > 0, 1, 0)) %>% tidyr::drop_na()

# data processing
mystock_final_data <- mystock_weeks_ret_ind %>% select(-c(open, high, low))
# lagg the data using lead
mystock_final_data_laged <- mystock_final_data %>% mutate(close_lead = lead(close), date_lead = lead(date)) %>% select(-c(date, close, direction)) %>% tidyr::drop_na()

# Save the file
saveRDS(mystock_final_data_laged, output_data)