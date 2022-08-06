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

# get the data, add the RSI indicator, calculate, the daily return, get the direction and remove the NA. 
AAPL_stock_data <- tq_get("AAPL", from = first_date, to = Sys.Date())
AAPL_stock_data$rsi <- RSI(AAPL_stock_data$close, n = 14)
AAPL_stock_data <-  AAPL_stock_data %>% tq_mutate(select = adjusted, mutate_fun = periodReturn, period = "daily", type = "arithmetic" ) %>% 
              dplyr::mutate(direction = ifelse(daily.returns > 0, 1, 0)) %>% tidyr::drop_na()


# Save the file
saveRDS(AAPL_stock_data, output_data)