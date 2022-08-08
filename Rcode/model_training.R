libs = c('yaml', 'dplyr', 'timetk', 'lubridate','xgboost','tidymodels', 'modeltime', 'reticulate','neptune','tidyquant')

sapply(libs[!libs %in% installed.packages()], install.packages)
sapply(libs, require, character.only = T)


# Load Variables
train_data_info =  read_yaml('configuration/parameters.yaml')[['train']]
input = train_data_info$input_data
initial = train_data_info$initial
assess = train_data_info$assess
model_mode = train_data_info$model_mode
n_trees = train_data_info$n_trees
model_engin = train_data_info$model_engin
grid_size = train_data_info$grid_size

# Read Data
stock_data = readRDS(input)
neptune_api_key = Sys.getenv('api_key')


# data spliting with rolling origin (as opposed to cross validation) using rsample package
rolling_df <- rsample::rolling_origin(stock_data, initial = initial, assess = assess, cumulative = FALSE, skip = 0)

#
# XGBoost model specification
xgboost_model <- 
  parsnip::boost_tree(
    mode = model_mode,
    trees = n_trees,
    min_n = tune(),
    tree_depth = tune(),
    learn_rate = tune(),
    loss_reduction = tune()
  ) %>% set_engine(model_engin, objective = "reg:squarederror")

# grid specification
xgboost_params <- 
  dials::parameters(
    min_n(),
    tree_depth(),
    learn_rate(),
    loss_reduction()
  )
#
xgboost_grid <- 
  dials::grid_max_entropy(
    xgboost_params, 
    size = grid_size
  )

# set workflow
xgboost_wf <- 
  workflows::workflow() %>%
  add_model(xgboost_model) %>% 
  add_formula(weekly.returns ~ .)


# hyperparameter tuning
xgboost_tuned <- tune::tune_grid(
  object = xgboost_wf,
  resamples = rolling_df,
  grid = xgboost_grid,
  metrics = yardstick::metric_set(rmse, rsq, mae),
  control = tune::control_grid(verbose = TRUE)
)

#
xgboost_best_params <- xgboost_tuned %>% tune::select_best("rmse")
xgboost_model_best <- xgboost_model %>%  finalize_model(xgboost_best_params)

# split into training and testing datasets. Stratify by weekly return
data_splits <- mystock_final_data_laged %>%  timetk::time_series_split(initial = initial, assess = assess)

training_df <- training(data_split)
test_df <- testing(data_split)

test_prediction <- xgboost_model_best %>%
  # fit the model on all the training data
  fit(
    formula = weekly.returns ~ ., 
    data    = training_df
  ) %>%
  # use the training model fit to predict the test data
  predict(new_data = test_df) %>%
  bind_cols(testing(data_split))

# measure the accuracy of our model using `yardstick`
xgboost_score <- test_prediction %>% yardstick::metrics(weekly.returns, .pred) %>% mutate(.estimate = format(round(.estimate, 2), big.mark = ","))

# Save best model
saveRDS(xgboost_model_best, best_model_output)






