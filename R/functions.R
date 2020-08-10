


# import our training data
import_training_data <- function() { 
  
  vroom::vroom(file_in("data/train_users.csv"))
  
}





# handle the "sessions.csv" data
process_raw_sessions_data <- function(session_raw = vroom::vroom(file_in("data/sessions.zip"))) {
  
  session_raw %>% 
    group_by(user_id) %>% 
    summarise(total_seconds = sum(secs_elapsed, na.rm = T),
              unique_devices = n_distinct(device_type, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(total_seconds = if_else(total_seconds > 0 , log(total_seconds), 0),
           total_seconds = cut(total_seconds, breaks = 5) %>% as.character(),
           unique_devices = if_else(unique_devices > 3, ">3", as.character(unique_devices))) %>% 
    
    write_rds("data/summarised_sessions.rds")
  
}






load_summarised_sessions <- function(){
  
  read_rds(file_in("data/summarised_sessions.rds"))
  
}







prep_raw_data <- function(raw_import = import_training_data(), 
                          summarised_sessions = load_summarised_sessions()) {
  
  set.seed(123)
  
  raw_import %>%
    filter(year(date_account_created) >= 2014) %>% 
    left_join(summarised_sessions, by = c("id" = "user_id"))
  
}





sample_and_split_the_data <- function(prepped_raw_data = readd(prepped_raw_data),
                                      sample_num = 5000) {
  
  
  max_sample_num <- min(nrow(prepped_raw_data), sample_num)
  
  set.seed(123)
  
  prepped_raw_data %>% 
    select(country_destination, 
           id,
           gender,
           age,
           signup_method, 
           language, 
           first_browser, 
           first_affiliate_tracked, 
           country_destination, 
           total_seconds, 
           unique_devices
    ) %>% 
    sample_n(max_sample_num, replace = F) %>% 
    initial_split(prop = 9/10)
  
  # arrange(date_account_created) %>% 
  # initial_time_split(prop = 9/10)
  
}





# these folds have to be a complete splitting of the training_split

setup_training_folds <- function(training_split = readd(training_split)) {
  
  set.seed(123)
  
  training_split %>%
    vfold_cv(v = 2)  
  
  # training_split %>%
  #   bootstraps(
  # # strata = country_destination, 
  # times = 1) %>% 
  #   pluck("splits", 1) %>% 
  #   analysis() %>% 
  #   count(id) %>% 
  #   arrange(desc(n))
  
  
  
  
}





setup_recipe <- function(training_split = readd(training_split)) {
  
  training_split %>%
    recipe(formula = country_destination ~ ., data = .) %>%
    
    update_role(id, new_role = "ID") %>%
    
    step_mutate(age = case_when(age > 90 ~ "too old", 
                                age < 15 ~ "too young",
                                TRUE ~ as.character((age %/% 10)*10))) %>%
    
    step_unknown(
      age,
      first_affiliate_tracked,
      total_seconds,
      unique_devices,
      new_level = "not-provided"
    ) %>%
    
    step_other(
      signup_method, 
      language, 
      first_browser, 
      first_affiliate_tracked
    ) %>%
    
    step_dummy(all_nominal(), -country_destination, -id)
  
}


# setup_recipe() %>% 
#   prep() %>% 
#   juice()
# 
# setup_recipe() %>% 
#   prep() %>% 
#   bake(new_data = testing(readd(initial_split)))



specify_model <- function() {
  
  boost_tree(
    trees = 1800, 
    tree_depth = 10,
    min_n = 22,
    loss_reduction = 1.29,
    sample_size = 0.5,
    mtry = 7,
    learn_rate = tune() ) %>%
    set_engine("xgboost") %>%
    set_mode("classification")
  
}




setup_config_grid <- function(training_split = training(readd(split_data)),
                              total_num_configs = 20, 
                              configs_to_run_now = 1) {
  
  set.seed(123)
  
  the_grid <- grid_latin_hypercube(
    # trees(),
    # tree_depth(),
    # min_n(),
    # loss_reduction(),
    # sample_prop(),
    # finalize(mtry(), training_split),
    learn_rate(),
    size = total_num_configs)
  
  
  # we sample from the complete grid by however many configs we want to try out in the present moment
  # making sure not to do any of the same ones twice
  
  available_to_run <- min(nrow(the_grid), total_num_configs)
  
  if (file.exists("model_config_results.rds")) {
    
    already_tried <- read_rds(file_in("model_config_results.rds"))
    num_tried_already <- nrow(already_tried)
    available_to_run <- available_to_run - num_tried_already
  }  
  
  
  the_grid %>% 
    {if (file.exists("model_config_results.rds")) 
    {anti_join(., already_tried)} else
    {.}} %>% 
    
    {if (file.exists("model_config_results.rds") & configs_to_run_now > available_to_run) 
    {sample_n(., available_to_run, replace = FALSE)}  else 
    {sample_n(., configs_to_run_now, replace = FALSE)} } 
}






create_workflow <- function(model_recipe = readd(model_recipe),
                            model_spec = readd(model_spec)) {
  
  workflows::workflow() %>%
    add_recipe(model_recipe) %>%
    add_model(model_spec)
  
}






use_parallel <- function(yes_or_no = FALSE) {
  
  if (yes_or_no) {
    
    all_cores <- parallel::detectCores(logical = FALSE)
    cl <- makePSOCKcluster(all_cores)
    registerDoParallel(cl)
    
  }
  
  
}





tune_with_grid <- function(model_wf = readd(model_wf),
                           training_folds = readd(training_folds),
                           model_configs = readd(model_configs)) {
  
  if(nrow(model_configs) != 0) {
    
    tune_grid(
      model_wf,
      resamples = training_folds,
      grid = model_configs,
      control = control_grid(save_pred = TRUE,
                             verbose = TRUE)
    )
  }
}






# this function compares our model configs to what we would have gotten if we had just chosen the top 5 destinations
# pct_improvement is the percent towards the competeition's winning entry's accuracy (from the baseline).

# competition asessement metric function (used in the function below)

challenge_metric <- function(truth_col, prediction_col) {
  
  (2^(prediction_col  == truth_col) - 1) / (log(row_number() + 1, base = 2))
  
}



evaluate_the_grid_results <- function(grid_result = readd(grid_result)) {
  
  
  if (!is.null(grid_result)) {
    
    # add complicated model metric to baseline pct_comparison values!!!
    
    grid_result %>% 
      mutate(order_of_dests_in_train = map(splits, ~.x %>% 
                                             training() %>% 
                                             count(country_destination, sort = T) %>% 
                                             head(5) %>% 
                                             pull(1)),
             prediction_vals = map(.predictions, ~.x %>% 
                                     select(.config, .row, country_destination, contains(".pred"), -.pred_class) %>% 
                                     tidyr::gather(prediction, value, -.row, -.config,  -country_destination) %>% 
                                     group_by(.config, .row) %>% 
                                     arrange(desc(value)) %>% 
                                     slice(1:5) %>%
                                     select(-value) %>% 
                                     separate(prediction, sep = "_", into = c(NA, "prediction"))),
             with_train_most_pop = map2(prediction_vals, order_of_dests_in_train, ~.x %>% 
                                          mutate(order_of_dests_in_train = .y)), 
             model_comparisons = map(with_train_most_pop, ~.x %>% 
                                       mutate(test_of_diff = order_of_dests_in_train != prediction, 
                                              baseline_score = challenge_metric(country_destination, order_of_dests_in_train),
                                              prediction_score = challenge_metric(country_destination, prediction)) %>% 
                                       summarise(any_difs = any(test_of_diff),
                                                 baseline_score = sum(baseline_score),
                                                 prediction_score = sum(prediction_score)) %>% 
                                       group_by(.config) %>% 
                                       summarise(test_records = n(),
                                                 num_with_dif = sum(any_difs),
                                                 baseline_score = mean(baseline_score),
                                                 prediction_score = mean(prediction_score)) %>% 
                                       mutate(pct_improvement = 100*(prediction_score - baseline_score)/(0.88697 - 0.85359)))) %>% 
      select(id, model_comparisons) %>% 
      unnest(model_comparisons) %>% 
      group_by(.config) %>% 
      summarise(n_slices = n(), 
                avg_pct_improvement = mean(pct_improvement),
                run_round = now()) %>% 
      left_join(
        
        grid_result %>% 
          tune::collect_metrics() %>% 
          select(.config, 1:last_col(6), .metric, mean) %>%
          spread(.metric, mean)
        , by = ".config"
        
      ) 
  }
}






save_grid_evaluation_to_rds <- function(grid_eval = readd(grid_eval)) {
  
  if (file.exists("model_config_results.rds")) {
    
    read_rds("model_config_results.rds") %>% 
      bind_rows(grid_eval) %>% 
      mutate(.config = paste0("Model", row_number())) %>% 
      write_rds("model_config_results.rds")}  
  
  else {
    
    grid_eval %>% 
      mutate(.config = paste0("Model", row_number())) %>% 
      write_rds("model_config_results.rds")   
    
  }
  
}




###################################################################

# these are the asessment plan's functions


best_config_from_collection <- function() {
  
  read_rds("model_config_results.rds") %>%
    arrange(desc(avg_pct_improvement)) %>% 
    slice(1)
  
  
}






get_testing_split_score <- function(finalized_model_trained = readd(finalized_model_trained)) {
  
  training_top_5 <- finalized_model_trained %>% 
    pluck("splits", 1) %>% 
    analysis() %>% 
    count(country_destination, sort = T) %>%
    head(5) %>%
    pull(country_destination)
  
  
  finalized_model_trained %>% 
    pluck(".predictions", 1) %>% 
    select(-.pred_class) %>% 
    tidyr::gather(prediction, value, -.row, -country_destination) %>%
    group_by(.row) %>%
    arrange(desc(value)) %>%
    slice(1:5) %>%
    select(-value) %>%
    separate(prediction, sep = "_", into = c(NA, "prediction")) %>%
    mutate(baseline_score = challenge_metric(country_destination, training_top_5),
           prediction_score = challenge_metric(country_destination, prediction)) %>%
    summarise(baseline_score = sum(baseline_score),
              prediction_score = sum(prediction_score)) %>%
    summarise(test_records = n(),
              baseline_score = mean(baseline_score),
              prediction_score = mean(prediction_score)) %>%
    mutate(pct_improvement = 100*(prediction_score - baseline_score)/(0.88697 - 0.85359))
  
}








create_kaggle_submission_file <- function() {
  
  set.seed(123)
  
  model_with_full_training <- fit(readd(finalized_wf), data = readd(prepped_raw_data) %>% sample_n(20000))
  
  unseen_data_with_predictions <- vroom::vroom("data/test_users.csv") %>%
    left_join(readd(summarised_sessions), by = c("id" = "user_id")) %>% 
    bind_cols(., predict(model_with_full_training, new_data = ., type = "prob"))
  
  
  unseen_data_with_predictions %>% 
    select(id, contains(".pred")) %>% 
    tidyr::gather(country, value, -id) %>% 
    group_by(id) %>% 
    arrange(desc(value)) %>% 
    slice(1:5) %>%
    select(-value) %>% 
    separate(country, sep = "_", into = c(NA, "country")) %>% 
    write_csv(paste0("output/submission ", today(), ".csv"))
  
}











###################################################################


# # this is only to be used when the metric for assessing each config is "simple"
# # ie, auc or rmse is what we want to use
# 
# evaluate_the_grid_results_simple <- function(grid_result = readd(grid_result)) {
#   
#   if (!is.null(grid_result)) {
#     
#     grid_result %>% 
#       tune::collect_metrics() %>% 
#       select(.config, 1:last_col(6), .metric, mean) %>%
#       spread(.metric, mean)
#     
#   }
# }
# 
# 
# 
# 
# 
# variable_importance_chart <- function(finalized_model_trained = readd(finalized_model_trained)) {
#   
#   finalized_model_trained %>%
#     pull_workflow_fit() %>%
#     vi() %>%
#     mutate(
#       Importance = abs(Importance),
#       Variable = fct_reorder(Variable, Importance)
#     ) %>%
#     ggplot(aes(x = Importance, y = Variable)) +
#     geom_col() +
#     scale_x_continuous(expand = c(0, 0)) +
#     labs(y = NULL)
#   
# }


###################################################################







