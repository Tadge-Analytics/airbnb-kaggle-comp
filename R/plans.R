


investigation_plan <- drake_plan(
  
  # prepare_summarised_sessions = process_raw_sessions_data(),
  
  summarised_sessions = load_summarised_sessions(),
  
  training_data_import = import_training_data(),
  
  prepped_raw_data = prep_raw_data(training_data_import, summarised_sessions),
  
  initial_split = sample_and_split_the_data(prepped_raw_data, 20000),
  
  training_split = training(initial_split),
  
  training_folds = setup_training_folds(training_split),
  
  model_recipe = setup_recipe(training_split),
  
  model_spec = specify_model(),
  
  model_wf = create_workflow(model_recipe, model_spec),
  
  model_configs = setup_config_grid(training_split, configs_to_run_now = 4),
  # model_configs = tibble(mtry = 11L,    trees = 1975L,    min_n = 19L,    tree_depth = 10L,    learn_rate = 0.0615633845352624,    loss_reduction = 6.26915316760277e-08,    sample_size = 0.14764075310668),

  grid_result = tune_with_grid(model_wf, training_folds, model_configs),

  grid_eval = evaluate_the_grid_results(grid_result),

  save_grid_evaluation = save_grid_evaluation_to_rds(grid_eval)

)




evaluation_plan <- drake_plan(
  
  best_model_config = best_config_from_collection(),
  
  finalized_wf = finalize_workflow(readd(model_wf), best_model_config),  # simply puts the model config values (from above) into the workflow
  
  finalized_model_trained = last_fit(finalized_wf, readd(initial_split)),
  
  testing_split_score = get_testing_split_score(finalized_model_trained), 
  
  # we might also use fit_resamples() to get a distribution for our expected unseen data result
  
)






