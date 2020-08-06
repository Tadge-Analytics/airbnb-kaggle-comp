
# drake::clean(destroy = T)


source("R/packages.R")  # Load your packages, e.g. library(drake).
source("R/functions.R") # Define your custom code as a bunch of functions.

source("R/plans.R")      # Create your drake plan.


use_parallel(yes_or_no = TRUE)


make(investigation_plan)


drake::build_times(grid_result)





make(evaluation_plan)








# tictoc::tic()
# create_kaggle_submission_file()
# tictoc::toc()
# # takes 1 minute(s)







# vis_drake_graph(plan)










