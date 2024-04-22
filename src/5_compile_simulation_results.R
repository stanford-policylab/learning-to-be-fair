require(tidyverse)

### Import data ----------------------------------------------------------------
data_dir <- here("data", "run_4")
sim_files <- fs::dir_ls(data_dir, regexp = "\\.rds$")
sim_output <- sim_files %>% 
  map_dfr(read_rds)

data_dir_spending_variations_expensive <- here("data", "run_8")
sim_files_spending_variations_expensive <- 
  fs::dir_ls(data_dir_spending_variations_expensive, regexp = "\\.rds$")
data_dir_spending_variations_cheaper <- here("data", "run_9")
sim_files_spending_variations_cheaper <- 
  fs::dir_ls(data_dir_spending_variations_cheaper, regexp = "\\.rds$")
sim_files_spending_variations <- c(sim_files_spending_variations_expensive,
                                   sim_files_spending_variations_cheaper)
sim_output_spending_variations <- sim_files_spending_variations %>% 
  map_dfr(read_rds)



### Prepare data ---------------------------------------------------------------
training_runs <- sim_output %>% 
  filter(!is_warmup,
         sim_num <= 7311) %>% 
  group_by(sim_num, method) %>% 
  mutate(bestguess_policy_treatment_expected_reward = 
           bestguess_policy_expected_reward - 
           bestguess_policy_control_expected_reward,
         cumulative_transits = cumsum(transit_decision),
         cumulative_rideshares = cumsum(rideshare_decision)) %>% 
  relocate(cumulative_transits, .after = transit_decision) %>% 
  relocate(cumulative_rideshares, .after = rideshare_decision) %>% 
  ungroup() %>% 
  mutate(method = as.character(method), 
         method = str_replace(method, "Egreedy", "ɛ-Greedy"))

num_training <- max(training_runs$person_ix)
num_sims <- length(unique(training_runs$sim_num))

print(glue("Number of simulations: {num_sims}"))