# Load the environment for R packages
renv::activate()

require(arm)
require(broom)
require(janitor)
require(tidyverse)
require(tidyselect)
require(glue)
require(parallel)
require(pracma)

theme_set(theme_bw(base_size = 14))

source("src/4_simulation/act_withcosts.R")
source("src/4_simulation/inference_withcosts.R")
source("src/4_simulation/learn_withcosts.R")
source("src/4_simulation/policy_withcosts.R")
source("src/4_simulation/population_withcosts.R")

# Setup the environment for Python / reticulate
Sys.unsetenv("RETICULATE_PYTHON")
library(reticulate)
use_virtualenv("src/4_simulation/nudge_simulation/", 
               required = TRUE)
gen_policy <- import_from_path("gen_policy_with_costs", path = "src/4_simulation/")


empirical_sample <- read_csv("data/scc_simulation_population_masked.csv") 

population_params <- list(param_rideshare_coef = 4,
                          param_transit_log_distance_coef = -0.75,
                          param_rideshare_distance_price_permi = 10,
                          param_transit_price_constant = 7.5,
                          param_max_distance = 20)

budget <- 5 # per person
lambda <- 0.0006

# # Uncomment this for grid search
# fixed_rideshare_pcts <- data.frame(fixed_rideshare_pct = seq(0.02, 0.08, 0.02))
# fixed_transit_pcts   <- data.frame(fixed_transit_pct   = seq(0.02, 0.08, 0.02))
# fixed_treatment_pcts <- 
#   crossing(fixed_rideshare_pcts, fixed_transit_pcts) %>% 
#   filter(fixed_rideshare_pct + fixed_transit_pct <= 1) %>% 
#   mutate(treatment_combo_number = row_number())

experiment_size <- 10 #1000
num_sims        <- 3 #2000
num_cores       <- 10
sim_num_start   <- 1000
save_path       <- "data/simulation_output/" # Trailing slash plz

sims <- seq(sim_num_start, sim_num_start + num_sims - 1)

run_learning <- function(sim_num) {
  
  set.seed(sim_num)
  Sys.sleep(sample(seq(1,num_cores)))
  exp_pop <- generate_samples(population_params, 
                              experiment_size) 
  experiment <- prep_run_experiment(exp_pop, 
                                    budget, 
                                    lambda = lambda, 
                                    seed = sim_num, 
                                    sim_params = population_params)
  
  print(glue("Starting experiments for sim_num {sim_num}"))
  
  # Comment this out for grid search
  treated_pop <- bind_rows(
    # Use these parameters for the main simulation
    experiment(method = "random"),
    experiment(method = "random", ignore_lambda = T),
    experiment(method = "random", stop_early_random = 50),
    experiment(method = "random", stop_early_random = 100),
    experiment(method = "random", stop_early_random = 150),
    experiment(method = "egreedy"),
    experiment(method = "egreedy", ignore_lambda = T),
    experiment(method = "thompson"),
    experiment(method = "thompson", ignore_lambda = T),
    experiment(method = "UCB"),
    experiment(method = "UCB", ignore_lambda = T),
    experiment(method = "oracle"),
    experiment(method = "oracle", ignore_lambda = T)

    # # Use these parameters to replicate Appendix F Paragraph 10 results
    # experiment(method = "UCB", num_sim_samples = 2000),
    # experiment(method = "UCB", num_sim_samples = 500),
    # experiment(method = "UCB")
  ) %>%
  mutate(sim_num = sim_num)

  # # Uncomment this for grid search
  # treated_pop <- fixed_treatment_pcts %>%
  #   group_by(treatment_combo_number) %>%
  #   group_modify(~ experiment(method = "fixed_treatments",
  #                             fixed_rideshare_pct = .x$fixed_rideshare_pct,
  #                             fixed_transit_pct   = .x$fixed_transit_pct)) %>%
  #   ungroup() %>%
  #   select(-treatment_combo_number) %>%
  #   mutate(sim_num = sim_num)
  
  savetime <- make_clean_names(Sys.time())
  sim_pad <- str_pad(sim_num, 5, "left", "0")
  treated_pop %>% 
    write_rds(glue("{save_path}{sim_pad}_{savetime}.rds"))
  
  treated_pop
}

print(Sys.time())
mclapply(sims, run_learning, mc.cores = num_cores) 
print(Sys.time())