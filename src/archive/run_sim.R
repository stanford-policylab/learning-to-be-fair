require(arm)
require(broom)
require(janitor)
require(tidyverse)
require(tidyselect)
require(glue)
require(scales)
require(parallel)
require(pracma)

theme_set(theme_bw(base_size = 14))

source("population.R")
source("inference.R")
source("policy.R")
source("learn.R")
source("act.R")

# LP solver setup
library(reticulate)
use_virtualenv('~/.pyenv/versions/nudge_simulation', required = TRUE)
gen_policy <- import('gen_policy')


params <- tibble(targetgroup_p = 0.5,
                 mod_age_coef = -1,
                 mod_age_targetgroup_coef = 0,
                 mod_transit_proximity_coef = 2, 
                 mod_transit_proximity_targetgroup_coef = -1, 
                 mod_rideshare_poverty_coef = 4,
                 mod_rideshare_poverty_targetgroup_coef = -2) 


budgets <- tribble(
  ~arm, ~budget,
  "control", 0.75, 
  "transit", 0.2,
  "rideshare", 0.05
) %>% 
  arrange(arm)

lambda <-  0.04 

experiment_size <- 1000
num_sims <- 2000
num_cores <- 40
sim_num_start <- 14000
# experiment_size <- 1000
# num_sims <- 1000
# num_cores <- 40
# sim_num_start <- 14000
save_path <- "/share/data/nudge/simulations/fair_allocation/test/" # Trailing slash plz

sims <- seq(sim_num_start, sim_num_start + num_sims - 1)

run_learning <- function(sim_num) {
  
  set.seed(sim_num)
  exp_pop <- generate_samples(params, experiment_size)
  
  rct_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "random", seed = sim_num, sim_params = params) 
  rct50_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "random", halt_random = 50, seed = sim_num, sim_params = params)
  rct100_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "random", halt_random = 100, seed = sim_num, sim_params = params)
  rct150_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "random", halt_random = 150, seed = sim_num, sim_params = params)
  
  greedy_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "greedy", seed = sim_num, sim_params = params)
  oracle_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "oracle", seed = sim_num, sim_params = params)
  thompson_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "thompson", seed = sim_num, sim_params = params)
  ucb_pop <- run_batch(exp_pop, budgets, lambda = lambda, method = "UCB", seed = sim_num, sim_params = params)
  
  treated_pop <- bind_rows(rct_pop,
                           rct50_pop,
                           rct100_pop,
                           rct150_pop,
                           greedy_pop,
                           thompson_pop,
                           ucb_pop,
                           oracle_pop) %>%
    mutate(sim_num = sim_num)
  
  savetime <- make_clean_names(Sys.time())
  sim_pad <- str_pad(sim_num, 5, "left", "0")
  treated_pop %>% 
    write_rds(glue("{save_path}{sim_pad}_{savetime}.rds"))
  
  treated_pop
}

print(Sys.time())
mclapply(sims, run_learning, mc.cores = num_cores) 
print(Sys.time())