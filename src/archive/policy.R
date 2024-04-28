
bind_random_policy <- function(pop, budgets) {
  
  random_treat_probs <- budgets %>% 
    pivot_wider(names_from = arm, 
                values_from = budget) %>% 
    rename_with(~ glue("{.}_treat_prob"))
  
  pop %>% 
    bind_cols(random_treat_probs)
  
}


bind_lp_policy <- function(pop, policy) {
  
  # # Nearest neighbors to determine policy
  prep_for_nn <- function(pop) {
    pop %>% 
      select(matches("inferred.*utility")) %>% 
      as.matrix()
  }
  
  nn_pop <- pop %>% 
    prep_for_nn()
  nn_policy <- policy %>% 
    prep_for_nn()
  
  distances <- distmat(nn_pop, nn_policy)
  best_match_ix <- apply(distances, 1, which.min)
  
  target_covars <- policy %>% 
    select(person_ix,
           contains("_treat_prob"))
  
  # Output: pop with probs for each arm
  pop %>% 
    mutate(best_match_ix = best_match_ix) %>% 
    left_join(target_covars, by = c("best_match_ix" = "person_ix")) 
}

unbind_policy <- function(pop) {
  pop %>% 
    select(!contains("best_match") &
             !contains("_treat_prob") &
             !contains("decision") &
             !contains("_expected_utility") &
             !appear
           )
}


calc_policy <- function(pop, budgets, lambda) {
  
  # Pull out utilities
  reward <- pop %>% 
    select(matches("inferred.*utility")) %>%
    select(sort(peek_vars())) %>% 
    as.matrix()
  
  # Get optimal solution from LP solver
  lp_output <- gen_policy$optimize(reward, 
                                   budgets$budget, 
                                   pop$targetgroup, 
                                   lambda) %>% 
    as_tibble() %>% 
    set_names(nm <- colnames(reward)) %>% 
    rename_with(~ str_replace(., "utility", "treat_prob")) %>% 
    rename_with(~ str_remove(., "inferred.")) 
  
  # Output policy 
  # (Which is a tibble of decisions you made with the LP population)
  bind_cols(pop, lp_output) %>% 
    select(person_ix,
           matches("inferred.*utility"),
           contains("_treat_prob"))
  
}
