
bind_fixed_treatment_policy <- function(pop, fixed_transit_pct, fixed_rideshare_pct) {
  
  control_prob <- 1 - (fixed_rideshare_pct + fixed_transit_pct)
  
  pop %>% 
    mutate(transit_policy_treat_prob   = fixed_transit_pct,
           rideshare_policy_treat_prob = fixed_rideshare_pct,
           control_policy_treat_prob   = control_prob)
  
}

bind_cheapest_action_policy <- function(pop) {
  
  # Temporarily use cost columns to add treatment probs
  # Then rename as such
  cheapest_action_treat_probs <- pop %>% 
    select(contains("_cost"),
           -control_cost) %>% 
    rowwise() %>% 
    mutate(min_cost = min(across(everything())),
           # If any cost equals the min cost, give it a 1. 
           # Else give it a 0.
           across(everything(), ~ as.numeric(. == min_cost))) %>% 
    select(-min_cost) %>% 
    # If there are any ties, assign them all equal probability
    mutate(across(everything(), ~ . / sum(across(everything()))),
           # Don't take the no-cost action, by design
           control_cost = 0) %>% 
    rename_with(~ str_replace(., "_cost", "_policy_treat_prob")) %>% 
    ungroup()
  
  pop %>% 
    bind_cols(cheapest_action_treat_probs)
    
}

bind_random_policy <- function(pop, budget, expected_mean_costs, limit_actions = "") {
  
  costly_arm_pct <- (budget / expected_mean_costs)

  # Calculate maximum treatments within budget
  # If no limit actions, pct in each costly arm is equal
  # If we have enough budget to treat almost everyone,
  # reserve 5% of the population for the control
  if (limit_actions == "") {  
    if (costly_arm_pct < 0.475) {
      control_prob <- (1 - (2 * costly_arm_pct))
    } else {
      costly_arm_pct <- 0.475
      control_prob <- 0.05
    }
  } else {
    if (costly_arm_pct < 0.95) {
      control_prob <- (1 - costly_arm_pct)
    } else {
      costly_arm_pct <- 0.95
      control_prob <- 0.05
    }
  }
  
  # Temporarily use cost columns to add treatment probs
  # Then rename as such
  random_treat_probs <- pop %>% 
    select(contains("_cost")) %>% 
    mutate(across(everything(), ~ 0),
           across(contains(glue("{limit_actions}_cost")), ~ costly_arm_pct),
           control_cost = control_prob) %>% 
    rename_with(~ str_replace(., "_cost", "_policy_treat_prob"))
  
  pop %>%
    bind_cols(random_treat_probs)

}

egreedy_unbind_policy <- function(pop) {
  pop %>%
    select(!contains("best_match") &
           !contains("policy_treat_prob") &
           !contains("decision") &
           !contains("observed")
    )
}

egreedy_epsilon_flipper <- function(pop, epsilon, budget, expected_mean_costs) {
  
  pop <- pop %>% 
    mutate(epsilon_flip = runif(n()),
           epsilon_flip = epsilon_flip < epsilon) 
  
  if (sum(pop$epsilon_flip) > 0) {
    
    flipped <- pop %>% 
      filter(epsilon_flip == TRUE) %>% 
      egreedy_unbind_policy() %>% 
      bind_random_policy(budget, expected_mean_costs) %>% 
      make_decision() 
    
    pop <- pop %>% 
      filter(epsilon_flip == FALSE) %>% 
      bind_rows(flipped) %>% 
      arrange(person_ix)
  } 
  
  pop
  
}


calc_policy <- function(pop, budget, lambda, batch_num = 0) {

  # Pull out rewards
  rewards <- pop %>% 
    select(contains("_inferred_reward")) %>%
    select(sort(peek_vars())) %>% 
    as.matrix()
  
  # Pull out costs
  costs <- pop %>% 
    select(contains("_cost")) %>% 
    select(sort(peek_vars())) %>% 
    as.matrix()
  
  # Get optimal solution from LP solver
  lp_output <- gen_policy$optimize(rewards, 
                                   costs,
                                   budget, 
                                   pop$targetgroup, 
                                   lambda) %>% 
    as_tibble(.name_repair = "minimal") %>% 
    set_names(nm <- colnames(rewards)) %>% 
    rename_with(~ str_replace(., "inferred_reward", "policy_treat_prob"))
  
  # # Use these to check penalties with diff values of lambda
  # bind_cols(pop, lp_output) %>%
  #   decision_crosser(".*_policy_treat_prob", cross_col_regex = "(.*)_cost", new_col_name = "observed_spending") %>%
  #   group_by(targetgroup) %>%
  #   summarize(sum(observed_spending), mean(observed_spending))

  # if (batch_num >= 50) {
  #   browser()
  # }

  
  # # Print warning about no-treatment arms
  # treat_counts <- lp_output %>% 
  #   summarize_all(sum) %>% 
  #   pivot_longer(everything()) 
  # no_treat_arms <- treat_counts %>% 
  #   filter(value == 0)
  # if (nrow(no_treat_arms) > 0) {
  #   print("Some arms not getting any treatments.")
  #   print(treat_counts)
  # }
  
  # Output policy 
  # (Which is a tibble of decisions you made with the LP population)
  bind_cols(pop, lp_output) %>% 
    select(person_ix,
           contains("_inferred_reward"),
           contains("_policy_treat_prob"),
           contains("_cost"))
  
}


bind_lp_policy <- function(pop, policy) {
  
  # # Nearest neighbors to determine policy
  prep_for_nn <- function(pop) {
    rewards <- pop %>%
      select(sort(contains("_inferred_reward")))
    costs <- pop %>% 
      select(sort(contains("_cost")))
    rewards_per_unit_cost <- 
      (rewards / costs) 
    rewards_per_unit_cost %>% 
      mutate(control_inferred_reward = rewards$control_inferred_reward) %>% 
      as.matrix()
  }
  
  # Select only reward variables
  nn_pop <- pop %>%
    prep_for_nn()
  nn_policy <- policy %>%
    prep_for_nn()
  
  # Find the nearest neighbor to nn_pop rewards
  distances <- distmat(nn_pop, nn_policy)
  best_match_ix <- apply(distances, 1, which.min)
  
  # Select only the policy columns 
  # (i.e., probability to provide each treatment)
  target_covars <- policy %>%
    select(person_ix,
           contains("_policy_treat_prob"))
  
  # Output: pop with treatment probs for each arm
  pop %>%
    mutate(best_match_ix = best_match_ix) %>%
    left_join(target_covars, by = c("best_match_ix" = "person_ix"))
}



calc_utility_penalty <- function(pop, decision_col_regex, lambda) {
  
  pop %>%
    decision_crosser(decision_col_regex, "(.*)_cost", "expected_spending") %>% 
    group_by(targetgroup) %>% 
    summarize(group_spending = sum(expected_spending),
              group_n = n(),
              group_spending_per_person = group_spending / group_n,
              .groups = "drop") %>% 
    ungroup() %>% 
    mutate(total_spending = sum(group_spending),
           total_n = sum(group_n),
           total_spending_per_person = total_spending / total_n,
           diff = abs(group_spending_per_person - total_spending_per_person),
           # Divide by 2 conforms with optimizer code
           # (In theory this should be the number of demo groups)
           penalty = diff * (lambda / 2)) %>% 
    summarize(penalty = sum(penalty), .groups = "drop") %>% 
    pull()
  
}

calc_policy_utility <- function(pop, decision_col_regex, reward_col_regex, lambda) {
  
  penalty <- pop %>% 
    calc_utility_penalty(decision_col_regex, lambda)
  
  pop <- pop %>% 
    decision_crosser(decision_col_regex, reward_col_regex, "expected_reward") 
  
  pop %>% 
    mutate(expected_utility = expected_reward - penalty) %>% 
    summarize(across(matches(".*expected_reward"), ~ sum(.)),
              expected_utility = sum(expected_utility),
              penalty = penalty) %>% 
    rename_with(~ str_c("bestguess_policy_", .))
  
}

calc_cumulative_penalized_utility <- function(slice_ix, pop, lambda) {
  
  sliced_pop <- pop %>%
    filter(!is_warmup | is_extended_warmup,
           person_ix <= slice_ix)

  penalty <- sliced_pop %>%
    calc_utility_penalty("_decision", lambda)

  sliced_pop %>%
    mutate(observed_reward  = observed_reward,
           observed_utility = observed_reward - penalty) %>%
    summarize(cumulative_observed_reward  = sum(observed_reward),
              cumulative_observed_utility = sum(observed_utility)) %>%
    mutate(person_ix = slice_ix,
           penalty_at_ix = penalty) %>%
    relocate(person_ix)
}

