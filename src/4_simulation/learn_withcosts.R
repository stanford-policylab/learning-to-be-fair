
get_one_row_that_matches_value <- function(pop, colname, value) {
  pop %>% 
    filter({{colname}} == value) %>% sample_n(1)
}

get_samples_of_all_values <- function(pop, colname) {
  
  unique_values <- pop %>% distinct({{colname}}) %>% pull()
  
  lapply(unique_values, get_one_row_that_matches_value, pop = pop, colname = {{colname}}) %>% 
    bind_rows()
  
}

run_experiment <- function(pop, 
                           budget, 
                           lambda,
                           ignore_lambda = FALSE,
                           seed = 42,
                           method = "",
                           extend_warmup_by = 0,
                           compensate_budget_for_extended_warmup = FALSE,
                           stop_early_random = Inf,
                           limit_actions_random = "",
                           fixed_transit_pct = 0,
                           fixed_rideshare_pct = 0,
                           epsilon = 0.1,
                           num_sim_samples = 1000,
                           sim_params) {
  
  oracle <- FALSE 
  thompson <- FALSE
  UCB <- FALSE
  random <- FALSE
  egreedy <- FALSE
  cheapest <- FALSE
  fixed_treatments <- FALSE
  if (method == "oracle") {
    oracle <- TRUE
  } else if (method == "thompson") {
    thompson <- TRUE
  } else if (method == "UCB") {
    UCB <- TRUE
  } else if (method == "egreedy") {
    egreedy <- TRUE
  } else if (method == "random") {
    random <- TRUE
  } else if (method == "cheapest") {
    cheapest <- TRUE
  } else if (method == "fixed_treatments") {
    fixed_treatments <- TRUE
  }
  
  if (limit_actions_random == "") {
    random_policy_addon <- ""
  } else {
    random_policy_addon <- glue(" with {limit_actions_random} only")
  }
  
  if (method == "random") {
    if (stop_early_random < Inf) {
      method_name <- glue("RCT to {stop_early_random}{random_policy_addon}")
    } else {
      method_name <- glue("RCT{random_policy_addon}")
    }
  } else if (method == "UCB") {
    method_name <- method
  } else {
    method_name <- str_to_title(method)
  }
  
  if (ignore_lambda) {
    method_name <- glue("{method_name} ignore lambda")
  }
  
  if (extend_warmup_by > 0) {
    method_name <- glue("{method_name} extend warmup by {extend_warmup_by}")
    if (compensate_budget_for_extended_warmup) {
      method_name <- glue("{method_name} with compensated budget")
    }
  }
  
  if (fixed_treatments) {
    method_name <- glue("{method_name} with {fixed_transit_pct} transit and {fixed_rideshare_pct} rideshare")
  }
  
  if (num_sim_samples != 1000) {
    method_name <- glue("{method_name}_{num_sim_samples}_sim_pop")
  }
  
  # Save immutable and mutable versions of budget
  original_budget <- budget
  target_budget <- budget
  
  # Initialize simulated population
  set.seed(seed)
  sim_pop <- generate_samples(sim_params, num_sim_samples)
  
  # Calculate expected mean costs to help allocate "randomly"
  # for random /deterministic policies 
  # (i.e., so they can still meet the budget)
  if (random | egreedy) {
    allowed_costs <- glue("{limit_actions_random}_cost")
    expected_mean_costs <- sim_pop %>% 
      summarize(across(contains(allowed_costs), mean)) %>% 
      mutate(total = sum(across(everything()))) %>% 
      pull(total) 
  }
  
  # Similar idea for a strategy which chooses the cheapest available treatment.
  # We also need to extend the warmup so that we spend our budget 
  # by the end of the entire experiment 
  # if we're always choosing the cheapest action once we start experimenting
  if (cheapest) {  
    expected_cheapest_cost <- sim_pop %>% 
      rowwise() %>% 
      mutate(min_cost = min(across(c(contains("_cost"), -control_cost)))) %>% 
      ungroup() %>% 
      summarize(mean = mean(min_cost)) %>% 
      pull(mean)
    samp_size <- nrow(pop)
    total_budget <- samp_size * budget
    extend_warmup_by <- round(samp_size - (total_budget / expected_cheapest_cost))
    print(glue("Detected cheapest method, starting cheapest treatment at person {extend_warmup_by}"))
  }
  
  # Create warmup population of 6 samples for valid model
  warmup_pop_universe <- generate_samples(sim_params, 1000)
  warmup_pop <- bind_rows(get_samples_of_all_values(warmup_pop_universe, targetgroup),
                          # get_samples_of_all_values(warmup_pop_universe, is_felony),
                          get_samples_of_all_values(warmup_pop_universe, is_male)) %>% 
    sample_n(n()) %>% # Shuffle the samples
    mutate(person_ix = row_number(),
           is_warmup = TRUE,
           control_policy_treat_prob = if_else(person_ix > 2, 1, 0), # Give everyone control except
           transit_policy_treat_prob = if_else(person_ix == 1, 1, 0), # Give first person transit
           rideshare_policy_treat_prob = if_else(person_ix == 2, 1, 0)) %>% # Give second person a ride
    make_decision() %>% 
    mutate(person_ix = person_ix - n())  
  
  # Allow for extended no-cost warmup 
  if (extend_warmup_by > 0) {
    if (compensate_budget_for_extended_warmup & method != "cheapest") {
      target_budget <- original_budget / (1 - (extend_warmup_by/nrow(pop)))
    }
    
    pop <- pop %>% 
      mutate(is_extended_warmup = person_ix <= extend_warmup_by)
    
    extended_warmup <- pop %>% 
      filter(is_extended_warmup) %>% 
      mutate(is_warmup = TRUE,
             control_policy_treat_prob = 1, # Give everyone no-cost control
             transit_policy_treat_prob = 0,
             rideshare_policy_treat_prob = 0) %>% 
      make_decision()
    
    warmup_pop <- bind_rows(warmup_pop,
                            extended_warmup)
    
    pop <- pop %>% filter(!is_extended_warmup)
    
  } else {
    pop <- pop %>% 
      mutate(is_extended_warmup = FALSE)
  }

  # Add warmup pop to the historical observed population
  hist_pop <- list()
  hist_pop[[1]] <- warmup_pop
  
  # Label experimental population as non-warmup
  pop <- pop %>% 
    mutate(is_warmup = FALSE)
  
  # Use correct lambda
  if (ignore_lambda) {
    true_lambda <- lambda
    lambda <- 0
  } else {
    true_lambda <- lambda
  }
  
  # Prepare record of budgets for adjustment
  running_budgets <- tribble(~"running_budget", ~"mean_observed_spending")
  running_budget <- target_budget
  
  # Run iterative learning and actions
  samp_size <- nrow(pop)
  for (batch_num in seq(1, samp_size)) {
    
    # Print occasional updates to console
    if (batch_num %% 10 == 0) {
      print(glue("Simulation #{seed}, {method_name} #{batch_num}."))
    }
    
    # Setup data
    train_pop <- bind_rows(hist_pop) %>% 
      select(-contains("model_ix"))
    batch_pop <- pop %>% 
      slice(batch_num:batch_num)
    
    
    # Calculate best-guess policy from historical observations
    if (oracle) {
      calc_inference_func <- calc_inferences_oracle
    } else {
      # Need to keep glm_model for later use (don't pipe!)
      glm_model <- glm_from_observations(train_pop) 
      fit_model <- get_mle_fit(glm_model)
      calc_inference_func <- calc_inferences(fit_model)
    }

    # Calculate expected utility from best-guess policy
    # on the simulated holdout population
    sim_pop_inferred <- sim_pop %>% 
      calc_inference_func() %>% 
      calc_reward("(.*_inferred)_appear_prob")
    sim_pop_bestguess_policy <- sim_pop_inferred %>% 
      calc_policy(original_budget, lambda)
    policy_bestguess_utility <- sim_pop %>% 
      left_join(sim_pop_bestguess_policy, by = "person_ix", suffix = c("", "_ignore")) %>% 
      select(!contains("_ignore")) %>% 
      calc_reward("(.*_true)_appear_prob") %>% 
      calc_policy_utility("(.*)_policy_treat_prob", "(.*)_true_reward", true_lambda)
    
    # Budget adjustments
    if (oracle | cheapest) {
      # Don't adjust for oracle or cheapest action strategy
      running_budget <- target_budget
    } else if (batch_num > 1 & !fixed_treatments) {
      budget_adjuster_rate <- 1
      spent_so_far <- train_pop %>% 
        filter(!is_warmup) %>% 
        summarize(sum(observed_spending)) %>% 
        pull()
      budgeted_so_far <- target_budget * batch_num
      budget_adjuster <- (budgeted_so_far / spent_so_far) ^ budget_adjuster_rate
      
      # If no money has been spent yet, leave the budget as-is
      budget_adjuster <- if_else(is.infinite(budget_adjuster), 1, budget_adjuster)
      
      # Adjust budget
      running_budget <- target_budget * budget_adjuster
    }
    
    
    # Calculate policies
    if (random) {
      
      # If an RCT, and we haven't reached the stop-early point yet,
      # keep allocating "random" actions
      if (stop_early_random > batch_num) {
        batch_pop <- batch_pop %>% 
          bind_random_policy(running_budget, expected_mean_costs, limit_actions = limit_actions_random) %>% 
          make_decision() 
      } else {

        # If we're at the stop-early point, 
        # save the best-guess model here        
        if (stop_early_random == batch_num) {
          saved_calc_inference_func <- calc_inference_func
          saved_sim_pop_bestguess_policy <- sim_pop_bestguess_policy
        }

        # If we're at or past the stop-early point,
        # act according to our best-guess model from the stop-early point
        batch_pop <- batch_pop %>%
          saved_calc_inference_func() %>% 
          calc_reward("(.*_inferred)_appear_prob") %>% 
          bind_lp_policy(saved_sim_pop_bestguess_policy) %>% 
          make_decision()
      }
      
    } else if (cheapest) {
      
      batch_pop <- batch_pop %>% 
        bind_cheapest_action_policy() %>% 
        make_decision()
      
    } else if (fixed_treatments) {
      batch_pop <- batch_pop %>% 
        bind_fixed_treatment_policy(fixed_transit_pct, fixed_rideshare_pct) %>% 
        make_decision()
      
    } else {
      
      # Get the correct inference function for Thompson/UCB
      if (thompson|UCB) {
        if (thompson) {
          fit_model <- get_posteriors_fit(glm_model, num_posteriors = 1)
          calc_inference_func <- calc_inferences(fit_model)
        } else if (UCB) {
          fit_model <- get_posteriors_fit(glm_model, num_posteriors = 100)
          calc_inference_func <- calc_inferences_UCB(fit_model)
        }
        # Replace the sim_pop_inferred created above with an
        # sim_pop_inferred created with the correct model
        sim_pop_inferred <- sim_pop %>% 
          calc_inference_func() %>% 
          calc_reward("(.*_inferred)_appear_prob")
      }
      
      # Calculate the optimal policy on the sim_pop
      # by passing it to the LP
      sim_pop_policy <- sim_pop_inferred %>% 
        calc_policy(running_budget, lambda)

      # Make the decision for this batch, given the
      # decisions optimized on sim_pop by the LP!
      batch_pop <- batch_pop %>%
        calc_inference_func() %>% 
        calc_reward("(.*_inferred)_appear_prob") %>% 
        bind_lp_policy(sim_pop_policy) %>% 
        make_decision()
      
      # For e-greedy approaches,
      # change the decision occasionally, 
      # at a rate determined by epsilon
      if (egreedy) {
        batch_pop <- batch_pop %>% 
          egreedy_epsilon_flipper(epsilon, running_budget, expected_mean_costs)
      }
      
    }
    
    # Stash what happened on this batch in hist_pop
    batch_pop <- bind_cols(batch_pop, policy_bestguess_utility) %>% 
      mutate(running_budget = running_budget,
             target_budget = target_budget,
             original_budget = original_budget,
             lambda = lambda,
             ignore_lambda = ignore_lambda)
    hist_pop[[batch_num + 1]] <- batch_pop  
    
  }
  
  # Combine all historical observations
  # over the course of the experiment
  output <- bind_rows(hist_pop)
  
  # Calculate cumulative realized utility
  # (for calculating regret later)
  ixs <- seq(1, nrow(output))
  cumulative_utilities <- bind_rows(lapply(ixs, 
                                           calc_cumulative_penalized_utility, 
                                           pop = output, 
                                           lambda = true_lambda))

  # Join with the cumulative realized utility and return
  output %>% 
    left_join(cumulative_utilities, by = "person_ix") %>% 
    mutate(method = method_name)
  
}

prep_run_experiment <- function(pop, budget, lambda, seed, sim_params) {
  
  function(method, ...) {
    run_experiment(pop, budget, lambda = lambda, method = method, seed = seed, sim_params = sim_params, ...)
  }
  
}
