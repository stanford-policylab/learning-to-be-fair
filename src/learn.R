
prep_lm_output <- function(lm_tibble) {
  
  lm_tibble %>% 
    rename_all(~ str_remove(., "TRUE")) %>% 
    rename_all(~ make_clean_names(.)) %>% 
    rename_all(~ glue("est_{.}_coef")) %>% 
    mutate(model_ix = factor(row_number())) %>% 
    select(model_ix, everything()) %>% 
    mutate(across(where(is.numeric), ~ replace_na(., 0))) # Crutch to force predictions with incomplete models
}

observe_from_treatment <- function(pop) {
  pop %>% 
    mutate(y_obs = if_else(treat == 1, truth.treat_appear, truth.cntrl_appear))
}

random_assignment <- function(pop, budget) {
  pop %>% 
    mutate(treat = rbernoulli(n(), p = budget)) %>% 
    observe_from_treatment()
}

glm_from_assignments <- function(pop) {
  
  pop <- pop %>% 
    rename_with(~ str_remove(., "_decision"))
  
  glm(appear ~  (age + transit:proximity + rideshare:poverty) +
                (age + transit:proximity + rideshare:poverty):targetgroup
                - 1,
      data = pop, family = "binomial")
  
}

get_mle_fit <- function(lm_obj) {
  
  fit_mle <- lm_obj %>% 
    tidy() %>% 
    select(term, estimate) %>% 
    pivot_wider(names_from = term, values_from = estimate) %>% 
    prep_lm_output()
  
}

get_posteriors_fit <- function(lm_obj, num_posteriors) {
  
  # Need to add 1 because sim returns a default model (intercept = 1, coefs = 0) 
  # if you only ask for 1 posterior
  # (Seems to be a bug (or a conceptual misunderstanding) with `sim`)
  if (num_posteriors == 1) {
    num_posteriors <- 2
    remove_posterior <- TRUE
  } else {
    remove_posterior <- FALSE
  }
  
  fit_posterior <- sim(lm_obj, n.sims = num_posteriors) %>% 
    coef() %>% 
    as_tibble() %>% 
    prep_lm_output()
  
  if (remove_posterior) {
    fit_posterior <- fit_posterior %>% 
      slice(1)
  }
  
  fit_posterior
  
}

run_rct <- function(pop, budgets) {
  
  pop <- pop %>% 
    bind_random_policy(budgets) %>% 
    make_decision()
  
  glm_rct <- glm_from_assignments(pop)
  
  list("glm" = glm_rct, "pop" = pop)
  
}

run_batch <- function(pop, 
                      budgets, 
                      lambda,
                      seed = 42,
                      method = "",
                      halt_random = Inf,
                      epsilon = 0.1,
                      sim_params) {
  
  oracle <- FALSE 
  thompson <- FALSE
  UCB <- FALSE
  random <- FALSE
  greedy <- FALSE
  if (method == "oracle") {
    oracle <- TRUE
  } else if (method == "thompson") {
    thompson <- TRUE
  } else if (method == "UCB") {
    UCB <- TRUE
  } else if (method == "random") {
    random <- TRUE
  } else if (method == "greedy") {
    greedy <- TRUE
  }
  
  if (method == "random") {
    if (halt_random < Inf) {
      method_name <- glue("RCT to {halt_random}")
    } else {
      method_name <- "RCT"
    }
  } else if (method == "UCB") {
    method_name <- method
  } else {
    method_name <- str_to_title(method)
  }

  # Initialize simulated population
  set.seed(seed)
  num_sim_samples <- 1000
  sim_pop <- generate_samples(sim_params, num_sim_samples)

  # Pull first 25 samples from pop for warmup
  warmup_n <- 25
  warmup_pop <- pop %>% 
    mutate(num_targetgroup = cumsum(targetgroup),
           num_potentgroup = cumsum(1-targetgroup)) %>% 
    filter(row_number() <= warmup_n) %>% 
    # filter(num_targetgroup <= 3 | num_potentgroup <= 3) %>%
    mutate(control_treat_prob = round(runif(n(), min = 0, max = 0.75)),
           transit_treat_prob = 1 - control_treat_prob,
           transit_treat_prob = if_else(transit_treat_prob == 1, runif(n()), 0),
           rideshare_treat_prob = if_else(transit_treat_prob < 1 & transit_treat_prob > 0,
                                        1  - transit_treat_prob, 0),
           transit_treat_prob = round(transit_treat_prob),
           rideshare_treat_prob = round(rideshare_treat_prob),
           control_treat_prob = case_when(num_targetgroup == 1 & targetgroup == 1 ~ 1, # control for first targetgroup person
                                          num_potentgroup == 1 & targetgroup == 0 ~ 1, # control for first potentgroup person
                                          num_targetgroup == 2 & targetgroup == 1 ~ 0, # transit for second targetgroup person
                                          num_potentgroup == 2 & targetgroup == 0 ~ 0, # transit for second potentgroup person
                                          num_targetgroup == 3 & targetgroup == 1 ~ 0, # rideshare for third targetgroup person
                                          num_potentgroup == 3 & targetgroup == 0 ~ 0, # rideshare for third potentgroup person
                                          TRUE ~ control_treat_prob),
           transit_treat_prob    = case_when(num_targetgroup == 1 & targetgroup == 1 ~ 0, # control for first targetgroup person
                                          num_potentgroup == 1 & targetgroup == 0 ~ 0, # control for first potentgroup person
                                          num_targetgroup == 2 & targetgroup == 1 ~ 1, # transit for second targetgroup person
                                          num_potentgroup == 2 & targetgroup == 0 ~ 1, # transit for second potentgroup person
                                          num_targetgroup == 3 & targetgroup == 1 ~ 0, # rideshare for third targetgroup person
                                          num_potentgroup == 3 & targetgroup == 0 ~ 0, # rideshare for third potentgroup person
                                          TRUE ~ transit_treat_prob),
           rideshare_treat_prob = case_when(num_targetgroup == 1 & targetgroup == 1 ~ 0, # control for first targetgroup person
                                          num_potentgroup == 1 & targetgroup == 0 ~ 0, # control for first potentgroup person
                                          num_targetgroup == 2 & targetgroup == 1 ~ 0, # transit for second targetgroup person
                                          num_potentgroup == 2 & targetgroup == 0 ~ 0, # transit for second potentgroup person
                                          num_targetgroup == 3 & targetgroup == 1 ~ 1, # rideshare for third targetgroup person
                                          num_potentgroup == 3 & targetgroup == 0 ~ 1, # rideshare for third potentgroup person
                                          TRUE ~ rideshare_treat_prob)
           ) %>% 
    make_decision()
  hist_pop <- list()
  hist_pop[[1]] <- warmup_pop
  
  # Remove warmup pop, calculate batch size
  pop <- pop %>% 
    slice(warmup_n + 1:nrow(pop))
  samp_size <- nrow(pop)
  
  # Prepare budgets for adjustment
  goal_budgets <- budgets
  effective_budgets <- list()
  effective_budgets[[1]] <- budgets %>% 
    pivot_wider(names_from = arm, values_from = budget)

  
  for (batch in seq(1, samp_size)) {
    
    if (batch %% 10 == 0) {
      print(glue("Simulation #{seed}, {method_name} #{batch}."))
    }
    
    # Setup data
    train_pop <- bind_rows(hist_pop) %>% 
      select(-contains("model_ix"))
    
    batch_pop <- pop %>% 
      slice(batch:batch)
    
    
    # Calculate best-guess from historical performance
    if (oracle) {
      calc_inference_func <- calc_inferences_oracle
    } else {
      # Need to keep glm_model for later use (don't pipe!)
      glm_model <- glm_from_assignments(train_pop) 
      fit_model <- get_mle_fit(glm_model)
      calc_inference_func <- calc_inferences(fit_model)
    }
    inferred_pop <- sim_pop %>% 
      calc_inference_func() %>% 
      calc_utility_simple(".*appear_prob") 
    policy_bestguess <- inferred_pop %>% 
      calc_policy(goal_budgets, lambda)
    best_guess_utility <- inferred_pop %>% 
      left_join(policy_bestguess, by = "person_ix") %>% 
      total_expected_true_utility("_treat_prob$", lambda)

    
    # Budget adjustments
    if (oracle) {
      # Don't adjust for oracle
      budgets <- goal_budgets
    } else if (batch > 1) {
      mean_effective_budgets <- bind_rows(effective_budgets) %>% 
        summarize_all(mean) %>% 
        pivot_longer(cols = everything(),
                     names_to = "arm",
                     values_to = "mean_effective_budgets")
      budgets <- train_pop %>%
        tail(batch - 1) %>% # Exclude warmup pop from budget calculations
        select(matches("_decision")) %>% 
        summarize_all(mean) %>% 
        pivot_longer(cols = everything(),
                     names_to = "arm",
                     values_to = "spent") %>% 
        mutate(arm = str_remove(arm, "_decision")) %>% 
        left_join(goal_budgets, by = "arm") %>% 
        mutate(budget_adjuster = spent  / budget,
               budget_adjuster = if_else(budget_adjuster == 0, 1, budget_adjuster),
               lambda = lambda) %>% 
        left_join(mean_effective_budgets, by = "arm") %>% 
        mutate(effective_budget = mean_effective_budgets / (budget_adjuster ^ lambda),
               effective_budget = effective_budget / sum(effective_budget)) %>% 
        # The second line above makes sure they add up to 1
        select(arm, budget = effective_budget) %>% 
        arrange(arm)
    }
    wide_budgets <- budgets %>% 
      pivot_wider(names_from = arm, values_from = budget)
    effective_budgets[[batch + 1]] <- wide_budgets
    

    # Calculate policies
    if (random) {
      
      if (halt_random > batch) {
        batch_pop <- batch_pop %>% 
          bind_random_policy(budgets) %>% 
          make_decision()
      } else {
        if (halt_random == batch) {
          saved_calc_inference_func <- calc_inference_func
          saved_policy <- policy_bestguess
        }
        batch_pop <- batch_pop %>%
          saved_calc_inference_func() %>% 
          calc_utility_simple(".*appear_prob") %>% 
          bind_lp_policy(saved_policy) %>% 
          make_decision()
      }
      
    } else {

      # Get policies
      if (thompson|UCB) {
        if (thompson) {
          fit_model <- get_posteriors_fit(glm_model, num_posteriors = 1)
          calc_inference_func <- calc_inferences(fit_model)
        } else if (UCB) {
          fit_model <- get_posteriors_fit(glm_model, num_posteriors = 100)
          calc_inference_func <- calc_inferences_UCB(fit_model)
        }
        inferred_pop <- sim_pop %>% 
          calc_inference_func() %>% 
          calc_utility_simple(".*appear_prob")
      }
      
      policy <- inferred_pop %>% 
        calc_policy(budgets, lambda)
      
  
      # Make the decision!
      batch_pop <- batch_pop %>%
        calc_inference_func() %>% 
        calc_utility_simple(".*appear_prob") %>% 
        bind_lp_policy(policy) %>% 
        make_decision()
      
      if (greedy) {
        batch_pop <- batch_pop %>% 
          epsilon_flip(epsilon, budgets)
      }
      
    }
    
    wide_budgets <- wide_budgets %>% 
      rename_with(~ glue("{.}_effective_budget"))
    batch_pop <- bind_cols(batch_pop, best_guess_utility, wide_budgets)
    hist_pop[[batch + 1]] <- batch_pop  
    
  }
  
  output <- bind_rows(hist_pop)
  
  # Calculate cumulative penalized realized utility
  # (for calculating regret later)
  ixs <- seq(warmup_n, nrow(output))
  cumulative_utilities <- bind_rows(lapply(ixs, calc_cumulative_penalized_utility, pop = output))

  output %>% 
    left_join(cumulative_utilities, by = "person_ix") %>% 
    mutate(method = method_name)
  
}
