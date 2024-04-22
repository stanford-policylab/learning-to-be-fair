
epsilon_flip <- function(pop, epsilon, budgets) {

  pop <- pop %>% 
    mutate(epsilon_flip = runif(n()),
           epsilon_flip = epsilon_flip < epsilon) 
  
  if (sum(pop$epsilon_flip) > 0) {
    
    flipped <- pop %>% 
      filter(epsilon_flip == TRUE) %>% 
      unbind_policy() %>% 
      bind_random_policy(budgets) %>% 
      make_decision()
    
    pop <- pop %>% 
      filter(epsilon_flip == FALSE) %>% 
      bind_rows(flipped) %>% 
      arrange(person_ix)
  } 
  
  pop
    
}


make_decision <- function(pop) {
  
  arms <- pop %>% 
    select(contains("treat_prob")) %>% 
    select(sort(peek_vars())) 
  
  # Choose an action for each row
  decision_positions <- arms %>% 
    apply(., 1, rmultinom, n = 1, size = 1) %>%  # Randomly draw action based on probs
    apply(., 2, which.max)  # Get position of action
  
  # Get name of action
  arm_names <- colnames(arms) %>% 
    str_remove("_treat_prob")
  decisions <- arm_names[decision_positions]
  
  # Get all arm columns (to fill with zeros on final output)
  all_arm_cols <- arms %>% 
    filter(FALSE) %>% 
    rename_with(~ str_replace(., "treat_prob", "decision"))
  
  # Add one-hot columns for decisions
  one_hot_decisions <- decisions %>% 
    as_tibble() %>% 
    mutate(person_ix = pop$person_ix,
           decision = 1) %>% 
    pivot_wider(id_cols = "person_ix", names_from = value, names_glue = "{value}_decision", values_from = decision) %>% 
    mutate(across(where(is.numeric), ~ replace_na(., 0))) %>% 
    bind_rows(all_arm_cols) %>% 
    mutate(across(contains("_decision"), ~ replace_na(., 0))) %>% 
    select(sort(peek_vars()))
  
  # Get the potential outcome for chosen action
  appearances <- pop %>% 
    select(matches(".*appear$")) %>% 
    select(sort(peek_vars())) %>% 
    mutate(decision_position = decision_positions) %>% 
    as.matrix() %>% 
    apply(., 1, function(x) {x[tail(x, n=1)]}) 
  
  # Get the utility for chosen action
  utilities <- pop %>%
    select(-matches(".*_utility")) %>% # In case the estimates are there already
    calc_utility_simple("truth.*_appear_prob") %>% 
    select(matches(".*_utility")) %>% 
    select(sort(peek_vars())) %>% 
    mutate(decision_position = decision_positions) %>% 
    as.matrix() %>% 
    apply(., 1, function(x) {x[tail(x, n=1)]})

  # Output: pop with decisions and observed outcomes
  pop %>% 
    mutate(decision = decisions,
           decision_utility = utilities,
           appear = appearances) %>% 
    left_join(one_hot_decisions, by = "person_ix") 
  
}


calc_true_utility <- function(pop, decision_col_regex) {
  
  # List required columns to compute expected utility
  create_true_utility_colname <- function(col_name) {
    action <- str_remove(col_name, decision_col_regex)
    glue("truth.{action}_utility")
  }
  treat_prob_cols <- pop %>% 
    select(matches(decision_col_regex)) %>% 
    colnames()
  subset_cols <- append(treat_prob_cols,
                        create_true_utility_colname(treat_prob_cols))
  
  # Compute expected utilities of actions
  # From multiplying treatment probabilities with true expected appearance rates
  expected_utilities <- pop %>% 
    select(person_ix, subset_cols) %>% 
    pivot_longer(cols = !person_ix) %>% 
    mutate(name = str_remove(name, "truth.")) %>% 
    separate(name, sep = "_", into = c("action", "source"), extra = "merge") %>% 
    pivot_wider(id_cols = c(person_ix, action), 
                names_from = source,
                values_from = value) %>% 
    mutate(expected_utility = treat_prob * utility) %>% 
    pivot_wider(id_cols = person_ix,
                values_from = expected_utility,
                names_from = action,
                names_glue = "{action}_expected_utility")
  
  # Calc total expected utility across actions
  # Output: pop with expected utility              
  pop %>% 
    left_join(expected_utilities, by = "person_ix") %>% 
    rowwise() %>% 
    mutate(overall_expected_utility = sum(c_across(contains("_expected_utility")))) %>% 
    ungroup()
}

calc_demo_balance <- function(pop, decision_col_regex) {
  pop %>% 
    select(person_ix, targetgroup, matches(decision_col_regex)) %>% 
    pivot_longer(cols = matches(decision_col_regex), names_to = "treatment", values_to = "decision") %>% 
    group_by(treatment, targetgroup) %>% 
    summarize(total_group_n = n(),
              total_group_treatments = sum(decision), .groups = "drop_last") %>% 
    mutate(total_treatments = sum(total_group_treatments),
           total_n = sum(total_group_n)) %>% 
    ungroup() %>% 
    mutate(group_pct = total_group_treatments / total_group_n,
           overall_pct = total_treatments / total_n,
           diff = abs(group_pct - overall_pct))
}

calc_demo_penalty <- function(pop, decision_col_regex, lambda) {
   pop %>% 
    calc_demo_balance(decision_col_regex) %>% 
    group_by(targetgroup) %>% 
    summarize(penalties = sum(diff) * (lambda / 2), .groups = "drop") %>% 
    # Divide by 2 conforms with optimizer code
    # (In theory this should be the number of demo groups)
    summarize(penalty = sum(penalties)) %>% 
    pull()
}

total_expected_true_utility <- function(pop, decision_col_regex, lambda) {
  penalty <- pop %>% 
    calc_demo_penalty(decision_col_regex, lambda)

  pop %>% 
    calc_true_utility(decision_col_regex) %>% 
    mutate(penalized_overall_expected_utility = overall_expected_utility - penalty) %>% 
    summarize(across(matches("truth.*_utility"), ~ sum(.)),
              total_overall_expected_utility_unpenalized = sum(overall_expected_utility),
              total_overall_expected_utility = sum(penalized_overall_expected_utility)) %>% 
    rename_with(~ str_replace(., "truth.", "baseline_all_"), cols = matches("truth."))
}

calc_cumulative_penalized_utility <- function(slice_ix, pop) {
  sliced_pop <- pop %>% 
    slice(1:slice_ix)
  
  penalty <- sliced_pop %>% 
    calc_demo_penalty("_decision$", lambda)
  
  sliced_pop %>% 
    mutate(decision_utility_unpenalized = decision_utility,
           decision_utility = decision_utility - penalty) %>%
    summarize(cumulative_decision_utility_unpenalized = sum(decision_utility_unpenalized),
              cumulative_decision_utility = sum(decision_utility)) %>% 
    mutate(person_ix = slice_ix,
           penalty = penalty) %>% 
    relocate(person_ix)
}
