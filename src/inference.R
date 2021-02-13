
utility_targetgroupbonus_form <- function(appearances_all, appearances_targetgroup) {
  appearances_all + 0.5 * appearances_targetgroup 
}

calc_utility_targetgroupbonus <- function(pop, outcome_col_regex) {
  pop %>% 
    mutate(across(matches(outcome_col_regex), 
                  ~ utility_targetgroupbonus_form(., . * targetgroup), 
                  .names = "{.col}_utility")) %>% 
    rename_with(~ str_remove(., "_appear_prob"),
                matches(glue("{outcome_col_regex}_utility")))
}

utility_simple_form <- function(appearances_all) {
  appearances_all 
}

calc_utility_simple <- function(pop, outcome_col_regex) {
  pop %>% 
    mutate(across(matches(outcome_col_regex), 
                  ~ utility_simple_form(.), 
                  .names = "{.col}_utility")) %>% 
    rename_with(~ str_remove(., "_appear_prob"),
                matches(glue("{outcome_col_regex}_utility")))
}

calc_inferences <- function(model_coefs) {
  
  function(pop) {
    # I found it necessary to do it this way to preserve passing multiple models 
    # (e.g., 25 posterior models at once)
    
    pop <- pop %>% 
      crossing(model_coefs)

    pop %>% 
      mutate(inferred.control_appear_prob = pop_model(transit = 0, rideshare = 0,
                                                      targetgroup, age, proximity, poverty,
                                                      est_age_coef, est_age_targetgroup_coef,
                                                      est_transit_proximity_coef, est_transit_proximity_targetgroup_coef, 
                                                      est_rideshare_poverty_coef, est_rideshare_poverty_targetgroup_coef),
             inferred.transit_appear_prob    = pop_model(transit = 1, rideshare = 0,
                                                      targetgroup, age, proximity, poverty,
                                                      est_age_coef, est_age_targetgroup_coef,
                                                      est_transit_proximity_coef, est_transit_proximity_targetgroup_coef, 
                                                      est_rideshare_poverty_coef, est_rideshare_poverty_targetgroup_coef),
             inferred.rideshare_appear_prob = pop_model(transit = 0, rideshare = 1,
                                                      targetgroup, age, proximity, poverty,
                                                      est_age_coef, est_age_targetgroup_coef,
                                                      est_transit_proximity_coef, est_transit_proximity_targetgroup_coef, 
                                                      est_rideshare_poverty_coef, est_rideshare_poverty_targetgroup_coef))
  } 
  
}

calc_inferences_UCB <- function(model_coefs, percentile = 0.95) {

  function(pop) {

    optimistic_appear_probs <- pop %>% 
      calc_inferences(model_coefs)() %>% 
      group_by(person_ix) %>% 
      summarize(across(matches("inferred.*appear_prob"), ~ quantile(., 0.95)), 
                .groups = "drop_last")
    
    pop %>% 
      left_join(optimistic_appear_probs, by = "person_ix") %>% 
      mutate(model_ix = as.factor(1))

  }

}

calc_inferences_oracle <- function(pop) {
  pop %>%
    mutate(across(matches("truth.*_appear_prob"), ~ ., .names = "inferred.{.col}"),
           model_ix = as_factor(1)) %>% 
    rename_with(~ str_replace(., "inferred.truth.", "inferred."))
}
