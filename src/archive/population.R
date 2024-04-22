
pop_model <- function(transit, 
                      rideshare,
                      targetgroup,
                      age, 
                      proximity,
                      poverty,
                      mod_age_coef,
                      mod_age_targetgroup_coef,
                      mod_transit_proximity_coef, 
                      mod_transit_proximity_targetgroup_coef, 
                      mod_rideshare_poverty_coef,
                      mod_rideshare_poverty_targetgroup_coef) {

  Z <-           (mod_age_coef * age + 
                  mod_transit_proximity_coef * transit * proximity + 
                  mod_rideshare_poverty_coef * rideshare * poverty) +
  targetgroup  * (mod_age_targetgroup_coef * age + 
                  mod_transit_proximity_targetgroup_coef * transit * proximity + 
                  mod_rideshare_poverty_targetgroup_coef * rideshare * poverty)
  
  # Inverse logit transformation
  1 / (1 + exp(-Z))
  
}

pop_sample <- function(num_sim,
                       targetgroup_p,
                       mod_age_coef,
                       mod_age_targetgroup_coef,
                       mod_transit_proximity_coef, 
                       mod_transit_proximity_targetgroup_coef, 
                       mod_rideshare_poverty_coef,
                       mod_rideshare_poverty_targetgroup_coef) {
  
  response_prob <- function(transit, rideshare, targetgroup,
                            age, proximity, poverty) {
    pop_model(transit, rideshare, targetgroup,
              age, proximity, poverty, 
              mod_age_coef, mod_age_targetgroup_coef,
              mod_transit_proximity_coef, mod_transit_proximity_targetgroup_coef, 
              mod_rideshare_poverty_coef, mod_rideshare_poverty_targetgroup_coef)
  }
  
  response <- function(prob, u_r) {
    as.integer(u_r <= prob)
  }
  
  tibble(
    
    targetgroup = as.integer(rbernoulli(num_sim, targetgroup_p)),
    
    age = runif(num_sim),
    proximity = runif(num_sim),
    poverty = runif(num_sim),
    
    truth.control_appear_prob = response_prob(transit = 0, rideshare = 0, targetgroup, age, proximity, poverty),
    truth.transit_appear_prob    = response_prob(transit = 1, rideshare = 0, targetgroup, age, proximity, poverty),
    truth.rideshare_appear_prob = response_prob(transit = 0, rideshare = 1, targetgroup, age, proximity, poverty),
    
    u_r = runif(num_sim),
    truth.control_appear = response(truth.control_appear_prob, u_r),
    truth.transit_appear    = response(truth.transit_appear_prob,    u_r),
    truth.rideshare_appear = response(truth.rideshare_appear_prob, u_r)
    
  ) 
}

generate_samples <- function(params, n) {
  param_names <- params %>% 
    colnames()
  
  params %>% 
    mutate(bol = pmap(., pop_sample, num_sim = n)) %>% 
    unnest(bol) %>% 
    relocate(all_of(param_names), .after = last_col()) %>% 
    mutate(person_ix = row_number()) %>% 
    select(person_ix, everything())
  
  
}
