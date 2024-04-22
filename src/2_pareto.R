# Load necessary libraries
require(tidyverse)
require(here)

theme_set(theme_bw())

# Parameters -------------------------------------------------------------------
n_clients <- 10000 # Total number of clients
fixed_budget <- 50000 # Fixed budget
cost_per_mile <- 5 * 2 # Two way
mean_distance_white      <- 2  # Average distance for White clients in miles
mean_distance_black_near <- 1   # Average distance for Black clients in miles
mean_distance_black_far  <- 10  # Average distance for Black clients in miles
proportion_near <- 0.25
sd_distance_near <- 1 # Standard deviation for distances
sd_distance_far <- 5
ride_treatment_effect <- 0.25
lambda <- 0.025


# Create synthetic population --------------------------------------------------
set.seed(2023)
population <- 
  tibble(id = 1:n_clients) %>%
  mutate(
    race = rep(c("White", "Black"), each = n_clients / 2),
    # Assign distances based on race with some overlap
    distance = if_else(race == "White",
                       abs(rnorm(n(), mean_distance_white, sd_distance_near)),
                       if_else(runif(n()) < proportion_near,
                               abs(rnorm(n(), mean_distance_black_near, 
                                         sd_distance_near)),
                               abs(rnorm(n(), mean_distance_black_far,  
                                         sd_distance_far)))),
    cost = distance * cost_per_mile,
    # Assuming equal treatment effect for simplicity
    treatment_effect = ride_treatment_effect,
    p_0 = 0.75,
    p_1 = p_0 + ride_treatment_effect
  )

population %>% 
  ggplot(aes(x = distance, color = race)) +
  geom_density()


# Function to calculate trade-offs  --------------------------------------------
calculate_tradeoffs <- function(population, fixed_budget, threshold) {
  treated_black <- population %>%
    filter(race == "Black") %>% 
    arrange(cost) %>% 
    mutate(cum_cost = cumsum(cost)) %>% 
    filter(cost <= threshold,
           cum_cost <= fixed_budget)
  
  remaining_budget <- fixed_budget - sum(treated_black$cost)
  
  treated_white <- population %>%
    filter(race == "White") %>%
    arrange(cost) %>% 
    mutate(cum_cost = cumsum(cost)) %>% 
    filter(cum_cost <= remaining_budget)
  
  num_black <- treated_black %>% nrow()
  num_white <- treated_white %>% nrow()
  
  average_spending_black <- (treated_black %>%
                               summarize(total_cost = sum(cost)) %>%
                               pull(total_cost)) / (n_clients / 2)
  average_spending_white <- (treated_white %>%
                               summarize(total_cost = sum(cost)) %>%
                               pull(total_cost)) / (n_clients / 2)
  
  treated_population <- bind_rows(treated_black, treated_white)
  total_appearances <- sum(treated_population$treatment_effect)
  
  return(tibble(threshold, 
                num_black,
                num_white,
                average_spending_black, 
                average_spending_white,
                total_appearances))
}


# Calculate trade-offs for different thresholds  -------------------------------
thresholds <- seq(from = cost_per_mile * min(population$distance), 
                  to   = cost_per_mile * max(population$distance), 
                  by   = 0.5)

tradeoff_results <- map_df(thresholds, 
                           ~ calculate_tradeoffs(population, 
                                                 fixed_budget, 
                                                 .x)) 


# Calculate points on Pareto curve  --------------------------------------------
max_appearances <- tradeoff_results %>% 
  slice_max(order_by = total_appearances) %>% 
  # This just approximates the middle x and y values if there are multiple maxes
  summarize(average_spending_black = mean(average_spending_black),
            total_appearances = mean(total_appearances)) %>% 
  mutate(label = "Maximum appearances",
         nudge_x = 0.0625,
         nudge_y = 20)

max_utility <- tradeoff_results %>% 
  mutate(utility = total_appearances / n() - lambda * 
           (abs(fixed_budget / n_clients - average_spending_black) + 
              abs(fixed_budget / n_clients - average_spending_white))) %>% 
  slice_max(order_by = utility) %>% 
  # This just approximates the middle x and y values if there are multiple maxes
  summarize(average_spending_black = mean(average_spending_black),
            total_appearances = mean(total_appearances)) %>% 
  mutate(label = "Maximum utility",
         nudge_x = 0.125,
         nudge_y = 10)

spending_parity <- tradeoff_results %>% 
  mutate(dist_from_spending_parity = 
           abs((fixed_budget / n_clients) - average_spending_black)) %>% 
  slice_min(dist_from_spending_parity) %>% 
  # This just approximates the middle x and y values if there are multiple mins
  summarize(average_spending_black = mean(average_spending_black),
            total_appearances = mean(total_appearances)) %>% 
  mutate(label = "Spending parity",
         nudge_x = 0.125,
         nudge_y = 10)

equalized_fnr <- tradeoff_results %>%
  # Turns out that the equalized FNR point is the same as the demographic parity
  # point, so just use demographic parity as a proxy here, since it doesn't 
  # require us to calculate potential outcomes
  mutate(num_gap = abs(num_black - num_white)) %>% 
  slice_min(num_gap) %>% 
  # This just approximates the middle x and y values if there are multiple mins
  summarize(average_spending_black = mean(average_spending_black),
            total_appearances = mean(total_appearances)) %>% 
  mutate(label = "Equalized FNR",
         nudge_x = 0.125,
         nudge_y = 10)

labeled_points <- bind_rows(
  max_appearances,
  max_utility,
  spending_parity,
  equalized_fnr
)


# Plot Pareto curve  -----------------------------------------------------------
pareto_frontier <- ggplot(tradeoff_results, 
                          aes(x = average_spending_black, 
                              y = total_appearances)) +
  geom_vline(aes(xintercept = average_spending_black,
                 color = label),
             alpha = 0.5,
             linetype = "dashed",
             data = labeled_points) +
  geom_hline(aes(yintercept = total_appearances,
                 color = label),
             alpha = 0.5,
             linetype = "dashed",
             data = labeled_points) +
  geom_line() +
  geom_point(aes(color = label),
             size = 3,
             data = labeled_points) +
  geom_label(aes(color = label,
                 label = label,
                 x = average_spending_black + nudge_x,
                 y = total_appearances + nudge_y),
             label.size = NA,
             hjust = 0,
             data = labeled_points) +
  scale_x_continuous(labels = scales::dollar) +
  scale_color_discrete(guide = FALSE) +
  labs(title = "", 
       x = "Average spending per Black client", 
       y = "Number of additional appearances") +
  theme_bw(base_size = 14)

population %>% 
  summarize(mean(cost), .by = race)

tradeoff_results %>%
  mutate(spending_break = ((average_spending_black + 0.1) %/% 2.5) * 2.5) %>%
  group_by(spending_break) %>%
  mutate(distance_to_nearest_break = average_spending_black - 
           spending_break) %>%
  slice_min(distance_to_nearest_break, with_ties = F) %>% 
  mutate(spending_break =  scales::dollar(spending_break)) %>% 
  select(spending_break, average_spending_black, 
         average_spending_white, total_appearances)

ggsave(here("output", "figure_2.pdf"),
       pareto_frontier,
       width = 7, height = 5)
