## ----setup--------------------------------------------------------------------
require(tidyverse)
require(glue)
require(scales)
require(ggrepel)
require(stargazer)
require(here)

theme_set(theme_bw(base_size = 14))

brewer_set2_methods <- c("RA" = '#e41a1c',
                         "RA to 50" = '#984ea3',
                         "RA to 100" = '#4daf4a',
                         "RA to 150" = '#377eb8',
                         "ɛ-Greedy" = '#ff7f00',
                         "UCB" = '#f781bf',
                         "Thompson" = '#a65628')

## ----import_data--------------------------------------------------------------


data_dir <- here("data", "run_4")
sim_files <- fs::dir_ls(data_dir, regexp = "\\.rds$")

sim_output <- sim_files %>% 
  map_dfr(read_rds)

# Add cumulative counts
training_runs <- sim_output %>% 
  filter(!is_warmup) %>% 
  group_by(sim_num, method) %>% 
  mutate(bestguess_policy_treatment_expected_reward = bestguess_policy_expected_reward - bestguess_policy_control_expected_reward,
         cumulative_transits = cumsum(transit_decision),
         cumulative_rideshares = cumsum(rideshare_decision)) %>% 
  relocate(cumulative_transits, .after = transit_decision) %>% 
  relocate(cumulative_rideshares, .after = rideshare_decision) %>% 
  ungroup() %>% 
  # Post simulation tweaks
  mutate(method = as.character(method), 
         method = str_replace(method, "Egreedy", "ɛ-Greedy"))

num_training <- max(training_runs$person_ix)
num_sims <- length(unique(training_runs$sim_num))

print(glue("Number of simulations: {num_sims}"))


## ----summary_function---------------------------------------------------------

summarize_vals <- function(data, colname) {

  data %>% 
    group_by(method, person_ix) %>% 
    summarize(mean = mean({{colname}}),
              sd = sd({{colname}}),
              se = sd / sqrt(n()),
              ci_size = se * 2,
              ci_upper = mean + ci_size,
              ci_lower = mean - ci_size,
              .groups = "drop") %>% 
    ungroup()

}

## ----policy_performance_plot--------------------------------------------------

# Best guess pct of optimal
oracle_bestguess <- training_runs %>% 
  filter(method == "Oracle") %>% 
  select(sim_num, person_ix, 
         oracle_treatment_expected_reward = bestguess_policy_treatment_expected_reward)
pct_of_optimal <- training_runs %>% 
  filter(method != "Oracle",
         !str_detect(method, "ignore lambda"),
         !str_detect(method, "only")) %>% 
  left_join(oracle_bestguess, by = c("sim_num", "person_ix")) %>% 
  mutate(pct_oracle_treatment_expected_reward = 
           bestguess_policy_treatment_expected_reward / oracle_treatment_expected_reward) %>% 
  filter(!is.na(pct_oracle_treatment_expected_reward)) %>% 
  group_by(method, person_ix) %>% 
  summarize_vals(pct_oracle_treatment_expected_reward) %>% 
  rename(Method = method) %>% 
  mutate(Method = str_replace(Method, "RCT", "RA"))

pct_of_optimal_chart <- pct_of_optimal %>% 
  ggplot(aes(x = person_ix, y = mean, color = Method)) +
  geom_line(size = 0.75) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Method), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, color = "dimgray", linetype = "dashed") +
  xlab("Iteration \U0001d48a") + ylab("Performance compared to oracle") +
  theme(axis.title.x = ggtext::element_markdown()) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = brewer_set2_methods, aesthetics = c("colour", "fill")) +
  geom_text_repel(aes(label = Method),
                  nudge_x = num_training * 0.05,
                  hjust = 0,
                  ylim = c(NA, 1),
                  data = pct_of_optimal %>% filter(person_ix == num_training),
                  direction = "y",
                  segment.color = "dimgray",
                  arrow = arrow(length = unit(0.01, "npc"))) +
  coord_cartesian(xlim = c(0, 1175), ylim = c(0.8, 1)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000)) +
  theme(legend.position = "none")
ggsave(plot = pct_of_optimal_chart,
       filename = "~/nudge/analysis/simulation/pct_of_optimal.pdf",
       width = 7, height = 5,
       device=cairo_pdf)

## ----ignored_parity plot------------------------------------------------------------

# Best guess pct of optimal
pct_of_optimal_ignore_lambda <- training_runs %>% 
  filter(!str_detect(method, "Oracle"),
         str_detect(method, "ignore lambda"),
         !str_detect(method, "only")) %>% 
  left_join(oracle_bestguess, by = c("sim_num", "person_ix")) %>% 
  mutate(method = str_replace(method, " ignore lambda", ""),
         pct_oracle_treatment_expected_reward = 
         bestguess_policy_treatment_expected_reward / oracle_treatment_expected_reward) %>% 
  filter(!is.na(pct_oracle_treatment_expected_reward)) %>% 
  group_by(method, person_ix) %>% 
  summarize_vals(pct_oracle_treatment_expected_reward) %>% 
  rename(Method = method)

pct_of_optimal_ignore_lambda_chart <- pct_of_optimal_ignore_lambda %>% 
  ggplot(aes(x = person_ix, y = mean, color = Method)) +
  geom_line(size = 0.75) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Method), alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, color = "dimgray", linetype = "dashed") +
  xlab("Iteration \U0001d48a") + ylab("Performance compared to oracle") +
  theme(axis.title.x = ggtext::element_markdown()) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = brewer_set2_methods, aesthetics = c("colour", "fill")) +
  geom_text_repel(aes(label = Method),
                  nudge_x = num_training * 0.05,
                  hjust = 0,
                  ylim = c(NA, 1),
                  data = pct_of_optimal_ignore_lambda %>% filter(person_ix == num_training),
                  direction = "y",
                  segment.color = "dimgray",
                  arrow = arrow(length = unit(0.01, "npc"))) +
  coord_cartesian(xlim = c(0, 1175), ylim = c(0.8, 1)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000)) +
  theme(legend.position = "none")
ggsave(plot = pct_of_optimal_ignore_lambda_chart,
       filename = "~/nudge/analysis/simulation/pct_of_optimal_ignore_lambda.pdf",
       width = 7, height = 5,
       device=cairo_pdf)

simulation_budget <- 5

spending_disparities <- training_runs %>% 
  filter(str_detect(method, "UCB|Thompson|Greedy"))%>% 
  group_by(method) %>% 
  summarize(spending_disparity = mean(observed_spending[targetgroup == 1]) - simulation_budget,
            .groups = "drop_last") %>% 
  separate(method, c("method", "penalty"), sep = " ignore ") %>% 
  mutate(penalty = str_replace(penalty, "lambda", "No penalty"),
         penalty = str_replace_na(penalty, "With penalty"),
         spending_disparity = dollar(spending_disparity)) %>% 
  arrange(desc(method), desc(penalty)) %>% 
  pivot_wider(id_cols = method, names_from = penalty, values_from = spending_disparity) 

spending_disparities %>% 
  print(n = 10)

spending_disparities %>% 
  stargazer(summary = FALSE, rownames = FALSE)
  


## -regret_plot--------------------------------------------------------------
# Regret
# cumulative_decision_utility
oracle_trialtreat <- training_runs %>% 
  filter(method == "Oracle") %>% 
  select(sim_num, person_ix, oracle_cumulative_observed_utility = cumulative_observed_utility)
regrets <- training_runs %>% 
  filter(method != "Oracle",
         !str_detect(method, "ignore lambda"),
         !str_detect(method, "only")) %>% 
  left_join(oracle_trialtreat, by = c("sim_num", "person_ix")) %>% 
  mutate(regret = oracle_cumulative_observed_utility - cumulative_observed_utility) %>% 
  filter(!is.na(regret)) %>% 
  group_by(method, person_ix) %>% 
  summarize_vals(regret) %>% 
  rename(Method = method) %>% 
  mutate(Method = str_replace(Method, "RCT", "RA"))

regret_chart <- regrets %>% 
  # filter(Method %in% c("RCT", "UCB", "ɛ-Greedy", "Thompson")) %>% 
  ggplot(aes(x = person_ix, y = mean, color = Method)) +
  geom_line(size = 0.75) +
  geom_hline(yintercept = 0, color = "dimgray", linetype = "dashed") +
  # geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Method), alpha = 0.2, color = NA) +
  xlab("Iteration \U0001d48a") + ylab("Regret (lost utility)") +
  scale_colour_manual(values = brewer_set2_methods, aesthetics = c("colour", "fill")) +
  geom_text_repel(aes(label = Method),
                  nudge_x = num_training * 0.05,
                  hjust = 0,
                  data = regrets %>% filter(person_ix == num_training),
                  direction = "y",
                  segment.color = "dimgray",
                  arrow = arrow(length = unit(0.01, "npc"))) +
  coord_cartesian(xlim = c(0, 1175)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000)) +
  theme(legend.position = "none")
ggsave(plot = regret_chart,
     filename = "~/nudge/analysis/simulation/regret.pdf",
     width = 7, height = 5,
     device = cairo_pdf)


## ----budget_adherence_plot----------------------------------------------------

# Budgets

spending <- training_runs %>% 
  filter(!str_detect(method, "ignore lambda"),
         !str_detect(method, "only")) %>% 
  group_by(method, sim_num) %>% 
  mutate(cumulative_spending = cumsum(observed_spending)) %>% 
  ungroup() %>% 
  summarize_vals(cumulative_spending) %>% 
  rename(Method = method) %>% 
  mutate(Method = str_replace(Method, "RCT", "RA"))
  
spending_chart <- spending %>% 
  ggplot(aes(x = person_ix, y = mean, color = Method)) +
  annotate("label", x = num_training + 5, y = num_training * 5, label = "Budget", label.size = 0, hjust = "left") +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Method), alpha = 0.2, color = NA) +
  geom_segment(aes(x = 0, y = 0, xend = num_training, yend = num_training * 5), color = "black", linetype = "dashed") +
  xlab("Iteration \U0001d48a") +
  ylab("Number of treatments") +
  scale_colour_manual(values = brewer_set2_methods, aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(0,1000,250)) + 
  coord_cartesian(xlim = c(0, 1100)) +
  theme(legend.position = c(0.17, 0.72), 
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.7), colour = NA, size = 0))

ggsave(plot = spending_chart,
       filename = "~/nudge/analysis/simulation/budget_adherence.pdf",
       width = 7, height = 5, device=cairo_pdf)



## ---- spending vs. quality chart----------------------------------------------------

data_dir_spending_variations_expensive <- "/share/data/nudge/simulations/fair_allocation/run_8/"
sim_files_spending_variations_expensive <- fs::dir_ls(data_dir_spending_variations_expensive, regexp = "\\.rds$")

data_dir_spending_variations_cheaper <- "/share/data/nudge/simulations/fair_allocation/run_9/"
sim_files_spending_variations_cheaper <- fs::dir_ls(data_dir_spending_variations_cheaper, regexp = "\\.rds$")

sim_files_spending_variations <- c(sim_files_spending_variations_expensive,
                                   sim_files_spending_variations_cheaper)
sim_output_spending_variations <- sim_files_spending_variations %>% 
  map_dfr(read_rds)


spending_variations <- sim_output_spending_variations %>%
  bind_rows(sim_output %>% filter(method %in% c("UCB", "RCT"))) %>% 
  filter(!is_warmup) %>% 
  mutate(method_group = if_else(method == "UCB",
                                "UCB",
                                "RCT"),
         is_spending_variation = if_else(method %in% c("UCB", "RCT"),
                                         FALSE, TRUE)) %>% 
  group_by(sim_num, method, method_group, is_spending_variation) %>% 
  arrange(person_ix) %>% 
  mutate(cumulative_observed_spending = cumsum(observed_spending),
         rideshare_treat_prop = mean(rideshare_decision),
         transit_treat_prop = mean(transit_decision)) %>% 
  group_by(method, method_group, is_spending_variation) %>% 
  mutate(average_spending = mean(observed_spending)) %>% 
  ungroup() %>% 
  mutate(method = str_replace(method, "RCT", "RA"),
         method_group = str_replace(method_group, "RCT", "RA"))

spending_variation_chart <- spending_variations %>% 
  filter(person_ix == 1000) %>% 
  group_by(method, method_group, is_spending_variation) %>% 
  slice(1:100) %>% 
  summarize(n = n(),
            average_total_spending = mean(cumulative_observed_spending),
            average_pp_spending = average_total_spending / 1000,
            cumulative_observed_utility = mean(cumulative_observed_utility),
            bestguess_policy_expected_utility = mean(bestguess_policy_expected_utility)) %>% 
  ggplot(aes(x = cumulative_observed_utility,
             y = bestguess_policy_expected_utility,
             color = average_total_spending,
             # group = method,
             shape = method_group)) +
  geom_point(aes(size = average_total_spending), 
             # shape = 1, 
             alpha = 0.9,
             stroke = 1) +
  annotate(geom = "segment", x = 650, y = 610, xend = 682, yend = 687) +
  annotate(geom = "label",   x = 650, y = 610, label = "UCB", ) +
  scale_x_continuous(name = "Utility during learning phase", limits = c(500, 1000)) +
  scale_y_continuous(name = "Utility from best-guess policy", limits = c(300, 750)) +
  scale_size_continuous(name = "Total spending", 
                        range = c(1, 10), 
                        breaks = c(1000, 5000, 10000, 25000, 50000),
                        labels = scales::dollar) + 
  scale_color_gradientn(name = "Total spending",
                        colors = c("#377eb8", "#377eb8", '#e41a1c', '#e41a1c', "#4daf4a", "#4daf4a"),
                        values = c(0, 0.092, 0.0921, 0.095, 0.09551, 1),
                        breaks = c(1000, 5000, 10000, 25000, 50000),
                        labels = scales::dollar,
                        guide = guide_legend()) +
  scale_shape_manual(name = "Method", values = c(1,2)) +
  guides(colour = guide_legend(override.aes = list(shape = 1),
                               order = 2),
         size = guide_legend(order = 2),
         shape =  guide_legend(order = 1)) +
  theme(legend.box = "horizontal",
        legend.position = c(0.725, 0.25), 
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="dimgray")) 

 ggsave(plot = spending_variation_chart,
       filename = "~/nudge/analysis/simulation/spending_variations.pdf",
       width = 7, height = 5,
       device=cairo_pdf)
 
 
 
## ---- reference population size chart --------------------------------------
 
data_dir_ref_pop_sizes <- "/share/data/nudge/simulations/fair_allocation/run_10/"
sim_files_ref_pop_sizes <- fs::dir_ls(data_dir_ref_pop_sizes, regexp = "\\.rds$")

sim_output_ref_pop_sizes <- sim_files_ref_pop_sizes %>% 
  map_dfr(read_rds)

# How many sims?
sim_output_ref_pop_sizes %>% 
  distinct(method, sim_num) %>% 
  count(method)
 
sim_output_ref_pop_sizes %>% 
  filter(sim_num == 1000,
         person_ix > 0) %>% 
  select(person_ix, method, decision) %>% 
  arrange(person_ix, method) %>% 
  head(100) %>% print(n = 100)

utility_compared <- sim_output_ref_pop_sizes %>% 
  filter(!is_warmup) %>% 
  pivot_wider(id_cols = c(sim_num, person_ix),
              names_from = method,
              values_from = cumulative_observed_utility) %>% 
  mutate(ucb_1000_2000_proportion = UCB_2000_sim_pop / UCB,
         ucb_1000_500_proportion  = UCB_500_sim_pop  / UCB) %>% 
  group_by(person_ix) %>% 
  summarize(ucb_1000_2000_proportion = mean(ucb_1000_2000_proportion),
            ucb_1000_500_proportion = mean(ucb_1000_500_proportion)) 

# Compare total observed utility
utility_compared %>% 
  mutate(across(contains("proportion"), ~ . * 100000)) %>% 
  filter(person_ix == 1000) 

# Calculate rough number of possible feature combinations
sim_output_ref_pop_sizes %>% 
  filter(sim_num == 1003,
         method == "UCB") %>% 
  select(targetgroup:appt_hist_inv) %>% 
  summarize(across(everything(), ~ n_distinct(.))) %>% 
  pivot_longer(everything(), names_to = "feature", values_to = "num_values") %>% 
  mutate(cum_product = cumprod(num_values))

2 * 2 * 2 * 20 * 20 * 16 * 20
2 * 2 * 2 * 10 * 10 * 10 * 10


## ---- understanding spending variation --------------------------------------

cumulative_spending_variations <- sim_output %>% 
  filter(method %in% c("RCT", "Egreedy", "Thompson", "UCB"),
         !is_warmup) %>% 
  arrange(person_ix) %>% 
  group_by(method, sim_num) %>% 
  mutate(cumulative_spending = cumsum(observed_spending)) %>% 
  ungroup() %>% 
  filter(person_ix == 1000) %>% 
  mutate(percent_of_budget = cumulative_spending / 5000) 

cumulative_spending_variation_chart <- cumulative_spending_variations %>% 
  ggplot(aes(x = percent_of_budget)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(labels = percent,
                     name = "Total spending as percent of budget") +
  scale_y_continuous(name = "") +
  coord_cartesian(xlim = c(0.83, 1.18))

ggsave(plot = cumulative_spending_variation_chart,
       filename = "~/nudge/analysis/simulation/cumulative_spending_variations.pdf",
       width = 7, height = 5,
       device=cairo_pdf)

cumulative_spending_variations %>% 
  mutate(diff_from_100 = 1 - percent_of_budget,
         abs_diff_from_100 = abs(diff_from_100)) %>% 
  arrange(diff_from_100) %>% 
  mutate(cum_pct = row_number() / n()) %>% 
  filter(diff_from_100 < 0.05) %>% 
  select(diff_from_100, cum_pct) %>% 
  tail()

cumulative_spending_variations %>% 
  mutate(diff_from_100 = 1 - percent_of_budget,
         abs_diff_from_100 = abs(diff_from_100)) %>% 
  arrange(diff_from_100) %>% 
  mutate(cum_pct = row_number() / n()) %>% 
  filter(cum_pct < 0.8) %>% 
  select(diff_from_100, cum_pct) %>% 
  tail()


## ---- characterizing appearance rates ---------------------------------------

sim_output %>% 
  summarize(mean(control_true_appear_outcome))

sim_output %>% 
  group_by(targetgroup) %>% 
  summarize(mean(control_true_appear_outcome))

