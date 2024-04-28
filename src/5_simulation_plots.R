### Setup ----------------------------------------------------------------------
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

calculate_mean_and_ci <- function(data, colname) {
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



### Import data ----------------------------------------------------------------
# Location of main simulation data
data_dir                        <- here("data", "simulation_output", 
                                        "main_sim")

# Location of spending variations simulation data
data_dir_spending_var_expensive <- here("data", "simulation_output", 
                                        "spending_vars_expensive")
data_dir_spending_var_cheaper   <- here("data", "simulation_output", 
                                        "spending_vars_cheaper")

sim_files <- fs::dir_ls(data_dir, regexp = "\\.rds$")
sim_output <- sim_files %>% 
  map_dfr(read_rds)

sim_files_spending_variations_expensive <- 
  fs::dir_ls(data_dir_spending_var_expensive, regexp = "\\.rds$")
sim_files_spending_variations_cheaper <- 
  fs::dir_ls(data_dir_spending_var_cheaper, regexp = "\\.rds$")
sim_files_spending_variations <- c(sim_files_spending_variations_expensive,
                                   sim_files_spending_variations_cheaper)
sim_output_spending_variations <- sim_files_spending_variations %>% 
  map_dfr(read_rds)



### Prepare data ---------------------------------------------------------------
training_runs <- sim_output %>% 
  filter(!is_warmup,
         sim_num <= 7311) %>% 
  group_by(sim_num, method) %>% 
  mutate(bestguess_policy_treatment_expected_reward = 
           bestguess_policy_expected_reward - 
           bestguess_policy_control_expected_reward,
         cumulative_transits = cumsum(transit_decision),
         cumulative_rideshares = cumsum(rideshare_decision)) %>% 
  relocate(cumulative_transits, .after = transit_decision) %>% 
  relocate(cumulative_rideshares, .after = rideshare_decision) %>% 
  ungroup() %>% 
  mutate(method = as.character(method), 
         method = str_replace(method, "Egreedy", "ɛ-Greedy"))

num_training <- max(training_runs$person_ix)
num_sims <- length(unique(training_runs$sim_num))

print(glue("Number of simulations: {num_sims}"))


### Figure 4 -------------------------------------------------------------------
oracle_trialtreat <- training_runs %>% 
  filter(method == "Oracle") %>% 
  select(sim_num, person_ix, 
         oracle_cumulative_observed_utility = cumulative_observed_utility)

figure_4_data <- training_runs %>% 
  filter(method != "Oracle",
         !str_detect(method, "ignore lambda"),
         !str_detect(method, "only")) %>% 
  left_join(oracle_trialtreat, by = c("sim_num", "person_ix")) %>% 
  mutate(regret = oracle_cumulative_observed_utility - 
           cumulative_observed_utility) %>% 
  filter(!is.na(regret)) %>% 
  group_by(method, person_ix) %>% 
  calculate_mean_and_ci(regret) %>% 
  rename(Method = method) %>% 
  mutate(Method = str_replace(Method, "RCT", "RA"))

figure_4 <- figure_4_data %>% 
  ggplot(aes(x = person_ix, y = mean, color = Method)) +
  geom_line(linewidth = 0.75) +
  geom_hline(yintercept = 0, color = "dimgray", linetype = "dashed") +
  xlab("Iteration \U0001d48a") + ylab("Regret (lost utility)") +
  scale_colour_manual(values = brewer_set2_methods, 
                      aesthetics = c("colour", "fill")) +
  geom_text_repel(aes(label = Method),
                  nudge_x = num_training * 0.05,
                  hjust = 0,
                  data = figure_4_data %>% filter(person_ix == num_training),
                  direction = "y",
                  segment.color = "dimgray",
                  arrow = arrow(length = unit(0.01, "npc"))) +
  coord_cartesian(xlim = c(0, 1175)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000)) +
  theme(legend.position = "none")

ggsave(plot = figure_4,
       filename = here("output", "figure_4.pdf"),
       width = 7, height = 5,
       device = cairo_pdf)


### Figure 5 -------------------------------------------------------------------
oracle_bestguess <- training_runs %>% 
  filter(method == "Oracle") %>% 
  select(sim_num, person_ix, 
         oracle_treatment_expected_reward = 
           bestguess_policy_treatment_expected_reward)

figure_5_data <- training_runs %>% 
  filter(method != "Oracle",
         !str_detect(method, "ignore lambda"),
         !str_detect(method, "only")) %>% 
  left_join(oracle_bestguess, by = c("sim_num", "person_ix")) %>% 
  mutate(pct_oracle_treatment_expected_reward = 
           bestguess_policy_treatment_expected_reward / 
           oracle_treatment_expected_reward) %>% 
  filter(!is.na(pct_oracle_treatment_expected_reward)) %>% 
  group_by(method, person_ix) %>% 
  calculate_mean_and_ci(pct_oracle_treatment_expected_reward) %>% 
  rename(Method = method) %>% 
  mutate(Method = str_replace(Method, "RCT", "RA"))

figure_5 <- figure_5_data %>% 
  ggplot(aes(x = person_ix, y = mean, color = Method)) +
  geom_line(linewidth = 0.75) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Method), 
              alpha = 0.2, color = NA) +
  geom_hline(yintercept = 1, color = "dimgray", linetype = "dashed") +
  xlab("Iteration \U0001d48a") + ylab("Performance compared to oracle") +
  theme(axis.title.x = ggtext::element_markdown()) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_colour_manual(values = brewer_set2_methods, 
                      aesthetics = c("colour", "fill")) +
  geom_text_repel(aes(label = Method),
                  nudge_x = num_training * 0.05,
                  hjust = 0,
                  ylim = c(NA, 1),
                  data = figure_5_data %>% filter(person_ix == num_training),
                  direction = "y",
                  segment.color = "dimgray",
                  arrow = arrow(length = unit(0.01, "npc"))) +
  coord_cartesian(xlim = c(0, 1175), ylim = c(0.8, 1)) +
  scale_x_continuous(breaks = c(0, 250, 500, 750, 1000)) +
  theme(legend.position = "none")

ggsave(plot = figure_5,
       filename = here("output", "figure_5.pdf"),
       width = 7, height = 5,
       device=cairo_pdf)


### Table 2 --------------------------------------------------------------------
simulation_budget <- 5

table_2 <- training_runs %>% 
  filter(str_detect(method, "UCB|Thompson|Greedy"))%>% 
  group_by(method) %>% 
  summarize(spending_disparity = mean(observed_spending[targetgroup == 1]) - 
              simulation_budget,
            .groups = "drop_last") %>% 
  separate(method, c("method", "penalty"), sep = " ignore ", fill = "right") %>% 
  mutate(penalty = str_replace(penalty, "lambda", "No penalty"),
         penalty = str_replace_na(penalty, "With penalty"),
         spending_disparity = dollar(spending_disparity)) %>% 
  arrange(desc(method), desc(penalty)) %>% 
  pivot_wider(id_cols = method, 
              names_from = penalty, 
              values_from = spending_disparity) 

table_2


### Figure 6 -------------------------------------------------------------------
figure_6_data <- sim_output_spending_variations %>%
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

figure_6 <- figure_6_data %>% 
  filter(person_ix == 1000) %>% 
  group_by(method, method_group, is_spending_variation) %>% 
  slice(1:100) %>% 
  summarize(n = n(),
            average_total_spending = mean(cumulative_observed_spending),
            average_pp_spending = average_total_spending / 1000,
            cumulative_observed_utility = mean(cumulative_observed_utility),
            bestguess_policy_expected_utility = 
              mean(bestguess_policy_expected_utility)) %>% 
  ggplot(aes(x = cumulative_observed_utility,
             y = bestguess_policy_expected_utility,
             color = average_total_spending,
             shape = method_group)) +
  geom_point(aes(size = average_total_spending), 
             alpha = 0.9,
             stroke = 1) +
  annotate(geom = "segment", x = 650, y = 610, xend = 682, yend = 687) +
  annotate(geom = "label",   x = 650, y = 610, label = "UCB", ) +
  scale_x_continuous(name = "Utility during learning phase", 
                     limits = c(500, 1000)) +
  scale_y_continuous(name = "Utility from best-guess policy", 
                     limits = c(300, 750)) +
  scale_size_continuous(name = "Total spending", 
                        range = c(1, 10), 
                        breaks = c(1000, 5000, 10000, 25000, 50000),
                        labels = scales::dollar) + 
  scale_color_gradientn(name = "Total spending",
                        colors = c("#377eb8", "#377eb8", '#e41a1c', 
                                   '#e41a1c', "#4daf4a", "#4daf4a"),
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
                                         linewidth=0.5, linetype="solid", 
                                         colour ="dimgray")) 

ggsave(plot = figure_6,
       filename = here("output", "figure_6.pdf"),
       width = 7, height = 5,
       device=cairo_pdf)


### Figure F.2 -----------------------------------------------------------------
figure_f2_data <- training_runs %>% 
  filter(!str_detect(method, "ignore lambda"),
         !str_detect(method, "only")) %>% 
  group_by(method, sim_num) %>% 
  mutate(cumulative_spending = cumsum(observed_spending)) %>% 
  ungroup() %>% 
  calculate_mean_and_ci(cumulative_spending) %>% 
  rename(Method = method) %>% 
  mutate(Method = str_replace(Method, "RCT", "RA"))

figure_f2 <- figure_f2_data %>% 
  ggplot(aes(x = person_ix, y = mean, color = Method)) +
  annotate("label", x = num_training + 5, y = num_training * 5, 
           label = "Budget", label.size = 0, hjust = "left") +
  geom_line() +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper, fill = Method), 
              alpha = 0.2, color = NA) +
  geom_segment(aes(x = 0, y = 0, xend = num_training, yend = num_training * 5), 
               color = "black", linetype = "dashed") +
  xlab("Iteration \U0001d48a") +
  ylab("Number of treatments") +
  scale_colour_manual(values = brewer_set2_methods, 
                      aesthetics = c("colour", "fill")) +
  scale_x_continuous(breaks = seq(0,1000,250)) + 
  coord_cartesian(xlim = c(0, 1100)) +
  theme(legend.position = c(0.17, 0.72), 
        legend.title = element_blank(),
        legend.background = element_rect(fill = alpha("white", 0.7), 
                                         colour = NA, linewidth = 0))

ggsave(plot = figure_f2,
       filename = here("output", "figure_ec2.pdf"),
       width = 7, height = 5, device=cairo_pdf)


### Figure F.3 -----------------------------------------------------------------

figure_f3_data <- sim_output %>% 
  filter(method %in% c("RCT", "Egreedy", "Thompson", "UCB"),
         !is_warmup) %>% 
  arrange(person_ix) %>% 
  group_by(method, sim_num) %>% 
  mutate(cumulative_spending = cumsum(observed_spending)) %>% 
  ungroup() %>% 
  filter(person_ix == 1000) %>% 
  mutate(percent_of_budget = cumulative_spending / 5000) 

figure_f3 <- figure_f3_data %>% 
  ggplot(aes(x = percent_of_budget)) +
  geom_histogram(bins = 50) +
  scale_x_continuous(labels = percent,
                     name = "Total spending as percent of budget") +
  scale_y_continuous(name = "") +
  coord_cartesian(xlim = c(0.83, 1.18))

ggsave(plot = figure_f3,
       filename = here("output", "figure_ec3.pdf"),
       width = 7, height = 5,
       device=cairo_pdf)
