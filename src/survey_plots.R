require(tidyverse)
require(lubridate)
require(janitor)

theme_set(theme_bw(size = 14))

# Load data --------------------------------------------------------------------

cutoff_datetime <- ymd_hms("2023-12-21 14:00:00")

responses <- read_csv("~/Downloads/LTBF survey December 21, 2023_20.09.csv") %>% 
  clean_names() %>% 
  slice(3:n()) %>% 
  filter(x18 == "5 people who live far from court",
         x19 == "5 Black people") %>% 
  mutate(start_date = ymd_hms(start_date),
         pref_black = case_when(pref_black_asc == "Option A" | 
                                  pref_black_desc == "Option E" ~ 0,
                                pref_black_asc == "Option B" | 
                                  pref_black_desc == "Option D" ~ 2.5,
                                pref_black_asc == "Option C" | 
                                  pref_black_desc == "Option C" ~ 5,
                                pref_black_asc == "Option D" | 
                                  pref_black_desc == "Option B" ~ 7.5,
                                pref_black_asc == "Option E" | 
                                  pref_black_desc == "Option A" ~ 10),
         political_party = coalesce(independent_q,
                                    political_aff)) %>% 
  filter(start_date > cutoff_datetime)

responses %>% 
  count(pref_black) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x = pref_black, y = percent)) +
  geom_bar(stat = "identity") +
  geom_label(aes(label = n),
             nudge_y = 0.02) +
  scale_y_continuous(name = "",
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(name = "",
                     breaks = c(0, 2.5, 5, 7.5, 10),
                     labels = scales::dollar)

party_plot <- responses %>% 
  filter(political_party %in% c("Democrat", "Republican")) %>% 
  group_by(political_party, pref_black) %>%
  summarize(n = n(), .groups = "drop_last") %>%
  mutate(average = sum(pref_black * n) / sum(n),
         percent = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x = pref_black, y = percent, fill = political_party)) +
  geom_bar(stat = "identity") +
  geom_vline(aes(xintercept = average),
             linetype = "dashed",
             data = . %>% distinct(political_party, average)) +
  facet_wrap(~ political_party) +
  scale_y_continuous(name = "",
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(name = "Average spending per Black client",
                     breaks = c(0, 2.5, 5, 7.5, 10),
                     labels = scales::dollar) +
  scale_fill_manual(values = c("Democrat" = "RoyalBlue",
                               "Republican" = "red"),
                    guide = FALSE)

party_plot

ggsave("~/Dropbox/Apps/Overleaf/Learning to be fair/figs/survey_results.pdf",
       party_plot,
       width = 6.5, height = 3)


gender_plot <- responses %>% 
  filter(gender %in% c("Man", "Woman", "Non-binary")) %>% 
  group_by(gender, pref_black) %>%
  summarize(n = n(), .groups = "drop_last") %>%
  mutate(average = sum(pref_black * n) / sum(n),
         percent = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x = pref_black, y = percent, fill = gender)) +
  geom_bar(stat = "identity") +
  geom_vline(aes(xintercept = average),
             linetype = "dashed",
             data = . %>% distinct(gender, average)) +
  facet_wrap(~ gender) +
  scale_y_continuous(name = "",
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(name = "Average spending per Black client",
                     breaks = c(0, 2.5, 5, 7.5, 10),
                     labels = scales::dollar) +
  scale_fill_discrete(h.start = 0,
                      direction = -1,
                      guide = FALSE)

ggsave("~/Dropbox/Apps/Overleaf/Learning to be fair/figs/survey_results_gender.pdf",
       gender_plot,
       width = 6.5, height = 3)


race_ethnicity_plot <- responses %>% 
  mutate(race = case_when(str_detect(hispanic, "Yes") ~ "Hispanic",
                          str_detect(race, "Black") ~ "Black",
                          str_detect(race, "Asian") ~ "Asian",
                          str_detect(race, "White") ~ "White")) %>% 
  filter(!is.na(race)) %>% 
  group_by(race, pref_black) %>%
  summarize(n = n(), .groups = "drop_last") %>%
  mutate(average = sum(pref_black * n) / sum(n),
         percent = n / sum(n)) %>%
  ungroup() %>% 
  ggplot(aes(x = pref_black, y = percent, fill = race)) +
  geom_bar(stat = "identity") +
  geom_vline(aes(xintercept = average),
             linetype = "dashed",
             data = . %>% distinct(race, average)) +
  facet_wrap(~ race) +
  scale_y_continuous(name = "",
                     labels = scales::percent,
                     expand = expansion(mult = c(0, 0.05))) +
  scale_x_continuous(name = "Average spending per Black client",
                     breaks = c(0, 2.5, 5, 7.5, 10),
                     labels = scales::dollar) +
  scale_fill_manual(values = c("Black" = '#984ea3',
                               "Asian" = '#4daf4a',
                               "White" = 'DodgerBlue',
                               "Hispanic" = '#ff7f00'),
                    guide = FALSE)

ggsave("~/Dropbox/Apps/Overleaf/Learning to be fair/figs/survey_results_race_ethnicity.pdf",
       race_ethnicity_plot,
       width = 5.5, height = 5.5)
