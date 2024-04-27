require(tidyverse)
require(here)
require(sf)
require(ggmap)


### Figure 1a ------------------------------------------------------------------

# First-time users of `ggmap` will need to sign up for a Google API key here:
# https://mapsplatform.google.com/
# and then will need to provide their key to the function below: 
# register_google()

# Load data
client_addresses <- read_csv(here("data", "scc_client_map_data_masked.csv")) %>% 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, agr = "constant") %>% 
  filter(race %in% c("Hispanic", "Asian", "Black", "White")) %>%
  arrange(desc(race)) %>%
  mutate(race = factor(race, levels = sort(unique(race))))

# Set location of main county courthouse
scc_courthouse   <- st_as_sf(data.frame(lon = -121.90875, lat = 37.350746), 
                             coords = c("lon", "lat"),
                             crs = 4326, agr = "constant")

# Set center and boundaries of map
map_center_x <- -121.91
map_center_y <- 37.34
map_center   <- c("lon" = map_center_x, 
                  "lat" = map_center_y)
scc_base_map <- get_map(map_center, 
                        maptype = 'terrain',
                        color = 'bw', 
                        zoom = 11)

# These are derived from the bounding box that Google provides
bbox <- c("left"   = -122.12938,
          "bottom" = 37.16483,
          "right"  = -121.68993,
          "top"    = 37.51422)

# Make map
scc_clients_map <- ggmap(scc_base_map, darken = c(0.7, "white")) +
  geom_sf(data = client_addresses, inherit.aes = FALSE, 
          aes(color = race), shape = 16, size=1.6, alpha = 0.6) +
  theme(axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank(),
        legend.position = c(0.9225, 0.9),
        plot.margin=unit(c(-0.01, 0, -0.04, -0.04), "null")) +
  scale_color_manual(values = c("Asian"    = "#00a400", 
                                "Hispanic" = "#ff7700", 
                                "Black"    = "#984ea3", 
                                "White"    = "#00ABFD")) + 
  guides(color = guide_legend(override.aes = list(shape = 16, 
                                                  alpha = 1, size = 4))) +
  xlab("") + ylab("") +
  annotate("text", 
           x = unlist(scc_courthouse$geometry)[1], 
           y = unlist(scc_courthouse$geometry)[2], 
           label = "★", size = 10, color = "black") #,
           # This font family isn't required, it just makes a nicer star!
           # family = "AppleGothic") 

# Export map
ggsave(here("output", "figure_1a.pdf"),
       scc_clients_map,
       device = cairo_pdf, # cairo_pdf is required to export star to pdf
       width = 5.1, height = 5)



### Figure 1b ------------------------------------------------------------------

theme_set(theme_bw())

plot_data <- read_csv(here("data", "scc_no_parity_illustration.csv"))

num_years <- interval(ymd("2021-01-04"),
                      ymd("2023-12-18")) / years(1)

annnual_budget <- 50000
total_budget <- annnual_budget * num_years

highlighted_point <- plot_data %>% 
  filter(spending > total_budget) %>% 
  slice_min(spending, with_ties = FALSE) 

print(highlighted_point %>% 
        select(per_person_spending_target,
               per_person_spending_majority))

spending_disparity_chart <- plot_data %>% 
  ggplot(aes(x = per_person_spending_majority,
             y = per_person_spending_target)) +
  geom_abline(slope = 1, intercept = 0,
              color = "dimgray", linetype = "dashed") +
  geom_line() +
  geom_vline(aes(xintercept = per_person_spending_majority),
             data = highlighted_point, 
             linewidth = 1, linetype = "dotted", color = "red") +
  geom_hline(aes(yintercept = per_person_spending_target),
             data = highlighted_point, 
             linewidth = 1, linetype = "dotted", color = "red") +
  geom_point(color = "red",
             data = highlighted_point,
             size = 5) +
  scale_x_continuous(name = "Average spending for White clients",
                     labels = scales::dollar) +
  scale_y_continuous(name = "Average spending for Vietnamese clients",
                     labels = scales::dollar) +
  coord_equal(xlim = c(0, 15),
              ylim = c(0, 15))

spending_disparity_chart

ggsave(here("output", "figure_1b.pdf"),
       spending_disparity_chart,
       width = 5, height = 5)
