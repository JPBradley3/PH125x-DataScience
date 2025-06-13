# Load required packages
library(tidyverse)
library(maps)
library(viridis)
library(patchwork)

# Load the raw data
brfss_raw <- readRDS("C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/data_processed/brfss_2014-2022_raw.rds")

# Check if we have the required columns
print("Columns in the data:")
print(names(brfss_raw))

# Create dummy data for demonstration
set.seed(123)
state_data <- tibble(
  state_name = tolower(state.name),
  trans_percent = runif(length(state.name), 0.1, 2.5),
  lgb_percent = runif(length(state.name), 1.0, 8.0)
)

# Prepare data for plotting
state_data_long <- state_data %>%
  pivot_longer(
    cols = c(trans_percent, lgb_percent),
    names_to = "population_group",
    values_to = "percent"
  ) %>%
  mutate(
    population_group = case_when(
      population_group == "trans_percent" ~ "% Transgender",
      population_group == "lgb_percent" ~ "% LGB",
      TRUE ~ population_group
    )
  )

# Get US state boundaries
us_states <- map_data("state")

# Join the percent data onto map polygons
plot_df <- us_states %>%
  left_join(state_data_long, by = c("region" = "state_name"))

# Map for % Transgender
p_trans <- plot_df %>%
  filter(population_group == "% Transgender") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "gray70") +
  scale_fill_viridis_c(
    option = "turbo",
    labels = scales::percent_format(scale = 1),
    name = "% Transgender"
  ) +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "% Transgender Population by State (Simulated Data)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Map for % LGB
p_lgb <- plot_df %>%
  filter(population_group == "% LGB") %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(aes(fill = percent), color = "gray70") +
  scale_fill_viridis_c(
    option = "plasma",
    labels = scales::percent_format(scale = 1),
    name = "% LGB"
  ) +
  coord_fixed(1.3) +
  theme_void() +
  labs(title = "% LGB Population by State (Simulated Data)") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14)
  )

# Combine the two state maps side-by-side
combined_plot <- p_trans + p_lgb +
  patchwork::plot_annotation(
    title = "Simulated Percentage of Adults by State",
    subtitle = "Comparison of Transgender and LGB Populations",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16),
                  plot.subtitle = element_text(hjust = 0.5, size = 12))
  )

# Print the combined plot
print(combined_plot)

# Save the plots
ggsave("C:/Users/Parker/Documents/DataScienceClass/Capstone/PH125x-DataScience/Capstone/Drafting/plots/simulated_map.png", 
       combined_plot, width = 12, height = 8)