# Load required libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(waffle)

# Create sample data frame for university bookstore prices
university_prices <- data.frame(
  University = rep(c("Columbia", "NYU", "Fordham", "CUNY"), each = 3),
  Item = rep(c("Hat", "Top", "Bottoms"), 4),
  Price = c(
    16.00, 44.98, 19.98,  # Columbia (top, shorts, hat)
    14.95, 25.00, 15.00,  # NYU (top, shorts, hat)
    11.96, 26.00, 18.00,  # Fordham (top, shorts, hat)
    19.00, 21.00, 15.00   # CUNY (top, shorts, hat)
  )
)

# Set the order of universities
university_prices$University <- factor(university_prices$University, 
                                       levels = c("Columbia", "NYU", "Fordham", "CUNY"))

# Calculate number of $10 units for each item
university_prices <- university_prices %>%
  mutate(
    Units = ceiling(Price/3)
  )

# Create data for waffle chart
waffle_data <- university_prices %>%
  group_by(University) %>%
  mutate(
    Total = sum(Price),
    Label = sprintf("%s\n$%.2f", University[1], Total)
  ) %>%
  ungroup()

# Create color gradients for each university
color_mapping <- list(
  "Columbia" = c(
    "Hat" = "#C4D8E2",     # Light Columbia Blue
    "Top" = "#75B2DD",     # Medium Columbia Blue
    "Bottoms" = "#061E44"  # Dark Columbia Blue
  ),
  "NYU" = c(
    "Hat" = "#9B7BB8",     # Light NYU Purple
    "Top" = "#57068C",     # Medium NYU Purple
    "Bottoms" = "#2A0344"  # Dark NYU Purple
  ),
  "Fordham" = c(
    "Hat" = "#FF9EAE",     # Light Fordham Maroon
    "Top" = "#900028",     # Medium Fordham Maroon
    "Bottoms" = "#4A0015"  # Dark Fordham Maroon
  ),
  "CUNY" = c(
    "Hat" = "#7B97D1",     # Light CUNY Blue
    "Top" = "#1D3A83",     # Medium CUNY Blue
    "Bottoms" = "#0E1C40"  # Dark CUNY Blue
  )
)

# Create the pictogram chart
ggplot(waffle_data, aes(fill = interaction(University, Item), values = Units)) +
  geom_waffle(
    n_rows = 1,
    size = 0.5,
    color = "white",
    flip = FALSE,
    aes(group = University)
  ) +
  facet_wrap(~University, ncol = 1, strip.position = "left") +
  scale_fill_manual(
    values = unlist(color_mapping)
  ) +
  coord_equal() +
  theme_minimal() +
  theme(
    strip.text = element_blank(),  # Remove university names
    legend.position = "none",      # Remove legend
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.spacing = unit(1, "lines"),
    plot.caption = element_text(hjust = 1, size = 9, color = "grey40")  # Right-aligned caption
  ) +
  labs(
    caption = "Data from each universities' bookstores | Chart by Xinlin Jiang"
  )

# Save the plot
ggsave("/Users/Grace/Desktop/2025-studio/project-2/nyc-us.png", 
       width = 15, height = 12, dpi = 300)