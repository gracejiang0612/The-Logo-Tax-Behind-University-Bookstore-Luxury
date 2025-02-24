# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(scales)
library(ggtext)
# Create the dataset
data <- data.frame(
  Product = c(
    "Classic Ball Cap", "Align Tank", "Swiftly Tech Racerback Tank 2.0",
    "Hotty Hot HR Short 2.5\" Lined", "Define Jacket *Luon",
    "Perfectly Oversized Crew", "Scuba Oversized Funnel-Neck Full",
    "Cates Tee", "Everywhere Backpack", "Steady State Pullover Hoodie",
    "Zeroed In Track Jacket", "Evolution Short-Sleeve Polo",
    "Pace Breaker Short 5\"LL", "Metal Vent Tech Short Sleeve",
    "Soft Jersey Short Sleeve", "Scuba Oversized Funnel-Neck",
    "Everywhere Belt Bag", "Scuba Oversized Half-Zip Hoodie"
  ),
  Bookstore_Price = c(
    54.98, 89.98, 79.98, 89.98, 154.98, 139.98, 154.98, 74.98,
    129.98, 169.98, 154.98, 114.98, 89.98, 104.98, 74.98, 154.98,
    54.98, 154.98
  ),
  Lululemon_Price = c(
    38, 68, 58, 68, 118, 108, 128, 48, 78, 128, 128, 78, 68, 78,
    58, 128, 38, 118
  )
)

# Calculate markup percentage and order from highest to lowest
data <- data %>%
  mutate(
    Markup_Percentage = round((Bookstore_Price - Lululemon_Price) / Lululemon_Price * 100, 1)
  ) %>%
  arrange(Markup_Percentage) %>%
  mutate(Product = factor(Product, levels = Product))


# Create the visualization
ggplot(data, aes(y = Product)) +
  # Add bars for price comparison
  geom_segment(
    aes(x = Lululemon_Price, xend = Bookstore_Price, 
        yend = Product),
    color = "#CCEEF9",
    size = 5
  ) +
  geom_point(aes(x = Lululemon_Price), 
             color = "#EE6677", 
             size = 3) +
  geom_point(aes(x = Bookstore_Price), 
             color = "#4477AA", #4477AA
             size = 3) +
  # Add markup percentage
  geom_text(
    aes(x = Bookstore_Price + 5,
        label = sprintf("+%.1f%%", Markup_Percentage)),
    hjust = 0,
    size = 3
  ) +
  # Add annotation in the middle
  annotate(
    "richtext",
    x = 150,
    y = 14,
    label = "<b><span style='color:#4477AA'>Blue bars</span> show the price markup amount</b>",
    size = 4,
    fill = NA,
    label.color = NA,
    hjust = 0.5
  ) +
  # Customize theme and labels
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_markdown(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "grey40"),
    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(20, 80, 20, 20)
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
labs(
    title = "<span style='color:#4477AA'>Columbia Bookstore prices</span> show significant markup compared to <span style='color:#EE6677'>regular Lululemon prices</span>",
    subtitle = "Columbia University Bookstore vs. Lululemon Price Comparison",
    x = "Price ($)",
    caption = "Data from Columbia Bookstore| Chart by Xinlin Jiang"
  ) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_markdown(face = "bold", size = 14),
    plot.title.position = "plot",  # Add this line to align title to the left
    plot.subtitle = element_text(size = 11, color = "grey40"),
    plot.caption = element_text(size = 9, color = "grey40"),
    plot.margin = margin(20, 80, 20, 20)
  )

# Save the plot
ggsave("/Users/Grace/Desktop/2025-studio/project-2/lululemon_price_comparison.png", 
       width = 15, height = 12, dpi = 300)