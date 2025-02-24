# Load required libraries
library(ggplot2)
library(readxl)
library(dplyr)

# Read the Excel file
data <- read_excel("/Users/Grace/Desktop/2025-studio/project-2/university_price_analysis_sum.xlsx")

# Sort data by total_cost in descending order
data <- data %>%
  arrange(desc(total_cost))

# Create the dot and line chart
ggplot(data, aes(x = reorder(University, -total_cost), y = total_cost)) +
  geom_point(size = 3, color = "#75B2DD") +  # Add dots
  geom_line(group = 1, color = "#75B2DD") +  # Add lines connecting the dots
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis text
    axis.ticks.x = element_blank(),  # Remove x-axis ticks
    panel.grid.minor = element_blank()  # Remove minor grid lines
  ) +
  labs(
    x = "",
    y = ""
  )
# Save the plot
ggsave("/Users/Grace/Desktop/2025-studio/project-2/set.png", 
       width = 15, height = 12, dpi = 300)