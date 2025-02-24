library(tidyverse) 
library(readr)      

# Read the CSV file
pennant_data <- read_csv("/Users/Grace/Desktop/2025-studio/project-2/pennant.csv")

# Clean and prepare the data
pennant_clean <- pennant_data %>%
  filter(!is.na(Price), !is.na(University)) %>%
  arrange(desc(Price)) %>%
  # Create an index for x-axis instead of university names
  mutate(index = row_number()) %>%
  # Create a color flag for highest and lowest prices
  mutate(color_flag = case_when(
    Price == max(Price) ~ "Extreme",
    Price == min(Price) ~ "Extreme",
    TRUE ~ "Normal"
  ))

# Create the simplified bar plot
ggplot(pennant_clean, aes(x = index, y = Price, fill = color_flag)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Extreme" = "#4A708B", "Normal" = "#C6E2FF")) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),  # Remove x-axis labels
    axis.ticks.x = element_blank(), # Remove x-axis ticks
    legend.position = "none"        # Remove legend
  ) +
  labs(
    x = "",
    y = "Price ($)"
  ) +
  scale_y_continuous(labels = scales::dollar_format())

# Save the plot
ggsave("anonymous_pennant_prices.png", width = 10, height = 6)