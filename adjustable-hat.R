library(tidyverse)
library(readxl)    

# Read the Excel file
pennant_data <- read_excel("/Users/Grace/Desktop/2025-studio/project-2/adjustable-hat.xlsx")

# Clean and prepare the data
pennant_clean <- pennant_data %>%
  filter(!is.na(Price)) %>%  # Keep this filter for NA prices
  arrange(desc(Price)) %>%
  # Create an index for x-axis instead of university names
  mutate(index = row_number()) %>%
  # Create a color flag for highest and lowest prices
  mutate(color_flag = case_when(
    Price == max(Price, na.rm = TRUE) ~ "Extreme",
    Price == min(Price, na.rm = TRUE) ~ "Extreme",
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
    y = ""
  ) +
  scale_y_continuous(labels = scales::dollar_format())

# Save the plot
ggsave("ajustable-hat.png", width = 10, height = 6)