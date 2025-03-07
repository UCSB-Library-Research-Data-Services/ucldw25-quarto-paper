# Bar chart for Fetching average time per Breed
# Load the data
timevsbreed <- read.csv("data/cat-fetching.csv")
        
# Load necessary libraries
  library(ggplot2)
        
# Code for bar chart
  ggplot(timevsbreed, aes(x = reorder(breed, -average_hours_per_week), y = average_hours_per_week, fill = breed)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7, alpha = 0.7) +
  labs(title = "Fetching average time per breed",
       x = "Breed", 
       y = "Average Hours per Week",
       fill = "Breed") +
  scale_fill_brewer(palette = "Set3") +  # Choosing colors
  scale_y_continuous(breaks = seq(0, 3.0, by = 0.5)) +  # Set y-axis
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
       legend.position = "none",  # Hide the legend
       panel.grid.major.x = element_blank(),  # Remove vertical grid lines
       panel.grid.minor.x = element_blank(),  # Remove vertical grid lines
       panel.grid.major.y = element_line(color = "gray", linewidth = 0.1))  # Keep major horizontal grid lines only
  