set.seed(55)  # Set seed for reproducibility

# List of possible values for each variable
breed <- c("Siamese", "Moggie", "Persian", "Maine Coon", "Bengal", "British Shorthair", "Sphynx", "Ragdoll", "Russian Blue", "Norwegian Forest")
times_of_day <- c("Morning", "Afternoon", "Evening")
energy_levels <- c("Low", "Medium", "High")
toys <- c("Rubber Bands", "Hair Ties", "Spheric Objects", "Stationary Items", "Rope", "Textile", "Plush Toys", "Catnip Toys")
health_condition <- c("Healthy", "Minor Illness", "Chronic Illness", "Overweight/Obese")

# Generating synthetic data for 1543 cats
catfetching <- data.frame(
  age = sample(1:12, 1543, replace = TRUE),  # Random age between 1 and 12
  sex = sample(c("Male", "Female"), 1543, replace = TRUE),  # Random sex
  breed = sample(breed, 1543, replace = TRUE),  # Random breed
  only_cat = sample(c("Yes", "No"), 1543, replace = TRUE),  # Random "Yes" or "No"
  preferred_time = sample(times_of_day, 1543, replace = TRUE),  # Random time of day
  energy_level = sample(energy_levels, 1543, replace = TRUE),  # Random energy level
  average_hours_per_week = sample(1:4, 1543, replace = TRUE),  # Random hours between 1 and 20
  preferred_toy = sample(toys, 1543, replace = TRUE),  # Random toy choice
  household = sample(1:5, 1543, replace = TRUE),  # Random number on household between 1 and 5
  health_condition = sample(health_condition, 1543, replace = TRUE)  # Random health condition (No trailing comma)
)


# View the first few rows of the dataset
head(catfetching)

# Save the dataset to a CSV file
write.csv(catfetching, "/Users/rcurty/quarto-paper/data/cat_fetching-data.csv", row.names = FALSE)

#SUMMARIES

# Get summary statistics for all numeric variables
summary(catfetching$age)
summary(catfetching$average_hours_per_week)

# Frequency count for the sex column
table(catfetching$sex)

# Frequency count for preferred toy
table(catfetching$preferred_toy)

# Frequency count for health condition
table(catfetching$health_condition)

# Histogram for age distribution
hist(catfetching$age, main = "Age Distribution of Cats", xlab = "Age", col = "lightblue", border = "black")

# Barplot for the distribution of sex (Male vs. Female)
barplot(table(catfetching$sex), main = "Sex Distribution of Cats", col = c("orange", "lightgreen"))

# Boxplot of average hours per week by sex
boxplot(average_hours_per_week ~ sex, data = catfetching, main = "Average Hours per Week by Sex", 
        xlab = "Sex", ylab = "Average Hours per Week", col = c("orange", "lightgreen"))

# Barplot for the distribution of health condition
barplot(table(catfetching$health_condition), main = "Health Condition of Cats", col = "lightgreen")

# Load ggplot2 package
library(ggplot2)

# Box plot of Average Hours per Week by Preferred Toy using ggplot2
ggplot(catfetching, aes(x = preferred_toy, y = average_hours_per_week)) +
  geom_boxplot(fill = "lightblue", color = "darkblue", notch = TRUE) +  # Box plot with color and notch
  labs(title = "Comparison of Average Hours per Week by Preferred Toy", 
       x = "Preferred Toy", 
       y = "Average Hours per Week") + 
  theme_minimal() +  # Use minimal theme for a clean look
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability


# Load necessary libraries
library(ggplot2)

# Create boxplot comparing average time spent per week by preferred toy
ggplot(catfetching, aes(x = preferred_toy, y = average_hours_per_week, fill = preferred_toy)) +
  geom_boxplot(alpha = 0.6) +  # Boxplot with some transparency
  labs(title = "Comparison of Average Time Spent per Week by Preferred Toy", 
       x = "Preferred Toy", 
       y = "Average Time Spent per Week (hours)") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.position = "none")  # Remove legend


# Boxplot comparing average time spent per week by preferred toy and sex
ggplot(catfetching, aes(x = preferred_toy, y = average_hours_per_week, fill = sex)) +
  geom_boxplot(alpha = 0.6) +  # Add boxplot with transparency
  labs(title = "Comparison of Average Time Spent per Week by Preferred Toy and Sex", 
       x = "Preferred Toy", 
       y = "Average Time Spent per Week (hours)", 
       fill = "Sex") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "bottom")  # Move legend to the bottom


# Violin plot comparing time spent per week by preferred toy and sex
ggplot(catfetching, aes(x = preferred_toy, y = average_hours_per_week, fill = sex)) +
  geom_violin(alpha = 0.6) +  # Add violin plot with transparency
  labs(title = "Violin Plot: Time Spent per Week by Preferred Toy and Sex", 
       x = "Preferred Toy", 
       y = "Average Time Spent per Week (hours)", 
       fill = "Sex") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "bottom")  # Position legend at the bottom

# Bar chart for Fetching average time per Breed
ggplot(catfetching, aes(x = breed, y = average_hours_per_week, fill = breed)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7, alpha = 0.7) +
  labs(title = "Fetching average time per Breed",
       x = "Breed", 
       y = "Average Hours per Week",
       fill = "Breed") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
        legend.position = "none")  # Hide the legend

# New attempt
# Load required libraries
library(ggplot2)
library(dplyr)

# Reorder the breed factor based on average hours per week (mean)
breed_order <- catfetching %>%
  group_by(breed) %>%
  summarize(mean_time = mean(average_hours_per_week)) %>%
  arrange(mean_time)  # Arrange in ascending order of mean time

# Reorder the breed factor in the dataset
catfetching$breed <- factor(catfetching$breed, levels = breed_order$breed)

# Create the bar plot with the breeds ordered by mean time
ggplot(catfetching, aes(x = breed, y = average_hours_per_week, fill = breed)) +
  stat_summary(fun = "mean", geom = "bar", position = "dodge", width = 0.7, alpha = 0.7) +
  labs(title = "Average Hours per Week by Breed (Ordered in Crescent)",
       x = "Breed", 
       y = "Average Hours per Week",
       fill = "Breed") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better readability
        legend.position = "none")  # Hide the legend if not necessary


# Get summary statistics for all numeric variables
summary(catfetching$age)
summary(catfetching$average_hours_per_week)

# Frequency count for the sex column
table(catfetching$sex)

# Frequency count for preferred toy
table(catfetching$preferred_toy)

# Loading necessary libraries
library(dplyr)
library(tidyr)

# Summary statistics for all numeric variables
numeric_summary <- catfetching %>%
  summarise(
    age_mean = mean(age, na.rm = TRUE),
    age_sd = sd(age, na.rm = TRUE),
    hours_per_week_mean = mean(average_hours_per_week, na.rm = TRUE),
    hours_per_week_sd = sd(average_hours_per_week, na.rm = TRUE)
  )

# Frequency counts for categorical variables
sex_count <- table(catfetching$sex)
toy_count <- table(catfetching$preferred_toy)

# Displaying results
numeric_summary
sex_count
toy_count

# Install the required libraries if you don't have them
install.packages("knitr")
install.packages("kableExtra")

library(knitr)
library(kableExtra)

# Frequency count for the sex column
sex_count <- table(catfetching$sex)

# Print the frequency count for 'sex' nicely
sex_count_table <- as.data.frame(sex_count)
colnames(sex_count_table) <- c("Sex", "Frequency")
kable(sex_count_table, caption = "Frequency Count for Sex") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))

# Frequency count for the preferred toy column
toy_count <- table(catfetching$preferred_toy)

# Print the frequency count for 'preferred_toy' nicely
toy_count_table <- as.data.frame(toy_count)
colnames(toy_count_table) <- c("Preferred Toy", "Frequency")
kable(toy_count_table, caption = "Frequency Count for Preferred Toy") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "responsive"))



#second age histogram
# Improved histogram with custom x-axis scale
hist(catfetching$age, 
     main = "Sample Age Distribution", 
     xlab = "Age", 
     ylab = "Frequency", 
     col = "lightblue",       # Fill color
     border = "gray",         # Border color
     breaks = 15,             # Number of bins
     probability = TRUE,      # Add density curve instead of frequency
     xlim = c(0, 250),        # Set x-axis scale from 0 to 250
     ylim = c(0, 0.1),        # Adjust y-axis limit to fit the density
     cex.main = 1.5,          # Increase title size
     cex.lab = 1.2,           # Increase axis label size
     cex.axis = 1.1,          # Increase axis tick label size
     col.main = "darkblue",   # Title color
     col.lab = "darkred",     # Axis label color
     border = "black",        # Darker border for clarity
     las = 1)                 # Make axis labels horizontal

# Add a density curve for a smoother look
lines(density(catfetching$age), col = "darkgreen", lwd = 2)  # Green line for density curve

# Add grid lines for better readability
grid(col = "gray", lty = "dotted")  # Light gray dotted grid lines



