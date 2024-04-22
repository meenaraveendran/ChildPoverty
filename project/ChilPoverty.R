install.packages("tidyverse")
install.packages("ggplot2")
install.packages("maps")

# Load necessary libraries
library(tidyverse)
library(ggplot2)

# Read the dataset
data <- read.csv("/cloud/project/unicef_indicator_1.csv")
metadata <- read.csv("/cloud/project/unicef_metadata.csv")

print(colnames(metadata))

# Filter the data to include only 'Male' and 'Female'
gender_data <- data %>%
  filter(sex %in% c("Male", "Female"))

# Group by gender and summarize to find the average percentage of child poverty
gender_averages <- gender_data %>%
  group_by(sex) %>%
  summarize(average_percentage = mean(obs_value, na.rm = TRUE))

# Create the bar plot for average percentages
ggplot(gender_averages, aes(x = sex, y = average_percentage, fill = sex)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.2f%%", average_percentage)), vjust = -0.5, color = "black", size = 3.5) +
  labs(title = "Average Percentage of Child Poverty by Gender",
       x = "Gender",
       y = "Average Percentage") +
  theme_minimal()

# Calculate average percentage of child poverty per country and time period
average_poverty <- data %>%
  group_by(country, time_period) %>%
  summarize(average_poverty = mean(obs_value, na.rm = TRUE), .groups = 'drop')

# Get world map data
world_map <- map_data("world")

# Merge your data with the world map data
world_data <- world_map %>%
  left_join(average_poverty, by = c("region" = "country"))

# Plot the data
ggplot(data = world_data, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = average_poverty)) +
  scale_fill_gradient(low = "pink", high = "red", name = "Average % of Child Poverty") +
  coord_fixed(1.3) +
  labs(fill = "Average % of Child Poverty",
       title = "World Map of Child Poverty by Country and Time Period") +
  theme_minimal() +
  theme(legend.position = "bottom")

filtered_data <- read.csv("/cloud/project/FilteredData.csv")

print(colnames(filtered_data))

# Correct column names by replacing all dots and excess punctuation
names(filtered_data) <- names(filtered_data) %>%
  str_replace_all("\\.", " ") %>%
  str_replace_all(" {2,}", " ") %>%
  str_trim()

# Make sure column names are corrected by printing them
print(names(filtered_data))

# Filter by sex 
filter_data <- filtered_data %>%
  filter(sex %in% c("Male", "Female"))

print(names(filter_data))

ggplot(filter_data, aes(x = `Life expectancy at birth total years`, y = obs_value, color = sex)) +
  geom_point() +
  labs(
    title = "Scatterplot of Life Expectancy vs. Child Poverty",
    x = "Life Expectancy at Birth (years)",
    y = "Average Percentage of Child Poverty",
    caption = "Data includes both Male and Female"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Male" = "yellow", "Female" = "orange"))


# Scatter plot for GNI on top 10 countries 

# Correct column names by replacing all dots and excess punctuation
names(metadata) <- names(metadata) %>%
  str_replace_all("\\.", " ") %>%
  str_replace_all(" {2,}", " ") %>%
  str_trim()

print(colnames(metadata))

# List of top 10 countries
top_countries <- c("United States", "China", "Japan", "Germany", "United Kingdom", 
                   "France", "Brazil", "Italy", "Russia", "Canada")

# Prepare and plot the data for top 10 countries
metadata %>%
  filter(!is.na(`GNI current US`) & country %in% top_countries) %>%  # Filter for top 10 countries and non-missing GNI values
  mutate(year = as.Date(paste(year, "01", "01", sep = "-"))) %>%  # Create a proper date column
  ggplot(aes(x = year, y = `GNI current US`, group = country, color = country)) +  # Plotting GNI over time for each country
  geom_line() +  # Use line type for time series
  scale_y_continuous(labels = scales::unit_format(unit = "B", scale = 1e-9),  # Format y-axis labels with "b" for billion
                     breaks = c(0, 1000e9, 2000e9, 3000e9, 4000e9),  # Define breaks for y-axis in billions
                     limits = c(0, 4000e9),  # Set limits for y-axis
                     name = "GNI (current US$)") +  # Label for y-axis
  labs(title = "Time Series of GNI for Top 10 Countries",
       x = "Year",
       y = "GNI (current US$)",
       color = "Country") +
  theme_minimal() +
  theme(legend.position = "bottom")  # Adjust legend position



