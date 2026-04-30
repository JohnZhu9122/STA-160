# STA 160 Mid-Quarter Project 
# Topic: Palmer Penguin Niche Partitioning: A Data-Driven Analysis of Morphological Differentiation
# Jiawei Zhu

# 1. Load Necessary Libraries
library(palmerpenguins)
library(tidyverse)


# Save the original dataset to a CSV file, that to show there are some NA values in the original dataset, and to make it easier for others to access the data without needing to load the palmerpenguins package.
write.csv(penguins, "penguins.csv", row.names = FALSE)

# For Data Cleaning
# Remove rows with NA values in the relevant columns to ensure data integrity
new_penguins <- penguins %>%
  filter(!is.na(bill_length_mm) & !is.na(bill_depth_mm) & 
         !is.na(flipper_length_mm) & !is.na(body_mass_g))

nrow(new_penguins)



# Visualizations Data Cleaning
# First Graph: Flipper Length vs Body Mass (Low Explanatory Power due to overlap)
new_body <- ggplot(new_penguins, aes(x = flipper_length_mm, y = body_mass_g, color = species)) +
  geom_point(alpha = 0.7) +
  labs(title = "Figure 1: Flipper Length vs Body Mass",
       x = "Flipper Length (mm)",
       y = "Body Mass (g)") +
  theme_minimal() +
  scale_color_manual(values = c("Adelie" = "blue", "Chinstrap" = "orange", "Gentoo" = "green"))

# Second Graph: Bill Morphometrics (High Explanatory Power - Clear Clusters)
new_bill <- ggplot(new_penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.7) +
  labs(title = "Figure 2: Bill Length vs Bill Depth",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  theme_minimal() +
  scale_color_manual(values = c("Adelie" = "blue", "Chinstrap" = "orange", "Gentoo" = "green"))

# Third Graph: Advanced Morphological Clusters by Island (Sympatric Evidence)
# Adding 95% confidence ellipses and faceting by island
new_advanced <- ggplot(new_penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(alpha = 0.5) + 
  stat_ellipse(level = 0.95, linetype = 2) + 
  facet_wrap(~island) + 
  theme_bw() + 
  labs(title = "Figure 3: Morphological Clusters by Species and Island",
       x = "Bill Length (mm)",
       y = "Bill Depth (mm)") +
  scale_color_manual(values = c("Adelie" = "blue", "Chinstrap" = "orange", "Gentoo" = "green"))

# Count of Each Species on Dream Island
dreams_penguins <- new_penguins %>%
  filter(island == "Dream") %>%
  count(species)

dreams_penguins

# Fourth Graph: ECDF of Bill Length by Species (Additional Insight into Morphological Differentiation)w
  new_ecdf <- ggplot(new_penguins, aes(x = bill_length_mm, color = species)) +
  geom_step(stat = "ecdf") +
  labs(title = "Figure 4: ECDF of Bill Length by Species",
       x = "Bill Length (mm)",
       y = "ECDF") +
  theme_minimal() +
  scale_color_manual(values = c("Adelie" = "blue", "Chinstrap" = "orange", "Gentoo" = "green"))



# For easily to check the graphs
ggsave(plot = new_body, filename = "Figure_1.png", width = 8, height = 6, dpi = 300)
ggsave(plot = new_bill, filename = "Figure_2.png", width = 8, height = 6, dpi = 300)
ggsave(plot = new_advanced, filename = "Figure_3.png", width = 9, height = 6, dpi = 300)
ggsave(plot = new_ecdf, filename = "Figure_4.png", width = 8, height = 6, dpi = 300)