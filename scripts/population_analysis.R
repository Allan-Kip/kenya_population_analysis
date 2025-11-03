# ============================================================
# Kenya Population Analysis (2019 Census)
# ============================================================

# Required Libraries
library(tidyverse)
library(readr)
library(scales)

# ------------------------------------------------------------
# 1. Load Dataset

population <- read_csv("data/2019-population_census-report-per-county.csv")

# Preview Data
glimpse(population)
head(population)

# ------------------------------------------------------------
# 2. Data Cleaning
# ------------------------------------------------------------
# Rename and clean numeric columns
population <- population %>%
  rename(County = County, Population = Total_Population19) %>%
  mutate(Population = as.numeric(gsub(",", "", Population)))

# Create outputs directory if missing
if (!dir.exists("outputs")) dir.create("outputs")

# ------------------------------------------------------------
# 3. Descriptive Statistics
# ------------------------------------------------------------
summary_stats <- population %>%
  summarise(
    Total_Countries = n(),
    Mean_Pop = mean(Population, na.rm = TRUE),
    Median_Pop = median(Population, na.rm = TRUE),
    Max_Pop = max(Population, na.rm = TRUE),
    Min_Pop = min(Population, na.rm = TRUE),
    SD_Pop = sd(Population, na.rm = TRUE)
  )

print(summary_stats)

# ------------------------------------------------------------
# 4. Top 10 Most Populated Counties
# ------------------------------------------------------------
top10 <- population %>%
  arrange(desc(Population)) %>%
  slice_head(n = 10)

plot_top10 <- ggplot(top10, aes(x = reorder(County, Population), y = Population, fill = County)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Top 10 Most Populated Counties (2019 Census)",
       x = "County", y = "Population") +
  theme_minimal()

ggsave("outputs/top10_counties.png", plot_top10, width = 8, height = 6)

# ------------------------------------------------------------
# 5. Least Populated Counties
# ------------------------------------------------------------
bottom10 <- population %>%
  arrange(Population) %>%
  slice_head(n = 10)

plot_bottom10 <- ggplot(bottom10, aes(x = reorder(County, Population), y = Population, fill = County)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(labels = comma) +
  labs(title = "Least Populated Counties (2019 Census)",
       x = "County", y = "Population") +
  theme_minimal()

ggsave("outputs/bottom10_counties.png", plot_bottom10, width = 8, height = 6)

# ------------------------------------------------------------
# 6. Population Distribution
# ------------------------------------------------------------
plot_distribution <- ggplot(population, aes(x = Population)) +
  geom_histogram(binwidth = 200000, fill = "steelblue", color = "white") +
  scale_x_continuous(labels = comma) +
  labs(title = "Population Distribution Across Counties",
       x = "Population", y = "Number of Counties") +
  theme_minimal()

ggsave("outputs/population_distribution.png", plot_distribution, width = 8, height = 6)

# ------------------------------------------------------------
# 7. Population Share (Pie Chart)
# ------------------------------------------------------------
plot_pie <- population %>%
  arrange(desc(Population)) %>%
  mutate(share = Population / sum(Population) * 100) %>%
  slice(1:8) %>%
  ggplot(aes(x = "", y = share, fill = County)) +
  geom_col() +
  coord_polar(theta = "y") +
  labs(title = "Population Share by Top 8 Counties") +
  theme_void()

ggsave("outputs/population_share_pie.png", plot_pie, width = 7, height = 7)

# ------------------------------------------------------------
# 8. Save Summary Report
# ------------------------------------------------------------
write_csv(summary_stats, "outputs/population_summary.csv")


