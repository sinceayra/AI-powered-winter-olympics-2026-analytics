# ============================================================
#  2026 WINTER OLYMPICS — EXPLORATORY DATA ANALYSIS (EDA)
#  Tool: R  |  Dataset: olympics_athletes_dataset_2026.csv
# ============================================================

# ── LIBRARIES ───────────────────────────────────────────────
library(tidyverse)    # dplyr, ggplot2, tidyr, stringr, readr
library(lubridate)    # date handling
library(scales)       # axis formatting
library(ggthemes)     # clean plot themes
library(viridis)      # colour palettes

# ── LOAD DATA ───────────────────────────────────────────────
df <- read_csv("olympics_2026_cleaned - Sheet1.csv")


# ============================================================
#  PHASE 1 — DATA STRUCTURE & OVERVIEW
#  Q1. What is the shape and basic structure of the dataset?
#  Q2. How many missing values are there per column?
#  Q3. What are the data types of each column?
#  Q4. What are the summary statistics for numeric columns?
# ============================================================

# Q1 — Shape and first look
cat("Rows:", nrow(df), "\n")
cat("Columns:", ncol(df), "\n")
glimpse(df)         # column names + types + first values
head(df, 10)        # first 10 rows

# Q2 — Missing values per column
missing_summary <- df %>%
  summarise(across(everything(), ~ sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "missing_count") %>%
  mutate(missing_pct = round(missing_count / nrow(df) * 100, 1)) %>%
  arrange(desc(missing_count))

print(missing_summary)

# Visualise missing values
ggplot(missing_summary %>% filter(missing_count > 0),
       aes(x = reorder(column, missing_count), y = missing_pct)) +
  geom_col(fill = "#E05C5C") +
  coord_flip() +
  labs(title = "Missing Values by Column (%)",
       x = "Column", y = "% Missing") +
  theme_minimal()

# Q3 — Data types
sapply(df, class)

# Q4 — Numeric summary statistics
df %>%
  select(age, height_cm, weight_kg, total_olympics_attended,
         total_medals_won, result_value) %>%
  summary()


# ============================================================
#  PHASE 2 — COUNTRY ANALYSIS
#  Q5.  Which country sent the most athletes?
#  Q6.  Which country won the most gold medals?
#  Q7.  Which country has the best medal-to-athlete ratio?
#  Q8.  What does the full medal breakdown look like per country?
#  Q9.  Which country has the highest average Olympic experience
#       per athlete — indicating a stronger sports infrastructure?
# ============================================================

# Q5 — Athletes per country
athletes_per_country <- df %>%
  count(country_name, sort = TRUE)

ggplot(athletes_per_country %>% head(12),
       aes(x = reorder(country_name, n), y = n)) +
  geom_col(fill = "#4A90D9") +
  labs(title = "Q5 — Number of Athletes per Country (Top 12)",
       x = "Country", y = "Athlete Count")

# Q6 — Gold medals per country
gold_by_country <- df %>%
  filter(medal == "Gold") %>%
  count(country_name, sort = TRUE) %>%
  rename(gold_medals = n)

ggplot(gold_by_country,
       aes(x = reorder(country_name, gold_medals), y = gold_medals)) +
  geom_col(fill = "#F5C518") +
  labs(title = "Q6 — Gold Medals Won per Country",
       x = "Country", y = "Gold Medals") 

# Q7 — Medal-to-athlete ratio
medal_ratio <- df %>%
  group_by(country_name) %>%
  summarise(
    total_athletes = n(),
    total_medals   = sum(!is.na(medal)),
    ratio          = round(total_medals / total_athletes * 100, 1)
  ) %>%
  arrange(desc(ratio))

print(medal_ratio)

ggplot(medal_ratio,
       aes(x = reorder(country_name, ratio), y = ratio)) +
  geom_col(fill = "#5CB85C") +
  coord_flip() +
  labs(title = "Q7 — Medal-to-Athlete Ratio by Country (%)",
       x = "Country", y = "% of Athletes Who Won a Medal") +
  theme_minimal()

# Q8 — Full medal breakdown per country (stacked bar)
medal_breakdown <- df %>%
  filter(!is.na(medal)) %>%
  count(country_name, medal)

ggplot(medal_breakdown,
       aes(x = reorder(country_name, n, sum), y = n, fill = medal)) +
  geom_col() +
  scale_fill_manual(values = c("Gold" = "#F5C518",
                               "Silver" = "#B0B0B0",
                               "Bronze" = "#CD7F32")) +
  coord_flip() +
  labs(title = "Q8 — Medal Breakdown by Country",
       x = "Country", y = "Number of Medals", fill = "Medal") +
  theme_minimal()

# Q9 — Average Olympic experience per country
avg_experience <- df %>%
  group_by(country_name) %>%
  summarise(avg_olympics_attended = mean(total_olympics_attended, na.rm = TRUE)) %>%
  arrange(desc(avg_olympics_attended))

ggplot(avg_experience,
       aes(x = reorder(country_name, avg_olympics_attended),
           y = avg_olympics_attended)) +
  geom_col(fill = "#9B59B6") +
  coord_flip() +
  labs(title = "Q9 — Avg Olympic Appearances per Athlete by Country",
       x = "Country", y = "Avg Olympics Attended") +
  theme_minimal()


# ============================================================
#  PHASE 3 — ATHLETE DEMOGRAPHICS
#  Q10. What is the age distribution of all athletes?
#  Q11. Do medalists tend to be older or younger than non-medalists?
#  Q12. What is the overall gender split?
#  Q13. Are certain sports dominated by one gender?
#  Q14. What is the most common age bracket among gold medalists?
# ============================================================

# Q10 — Age distribution (all athletes)
ggplot(df, aes(x = age)) +
  geom_histogram(binwidth = 2, fill = "#4A90D9", colour = "white") +
  labs(title = "Q10 — Age Distribution of All Athletes",
       x = "Age", y = "Count") +
  theme_minimal()
# Q11 — Age comparison: medalists vs non-medalists
df_age <- df %>%
  mutate(medalist = ifelse(is.na(medal), "No Medal", "Medalist"))

ggplot(df_age, aes(x = medalist, y = age, fill = medalist)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Medalist" = "#F5C518", "No Medal" = "#95A5A6")) +
  labs(title = "Q11 — Age: Medalists vs Non-Medalists",
       x = "", y = "Age") +
  theme_minimal() +-
  theme(legend.position = "none")

# Also break down by medal type
df %>%
  mutate(medal = replace_na(medal, "No Medal")) %>%
  ggplot(aes(x = medal, y = age, fill = medal)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = c("Gold"     = "#F5C518",
                               "Silver"   = "#B0B0B0",
                               "Bronze"   = "#CD7F32",
                               "No Medal" = "#95A5A6")) +
  labs(title = "Q11b — Age by Medal Type",
       x = "Medal", y = "Age") +
  theme_minimal() +
  theme(legend.position = "none")

# Q12 — Overall gender split
gender_count <- df %>% count(gender)

ggplot(gender_count, aes(x = gender, y = n, fill = gender)) +
  geom_col(width = 0.5) +
  scale_fill_manual(values = c("Female" = "#E91E8C", "Male" = "#1E90FF")) +
  labs(title = "Q12 — Gender Split of All Athletes",
       x = "Gender", y = "Count") +
  theme_minimal() +
  theme(legend.position = "none")

# Q13 — Gender split by sport
ggplot(df, aes(x = reorder(sport, sport, length), fill = gender)) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("Female" = "#E91E8C", "Male" = "#1E90FF")) +
  scale_y_continuous(labels = percent_format()) +
  coord_flip() +
  labs(title = "Q13 — Gender Distribution within Each Sport",
       x = "Sport", y = "Proportion", fill = "Gender") +
  theme_minimal()

# Q14 — Gold medalists by age bracket
gold_ages <- df %>%
  filter(medal == "Gold") %>%
  mutate(age_bracket = cut(age,
                           breaks = c(17, 22, 27, 32, 37, 42),
                           labels = c("18–22", "23–27", "28–32", "33–37", "38–41")))

ggplot(gold_ages, aes(x = age_bracket)) +
  geom_bar(fill = "#F5C518") +
  labs(title = "Q14 — Gold Medalists by Age Bracket",
       x = "Age Bracket", y = "Count") +
  theme_minimal()


# ============================================================
#  PHASE 4 — SPORT & EVENT ANALYSIS
#  Q15. Which sport has the most athletes in the dataset?
#  Q16. Which sport produced the most medal-winners?
#  Q17. What is the split between team and individual medals?
#  Q18. Which sport has the highest proportion of record holders?
# ============================================================

# Q15 — Athletes per sport
sport_count <- df %>% count(sport, sort = TRUE)

ggplot(sport_count,
       aes(x = reorder(sport, n), y = n)) +
  geom_col(fill = "#E67E22") +
  coord_flip() +
  labs(title = "Q15 — Number of Athletes per Sport",
       x = "Sport", y = "Count") +
  theme_minimal()

# Q16 — Medalists per sport
medalists_per_sport <- df %>%
  filter(!is.na(medal)) %>%
  count(sport, sort = TRUE)

ggplot(medalists_per_sport,
       aes(x = reorder(sport, n), y = n)) +
  geom_col(fill = "#27AE60") +
  coord_flip() +
  labs(title = "Q16 — Medal Winners per Sport",
       x = "Sport", y = "Medal Count") +
  theme_minimal()

# Q17 — Team vs Individual medal split
team_vs_ind <- df %>%
  filter(!is.na(medal)) %>%
  count(team_or_individual, medal) %>%
  mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze")))

ggplot(team_vs_ind,
       aes(x = team_or_individual, y = n, fill = medal)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Gold"   = "#F5C518",
                               "Silver" = "#B0B0B0",
                               "Bronze" = "#CD7F32")) +
  labs(title = "Q17 — Team vs Individual Medal Distribution",
       x = "", y = "Medal Count", fill = "Medal") +
  theme_minimal()

# Q18 — Record holders by sport
record_by_sport <- df %>%
  group_by(sport) %>%
  summarise(
    total        = n(),
    record_holders = sum(is_record_holder == "Olympic Record"),
    pct          = round(record_holders / total * 100, 1)
  ) %>%
  filter(record_holders > 0)

print(record_by_sport)


# ============================================================
#  PHASE 5 — PHYSICAL ATTRIBUTES
#  Q19. What is the height distribution by gender?
#  Q20. What is the weight distribution by gender?
#  Q21. Is there a height-weight relationship? Does it differ by sport?
#  Q22. Do medalists have different physical profiles than non-medalists?
#  Q23. How does BMI vary across sports?
# ============================================================

# Q19 — Height by gender
ggplot(df, aes(x = height_cm, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Female" = "#E91E8C", "Male" = "#1E90FF")) +
  labs(title = "Q19 — Height Distribution by Gender",
       x = "Height (cm)", y = "Density", fill = "Gender") +
  theme_minimal()

# Q20 — Weight by gender
ggplot(df, aes(x = weight_kg, fill = gender)) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(values = c("Female" = "#E91E8C", "Male" = "#1E90FF")) +
  labs(title = "Q20 — Weight Distribution by Gender",
       x = "Weight (kg)", y = "Density", fill = "Gender") +
  theme_minimal()

# Q21 — Height vs Weight scatter coloured by sport
ggplot(df, aes(x = height_cm, y = weight_kg, colour = sport)) +
  geom_point(alpha = 0.7, size = 2) +
  labs(title = "Q21 — Height vs Weight by Sport",
       x = "Height (cm)", y = "Weight (kg)", colour = "Sport") +
  theme_minimal() +
  theme(legend.text = element_text(size = 7))

# Q22 — Physical profile: medalists vs non-medalists
df_phys <- df %>%
  mutate(medalist = ifelse(is.na(medal), "No Medal", "Medalist"))

# Height comparison
ggplot(df_phys, aes(x = medalist, y = height_cm, fill = medalist)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Q22a — Height: Medalists vs Non-Medalists",
       x = "", y = "Height (cm)") +
  theme_minimal() +
  theme(legend.position = "none")

# Weight comparison
ggplot(df_phys, aes(x = medalist, y = weight_kg, fill = medalist)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Q22b — Weight: Medalists vs Non-Medalists",
       x = "", y = "Weight (kg)") +
  theme_minimal() +
  theme(legend.position = "none")

# Q23 — BMI by sport
df_bmi <- df %>%
  mutate(bmi = round(weight_kg / (height_cm / 100)^2, 1))

ggplot(df_bmi, aes(x = reorder(sport, bmi, median), y = bmi)) +
  geom_boxplot(fill = "#3498DB", alpha = 0.6) +
  coord_flip() +
  labs(title = "Q23 — BMI Distribution by Sport",
       x = "Sport", y = "BMI") +
  theme_minimal()


# ============================================================
#  PHASE 6 — CAREER & PERFORMANCE ANALYSIS
#  Q24. Who are the top 10 athletes by career medals won?
#  Q25. Does attending more Olympics lead to more medals?
#  Q26. Who are the Olympic record holders and what are their stats?
#  Q27. What is the score distribution for Figure Skating?
# ============================================================

# Q24 — Top 10 athletes by total medals won
top_athletes <- df %>%
  select(athlete_name, country_name, sport,
         total_medals_won, gold_medals, silver_medals, bronze_medals) %>%
  distinct(athlete_name, .keep_all = TRUE) %>%
  arrange(desc(total_medals_won)) %>%
  head(10)

print(top_athletes)

ggplot(top_athletes,
       aes(x = reorder(athlete_name, total_medals_won),
           y = total_medals_won, fill = country_name)) +
  geom_col() +
  coord_flip() +
  labs(title = "Q24 — Top 10 Athletes by Career Medal Count",
       x = "Athlete", y = "Total Medals Won", fill = "Country") +
  theme_minimal()

# Q25 — Olympic experience vs medals won (scatter)
exp_medals <- df %>%
  distinct(athlete_name, .keep_all = TRUE) %>%
  select(athlete_name, total_olympics_attended, total_medals_won, sport)

ggplot(exp_medals,
       aes(x = total_olympics_attended, y = total_medals_won)) +
  geom_jitter(alpha = 0.5, colour = "#E67E22", width = 0.1) +
  geom_smooth(method = "lm", se = TRUE, colour = "#2C3E50") +
  labs(title = "Q25 — Does More Olympic Experience = More Medals?",
       x = "Total Olympics Attended", y = "Total Medals Won") +
  theme_minimal()

# Q26 — Olympic record holders
record_holders <- df %>%
  filter(is_record_holder == "Olympic Record") %>%
  select(athlete_name, country_name, sport, event, medal,
         result_value, result_unit, age, height_cm, weight_kg)

print(record_holders)

# Q27 — Figure Skating score distribution
figure_skating_scores <- df %>%
  filter(sport == "Figure Skating", !is.na(result_value))

ggplot(figure_skating_scores,
       aes(x = result_value, fill = medal)) +
  geom_histogram(binwidth = 10, colour = "white") +
  scale_fill_manual(values = c("Gold"   = "#F5C518",
                               "Silver" = "#B0B0B0",
                               "Bronze" = "#CD7F32")) +
  labs(title = "Q27 — Figure Skating Score Distribution by Medal",
       x = "Score (Points)", y = "Count", fill = "Medal") +
  theme_minimal()


# ============================================================
#  PHASE 7 — BONUS CROSS-ANALYSIS
#  Q28. Do certain coaches produce more medalists?
#  Q29. What is the correlation between all numeric variables?
# ============================================================

# Q28 — Coaches with most medalists
coach_medals <- df %>%
  filter(!is.na(medal)) %>%
  count(coach_name, sort = TRUE) %>%
  head(10)

ggplot(coach_medals,
       aes(x = reorder(coach_name, n), y = n)) +
  geom_col(fill = "#8E44AD") +
  coord_flip() +
  labs(title = "Q28 — Top 10 Coaches by Number of Medal-Winning Athletes",
       x = "Coach", y = "Medals") +
  theme_minimal()

# Q29 — Correlation matrix of numeric columns
numeric_cols <- df %>%
  select(age, height_cm, weight_kg, total_olympics_attended,
         total_medals_won, gold_medals, silver_medals, bronze_medals,
         country_total_gold, country_total_medals)

cor_matrix <- cor(numeric_cols, use = "complete.obs")
print(round(cor_matrix, 2))

# Visualise correlation matrix
cor_long <- cor_matrix %>%
  as.data.frame() %>%
  rownames_to_column("var1") %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation")

ggplot(cor_long, aes(x = var1, y = var2, fill = correlation)) +
  geom_tile(colour = "white") +
  scale_fill_gradient2(low  = "#E74C3C", mid  = "white",
                       high = "#2ECC71", midpoint = 0,
                       limits = c(-1, 1)) +
  labs(title = "Q29 — Correlation Matrix of Numeric Variables",
       x = "", y = "", fill = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ── END OF EDA SCRIPT ───────────────────────────────────────

