#Data Analysis Assignment

#============================================================================================================
#TEE EN YONG (TP074195)
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(scales)
library(viridis)


#============================================================================================================
#3.2: Data Cleaning for sbytes and dybtes with attack
cleaned_bytes_dataset <- read_csv("5. UNSW_NB15.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    sbytes = as.numeric(sbytes),
    dbytes = as.numeric(dbytes),
    attack_cat = as.factor(attack_cat),
  ) %>%
  
  #3.2.1 Handling missing values
  drop_na(sbytes, dbytes, attack_cat) %>%
  select(sbytes, dbytes, attack_cat)

#3.2.2: Handling Outliers
cleaned_bytes_dataset <- cleaned_bytes_dataset %>%
  filter(
    sbytes >= 0 & sbytes <= quantile(sbytes, 0.99),
    dbytes >= 0 & dbytes <= quantile(dbytes, 0.99)
  )

#3.2.3: Duplicates Checking
cleaned_bytes_dataset <- cleaned_bytes_dataset %>%
  distinct()

#3.2.4: Data Type Checking
str(cleaned_bytes_dataset)

#3.2.5: Handle Inconsistent Categorical Entries
levels(cleaned_bytes_dataset$attack_cat)

#3.2.6: Check for Negative or Zero Values
cleaned_bytes_dataset <- cleaned_bytes_dataset %>%
  filter(sbytes > 0, dbytes > 0)

#3.2.7: Normalize or Scale Data
cleaned_bytes_dataset <- cleaned_bytes_dataset %>%
  mutate(
    sbytes_scaled = scale(sbytes),
    dbytes_scaled = scale(dbytes)
  )

#3.2.8: Final Visualization
ggplot(cleaned_bytes_dataset, aes(x = sbytes, fill = attack_cat)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Density Plot of sbytes by Attack Category", x = "sbytes (log scale)", y = "Density")

ggplot(cleaned_bytes_dataset, aes(x = dbytes, fill = attack_cat)) +
  geom_density(alpha = 0.5) +
  scale_x_log10() +
  theme_minimal() +
  labs(title = "Density Plot of dbytes by Attack Category", x = "dbytes (log scale)", y = "Density")


#============================================================================================================
#1: How do the amounts of source bytes (sbytes) differ across various attack categories?

sbytes_summary <- cleaned_bytes_dataset %>%
  group_by(attack_cat) %>%
  summarise(mean_sbytes = mean(sbytes, na.rm = TRUE),
            median_sbytes = median(sbytes, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(mean_sbytes))

print(sbytes_summary)


ggplot(sbytes_summary, aes(x = reorder(attack_cat, -mean_sbytes), y = mean_sbytes, fill = attack_cat)) +
  geom_col() +
  labs(title = "Average Source Bytes by Attack Category",
       x = "Attack Category", y = "Average Source Bytes") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#============================================================================================================
#2: How do the amounts of destination bytes (dbytes) vary among different attack categories?
dbytes_summary <- cleaned_bytes_dataset %>%
  group_by(attack_cat) %>%
  summarise(mean_dbytes = mean(dbytes, na.rm = TRUE),
            median_dbytes = median(dbytes, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(mean_dbytes))

print(dbytes_summary)

ggplot(dbytes_summary, aes(x = reorder(attack_cat, -mean_dbytes), y = mean_dbytes, fill = attack_cat)) +
  geom_col() +
  labs(title = "Average Destination Bytes by Attack Category",
       x = "Attack Category", y = "Mean Destination Bytes") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#============================================================================================================
#3: What is the relationship between the total source bytes (sbytes) and destination bytes (dbytes) for each attack and normal category?
bytes_summary <- cleaned_bytes_dataset %>%
  mutate(traffic_type = ifelse(tolower(attack_cat) == "normal", "Normal", "Attack")) %>%
  group_by(traffic_type) %>%
  summarise(
    total_sbytes = sum(sbytes, na.rm = TRUE) / 1e6,
    total_dbytes = sum(dbytes, na.rm = TRUE) / 1e6,
    mean_sbytes = mean(sbytes, na.rm = TRUE),
    mean_dbytes = mean(dbytes, na.rm = TRUE)
  )

print(bytes_summary)

bytes_summary_long <- bytes_summary %>%
  pivot_longer(cols = c(total_sbytes, total_dbytes),
               names_to = "byte_type",
               values_to = "total_value")

ggplot(bytes_summary_long, aes(x = traffic_type, y = total_value, fill = byte_type)) +
  geom_col() +
  facet_wrap(~ byte_type, scales = "free_y") +
  theme_minimal() +
  labs(
    title = "Total Bytes by Traffic Type",
    x = "Traffic Type",
    y = "Total Bytes (MB)"
  )

#============================================================================================================
#Hypothesis Testing: Two-sample t-test for Data Analysis 3

summary_bytes_test <- cleaned_bytes_dataset %>%
  group_by(traffic_type) %>%
  summarise(
    total_sbytes = sum(sbytes, na.rm = TRUE),
    total_dbytes = sum(dbytes, na.rm = TRUE),
    mean_sbytes  = mean(sbytes, na.rm = TRUE),
    mean_dbytes  = mean(dbytes, na.rm = TRUE)
  )

print(summary_bytes_test)

cleaned_bytes_dataset$total_bytes <- cleaned_bytes_dataset$sbytes + cleaned_bytes_dataset$dbytes

t_test_result <- t.test(
  total_bytes ~ traffic_type,
  data = cleaned_bytes_dataset
)

print(t_test_result)

#==============================================================================================================


# AFRAH ARSHAD SHAH (TP085430)
# 2.2 Data Cleaning
# ============================================================
# Load libraries
# ============================================================
library(dplyr)
library(readr)
library(ggplot2)

# Import dataset 
unsw <- read_csv("5. UNSW_NB15.csv", show_col_types = FALSE)

unsw <- unsw %>% 
  select(dur, sbytes, dbytes, attack_cat)

# Quick check
glimpse(unsw)


# 2.2.1 Checking & Handling Missing Values

colSums(is.na(unsw))   
unsw <- unsw %>% drop_na()

# 2.2.2 Checking & Handling Outliers (Simple IQR method)

numeric_cols <- c("dur", "sbytes", "dbytes")

cap_iqr <- function(x){
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQRv <- Q3 - Q1
  lower <- Q1 - 1.5*IQRv
  upper <- Q3 + 1.5*IQRv
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

unsw <- unsw %>%
  mutate(across(all_of(numeric_cols), cap_iqr))


# 2.2.3 Checking for Duplicates

sum(duplicated(unsw))        # Check duplicates
unsw <- unsw %>% distinct()  # Remove duplicates


# 2.2.4 Data Type Checking

str(unsw) 

unsw$attack_cat <- as.factor(unsw$attack_cat)


# 2.2.5 Handle Inconsistent Categorical Entries

unsw$attack_cat <- tolower(unsw$attack_cat)
unsw$attack_cat <- trimws(unsw$attack_cat)


# 2.2.6 Check for Negative or Zero Values

summary(unsw[, numeric_cols])


unsw <- unsw %>%
  mutate(across(all_of(numeric_cols),
                ~ ifelse(.x < 0, 0, .x)))


# 2.2.7 Normalize / Scale Data 

unsw <- unsw %>%
  mutate(
    dur_scaled = (dur - min(dur))/(max(dur)-min(dur)),
    sbytes_scaled = (sbytes - min(sbytes))/(max(sbytes)-min(sbytes)),
    dbytes_scaled = (dbytes - min(dbytes))/(max(dbytes)-min(dbytes))
  )

# 2.2.8 Final Visualization

boxplot(unsw$dur, main = "Duration (After Cleaning)")
boxplot(unsw$sbytes, main = "Source Bytes (After Cleaning)")
boxplot(unsw$dbytes, main = "Destination Bytes (After Cleaning)")

# cleaned dataset

write_csv(unsw, "UNSW_cleaned_selected_columns.csv")
                     
#==============================================================================================================
# 1. What is the average connection duration for each attack category?
avg_duration <- unsw %>%
  group_by(attack_cat) %>%
  summarise(
    mean_duration = mean(dur, na.rm = TRUE),
    median_duration = median(dur, na.rm = TRUE),
    count = n()
  )

# 2. How do source and destination byte counts differ among attack types?
byte_summary <- unsw %>%
  group_by(attack_cat) %>%
  summarise(
    avg_sbytes = mean(sbytes, na.rm = TRUE),
    avg_dbytes = mean(dbytes, na.rm = TRUE)
  )

# extra feature 
unsw <- unsw %>% mutate(total_bytes = sbytes + dbytes)

# 3. Is there a relationship between connection duration and total data transferred?

cor(unsw$dur, unsw$total_bytes, use = "complete.obs")



                     
                     
#==============================================================================================================
# WONG ZHENG HAN (TP074212)
# Libraries
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)
library(tidyr)

#3.1: Data Cleaning for dur and sbytes with attack category
dur_sbytes_dataset <- read_csv("5. UNSW_NB15.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    dur       = as.numeric(dur),
    sbytes    = as.numeric(sbytes),
    attack_cat = str_to_lower(attack_cat),
    attack_cat = as.factor(attack_cat)
  ) %>%

  #3.1.1 Handling missing values
  drop_na(dur, sbytes, attack_cat) %>%

  # Keep only what is needed for this objective
  select(dur, sbytes, attack_cat)

#3.1.2: Handling Outliers (cap extreme 1% tails)
dur_sbytes_dataset <- dur_sbytes_dataset %>%
  filter(
    dur    >= 0 & dur    <= quantile(dur, 0.99),
    sbytes >= 0 & sbytes <= quantile(sbytes, 0.99)
  )

#3.1.3: Duplicates Checking
dur_sbytes_dataset <- dur_sbytes_dataset %>%
  distinct()

#3.1.4: Data Type Checking
str(dur_sbytes_dataset)

#3.1.5: Handle Inconsistent Categorical Entries
levels(dur_sbytes_dataset$attack_cat)

#3.1.6: Check for Non-positive Values
dur_sbytes_dataset <- dur_sbytes_dataset %>%
  filter(dur > 0, sbytes > 0)

#3.1.7: Normalize or Scale Data (for modelling if needed)
dur_sbytes_dataset <- dur_sbytes_dataset %>%
  mutate(
    dur_scaled    = as.numeric(scale(dur)),
    sbytes_scaled = as.numeric(scale(sbytes))
  )

  #3.1.8: Final Visualization
cleaned_dataset <- cleaned_dataset %>%
  mutate(
    dur_group = case_when(
      dur < 1 ~ "Short",
      dur < 10 ~ "Medium",
      TRUE ~ "Long"
    )
  )

summary_sbytes <- cleaned_dataset %>%
  group_by(dur_group) %>%
  summarise(avg_sbytes = mean(sbytes))

ggplot(summary_sbytes, aes(x = dur_group, y = avg_sbytes)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Average Source Bytes by Connection Duration",
       x = "Duration Group",
       y = "Average sbytes")

#===============================================================
#Objective 4: To Investigate how connection duration relates to 
#             the amount of data sent by the source device.
#===============================================================

# Analysis 4-1:
# Do longer connections generally transmit more source bytes?

# Correlation (overall)
cor_test_result <- cor.test(dur_sbytes_dataset$dur, dur_sbytes_dataset$sbytes)
cor_test_result

# Scatterplot with trendline (log scale helps because sbytes is very skewed)
dur_sbytes_dataset <- dur_sbytes_dataset %>%
  mutate(
    dur_group = cut(
      dur,
      breaks = c(0, 0.01, 0.1, 1, 10, Inf),
      labels = c("Very Short", "Short", "Medium", "Long", "Very Long")
    )
  )

# Summarise average sbytes for each duration group
dur_sbytes_summary <- dur_sbytes_dataset %>%
  group_by(dur_group) %>%
  summarise(
    avg_sbytes = mean(sbytes, na.rm = TRUE)
  )

# Bar chart with correct sbytes values
ggplot(dur_sbytes_summary, aes(x = dur_group, y = avg_sbytes)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Average Source Bytes by Connection Duration",
    x = "Connection Duration Group",
    y = "Average Source Bytes"
  )

# Analysis 4-2:
# Are there specific attack categories where long connections transmit unusually high or low amounts of data?

# Summary statistics by attack category
attack_summary <- dur_sbytes_dataset %>%
  group_by(attack_cat) %>%
  summarise(
    n              = n(),
    mean_dur       = mean(dur),
    median_dur     = median(dur),
    mean_sbytes    = mean(sbytes),
    median_sbytes  = median(sbytes),
    .groups = "drop"
  )

attack_summary

# Boxplot of dur by attack category
ggplot(dur_sbytes_dataset, aes(x = attack_cat, y = dur)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8) +
  scale_y_log10() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Average Connection Duration by Attack Category",
    x = "Attack Category",
    y = "Average Duration (log scale)"
  )

# Boxplot of dur by attack category
ggplot(dur_sbytes_dataset, aes(x = attack_cat, y = dur)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Connection Duration by Attack Category",
    x = "Attack Category",
    y = "Duration (log10 scale)"
  )

# Analysis 4-3:
# Do short-duration connections ever send disproportionately large amounts of data?

# Define thresholds for "short" duration and "high" sbytes (using quartiles)
dur_short_threshold   <- quantile(dur_sbytes_dataset$dur, 0.25)
sbytes_high_threshold <- quantile(dur_sbytes_dataset$sbytes, 0.75)

short_high_burst <- dur_sbytes_dataset %>%
  filter(dur <= dur_short_threshold,
         sbytes >= sbytes_high_threshold)

# Inspect how many such cases there are by attack_cat
burst_summary <- short_high_burst %>%
  group_by(attack_cat) %>%
  summarise(
    n_cases = n(),
    mean_dur = mean(dur),
    mean_sbytes = mean(sbytes),
    .groups = "drop"
  )

burst_summary

# Visualise these "burst" connections on the same scatterplot
ggplot(dur_sbytes_dataset, aes(x = dur, y = sbytes)) +
  geom_point(alpha = 0.15) +
  geom_point(
    data = short_high_burst,
    aes(x = dur, y = sbytes, color = attack_cat),
    alpha = 0.8
  ) +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Short-Duration, High-Source-Bytes Connections Highlighted",
    x = "Connection Duration (log10 scale)",
    y = "Source Bytes (log10 scale)"
  )

# ============================================
# HYPOTHESIS TESTING: TWO-SAMPLE T-TEST
# ============================================

# Hypothesis:
# H0 (Null): There is no significant difference in mean sbytes between Normal and Attack traffic.
# H1 (Alternative): There is a significant difference in mean sbytes between Normal and Attack traffic.

normal_data <- cleaned_dur_sbytes_dataset %>%
  filter(attack_cat == "Normal") %>%
  pull(sbytes)

attack_data <- cleaned_dur_sbytes_dataset %>%
  filter(attack_cat != "Normal") %>%
  pull(sbytes)

t.test(normal_data, attack_data, alternative = "two.sided")

#==============================================================================================================
#QUAN JIA YONG
#==============================================================================================================
#ALTAYEB ABDELGADIR MOHAMED




#==============================================================================================================
#Complex Hypothesis (Conclusion)
