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
initial_n <- nrow(cleaned_bytes_dataset)
initial_n_dupes <- sum(duplicated(cleaned_bytes_dataset))

cat("Initial rows:", initial_n, "\n")
cat("Duplicate rows:", initial_n_dupes, "\n")

cleaned_bytes_dataset <- cleaned_bytes_dataset %>% distinct()

after_n <- nrow(cleaned_bytes_dataset)
removed_dupes <- initial_n - after_n

cat("Rows after removing duplicates:", after_n, "\n")
cat("Duplicates removed:", removed_dupes, "\n")

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

cleaned_bytes_dataset <- cleaned_bytes_dataset %>%
  mutate(traffic_type = ifelse(tolower(attack_cat) == "normal", "Normal", "Attack"))

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
library(tidyr)


# Import dataset 
unsw <- read_csv("5. UNSW_NB15.csv", show_col_types = FALSE) %>%
  select(dur, sbytes, dbytes, attack_cat)   

glimpse(unsw)

# 2.2.1 Checking & Handling Missing Values

print("Missing values per column:")
print(colSums(is.na(unsw)))

unsw <- unsw %>% drop_na()
print(paste("Rows after removing NA:", nrow(unsw)))
# 2.2.2 Checking & Handling Outliers (Simple IQR method)

numeric_cols <- c("dur", "sbytes", "dbytes")

cap_iqr <- function(x){
  if (!is.numeric(x)) return(x)
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQRv <- Q3 - Q1
  lower <- Q1 - 1.5 * IQRv
  upper <- Q3 + 1.5 * IQRv
  x[x < lower] <- lower
  x[x > upper] <- upper
  return(x)
}

unsw <- unsw %>% mutate(across(all_of(numeric_cols), cap_iqr))


# 2.2.3 Checking for Duplicates

dup_count <- sum(duplicated(unsw))
print(paste("Duplicate rows found:", dup_count))

unsw <- unsw %>% distinct()
print(paste("Rows after removing duplicates:", nrow(unsw)))


# 2.2.4 Data Type Checking

unsw <- unsw %>%
  mutate(attack_cat = as.factor(tolower(trimws(as.character(attack_cat)))))

str(unsw)

# 2.2.5 Handle Inconsistent Categorical Entries

print("Attack category levels:")
print(levels(unsw$attack_cat))

# 2.2.6 Check for Negative or Zero Values

print("Summary of numeric columns (after cleaning):")
print(summary(unsw[, numeric_cols]))

unsw <- unsw %>%
  mutate(across(all_of(numeric_cols), ~ ifelse(.x < 0, 0, .x)))


# 2.2.7 Normalize data

unsw <- unsw %>%
  mutate(
    dur_scaled = (dur - min(dur, na.rm = TRUE)) / (max(dur, na.rm = TRUE) - min(dur, na.rm = TRUE)),
    sbytes_scaled = (sbytes - min(sbytes, na.rm = TRUE)) / (max(sbytes, na.rm = TRUE) - min(sbytes, na.rm = TRUE)),
    dbytes_scaled = (dbytes - min(dbytes, na.rm = TRUE)) / (max(dbytes, na.rm = TRUE) - min(dbytes, na.rm = TRUE))
  )

# total_bytes 

unsw <- unsw %>% mutate(total_bytes = sbytes + dbytes)

# 2.2.8 Final Visualization

ggplot(unsw, aes(x = "", y = dur)) +
  geom_boxplot() + labs(title = "Duration (After Cleaning)", y = "dur", x = "") +
  theme_minimal()

ggplot(unsw, aes(x = "", y = sbytes)) +
  geom_boxplot() + labs(title = "Source Bytes (After Cleaning)", y = "sbytes", x = "") +
  theme_minimal()

ggplot(unsw, aes(x = "", y = dbytes)) +
  geom_boxplot() + labs(title = "Destination Bytes (After Cleaning)", y = "dbytes", x = "") +
  theme_minimal()

# cleaned dataset

write_csv(unsw, "UNSW_cleaned_selected_columns.csv")
message("Saved cleaned selected columns to UNSW_cleaned_selected_columns.csv")
                     
#==============================================================================================================
# 1. What is the average connection duration for each attack category?
avg_duration <- unsw %>%
  group_by(attack_cat) %>%
  summarise(mean_duration = mean(dur, na.rm = TRUE),
            median_duration = median(dur, na.rm = TRUE),
            n = n()) %>%
  arrange(desc(mean_duration))

print(avg_duration)

ggplot(avg_duration, aes(x = attack_cat, y = mean_duration)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average Duration by Attack Category",
       x = "Attack Category", y = "Mean Duration (s)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2. How do source and destination byte counts differ among attack types?
byte_summary <- unsw %>%
  group_by(attack_cat) %>%
  summarise(avg_sbytes = mean(sbytes, na.rm = TRUE),
            avg_dbytes = mean(dbytes, na.rm = TRUE))

print(byte_summary)

byte_summary_long <- byte_summary %>%
  pivot_longer(cols = c(avg_sbytes, avg_dbytes),
               names_to = "byte_type", values_to = "avg_value")

ggplot(byte_summary_long, aes(x = attack_cat, y = avg_value, fill = byte_type)) +
  geom_col(position = "dodge") +
  labs(title = "Average Source vs Destination Bytes",
       x = "Attack Category", y = "Average Bytes", fill = "Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# 3. Is there a relationship between connection duration and total data transferred?

unsw <- unsw %>% mutate(total_bytes = sbytes + dbytes)

unsw <- unsw %>% mutate(
  duration_group = case_when(
    dur <= 0.3 ~ "Short",
    dur <= 0.8 ~ "Medium",
    TRUE ~ "Long"
  )
)

duration_bytes_summary <- unsw %>%
  group_by(duration_group) %>%
  summarise(avg_total_bytes = mean(total_bytes, na.rm = TRUE))

print(duration_bytes_summary)

ggplot(duration_bytes_summary,
       aes(x = duration_group,
           y = avg_total_bytes)) +
  geom_col(fill = "skyblue") +
  labs(title = "Average Total Bytes by Duration Group",
       x = "Duration Group",
       y = "Average Total Bytes") +
  theme_minimal()

# -----------------------------
# Hypothesis Testing: 
# Is there a relationship between duration and total bytes?
# -----------------------------

cor_value <- cor(unsw$dur, unsw$total_bytes, use = "complete.obs")
print(paste("Correlation between duration and total bytes:", round(cor_value, 4)))

cor_test <- cor.test(unsw$dur, unsw$total_bytes, method = "pearson")

print(cor_test)    

# ===============================================================
# EXTRA FEATURE: Correlation Heatmap 
# This produces a correlation matrix and a heatmap for dur, sbytes, dbytes, total_bytes
# ===============================================================
cor_data <- unsw %>% select(dur, sbytes, dbytes, total_bytes)
cor_matrix <- cor(cor_data, use = "complete.obs")

print("Correlation matrix (dur, sbytes, dbytes, total_bytes):")
print(round(cor_matrix, 4))

heatmap(cor_matrix,
        main = "Correlation Heatmap of Numeric Features",
        xlab = "Variables", ylab = "Variables",
        col = heat.colors(20))



#==============================================================================================================
# WONG ZHENG HAN (TP074212)
# Libraries
library(readr)
library(dplyr)
library(janitor)
library(ggplot2)
library(stringr)
library(tidyr)

# 3.1: Data Cleaning for dur and sbytes with attack category
cleaned_dataset <- read_csv("5. UNSW_NB15.csv", show_col_types = FALSE) %>%
  clean_names() %>%
  mutate(
    dur        = as.numeric(dur),
    sbytes     = as.numeric(sbytes),
    attack_cat = str_to_lower(attack_cat),
    attack_cat = as.factor(attack_cat)
  ) %>%
  
  # 3.1.1 Handling missing values
  drop_na(dur, sbytes, attack_cat) %>%
  
  # Keep only what is needed for this objective
  select(dur, sbytes, attack_cat)

# 3.1.2: Handling Outliers (cap extreme 1% tails)
cleaned_dataset <- cleaned_dataset %>%
  filter(
    dur    >= 0 & dur    <= quantile(dur, 0.99),
    sbytes >= 0 & sbytes <= quantile(sbytes, 0.99)
  )

# 3.1.3: Duplicates Checking
cleaned_dataset <- cleaned_dataset %>%
  distinct()

# 3.1.4: Data Type Checking
str(cleaned_dataset)

# 3.1.5: Handle Inconsistent Categorical Entries
levels(cleaned_dataset$attack_cat)

# 3.1.6: Check for Non-positive Values
cleaned_dataset <- cleaned_dataset %>%
  filter(dur > 0, sbytes > 0)

# 3.1.7: Normalize or Scale Data
cleaned_dataset <- cleaned_dataset %>%
  mutate(
    dur_scaled    = as.numeric(scale(dur)),
    sbytes_scaled = as.numeric(scale(sbytes))
  )

# 3.1.8: Final Visualization
cleaned_dataset <- cleaned_dataset %>%
  mutate(
    dur_group = case_when(
      dur < 1  ~ "Short",
      dur < 10 ~ "Medium",
      TRUE     ~ "Long"
    )
  )

summary_sbytes <- dur_sbytes_dataset %>%
  group_by(dur_group) %>%
  summarise(avg_sbytes = mean(sbytes), .groups = "drop")

ggplot(summary_sbytes, aes(x = dur_group, y = avg_sbytes)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Average Source Bytes by Connection Duration (3 Groups)",
    x = "Duration Group",
    y = "Average sbytes"
  )

# Summary stats for dur and sbytes
summary(cleaned_dataset[, c("dur", "sbytes")])

attack_sbytes <- cleaned_dataset %>%
  group_by(attack_cat) %>%
  summarise(avg_sbytes = mean(sbytes), .groups = "drop")

ggplot(attack_sbytes, aes(x = attack_cat, y = avg_sbytes)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(
    title = "Average sbytes by Attack Category",
    x = "Attack Category",
    y = "Average sbytes"
  )

# ---------- Analysis 4-1 ----------
# Do longer connections generally transmit more source bytes?

# Correlation (overall)
cor_test_result <- cor.test(cleaned_dataset$dur, cleaned_dataset$sbytes)
cor_test_result

# Create duration group for deeper analysis (new column)
cleaned_dataset <- cleaned_dataset %>%
  mutate(
    dur_group5 = cut(
      dur,
      breaks = c(0, 0.01, 0.1, 1, 10, Inf),
      labels = c("Very Short", "Short", "Medium", "Long", "Very Long")
    )
  )

# Summarise average sbytes for each duration group
dur_sbytes_summary <- cleaned_dataset %>%
  group_by(dur_group5) %>%
  summarise(
    avg_sbytes = mean(sbytes, na.rm = TRUE),
    .groups = "drop"
  )

# Bar chart with correct sbytes values
ggplot(dur_sbytes_summary, aes(x = dur_group5, y = avg_sbytes)) +
  geom_bar(stat = "identity", alpha = 0.9) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Average Source Bytes by Connection Duration",
    x = "Connection Duration Group",
    y = "Average Source Bytes"
  )

# ---------- Analysis 4-2 ----------
# Are there specific attack categories where long connections transmit unusually high or low amounts of data?

# Summary statistics by attack category
attack_summary <- cleaned_dataset %>%
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

# Boxplot of sbytes by attack category
ggplot(cleaned_dataset, aes(x = attack_cat, y = dur)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8) +
  scale_y_log10() +
  theme_minimal(base_size = 12) +
  labs(
    title = "Average Connection Duration by Attack Category",
    x = "Attack Category",
    y = "Average Duration (log scale)"
  )

# Boxplot of dur by attack category
ggplot(cleaned_dataset, aes(x = attack_cat, y = dur)) +
  stat_summary(fun = mean, geom = "bar", alpha = 0.8) +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Mean Connection Duration by Attack Category",
    x = "Attack Category",
    y = "Mean Duration (log10 scale)"
  )

# ---------- Analysis 4-3 ----------
# Do short-duration connections ever send disproportionately large amounts of data?

# Define thresholds for "short" duration and "high" sbytes (using quartiles)
dur_short_threshold   <- quantile(cleaned_dataset$dur, 0.25)
sbytes_high_threshold <- quantile(cleaned_dataset$sbytes, 0.75)

short_high_burst <- cleaned_dataset %>%
  filter(
    dur    <= dur_short_threshold,
    sbytes >= sbytes_high_threshold
  )

# Inspect how many such cases there are by attack_cat
burst_summary <- short_high_burst %>%
  group_by(attack_cat) %>%
  summarise(
    n_cases   = n(),
    mean_dur  = mean(dur),
    mean_sbytes = mean(sbytes),
    .groups = "drop"
  )

burst_summary

# Visualise these "burst" connections on the same scatterplot
cleaned_dataset <- cleaned_dataset %>%
  mutate(
    dur_group = cut(
      dur,
      breaks = c(0, 0.01, 0.1, 1, 10, max(dur)),
      labels = c("Very Short", "Short", "Medium", "Long", "Very Long")
    )
  )

# Bar chart: Average sbytes by duration group (log scale)
ggplot(cleaned_dataset, aes(x = dur_group5, y = sbytes)) +
  stat_summary(fun = mean, geom = "bar") +
  scale_y_log10() +
  theme_minimal() +
  labs(
    title = "Average Source Bytes by Connection Duration Group",
    x = "Connection Duration Group",
    y = "Average Source Bytes (log scale)"
  )

# ============================================
# HYPOTHESIS TESTING: TWO-SAMPLE T-TEST
# ============================================

# Hypothesis:
# H0 (Null): There is no significant difference in mean sbytes between Normal and Attack traffic.
# H1 (Alternative): There is a significant difference in mean sbytes between Normal and Attack traffic.

normal_data <- cleaned_dataset %>%
  filter(attack_cat == "normal") %>%
  pull(sbytes)

attack_data <- cleaned_dataset %>%
  filter(attack_cat != "normal") %>%
  pull(sbytes)

t.test(normal_data, attack_data, alternative = "two.sided")

#==============================================================================================================


#QUAN JIA YONG



library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)
library(gridExtra)


file_path <- "5. UNSW_NB15.csv"
data_raw <- read_csv(file_path, show_col_types = FALSE)

cat("Loaded dataset from:", file_path, "\n")
cat("Rows:", nrow(data_raw), "Columns:", ncol(data_raw), "\n")
print(head(data_raw, 5))


data <- data_raw 

# 6.2.1: Missing values
num_cols <- names(data)[sapply(data, is.numeric)]
char_cols <- names(data)[sapply(data, is.character)]

for (col in num_cols) {
  data[[col]][is.na(data[[col]])] <- median(data[[col]], na.rm = TRUE)
}

for (col in char_cols) {
  data[[col]][is.na(data[[col]])] <- "unknown"
}

# 6.2.2: Outlier capping (IQR)
numeric_interest <- c("spkts", "dpkts", "sbytes", "dbytes", "dur")
numeric_interest <- numeric_interest[numeric_interest %in% names(data)]

cap_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower <- q1 - 1.5 * iqr
  upper <- q3 + 1.5 * iqr
  pmin(pmax(x, lower), upper)
}

for (col in numeric_interest) {
  newcol <- paste0(col, "_capped")
  data[[newcol]] <- cap_outliers(data[[col]])
}

# 6.2.3: Remove duplicates
if ("id" %in% names(data)) {
  data <- data[!duplicated(data$id), ]
} else {
  data <- data[!duplicated(data), ]
}

# 6.2.4: Convert factors
factor_cols <- c("proto", "service", "state", "attack_cat")
factor_cols <- factor_cols[factor_cols %in% names(data)]

for (col in factor_cols) {
  data[[col]] <- as.factor(as.character(data[[col]]))
}

# 6.2.5: Normalize categorical entries
for (col in names(data)) {
  if (is.character(data[[col]])) {
    data[[col]] <- trimws(tolower(data[[col]]))
  }
}

if ("attack_cat" %in% names(data)) {
  data$attack_cat <- as.factor(tolower(as.character(data$attack_cat)))
}

# 6.2.6: Replace zero durations
if ("dur" %in% names(data)) {
  data$dur[data$dur == 0] <- 1e-6
}

# 6.2.7: Create derived features and scale
data <- data %>%
  mutate(
    total_packets = spkts + dpkts,
    total_bytes   = sbytes + dbytes,
    forward_packet_rate  = spkts / dur,
    backward_packet_rate = dpkts / dur
  )

scale_vars <- c(
  "spkts", "dpkts", "sbytes", "dbytes",
  "total_packets", "total_bytes", "forward_packet_rate"
)
scale_vars <- scale_vars[scale_vars %in% names(data)]
data[scale_vars] <- scale(data[scale_vars])



# Create attack indicator for hypothesis testing
if ("attack_cat" %in% names(data)) {
  data <- data %>% mutate(is_attack = ifelse(attack_cat == "normal", 0, 1))
} else if ("label" %in% names(data)) {
  data <- data %>% mutate(is_attack = ifelse(label == 0, 0, 1))
}




#  Analysis 1: Network Flow Trends 
if ("attack_cat" %in% names(data)) {
  
  # Mean total_packets by attack category
  trend_df <- data %>%
    group_by(attack_cat) %>%
    summarise(mean_total_packets = mean(total_packets, na.rm = TRUE))
  
  ggplot(trend_df, aes(x = attack_cat, y = mean_total_packets, fill = attack_cat)) +
    geom_bar(stat = "identity") +
    labs(title = "Mean Total Packets by Attack Category (Analysis 1)",
         x = "Attack Category", y = "Mean Total Packets") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = "none")
  
  # Mean forward_packet_rate by attack category
  trend_rate_df <- data %>%
    group_by(attack_cat) %>%
    summarise(mean_forward_rate = mean(forward_packet_rate, na.rm = TRUE))
  
  ggplot(trend_rate_df, aes(x = attack_cat, y = mean_forward_rate, fill = attack_cat)) +
    geom_bar(stat = "identity") +
    labs(title = "Mean Forward Packet Rate by Attack Category (Analysis 1)",
         x = "Attack Category", y = "Mean Forward Packet Rate") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    guides(fill = "none")
}

#  Analysis 2: Statistical Differences / Hypothesis Testing 
data$attack_flag <- ifelse(data$is_attack == 1, "Attack", "Normal")
hypo_df <- data %>%
  group_by(attack_flag) %>%
  summarise(mean_total_packets = mean(total_packets, na.rm = TRUE))

ggplot(hypo_df, aes(x = attack_flag, y = mean_total_packets, fill = attack_flag)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Total Packets: Attack vs Normal (Analysis 2)",
       x = "Flow Type", y = "Mean Total Packets") +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5)) +
  guides(fill = "none")




wilcox_res <- wilcox.test(total_packets ~ is_attack, data = data)
t_res <- t.test(total_packets ~ is_attack, data = data)

cat("\nWilcoxon test (attack vs normal):\n")
print(wilcox_res)

cat("\nT-test (attack vs normal):\n")
print(t_res)




#==============================================================================================================
#ALTAYEB ABDELGADIR MOHAMED

# Objective: To analyze the distribution of different types 
#            of cyber-attacks and identify which attacks 
#            are most prevalent.


# 1. Load Packages -----------------------------------------

library(tidyverse)

# 2. Data Import & Subset ---------------------------------

# Change the file name if your CSV is named differently
unsw <- read_csv("5. UNSW_NB15.csv")

# Keep only the columns needed for this objective
data_alt <- unsw %>%
  select(attack_cat, dur, sbytes, dbytes, spkts, dpkts, ct_state_ttl)

# 3. Data Preparation -------------------------------------

## 3.1 Checking Missing Values (5.2.1)
colSums(is.na(data_alt))  

# No NAs in these columns, so no drop_na() required
# data_alt <- data_alt %>% drop_na()

## 3.2 Handling Outliers (5.2.2)
# For this objective we keep real values, no trimming is applied.
# (Mentioned in report, no code needed.)

## 3.3 Remove Duplicates (5.2.3)
n_before <- nrow(data_alt)
data_alt <- data_alt %>% distinct()
n_after <- nrow(data_alt)
duplicates_removed <- n_before - n_after 

## 3.4 Data Type Checking (5.2.4)
str(data_alt)

data_alt <- data_alt %>%
  mutate(
    attack_cat   = as.factor(attack_cat),
    ct_state_ttl = as.factor(ct_state_ttl),
    dur          = as.numeric(dur),
    sbytes       = as.numeric(sbytes),
    dbytes       = as.numeric(dbytes),
    spkts        = as.numeric(spkts),
    dpkts        = as.numeric(dpkts)
  )

## 3.5 Handle Inconsistent Categories (5.2.5)
levels(data_alt$attack_cat)  


## 3.6 Remove Negative or Zero Values (5.2.6)
data_alt <- data_alt %>%
  filter(
    dur    > 0,
    sbytes > 0,
    dbytes > 0,
    spkts  > 0,
    dpkts  > 0
  )

## 3.7 Scaling (only used for interpretation if needed) (5.2.7)
data_alt <- data_alt %>%
  mutate(
    dur_scaled    = scale(dur),
    sbytes_scaled = scale(sbytes),
    dbytes_scaled = scale(dbytes),
    spkts_scaled  = scale(spkts),
    dpkts_scaled  = scale(dpkts)
  )

## 3.8 Final Visualization (5.2.8)
attack_freq <- data_alt %>%
  count(attack_cat) %>%
  arrange(desc(n)) %>%
  mutate(percentage = n / sum(n) * 100)

attack_freq

ggplot(attack_freq, aes(x = reorder(attack_cat, -n), y = n)) +
  geom_col() +
  labs(title = "Distribution of Attack Categories After Cleaning",
       x = "Attack Category", y = "Number of Records") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


############################################################
# 4. Exploratory Data Analysis (5.3)
############################################################

# 4.1 Summary Statistics (5.3.1) --------------------------

# (a) frequency table already created: attack_freq

# (b) identify most common attack category
top_attack <- attack_freq$attack_cat[1]
top_attack  

# create group variable: top attack vs all others
data_alt <- data_alt %>%
  mutate(
    group = if_else(attack_cat == top_attack,
                    as.character(top_attack),
                    "Other")
  )

# compare key features between top attack & others
feature_summary <- data_alt %>%
  group_by(group) %>%
  summarise(
    mean_dur    = mean(dur),
    median_dur  = median(dur),
    mean_sbytes = mean(sbytes),
    mean_dbytes = mean(dbytes),
    mean_spkts  = mean(spkts),
    mean_dpkts  = mean(dpkts),
    n           = n()
  )

feature_summary


# 4.2 Charts (5.3.2) --------------------------------------

## Chart 1: Frequency of Attack Categories
ggplot(attack_freq, aes(x = reorder(attack_cat, -n), y = n)) +
  geom_col() +
  labs(title = "Frequency of Attack Categories",
       x = "Attack Category",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Chart 2: Duration for top attack vs others (boxplot)
ggplot(data_alt, aes(x = group, y = dur)) +
  geom_boxplot(fill = "orange") +
  scale_y_log10() +
  labs(title = paste("Duration for", top_attack, "vs Others"),
       x = "Group",
       y = "Duration (log scale)") +
  theme_minimal()

## Chart 3: Source bytes for top attack vs others (boxplot)
ggplot(data_alt, aes(x = group, y = sbytes)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_log10() +
  labs(title = paste("Source Bytes for", top_attack, "vs Others"),
       x = "Group",
       y = "Source Bytes (log scale)") +
  theme_minimal()

## Chart 4: Average total packets per attack category
flow_summary <- data_alt %>%
  group_by(attack_cat) %>%
  summarise(
    mean_packets = mean(spkts + dpkts)
  ) %>%
  arrange(desc(mean_packets))

flow_summary   

ggplot(flow_summary, aes(x = reorder(attack_cat, -mean_packets), y = mean_packets)) +
  geom_col(fill = "purple") +
  labs(title = "Average Total Packets per Attack Type",
       x = "Attack Category",
       y = "Mean Packets") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Chart 5: ct_state_ttl counts across attacks
state_dist <- data_alt %>%
  count(attack_cat, ct_state_ttl)

ggplot(state_dist, aes(x = ct_state_ttl, y = n, fill = attack_cat)) +
  geom_col(position = "dodge") +
  labs(title = "ct_state_ttl Counts Across Attacks",
       x = "State/TTL",
       y = "Count") +
  theme_minimal()


############################################################
# 5. Hypothesis Test (5.4)
############################################################


dur_top <- data_alt %>%
  filter(group == top_attack) %>%
  pull(dur)

dur_other <- data_alt %>%
  filter(group == "Other") %>%
  pull(dur)

t_test_result <- t.test(dur_top, dur_other)   
t_test_result


############################################################



#==============================================================================================================
#Complex Hypothesis (Conclusion)

library(dplyr)
library(ggplot2)
library(tidyr)
library(scales)

#Data Preparation
#Selecting columns for analysis from  multiple dataset
combined_data <- bind_rows (
  cleaned_bytes_dataset %>% select(attack_cat, sbytes, dbytes),
  cleaned_dataset %>% select(attack_cat, dur, sbytes),
  data %>% select(attack_cat, dur, sbytes, dbytes, spkts, dpkts),
  data_alt %>% select(attack_cat, dur, sbytes, dbytes, spkts, dpkts)
) %>%
  distinct() %>%
  mutate(
    traffic_type = ifelse(tolower(attack_cat) == "normal", "Normal", "Attack"),
    dur        = ifelse(is.na(dur), 0, dur),
    spkts      = ifelse(is.na(spkts), 0, spkts),
    dpkts      = ifelse(is.na(dpkts), 0, dpkts),
    sbytes     = ifelse(is.na(sbytes), 0, sbytes),
    dbytes     = ifelse(is.na(dbytes), 0, dbytes)
  )

#Derived metrics
combined_data <- combined_data %>%
  mutate(
    total_bytes = sbytes + dbytes,
    total_packets = spkts + dpkts,
    forward_packet_rate  = ifelse(dur > 0, spkts / dur, 0),
    backward_packet_rate = ifelse(dur > 0, dpkts / dur, 0)
  )


#Hypothesis Testing

#Compare Total Bytes
t_bytes <- t.test(total_bytes ~ traffic_type, data = combined_data)
wilcox_bytes <- wilcox.test(total_bytes ~ traffic_type, data = combined_data)

#Compare Total Packets
t_packets <- t.test(total_packets ~ traffic_type, data = combined_data)
wilcox_packets <- wilcox.test(total_packets ~ traffic_type, data = combined_data)

#Compare Duration
t_duration <- t.test(dur ~ traffic_type, data = combined_data)
wilcox_duration <- wilcox.test(dur ~ traffic_type, data = combined_data)

#Correlation Analysis (check for significant correlations)
correlations <- combined_data %>%
  select(dur, sbytes, dbytes, total_bytes, total_packets, forward_packet_rate, backward_packet_rate) %>%
  cor(use = "complete.obs", method = "pearson")


#Results
cat("=== T-tests ===\n")
cat("\nTotal Bytes:\n"); print(t_bytes)
cat("\nTotal Packets:\n"); print(t_packets)
cat("\nDuration:\n"); print(t_duration)

cat("\n=== Wilcoxon Tests ===\n")
cat("\nTotal Bytes:\n"); print(wilcox_bytes)
cat("\nTotal Packets:\n"); print(wilcox_packets)
cat("\nDuration:\n"); print(wilcox_duration)

cat("\n=== Correlation Matrix ===\n")
print(correlations)
