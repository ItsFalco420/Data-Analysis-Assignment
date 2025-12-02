#Data Analysis

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

#AFRAH ARSHAD SHAH (TP085430)
# Objective 2: To examine how connection duration and 
#              data transfer volume vary across attack types.  

# ======================================================
# Data Cleaning
# ======================================================
unsw     <- read.csv("5. UNSW_NB15.csv", stringsAsFactors = FALSE)
features <- read.csv("6. NUSW-NB15_features (data description).csv", stringsAsFactors = FALSE)


dim(unsw)        # rows, columns
names(unsw)      # column names
head(unsw, 3)    # first 3 rows
str(unsw)        # structure: types of each column
# helper to snake_case with base R
make_snake <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)     # replace groups of non-alnums with _
  x <- gsub("^_|_$", "", x)           # trim leading _
  x <- gsub("__+", "_", x)            # collapse repeated __
  x
}
names(unsw) <- make_snake(names(unsw))
names(unsw)  
n_before <- nrow(unsw)
unsw <- unsw[!duplicated(unsw), ]
cat("Removed", n_before - nrow(unsw), "duplicate rows.\n")
# overall missing %
total_missing_pct <- mean(is.na(unsw)) * 100
cat(sprintf("Total missing: %.2f%%\n", total_missing_pct))

# missing by column
miss_by_col <- colSums(is.na(unsw))
miss_pct_by_col <- round(100 * miss_by_col / nrow(unsw), 2)
head(sort(miss_pct_by_col, decreasing = TRUE), 20)

# rows with any NA 
rows_with_any_na <- unsw[!complete.cases(unsw), ]
nrow(rows_with_any_na)
# Trim whitespace in *character* columns and turn "" into NA
for (col in names(unsw)) {
  if (is.character(unsw[[col]])) {
    unsw[[col]] <- trimws(unsw[[col]])
    unsw[[col]][unsw[[col]] == ""] <- NA
  }
}

# Remove commas in character columns
for (col in names(unsw)) {
  if (is.character(unsw[[col]])) {
    unsw[[col]] <- gsub(",", "", unsw[[col]])
  }
}
for (col in names(unsw)) {
  if (is.character(unsw[[col]])) {
    suppressWarnings({
      test_num <- as.numeric(unsw[[col]])
    })
    
    prop_parse <- sum(!is.na(test_num) & !is.na(unsw[[col]])) / sum(!is.na(unsw[[col]]))
    if (!is.nan(prop_parse) && prop_parse >= 0.9) {
      unsw[[col]] <- test_num
      cat("Converted", col, "to numeric\n")
    }
  }
}
row_na_pct <- rowMeans(is.na(unsw)) * 100
drop_idx <- which(row_na_pct > 40)
if (length(drop_idx) > 0) {
  unsw <- unsw[-drop_idx, ]
  cat("Dropped", length(drop_idx), "rows with >40% missing.\n")
}

mode_simple <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  tab <- table(x)
  names(tab)[which.max(tab)]
}

for (col in names(unsw)) {
  miss_pct <- mean(is.na(unsw[[col]])) * 100
  if (miss_pct == 0) next
  
  if (miss_pct < 5) {
    
    unsw <- unsw[!is.na(unsw[[col]]), ]
    cat(sprintf("Removed rows with NA in %s (%.2f%% < 5%%).\n", col, miss_pct))
  } else {
    
    if (is.numeric(unsw[[col]])) {
      med <- median(unsw[[col]], na.rm = TRUE)
      unsw[[col]][is.na(unsw[[col]])] <- med
      cat(sprintf("Imputed %s with median (%.2f%% missing).\n", col, miss_pct))
    } else {
      m <- mode_simple(unsw[[col]])
      unsw[[col]][is.na(unsw[[col]])] <- m
      cat(sprintf("Imputed %s with mode (%.2f%% missing).\n", col, miss_pct))
    }
  }
}
for (col in names(unsw)) {
  if (is.character(unsw[[col]])) {
    unsw[[col]] <- tolower(unsw[[col]])
  }
}
const_cols <- sapply(unsw, function(x) length(unique(x)) <= 1)
if (any(const_cols)) {
  cat("Removing constant columns:", paste(names(unsw)[const_cols], collapse = ", "), "\n")
  unsw <- unsw[, !const_cols, drop = FALSE]
}

str(unsw)
summary(unsw)

write.csv(unsw, "UNSW_NB15_cleaned.csv", row.names = FALSE)
cat("Saved cleaned file to: UNSW_NB15_cleaned.csv\n")


library(tidyverse)
unsw <- read_csv("UNSW_NB15_cleaned.csv", show_col_types = FALSE)
                     
glimpse(unsw)   
summary(unsw)  

unsw$attack_cat <- as.factor(unsw$attack_cat)
table(unsw$attack_cat)


# ======================================================
# Analysis 2-1: Average connection duration by attack type
# ======================================================

avg_duration <- tapply(unsw$dur, unsw$attack_cat, mean, na.rm = TRUE)
print(avg_duration)


graphics.off()
par(mfrow = c(1,1), mar = c(6,4,2,1) + 0.1)


boxplot(unsw$dur ~ unsw$attack_cat,
        main = "Connection Duration by Attack Type",
        xlab = "Attack Type",
        ylab = "Duration",
        las = 2,       
        cex.axis = 0.8) 

# ======================================================
# Analysis 2-2: Average bytes by attack type
# ======================================================

avg_sbytes <- tapply(unsw$sbytes, unsw$attack_cat, mean, na.rm = TRUE)
avg_dbytes <- tapply(unsw$dbytes, unsw$attack_cat, mean, na.rm = TRUE)


print(avg_sbytes)
print(avg_dbytes)


graphics.off()
par(mfrow = c(1,2), mar = c(6,4,2,1) + 0.1)
boxplot(unsw$sbytes ~ unsw$attack_cat,
        main = "Source Bytes by Attack Type",
        xlab = "Attack Type",
        ylab = "Source Bytes",
        las = 2, cex.axis = 0.8)
boxplot(unsw$dbytes ~ unsw$attack_cat,
        main = "Destination Bytes by Attack Type",
        xlab = "Attack Type",
        ylab = "Destination Bytes",
        las = 2, cex.axis = 0.8)
par(mfrow = c(1,1))



# ======================================================
# Analysis 2-3: Relationship between duration and total bytes
# ======================================================

unsw$total_bytes <- unsw$sbytes + unsw$dbytes


cor_value <- cor(unsw$dur, unsw$total_bytes, use = "complete.obs")
cat("Correlation between duration and total bytes:", cor_value, "\n")


graphics.off()
par(mfrow = c(1,1), mar = c(6,4,2,1) + 0.1)
plot(unsw$dur, unsw$total_bytes,
     main = "Duration vs Total Bytes",
     xlab = "Duration",
     ylab = "Total Bytes",
     pch = 16, col = "blue")

cat("\nScript finished running successfully.\n")
                     
#==============================================================================================================








                     
                     
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

#ALTAYEB ABDELGADIR MOHAMED




#==============================================================================================================
#Complex Hypothesis (Conclusion)