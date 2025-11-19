#Data Cleaning

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


#Data Analysis

#============================================================================================================
#TEE EN YONG (TP074195)
library(tidyverse)
library(janitor)
library(ggplot2)
library(scales)
library(reshape2)
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
ggplot(cleaned_bytes_dataset, aes(x = sbytes, y = dbytes)) +
  geom_hex(bins = 40) +
  scale_fill_viridis_c(option = "C") +
  scale_x_log10() +
  scale_y_log10() +
  theme_minimal() +
  labs(title = "Hexbin of sbytes vs dbytes", x = "sbytes (log scale)", y = "dbytes (log scale)")


#============================================================================================================
#1: How do the amounts of source bytes (sbytes) differ across various attack categories?

sbytes_summary <- cleaned_data_csv %>%
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
dbytes_summary <- cleaned_data_csv %>%
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
#3: What is the relationship between source bytes (sbytes) and destination bytes (dbytes) for each attack and normal category?
bytes_summary <- cleaned_data_csv %>%
  mutate(traffic_type = ifelse(tolower(attack_cat) == "normal", "Normal", "Attack")) %>%
  group_by(traffic_type) %>%
  summarise(
    total_sbytes = sum(sbytes, na.rm = TRUE),
    total_dbytes = sum(dbytes, na.rm = TRUE),
    mean_sbytes = mean(sbytes, na.rm = TRUE),
    mean_dbytes = mean(dbytes, na.rm = TRUE)
  )

print(bytes_summary)

bytes_long <- bytes_summary %>%
  mutate(across(starts_with("total_"), ~ .x / 1e6)) %>%
  select(traffic_type, total_sbytes, total_dbytes) %>%
  pivot_longer(cols = c(total_sbytes, total_dbytes),
               names_to = "byte_type", values_to = "total_value")

ggplot(bytes_long, aes(x = traffic_type, y = total_value, fill = byte_type)) +
  geom_col(position = "dodge") +
  labs(title = "Total Source and Destination Bytes by Traffic Type",
       x = "Traffic Type", y = "Total Bytes (MB)", fill = "Byte Type") +
  theme_minimal()

#============================================================================================================
#Hypothesis Testing: Two-sample t-test

cleaned_data_csv$total_bytes <- cleaned_data_csv$sbytes + cleaned_data_csv$dbytes

cleaned_data_csv$traffic_type <- ifelse(tolower(cleaned_data_csv$attack_cat) == "normal","Normal", "Attack")

t_test_results <- t.test(total_bytes ~ traffic_type,
                         data = cleaned_data_csv,
                         alternative = "two.sided",
                         var_equal = FALSE)

print(t_test_results)

#==============================================================================================================

#AFRAH ARSHAD SHAH (TP085430)
# Objective 2: To examine how connection duration and 
#              data transfer volume vary across attack types.  

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








                     
                     
#WONG ZHENG HAN

#QUAN JIA YONG

#ALTAYEB ABDELGADIR MOHAMED
