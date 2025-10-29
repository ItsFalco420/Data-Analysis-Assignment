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

#TEE EN YONG

#ALTAYEB ABDELGADIR MOHAMED

#WONG ZHENG HAN

#QUAN JIA YONG

#ALTAYEB ABDELGADIR MOHAMED