# ----------------------------------------------------
# Front-End Matters (Load Libraries)
# ----------------------------------------------------
library(tidyverse)
library(quanteda)
library(quanteda.textmodels) # Essential for textmodel_nb
library(stopwords)
library(caret) # Essential for Confusion Matrix and data splitting
library(dplyr)
library(ggplot2)

## Goal: Supervised Text Classification to predict if a comment 'contains_trust_factor' using Naive Bayes.

# ----------------------------------------------------
# 1. Load Data and Process (Similar to loading data_corpus_LMRD)
# ----------------------------------------------------
file_list <- sprintf("Final_project_data/AI%03d_comments_tokens.csv", 1:6)
comments <- file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# Create unique comment ID (doc_id) to aggregate word tokens into comments
comments <- comments %>%
  mutate(
    is_new_comment = c(TRUE, diff(as.numeric(reply)) != 0 | diff(likeCount) != 0),
    doc_id = cumsum(is_new_comment)
  )

# Create aggregated dataframe (for metadata/labels)
comments_agg <- comments %>%
  group_by(doc_id) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(doc_id)


# ----------------------------------------------------
# 2. Build DFM (Document-Feature Matrix)
# ----------------------------------------------------
# *** CHANGED: Use English stopwords ***
en_stopwords <- stopwords("en", source = "smart")

# 1. Create Tokens object and DFM
tokens_raw <- quanteda::tokens(comments$word)
docnames(tokens_raw) <- seq_len(ndoc(tokens_raw))
# *** CHANGED: Convert to lowercase for English text analysis ***
dfm_initial <- quanteda::dfm(tokens_raw, tolower = TRUE) 

# 2. Group/Aggregate DFM by doc_id (Rows become comments)
dfm_grouped <- quanteda::dfm_group(dfm_initial, groups = comments$doc_id)

# 3. Filter DFM 
dfm_final <- dfm_grouped
# *** CHANGED: Remove English stopwords ***
dfm_final <- quanteda::dfm_remove(dfm_final, pattern = en_stopwords) 
dfm_final <- quanteda::dfm_remove(dfm_final, pattern = "\\p{P}", valuetype = "regex")
dfm_final <- quanteda::dfm_remove(dfm_final, pattern = "^\\d+$", valuetype = "regex")

# Convert to final matrix ('X' features)
matrix_main <- as.matrix(dfm_final)


# ----------------------------------------------------
# 3. Create Supervised Target Variable ('y')
# ----------------------------------------------------
# *** CHANGED: Use the AI Trust dictionary ***
ai_trust_dict <- dictionary(list(
  trust = c("trust", "rely", "believe", "confidence", "depend", "safe", "secured", "faith", "reliable", "validated"),
  distrust = c("distrust", "doubt", "skeptic", "fear", "scare", "lie", "unreliable", "biased", "risky", "dangerous", "unsecure"),
  ai_capability = c("automate", "replace", "efficient", "faster", "smarter", "intelligent", "predictive", "power", "potential"),
  human_value = c("human", "emotion", "creative", "original", "judgement", "ethics", "moral", "value", "integrity", "bias")
))

# Extract all words used to define the 'Trust' label
# We combine 'trust' and 'ai_capability' for a positive 'trust' label, similar to the Chinese example.
trust_features_for_label <- unlist(ai_trust_dict[c("trust", "ai_capability")])

# Calculate Trust Word Count Score
matching_cols <- colnames(matrix_main) %in% trust_features_for_label
matrix_dict <- matrix(
  rowSums(matrix_main[, matching_cols, drop = FALSE]),
  ncol = 1,
  dimnames = list(rownames(matrix_main), "trust_score")
)

# Create Binary FACTOR Target Variable ('y')
# *** CHANGED: Variable name from 'contains_simp' to 'contains_trust' ***
comments_agg$contains_trust <- matrix_dict[, 1] > 0
comments_agg$contains_trust_factor <- factor(comments_agg$contains_trust, levels = c(FALSE, TRUE))


# ----------------------------------------------------
# 4. Address Data Leakage: Create New Feature Matrix X_cleaned
# ----------------------------------------------------
cat("\n--- Solving Data Leakage: Remove words used to create the Trust label ---\n")

# 1. Identify all columns (features) that were used to create the 'trust' label
cols_to_remove <- colnames(matrix_main) %in% trust_features_for_label

# 2. Create the new clean feature matrix X_cleaned, removing those words
X_cleaned <- matrix_main[, !cols_to_remove]
print(paste("Original (matrix_main) features:", ncol(matrix_main)))
print(paste("Cleaned (X_cleaned) features:", ncol(X_cleaned)))


# ----------------------------------------------------
# 5. Train/Test Data Split (Using X_cleaned as features)
# ----------------------------------------------------
set.seed(12345) 

# Split X features and y labels
# *** CHANGED: Target variable to 'contains_trust_factor' ***
train_idx <- createDataPartition(comments_agg$contains_trust_factor, p = 0.7, list = FALSE)

# Feature Matrix Split
matrix_train_cleaned <- X_cleaned[train_idx, ]
matrix_test_cleaned <- X_cleaned[-train_idx, ]

# Label Split (y remains the same)
y_train <- comments_agg$contains_trust_factor[train_idx]
y_test <- comments_agg$contains_trust_factor[-train_idx]

# Convert to DFM format
dfmTrain_cleaned <- quanteda::as.dfm(matrix_train_cleaned)
dfmTest_cleaned <- quanteda::as.dfm(matrix_test_cleaned)


# ----------------------------------------------------
# 6. Naïve Bayes Training and Prediction
# ----------------------------------------------------
cat("\n--- 6. Naive Bayes Training ---\n")

# Train the model
nb_model_cleaned <- textmodel_nb(dfmTrain_cleaned, y_train, distribution = "Bernoulli")

# Feature Matching (Align test features with train features)
dfmTestMatched_cleaned <- dfm_match(dfmTest_cleaned, features = featnames(dfmTrain_cleaned))


# Predict classification
predicted_cleaned <- predict(nb_model_cleaned, newdata = dfmTestMatched_cleaned)

# Evaluate Model Performance
cat("\n--- Naive Bayes Prediction Evaluation ---\n")
confusion_cleaned <- table(predicted_cleaned, y_test)
print("Confusion Matrix:")
# *** Changed the printing to explicitly show the result for clarity ***
print(confusionMatrix(confusion_cleaned, mode = "everything"))
# ----------------------------------------------------