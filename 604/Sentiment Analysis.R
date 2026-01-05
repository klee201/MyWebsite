# ----------------------------------------------------
# 0. Load packages
# ----------------------------------------------------
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(stopwords)
library(caret)
library(dplyr)
library(ggplot2)

# ----------------------------------------------------
# 1. Load Data and Process (Preparation for DFM)
# ----------------------------------------------------
# Load data files (similar to loading data_corpus_LMRD)
file_list <- sprintf("Final_project_data/AI%03d_comments_tokens.csv", 1:6)
comments <- file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# Create unique comment ID (doc_id) - necessary for aggregation
comments <- comments %>%
  mutate(
    # Assuming 'reply' and 'likeCount' columns still exist and indicate new comments
    is_new_comment = c(TRUE, diff(as.numeric(reply)) != 0 | diff(likeCount) != 0),
    doc_id = cumsum(is_new_comment)
  )

# Create aggregated dataframe (for metadata and final scores)
comments_agg <- comments %>%
  group_by(doc_id) %>%
  slice(1) %>%
  ungroup() %>%
  arrange(doc_id)

# ----------------------------------------------------
# 2. Build DFM (Full Feature Set)
# ----------------------------------------------------
# *** CHANGED: Use English stopwords ***
en_stopwords <- stopwords("en", source = "smart")

# 1. Create Tokens object and DFM (Pre-processed DFM like 'movieReviewDfm')
# NOTE: The input column 'comments$word' must now contain English words/tokens.
tokens_raw <- quanteda::tokens(comments$word)
docnames(tokens_raw) <- seq_len(ndoc(tokens_raw))
dfm_initial <- quanteda::dfm(tokens_raw, tolower = TRUE) # Convert to lower for English

# 2. Group/Aggregate DFM by doc_id
dfm_full <- quanteda::dfm_group(dfm_initial, groups = comments$doc_id)

# 3. Filter DFM (Apply filtering rules)
dfm_full <- quanteda::dfm_remove(dfm_full, pattern = en_stopwords) # Remove English stopwords
dfm_full <- quanteda::dfm_remove(dfm_full, pattern = "\\p{P}", valuetype = "regex") # Punctuation
dfm_full <- quanteda::dfm_remove(dfm_full, pattern = "^\\d+$", valuetype = "regex") # Numbers

# Convert to matrix for calculation (used in both dictionary methods)
matrix_main <- as.matrix(dfm_full)


# ====================================================
# A. DICTIONARY ANALYSIS (Custom Trust/Distrust Dictionary)
#    (Aligns with Tutorial: "Write your own dictionary")
# ====================================================

# 3.1 Define Custom Dictionary (TOPIC: Human Trust in AI)
# *** CHANGED: Replaced Chinese 'simp_dict' with English 'ai_trust_dict' ***
ai_trust_dict <- dictionary(list(
  trust = c("trust", "rely", "believe", "confidence", "depend", "safe", "secured", "faith", "reliable", "validated"),
  distrust = c("distrust", "doubt", "skeptic", "fear", "scare", "lie", "unreliable", "biased", "risky", "dangerous", "unsecure"),
  ai_capability = c("automate", "replace", "efficient", "faster", "smarter", "intelligent", "predictive", "power", "potential"),
  human_value = c("human", "emotion", "creative", "original", "judgement", "ethics", "moral", "value", "integrity", "bias")
))

# 3.2 APPLY DICTIONARY via dfm_lookup
# We need to convert back to DFM for dfm_lookup
dfm_ai_trust_lookup <- dfm_full %>% quanteda::dfm_lookup(dictionary = ai_trust_dict)
print("DFM after Custom AI Trust Dictionary Lookup (Counts per category):")
head(dfm_ai_trust_lookup, 5)

# 3.3 Create Target Score and Supervised Label ('y')
# Let's create a label for 'Trust' by taking the sum of the 'trust' and 'ai_capability' categories.
trust_features <- unlist(ai_trust_dict[c("trust", "ai_capability")])
matching_cols <- colnames(matrix_main) %in% trust_features
matrix_dict <- matrix(
  rowSums(matrix_main[, matching_cols, drop = FALSE]),
  ncol = 1,
  dimnames = list(rownames(matrix_main), "trust_score")
)

# Create the binary target variable (y) for classification
comments_agg$contains_trust_words <- matrix_dict[, 1] > 0
comments_agg$contains_trust_factor <- factor(comments_agg$contains_trust_words, levels = c(FALSE, TRUE))

# Final features for the classifier
matrix_combined <- cbind(matrix_main, matrix_dict)


# ====================================================
# B. SUPERVISED LEARNING (Classification)
# ====================================================

# 4. Train/test split
set.seed(123)
train_idx <- createDataPartition(comments_agg$contains_trust_factor, p = 0.7, list = FALSE)

matrix_train <- matrix_combined[train_idx, ]
y_train <- comments_agg$contains_trust_factor[train_idx]
matrix_test <- matrix_combined[-train_idx, ]
y_test <- comments_agg$contains_trust_factor[-train_idx]

# 4.2 Train Naive Bayes model
dfm_train_final <- quanteda::as.dfm(matrix_train)
nb_model <- textmodel_nb(dfm_train_final, y_train)

# 4.3 Predict and Evaluate
dfm_test_final <- quanteda::as.dfm(matrix_test)
pred <- predict(nb_model, dfm_test_final)

cat("\n### SUPERVISED LEARNING (Naive Bayes Classification)\n")
print("Confusion Matrix:")
confusionMatrix(pred, y_test)


# ====================================================
# C. DICTIONARY COMPARISON (English Sentiment Lexicon)
# ====================================================

# 5.1 Load Sentiment Dictionary Files
# *** CHANGED: Removed the 'iconv' calls for Chinese encoding (BIG5/UTF-8) ***
pos_path <- "Final_project_data/positive-words.txt"
neg_path <- "Final_project_data/negative-words.txt"

# Assuming these files now contain standard English words (e.g., from the Harvard or Bing list)
ntusd_pos_words <- readLines(con = pos_path, encoding = "UTF-8", skipNul = TRUE)
ntusd_neg_words <- readLines(con = neg_path, encoding = "UTF-8", skipNul = TRUE)

# 5.2 Map scores (Assigning weights)
features <- colnames(matrix_main)
feature_scores_ntusd <- rep(0, length(features))
names(feature_scores_ntusd) <- features
feature_scores_ntusd[features %in% ntusd_pos_words] <- 1
feature_scores_ntusd[features %in% ntusd_neg_words] <- -1

# 5.3 Execute matrix multiplication for Sentiment Score
sentiment_scores <- matrix_main %*% feature_scores_ntusd
comments_agg$sentiment <- as.vector(sentiment_scores)


# ----------------------------------------------------
# 6. Analysis and Visualization
# ----------------------------------------------------
cat("\n### LEXICON-BASED SENTIMENT RESULTS (External Lexicon)\n")
print("Mean Sentiment Score by Custom Trust Label:")
result_sentiment <- comments_agg %>%
  group_by(contains_trust_factor) %>% # *** CHANGED: Group by new target variable ***
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE))
print(result_sentiment)

# Plot sentiment differences
sentiment_plot <- ggplot(comments_agg, aes(x = contains_trust_factor, y = sentiment)) +
  geom_boxplot() +
  labs(x = "Comment Contains AI Trust Words (Target)", # *** CHANGED: Label text ***
       y = "Comment Sentiment Score (External Lexicon)", # *** CHANGED: Label text ***
       title = "Sentiment Distribution vs. Custom AI Trust Label") + # *** CHANGED: Title ***
  theme_minimal()

print(sentiment_plot)
# ----------------------------------------------------