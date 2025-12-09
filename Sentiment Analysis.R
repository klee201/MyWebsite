# ----------------------------------------------------
# 0. Load packages (as in Tutorial Front-End Matters)
# ----------------------------------------------------
library(tidyverse)
library(quanteda)
library(quanteda.textmodels)
library(stopwords)
library(caret)
library(dplyr)
library(ggplot2)
# Note: quanteda.dictionaries and tidytext packages are already covered 
# by the functionality in quanteda and the way you process the NTUSD data.

# ----------------------------------------------------
# 1. Load Data and Process (Preparation for DFM)
# ----------------------------------------------------
# Load data files (similar to loading data_corpus_LMRD)
file_list <- sprintf("Final_project_data/CN_SIMP%03d_comments_tokens.csv", 1:6)
comments <- file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# Create unique comment ID (doc_id) - necessary for aggregation
comments <- comments %>%
  mutate(
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
zh_stopwords <- stopwords("zh", source = "misc")

# 1. Create Tokens object and DFM (Pre-processed DFM like 'movieReviewDfm')
tokens_raw <- quanteda::tokens(comments$word)
docnames(tokens_raw) <- seq_len(ndoc(tokens_raw))
dfm_initial <- quanteda::dfm(tokens_raw, tolower = FALSE)

# 2. Group/Aggregate DFM by doc_id (The crucial Chinese text aggregation step)
dfm_full <- quanteda::dfm_group(dfm_initial, groups = comments$doc_id)

# 3. Filter DFM
dfm_full <- quanteda::dfm_remove(dfm_full, pattern = zh_stopwords)
dfm_full <- quanteda::dfm_remove(dfm_full, pattern = "\\p{P}", valuetype = "regex")
dfm_full <- quanteda::dfm_remove(dfm_full, pattern = "^\\d+$", valuetype = "regex")

# Convert to matrix for calculation (used in both dictionary methods)
matrix_main <- as.matrix(dfm_full)


# ====================================================
# A. DICTIONARY ANALYSIS (Custom Simp Dictionary)
#    (Aligns with Tutorial: "Write your own dictionary")
# ====================================================

# 3.1 Define Custom Simp Dictionary (Aligns with my_dict example)
simp_dict <- dictionary(list(
  simp_behavior = c("舔狗","simp","跪舔","上趕著","一廂情願","卑微",
                    "無底線","盲目付出","自我感動","單方面","單方面付出",
                    "一味付出","過度付出","得不到","不值得","不在乎","工具人"),
  victim_position = c("受害者","pua","不公平","不平等","男尊女卑","重男輕女",
                      "理所當然","天經地義","垃圾桶","還不如","陌生人"),
  financial_simping = c("atm","自助餐","麥當勞","人民幣","萬余元","多少錢"),
  relationship_imbalance = c("女朋友","男朋友","女孩子","男孩子",
                             "在一起","看不到","想不開")
))

# 3.2 APPLY DICTIONARY via dfm_lookup (Similar to movieReviewDfm_mydict)
# We need to convert back to DFM for dfm_lookup
dfm_simp_lookup <- dfm_full %>% quanteda::dfm_lookup(dictionary = simp_dict)
print("DFM after Custom Simp Dictionary Lookup (Counts per category):")
head(dfm_simp_lookup, 5)

# 3.3 Create Simp Score and Supervised Label ('y')
# The tutorial shows how to merge a lexicon/dictionary to create a score.
# Here, we use the raw count to create a binary label for classification.
simp_features <- unlist(simp_dict)
matching_cols <- colnames(matrix_main) %in% simp_features
matrix_dict <- matrix(
  rowSums(matrix_main[, matching_cols, drop = FALSE]),
  ncol = 1,
  dimnames = list(rownames(matrix_main), "simp_score")
)

comments_agg$contains_simp <- matrix_dict[, 1] > 0
comments_agg$contains_simp_factor <- factor(comments_agg$contains_simp, levels = c(FALSE, TRUE))

# Final features for the classifier
matrix_combined <- cbind(matrix_main, matrix_dict)


# ====================================================
# B. SUPERVISED LEARNING (Classification)
#    (The tutorial *recommends* this after dictionary limits are shown)
# ====================================================

# 4. Train/test split
set.seed(123)
train_idx <- createDataPartition(comments_agg$contains_simp_factor, p = 0.7, list = FALSE)

matrix_train <- matrix_combined[train_idx, ]
y_train <- comments_agg$contains_simp_factor[train_idx]
matrix_test <- matrix_combined[-train_idx, ]
y_test <- comments_agg$contains_simp_factor[-train_idx]

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
# C. DICTIONARY COMPARISON (NTUSD Lexicon)
#    (Aligns with Tutorial: "Dictionary Comparison" & "Writing Your Own Dictionary")
# ====================================================

# 5.1 Load NTUSD dictionary files (Similar to loading AFINN/Loughran)
pos_path <- "Final_project_data/正面詞無重複_9365詞.txt"
neg_path <- "Final_project_data/負面詞無重複_11230詞.txt"
ntusd_pos_raw <- readLines(con = pos_path, encoding = "UTF-8", skipNul = TRUE)
ntusd_neg_raw <- readLines(con = neg_path, encoding = "UTF-8", skipNul = TRUE)
ntusd_pos_words <- iconv(ntusd_pos_raw, from = "BIG5", to = "UTF-8")
ntusd_neg_words <- iconv(ntusd_neg_raw, from = "BIG5", to = "UTF-8")

# 5.2 Map scores (Assigning weights like the final tutorial example)
features <- colnames(matrix_main)
feature_scores_ntusd <- rep(0, length(features))
names(feature_scores_ntusd) <- features
feature_scores_ntusd[features %in% ntusd_pos_words[!is.na(ntusd_pos_words)]] <- 1
feature_scores_ntusd[features %in% ntusd_neg_words[!is.na(ntusd_neg_words)]] <- -1

# 5.3 Execute matrix multiplication for Sentiment Score (The weighted sum calculation)
sentiment_scores <- matrix_main %*% feature_scores_ntusd
comments_agg$sentiment <- as.vector(sentiment_scores)


# ----------------------------------------------------
# 6. Analysis and Visualization (Similar to plotting polarity)
# ----------------------------------------------------
cat("\n### LEXICON-BASED NTUSD SENTIMENT RESULTS\n")
print("Mean Sentiment Score by Simping Label (NTUSD):")
result_sentiment <- comments_agg %>%
  group_by(contains_simp_factor) %>%
  summarize(mean_sentiment = mean(sentiment, na.rm = TRUE))
print(result_sentiment)

# Plot sentiment differences (Similar to plotting polarity distribution)
sentiment_plot <- ggplot(comments_agg, aes(x = contains_simp_factor, y = sentiment)) +
  geom_boxplot() +
  labs(x = "Simping Label (Target)",
       y = "Comment Sentiment Score (NTUSD Lexicon)",
       title = "Sentiment Distribution vs. Custom Simping Label") +
  theme_minimal()

print(sentiment_plot)
# ----------------------------------------------------