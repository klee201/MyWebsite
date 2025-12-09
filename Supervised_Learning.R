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

## Goal: Supervised Text Classification to predict if a comment 'contains_simp_factor' using Naive Bayes.

# ----------------------------------------------------
# 1. Load Data and Process (Similar to loading data_corpus_LMRD)
# ----------------------------------------------------
file_list <- sprintf("Final_project_data/CN_SIMP%03d_comments_tokens.csv", 1:6)
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
zh_stopwords <- stopwords("zh", source = "misc")

# 1. Create Tokens object and DFM
tokens_raw <- quanteda::tokens(comments$word)
docnames(tokens_raw) <- seq_len(ndoc(tokens_raw))
dfm_initial <- quanteda::dfm(tokens_raw, tolower = FALSE)

# 2. Group/Aggregate DFM by doc_id (Rows become comments)
dfm_grouped <- quanteda::dfm_group(dfm_initial, groups = comments$doc_id)

# 3. Filter DFM 
dfm_final <- dfm_grouped
dfm_final <- quanteda::dfm_remove(dfm_final, pattern = zh_stopwords)
dfm_final <- quanteda::dfm_remove(dfm_final, pattern = "\\p{P}", valuetype = "regex")
dfm_final <- quanteda::dfm_remove(dfm_final, pattern = "^\\d+$", valuetype = "regex")

# Convert to final matrix ('X' features)
matrix_main <- as.matrix(dfm_final)


# ----------------------------------------------------
# 3. 創建監督式目標變數 ('y') - 保持不變
# ----------------------------------------------------
# 使用字典計算標籤 'y' (contains_simp_factor)

simp_dict <- dictionary(list(
  simp_behavior = c("舔狗","simp","跪舔","上趕著","一廂情願","卑微","無底線","盲目付出","自我感動","單方面","單方面付出","一味付出","過度付出","得不到","不值得","不在乎","工具人"),
  victim_position = c("受害者","pua","不公平","不平等","男尊女卑","重男輕女","理所當然","天經地義","垃圾桶","還不如","陌生人"),
  financial_simping = c("atm","自助餐","麥當勞","人民幣","萬余元","多少錢"),
  relationship_imbalance = c("女朋友","男朋友","女孩子","男孩子","在一起","看不到","想不開")
))

# 提取所有 Simp 詞彙
simp_features_to_remove <- unlist(simp_dict)

# 計算 Simp 詞彙總數
matching_cols <- colnames(matrix_main) %in% simp_features_to_remove
matrix_dict <- matrix(
  rowSums(matrix_main[, matching_cols, drop = FALSE]),
  ncol = 1,
  dimnames = list(rownames(matrix_main), "simp_score")
)

# 創建二元 FACTOR 目標變數 ('y')
comments_agg$contains_simp <- matrix_dict[, 1] > 0
comments_agg$contains_simp_factor <- factor(comments_agg$contains_simp, levels = c(FALSE, TRUE))


# ----------------------------------------------------
# 4. 解決數據洩漏問題：創建新的特徵矩陣 X_cleaned
# ----------------------------------------------------
cat("\n--- Solving Data Leakage: Remove the word which exist in Simp dictionary ---\n")

# 1. 找出所有屬於 Simp 字典的欄位 (在 matrix_main 中)
cols_to_remove <- colnames(matrix_main) %in% simp_features_to_remove

# 2. 創建新的乾淨特徵矩陣 X_cleaned，移除所有 Simp 字典詞彙
X_cleaned <- matrix_main[, !cols_to_remove]
print(paste("Original (matrix_main):", ncol(matrix_main)))
print(paste("Remove the word in Simp dictionary (X_cleaned):", ncol(X_cleaned)))


# ----------------------------------------------------
# 5. 訓練/測試數據分割 (使用 X_cleaned 作為特徵)
# ----------------------------------------------------
set.seed(12345) 

# 分割 X 特徵和 y 標籤
train_idx <- createDataPartition(comments_agg$contains_simp_factor, p = 0.7, list = FALSE)

# 特徵矩陣分割
matrix_train_cleaned <- X_cleaned[train_idx, ]
matrix_test_cleaned <- X_cleaned[-train_idx, ]

# 標籤分割 (y 保持不變)
y_train <- comments_agg$contains_simp_factor[train_idx]
y_test <- comments_agg$contains_simp_factor[-train_idx]

# 轉換為 DFM 格式
dfmTrain_cleaned <- quanteda::as.dfm(matrix_train_cleaned)
dfmTest_cleaned <- quanteda::as.dfm(matrix_test_cleaned)


# ----------------------------------------------------
# 6. Naïve Bayes 訓練與預測
# ----------------------------------------------------
cat("\n--- 6. Naive Bayes Training ---\n")

# 訓練模型
nb_model_cleaned <- textmodel_nb(dfmTrain_cleaned, y_train, distribution = "Bernoulli")
# summary(nb_model_cleaned) # 可選：檢查模型摘要

# 特徵匹配 (將測試集與訓練集特徵對齊)
dfmTestMatched_cleaned <- dfm_match(dfmTest_cleaned, features = featnames(dfmTrain_cleaned))


# 預測分類
predicted_cleaned <- predict(nb_model_cleaned, newdata = dfmTestMatched_cleaned)

# 評估模型性能
cat("\n--- Naive Bayes Prediction ---\n")
confusion_cleaned <- table(predicted_cleaned, y_test)
print("Confusion Matrix:")
print(confusionMatrix(confusion_cleaned, mode = "everything"))
# ----------------------------------------------------