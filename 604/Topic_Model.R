# === LDA_Topic_Model.R ===

# ----------------------------------------------------
# 0. 載入套件與數據準備
# ----------------------------------------------------
library(tidyverse)
library(text2vec) # 用於 LDA 訓練
library(readr)

# 重新載入數據並創建 doc_id (確保文檔聚合的基礎)
file_list <- sprintf("Final_project_data/CN_SIMP%03d_comments_tokens.csv", 1:6)

comments <- file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# 創建文檔 ID
comments <- comments %>%
  mutate(
    # 假設 reply 或 likeCount 變化時即為新評論
    is_new_comment = c(TRUE, diff(as.numeric(reply)) != 0 | diff(likeCount) != 0),
    doc_id = cumsum(is_new_comment)
  )

# ----------------------------------------------------
# 1. 聚合 tokens 為文檔文本 (每個 doc_id 對應一條評論)
# ----------------------------------------------------
comments_agg <- comments %>%
  group_by(doc_id) %>%
  # 將每個文檔的 tokens 重新組合成單個字符串
  summarise(text = paste(word[!is.na(word) & word != ""], collapse = " ")) %>%
  ungroup()

# ----------------------------------------------------
# 2. 創建迭代器、詞彙表與 DTM
# ----------------------------------------------------

# 創建迭代器
it <- itoken(comments_agg$text, 
             tokenizer = function(x) strsplit(x, " ", fixed = TRUE),
             ids = comments_agg$doc_id, 
             progressbar = TRUE)

# 創建詞彙表
vocab <- create_vocabulary(it)

# 修剪詞彙表 (移除低頻詞，提高主題質量)
# 這裡將最低詞頻設為 3 (比 K-means 的 10 安全，比 1 更具意義)
vocab <- vocab %>% 
  prune_vocabulary(term_count_min = 3, doc_proportion_max = 0.5)

# 創建 DTM
vectorizer <- vocab_vectorizer(vocab)
it <- itoken(comments_agg$text, 
             tokenizer = function(x) strsplit(x, " ", fixed = TRUE),
             ids = comments_agg$doc_id, 
             progressbar = TRUE)
dtm_lda <- create_dtm(it, vectorizer)

# ----------------------------------------------------
# 3. 訓練 LDA 模型
# ----------------------------------------------------

# 設定主題數量 K
num_topics <- 8 
num_iterations <- 500

print(paste("Starting LDA Topic Modeling with K =", num_topics))
set.seed(42)

# 初始化 LDA 模型 (已移除 vocabulary 參數)
lda_model <- LDA$new(n_topics = num_topics, 
                     doc_topic_prior = 0.1, 
                     topic_word_prior = 0.1)

# 訓練模型
doc_topic_distr <- lda_model$fit_transform(x = dtm_lda, 
                                           n_iter = num_iterations, 
                                           convergence_tol = 0.001,
                                           progressbar = TRUE)

print("LDA Training Complete.")

# ----------------------------------------------------
# 4. 結果分析 與 CSV 儲存 (新增部分)
# ----------------------------------------------------

# 提取主題-詞彙分佈矩陣
topic_word_distr <- lda_model$topic_word_distribution

# 設置要顯示的詞彙數量
top_n_words <- 10

# 提取每個主題最具代表性的 top_n_words
topic_words_list <- apply(topic_word_distr, 1, function(topic_prob) {
  top_terms <- head(sort(topic_prob, decreasing = TRUE), top_n_words)
  names(top_terms)
})

# 將結果轉換為更容易查看的數據框
topic_words_df <- as_tibble(topic_words_list) %>%
  mutate(Topic_Word_Rank = paste0("Word_", 1:top_n_words), .before = 1) %>%
  rename_with(~ paste0("Topic_", seq_along(.) - 1), -Topic_Word_Rank)

print("Top 10 Words for Each Topic:")
print(topic_words_df)

# *** 儲存主題詞結果 ***
write_csv(topic_words_df, "Final_project_data/lda_topic_words.csv")
print("Saved topic words to lda_topic_words.csv")


# 提取每個文檔的主題分佈
# 這裡 doc_topic_distr 已經是步驟 3 的結果
doc_topic_distr_df <- as_tibble(doc_topic_distr) %>%
  mutate(doc_id = comments_agg$doc_id, .before = 1)

print("Document-Topic Distribution (Head):")
print(head(doc_topic_distr_df))

# *** 儲存文檔主題分佈結果 ***
write_csv(doc_topic_distr_df, "Final_project_data/lda_doc_topic_distr.csv")
print("Saved document-topic distribution to lda_doc_topic_distr.csv")

