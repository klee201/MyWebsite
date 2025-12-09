# === Kmeans_Clustering.R (基於 FCM 共現特徵的詞彙聚類) ===

# ----------------------------------------------------
# 0. 載入套件與數據準備
# ----------------------------------------------------
library(tidyverse)
library(quanteda)
library(readr)

# 重新載入數據並創建 tokens (確保數據在環境中存在)
file_list <- sprintf("Final_project_data/CN_SIMP%03d_comments_tokens.csv", 1:6)

comments <- file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# 創建 tokens
tokens_comments <- tokens(comments$word, what = "fastestword")
zh_stopwords <- stopwords("zh", source = "misc")
tokens_comments <- tokens_remove(tokens_comments, zh_stopwords)

# === Kmeans_Clustering.R (最終修復版，移除停用詞過濾) ===

# ----------------------------------------------------
# 0. 載入套件與數據準備
# ----------------------------------------------------
library(tidyverse)
library(quanteda)
library(readr)

# 重新載入數據
file_list <- sprintf("Final_project_data/CN_SIMP%03d_comments_tokens.csv", 1:6)
comments <- file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# 創建 tokens (不再移除停用詞)
tokens_comments <- tokens(comments$word, what = "fastestword")

# ----------------------------------------------------
# 1. 創建並修剪特徵矩陣
# ----------------------------------------------------

# 1. 創建 DFM (用於穩健地篩選特徵)
dfm_temp <- dfm(tokens_comments)

# 2. 移除低頻詞，得到修剪後的 DFM 特徵名稱
# ***關鍵修正***: min_termfreq 設置為 1 (最低門檻，確保包含所有單次出現的詞彙)
dfm_trimmed <- dfm_temp %>%
  quanteda::dfm_trim(min_termfreq = 1) 
trimmed_features <- featnames(dfm_trimmed)


# 3. 創建原始 FCM
fcm_model_full <- fcm(tokens_comments, context = "document")

# 4. 手動過濾 FCM 特徵
fcm_model <- fcm_model_full[trimmed_features, trimmed_features]

# ----------------------------------------------------
# 2. 執行 K-Means 聚類分析 - 確保數據點唯一
# ----------------------------------------------------

# 將 FCM 轉換為原始矩陣。
word_matrix_raw <- as.matrix(fcm_model)

# ***關鍵修正：去除重複的詞彙特徵向量***
# K-means 需要唯一的數據點作為初始中心。
word_matrix <- unique(word_matrix_raw)

print(paste("原始詞彙數:", nrow(word_matrix_raw), "。唯一特徵向量數:", nrow(word_matrix)))

# ----------------------------------------------------
# 3. 決定 K 值: 肘部法則 (Elbow Method) - 動態調整 K
# ----------------------------------------------------

# 1. 檢查剩餘的詞彙數量 (數據點的數量)
N_words <- nrow(word_matrix)

if (N_words < 2) {
  stop("ERROR: 剩餘的唯一詞彙特徵數量少於 2 個。請檢查 min_termfreq (10) 是否設置太高。")
}

# 2. 設定最大 K 值
# 最大 K 值設定為 (唯一詞彙數 - 1) 與 15 之間的較小值
# 這是為了確保 K 不會超過數據點數量
max_k <- min(15, N_words - 1) 

set.seed(42)
wss <- numeric(max_k)

print(paste("Starting Elbow Method for K-Means. Testing K from 1 to", max_k))

for (i in 1:max_k) {
  # 這裡我們在詞彙的共現特徵上運行 K-means
  kmeans_result <- kmeans(word_matrix, centers = i, nstart = 5) 
  wss[i] <- kmeans_result$tot.withinss
}

# 繪製肘部曲線
plot(1:max_k, wss, type = "b", 
     xlab = "Number of Clusters (K)", 
     ylab = "Total Within Sum of Squares (WSS)",
     main = "Elbow Method for Optimal K (FCM-based Word Features)")


# ----------------------------------------------------
# 4. 運行最終模型並輸出結果 (請根據上圖結果設定 K)
# ----------------------------------------------------

# !!! 請根據繪圖結果，將 optimal_k 替換為肘部轉折點的值 !!!
optimal_k <- 5 # 假設肘部在 K=5 處

print(paste("Running final K-Means with K =", optimal_k))
final_kmeans <- kmeans(word_matrix, centers = optimal_k, nstart = 25)

# 將聚類結果加到詞彙列表中
word_cluster_df <- tibble(
  word = rownames(word_matrix),
  cluster = final_kmeans$cluster
)

# 找出每個集群中最靠近中心點的 10 個詞彙 (用於解釋語義場)
cluster_interpretation <- word_cluster_df %>%
  group_by(cluster) %>%
  slice_max(order_by = n(), n = 10) %>% # 這裡使用頻率代替距離，作為近似解釋
  ungroup() %>%
  select(cluster, word)

print("K-Means Cluster Interpretation (Top Words per Cluster):")
print(cluster_interpretation, n = 100)