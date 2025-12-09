# === Casual_Inference_Regression.R (主題概率對 LikeCount 的影響) ===

# 0. 載入必要的套件
# ----------------------------------------------------
library(tidyverse)
library(readr)
library(ggplot2) # 確保繪圖套件已載入
library(dplyr)
library(tibble) # 確保有 rownames_to_column

# 1. 數據載入與準備
# ----------------------------------------------------

# 設定檔案路徑 (假設這些檔案位於 'Final_project_data/' 中)
raw_file_list <- sprintf("Final_project_data/CN_SIMP%03d_comments_tokens.csv", 1:6)
lda_result_file <- "Final_project_data/lda_doc_topic_distr.csv"

# 載入所有原始 tokens 數據
comments_raw <- raw_file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# 轉換為數值類型
comments_raw <- comments_raw %>%
  mutate(likeCount = as.numeric(likeCount),
         reply = as.numeric(reply))

# 載入 LDA 結果 (主題概率)
doc_topic_distr_df <- read_csv(lda_result_file)


# 2. 重新創建 doc_id 並聚合 metrics
# ----------------------------------------------------

# 計算 doc_id 
comments_metrics <- comments_raw %>%
  mutate(
    is_new_comment = c(TRUE, diff(reply) != 0 | diff(likeCount) != 0),
    doc_id = cumsum(is_new_comment)
  ) %>%
  # 聚合指標
  group_by(doc_id) %>%
  summarise(
    likeCount = first(likeCount),
    reply = first(reply)
  ) %>%
  ungroup()


# 3. 合併數據集
# ----------------------------------------------------

# 內連接 metrics 和主題概率數據集
merged_df <- comments_metrics %>%
  inner_join(doc_topic_distr_df, by = "doc_id")

# 4. OLS 迴歸分析 (主題概率 -> likeCount)
# ----------------------------------------------------

print("--- OLS Regression Results (Outcome: likeCount) ---")
print("Reference Topic: V8 (PUA/Victim)")

ols_model_likecount <- lm(
  likeCount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7, 
  data = merged_df
)

# 打印完整的迴歸摘要
model_summary <- summary(ols_model_likecount)
print(model_summary)


# ----------------------------------------------------
# 5. 結果提取與可視化 (R Markdown 優化)
# ----------------------------------------------------

# **關鍵修正：創建 results_df (修復 '找不到物件' 的錯誤)**
coef_table <- as.data.frame(model_summary$coefficients)

results_df <- coef_table %>%
  setNames(c("Coef", "Std. Error", "t_value", "P_Value")) %>%
  rownames_to_column(var = "Topic") %>%
  # 計算顯著性標記
  mutate(
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # 為 plotting 準備 Topic 標籤
    Topic_Label = case_when(
      Topic == "(Intercept)" ~ "Intercept (Topic 8: PUA/Victim)",
      Topic == "V1" ~ "V1 (Financial/Money I)",
      Topic == "V2" ~ "V2 (Financial/Money II - Significant)",
      Topic == "V3" ~ "V3 (Relationship & Emotion)",
      Topic == "V4" ~ "V4 (Gender & Social Issues)",
      Topic == "V5" ~ "V5 (Society & Media)",
      Topic == "V6" ~ "V6 (Event Details & Questions)",
      Topic == "V7" ~ "V7 (Values & Morality)",
      TRUE ~ Topic
    )
  )

# 可視化迴歸係數 (使用 ggplot2)
p <- ggplot(results_df %>% filter(Topic != "(Intercept)"), 
            aes(x = Coef, y = reorder(Topic_Label, Coef), fill = P_Value < 0.05)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(xmin = Coef - `Std. Error`, xmax = Coef + `Std. Error`), 
                width = 0.2, position = position_dodge(0.9)) +
  # 調整顯著性標籤位置
  geom_text(aes(label = Significance, x = Coef + 0.1 * sign(Coef)), size = 5, hjust = 0, color = "black") + 
  scale_fill_manual(values = c("TRUE" = "#0072B2", "FALSE" = "gray70"), guide = "none") +
  labs(
    title = "The impact of topic on the number of likes on comments",
    subtitle = "Topic V2 (Finanace/PUA/Victim) Statistically significant positive effect(*)",
    x = "Regression Coefficient (Comparing to Topic 8)",
    y = "Topic"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# *** 關鍵變動：直接輸出 ggplot 物件 P，讓 R Markdown 處理渲染 ***
print(p)