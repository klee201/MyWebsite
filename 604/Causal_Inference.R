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

# 設定檔案路徑
raw_file_list <- sprintf("Final_project_data/AI%03d_comments_tokens.csv", 1:6)
lda_result_file <- "Final_project_data/lda_doc_topic_distr.csv"

# 檢查 doc_topic_distr_df 的前幾行和欄位名稱
print(head(doc_topic_distr_df))
print(colnames(doc_topic_distr_df))

# 載入所有原始 tokens 數據
comments_raw <- raw_file_list %>%
  map_df(~ read_csv(.x, show_col_types = FALSE))

# 轉換為數值類型
comments_raw <- comments_raw %>%
  mutate(likeCount = as.numeric(likeCount),
         reply = as.numeric(reply))

# 載入 LDA 結果 (主題概率)
doc_topic_distr_df <- read_csv(lda_result_file)

# 2. 重新創建 doc_id 並聚合 metrics (修正以確保 doc_id 為整數)
# ----------------------------------------------------
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
  ungroup() %>%
  mutate(doc_id = as.integer(doc_id)) # 確保 doc_id 為整數


# 3. 合併數據集並進行 Log 轉換
# ----------------------------------------------------

doc_topic_distr_df <- read_csv(lda_result_file) %>%
  mutate(doc_id = as.integer(doc_id)) # 確保 doc_id 為整數

merged_df <- comments_metrics %>%
  inner_join(doc_topic_distr_df, by = "doc_id") %>%
  # *** 關鍵優化：Log 轉換依變數 ***
  mutate(log_likeCount = log(likeCount + 1))


# 4. OLS 迴歸分析 (使用 Log 轉換後的依變數並加入控制變數)
# ----------------------------------------------------

print("--- OLS Regression Results (Outcome: log(likeCount + 1)) ---")
print("Reference Topic: V8 (PUA/Victim). Added Control Variable: reply")

ols_model_log_likecount <- lm(
  log_likeCount ~ V1 + V2 + V3 + V4 + V5 + V6 + V7 + reply, # **加入 reply 作為控制變數**
  data = merged_df
)

# 打印完整的迴歸摘要
model_summary_log <- summary(ols_model_log_likecount)
print(model_summary_log)


# ----------------------------------------------------
# 5. 結果提取與可視化 (R Markdown 優化)
# ----------------------------------------------------

# 取得係數矩陣並轉為資料框
coef_table <- as.data.frame(model_summary_log$coefficients)

results_df <- coef_table %>%
  # *** 關鍵修正：明確只保留前四欄，以應對 4 欄或 5 欄的係數表格式 ***
  select(1:4) %>% # 只選擇前 4 欄：Estimate, Std. Error, t value, P value
  
  # 現在我們確定只有 4 欄，可以安全地使用 4 個名稱
  setNames(c("Coef", "Std. Error", "t_value", "P_Value")) %>% 
  
  # 將行名稱（即變數名稱 V1, V2, ...）轉換為名為 "Topic" 的新欄位
  rownames_to_column(var = "Topic") %>%
  
  # 計算顯著性標記，並使用修正後的主題標籤
  mutate(
    Significance = case_when(
      P_Value < 0.001 ~ "***",
      P_Value < 0.01 ~ "**",
      P_Value < 0.05 ~ "*",
      TRUE ~ ""
    ),
    # 修正主題標籤
    Topic_Label = case_when(
      Topic == "(Intercept)" ~ "Intercept (Ref. Topic 8: Sam Altman / Interview)",
      Topic == "V1" ~ "V1 (People / Human / Technology)",
      Topic == "V2" ~ "V2 (Models / Humanity / Technology)",
      Topic == "V3" ~ "V3 (Art / Data / Artists)",
      Topic == "V4" ~ "V4 (News / Fake / Video)",
      Topic == "V5" ~ "V5 (Energy / Power / Change)",
      Topic == "V6" ~ "V6 (Internet / Real / World)",
      Topic == "V7" ~ "V7 (God / Life / Earth)",
      Topic == "reply" ~ "Control: Reply Count", 
      TRUE ~ Topic
    )
  )

# 顯示前幾行確認是否正確
#print(head(results_df))

# 可視化迴歸係數 (使用 ggplot2)
p <- ggplot(results_df %>% filter(Topic != "(Intercept)"), 
            aes(x = Coef, y = reorder(Topic_Label, Coef), fill = P_Value < 0.05)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(xmin = Coef - `Std. Error`, xmax = Coef + `Std. Error`), 
                width = 0.2, position = position_dodge(0.9)) +
  # 調整顯著性標籤位置
  geom_text(aes(label = Significance, x = Coef + 0.1 * sign(Coef)), size = 5, hjust = 0, color = "black") + 
  scale_fill_manual(values = c("TRUE" = "#0072B2", "FALSE" = "gray70"), guide = "none") +
  # 調整 ggplot 標籤
  labs(
    title = "The impact of topic on the log number of likes on comments",
    subtitle = "Reference Topic V8: Sam Altman / Interview. Control: Reply Count.", # **更新這裡**
    x = "Regression Coefficient (Comparing to Topic 8)",
    y = "Topic"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))

# *** 關鍵變動：直接輸出 ggplot 物件 P，讓 R Markdown 處理渲染 ***
print(p)