# Detach plyr if loaded
#if("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)

#library(dplyr)

library(stringr)
library(tidytext)
library(readr)
library(wordcloud)
library(RColorBrewer)

# ---- Define folder ----
#folder <- "Final_project_data"
#common_file <- file.path(folder, "traditional_common_words_combined.csv")

# ---- Load the common words CSV ----
#common_words <- read_csv(common_file, show_col_types = FALSE)

# ---- Assume you have a 'simping_comment' column (1 = simping, 0 = non-simping) ----
# If not, create it using keywords:
#simp_keywords <- c("simp", "舔狗", "舔", "陪玩", "金主", "撈女")
#common_words <- common_words %>%
  #mutate(simping_comment = ifelse(str_detect(traditional_text, 
                                             #                       paste(simp_keywords, collapse = "|")), 1, 0))


# Split words into two groups: simping vs non-simping
#word_list <- lapply(0:1, function(flag) {
#  df <- filter(common_words, simping_comment == flag)
#  vec <- setNames(df$total_count, df$traditional_text)
#  vec[vec > 0]  # remove zero-count words
#})
#
# Assign group names
#names(word_list) <- c("Non-simping", "Simping")

# Remove empty groups to avoid comparison.cloud error
#word_list <- word_list[sapply(word_list, length) > 0]

# Generate comparison word cloud if at least 2 groups remain
#if(length(word_list) >= 2){
  #set.seed(1234)
  #  comparison.cloud(
    #word_list,
    #max.words = 200,
    #colors = brewer.pal(8, "Dark2"),
    #    title.size = 1.5
    #)
  #  cat("Comparison Word Cloud generated.\n")
  #} else {
#  cat("Not enough groups with words to generate comparison cloud.\n")
#}


#----------if work delete up---------------


# ---- Load CSV ----
common_file <- "Final_project_data/traditional_common_words_combined.csv"
word_freq <- read_csv(common_file, show_col_types = FALSE)

# Ensure frequencies are numeric
word_freq$total_count <- as.numeric(word_freq$total_count)

# ---- Check if data exists ----
if(nrow(word_freq) > 0) {
  
  set.seed(1234)
  
  wordcloud(
    words = word_freq$traditional_text,
    freq = word_freq$total_count,   # <-- USE THIS COLUMN
    min.freq = 1,
    max.words = 100,
    random.order = FALSE,
    scale = c(5, 0.7),              # make size differences visible
    colors = brewer.pal(3, "Dark2")
  )
  
  cat("Word Cloud generated for: traditional_common_words_combined.csv\n")
  
} else {
  cat("No data available in traditional_common_words_combined.csv\n")
}
