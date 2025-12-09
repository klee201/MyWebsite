#library(dplyr)
library(readr)
library(purrr)

# ---- Define folder with tokenized CSV files ----
folder <- "Final_project_data"
token_files <- list.files(folder, pattern = "_tokens\\.csv$", full.names = TRUE)

# ---- Read each CSV and compute word frequency ----
word_list <- map(token_files, function(file) {
  df <- read_csv(file, show_col_types = FALSE)
  
  # Optional: ensure all tokens are Chinese words
  df <- df %>% filter(nchar(word) > 0)
  
  # Compute word frequency for this file
  freq_df <- df %>%
    dplyr::count(word, sort = TRUE) %>%
    mutate(file = basename(file))
  
  return(freq_df)
})

# ---- Combine all files into one data frame ----
all_words <- bind_rows(word_list)

# ---- Find words that appear in multiple files and sum frequencies ----
common_words <- all_words %>%
  group_by(word) %>%
  summarise(
    total_count = sum(n),                 # total frequency across all files
    #file_count = n_distinct(file),        # number of files it appears in
    #files = paste(unique(file), collapse = ", ")
  ) %>%
  #filter(file_count > 1) %>%             # only keep words appearing in more than 1 file
  arrange(desc(total_count))             # sort by total frequency

# ---- Save to CSV ----
output_file <- file.path(folder, "common_words_across_files.csv")
write_csv(common_words, output_file)

cat("Common words saved to:", output_file, "\n")