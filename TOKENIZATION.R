library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(tidytext)

folder <- "Final_project_data"
csv_files <- list.files(folder, pattern = "_cleaned\\.csv$", full.names = TRUE)

for (file in csv_files) {
  #message("Tokenizing file: ", basename(file))
  
  simp_text_clean <- read_csv(file, show_col_types = FALSE)
  
  # Make sure the column exists
  if(!"text" %in% colnames(simp_text_clean)){
    stop("The CSV file ", basename(file), " does not contain a 'text' column!")
  }
  
  # Tokenize safely
  simp_tokens <- simp_text_clean %>%
    filter(!is.na(.data[["text"]])) %>%
    unnest_tokens(word, text)
  
  data("stop_words")
  
  simp_tokens_clean <- simp_tokens %>%
    anti_join(stop_words, by = "word") %>%
    filter(!str_detect(word, "^[0-9]+$")) %>%
    filter(nchar(word) > 2)
  
  # Optional: save tokenized file
  token_name <- gsub("_cleaned\\.csv$", "_tokens.csv", basename(file))
  token_path <- file.path(folder, token_name)
  write_csv(simp_tokens_clean, token_path)
  
  #message("Tokens saved as: ", token_name, "\n")
}

cat("Tokenization complete!\n")
