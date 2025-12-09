# Detach plyr if loaded (avoid conflict with dplyr)
#if("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)

library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(quanteda)
library(tidytext)


# Count the frequency of each word
#word_frequency <- simp_tokens_clean %>%
#  dplyr::count(word, sort = TRUE) %>%
#  mutate(rank = row_number())


# View the top 20 most frequent words
#head(word_frequency, 20)

#-------------------if it work delete the upon -----------------

# ---- Load Packages ----
# Detach plyr if loaded (avoid conflict with dplyr)
if("package:plyr" %in% search()) detach("package:plyr", unload = TRUE)

library(dplyr)
library(stringr)
library(readr)
library(purrr)
library(quanteda)
library(tidytext)

# ---- Define Folder ----
folder <- "Final_project_data"  # your token files folder

# ---- Find all tokenized CSV files ----
token_files <- list.files(folder, pattern = "_tokens\\.csv$", full.names = TRUE)

# ---- Process Each Token File ----
for (file in token_files) {
  #message("Processing file: ", basename(file))
  
  # Read tokenized data
  simp_tokens_clean <- read_csv(file, show_col_types = FALSE)
  
  # ---- Word Frequency Calculation ----
  # Compute word frequency using simp_tokens_clean
  word_frequency <- simp_tokens_clean %>%
    dplyr::count(word, sort = TRUE) %>%
    dplyr::mutate(rank = dplyr::row_number())
  
  # Preview top 20 most frequent words (optional)
  #print(head(word_frequency, 20))
  
  # ---- Save Word Frequency Output ----
  freq_name <- gsub("_tokens\\.csv$", "_wordfreq.csv", basename(file))
  freq_path <- file.path(folder, freq_name)
  
  write_csv(word_frequency, freq_path)
  
  #message("Word frequency saved as: ", freq_name, "\n")
}

cat("Word Frequency Calculation Complete!\n")
