#Translate Simplified Chinese into Tradition Chinese and Combine them together
#Also remove the non-related English from the CSV file
#Remove the special characters
#Included lemmatization or stemming, such as, girl/girls and woman/women

# install.packages("textstem")
# Install the tmcn package
#install.packages("tmcn")
#install.packages("dplyr") # For data manipulation (if you use pipes)

# Load the necessary libraries
library(textstem)
library(tmcn)
library(dplyr)

# 1. Read the CSV file into an R data frame with correct encoding
folder <- "Final_project_data"
common_file <- file.path(folder, "common_words_across_files.csv")
df <- read.csv(common_file, encoding = "UTF-8", stringsAsFactors = FALSE)
#common_words <- read_csv(common_file, show_col_types = FALSE)
# Display the original data (optional)
head(df)

# 2. Translate the Simplified Chinese column to Traditional Chinese
# The function toTrad() is used for Simplified to Traditional conversion
# The default 'rev = FALSE' means simplified to traditional

df_translated <- df %>%
  mutate(
    traditional_text = toTrad(word)
  )


# Keep only 'total_count' and 'traditional_text'
df_final <- df_translated %>%
  select(total_count, traditional_text)

# Combine rows with the same traditional_text and sum the counts
df_combined <- df_translated %>%
  group_by(traditional_text) %>%
  summarise(total_count = sum(total_count, na.rm = TRUE)) %>%
  arrange(desc(total_count))  # Optional: sort by count



words <- c("girls", "women", "simping", "lovers")
lemmatized <- lemmatize_words(words)
print(lemmatized)


# Define the whitelist words to keep and their normalized form
whitelist_map <- c(
  "pua" = "pua",
  "girl" = "girl",
  "girls" = "girl",      # normalize to "girl"
  "atm" = "atm",
  "money" = "money",
  "simp" = "simp",
  "woman" = "woman",
  "women" = "woman",     # normalize to "woman"
  "lover" = "lover",
  "feminist" = "feminist"
)

# Create regex pattern for whitelist words (case-insensitive)
pattern_whitelist <- paste0("\\b(", paste(names(whitelist_map), collapse = "|"), ")\\b")

# Clean the traditional_text
df_cleaned <- df_translated %>%
  # Remove digits
  mutate(traditional_text = gsub("[0-9]", "", traditional_text)) %>%
  # Remove punctuation (.,) and spaces
  mutate(traditional_text = gsub("[.,\\s_，’']", "", traditional_text)) %>%
  
  # Remove English letters not in whitelist
  rowwise() %>%
  mutate(traditional_text = {
    # Extract whitelist words if present
    keep_words <- regmatches(traditional_text, gregexpr(pattern_whitelist, traditional_text, ignore.case = TRUE))[[1]]
    # Normalize whitelist words
    if(length(keep_words) > 0){
      keep_words <- lemmatize_words(keep_words)
    }
    # Remove all other letters
    cleaned <- gsub("[A-Za-z]", "", traditional_text)
    # Combine cleaned Chinese text with whitelist English words
    paste0(cleaned, paste(keep_words, collapse = ""))
  }) %>%
  ungroup() %>%
  # Remove empty traditional_text rows
  filter(traditional_text != "") %>%
  # Combine duplicates
  group_by(traditional_text) %>%
  summarise(total_count = sum(total_count, na.rm = TRUE)) %>%
  arrange(desc(total_count))




# 3. Write the translated data frame back to a new CSV file
# Write the CSV without the first column
write.csv(df_cleaned, 
          file.path(folder, "traditional_common_words_combined.csv"), 
          row.names = FALSE,      # <- prevents adding the first column
          fileEncoding = "UTF-8")

cat("Translation complete! Output saved to 'your_output_file.csv'\n")

