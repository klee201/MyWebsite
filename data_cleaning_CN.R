

library(stringr)

# Define the folder path
folder_path <- "Final_project_data"

# List all CSV files in the folder
files <- list.files(folder_path, pattern = "\\.csv$", full.names = TRUE)

# Loop through each file
for (file in files) {
  #message("Cleaning file: ", basename(file))
  # Read the CSV
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Keep only the necessary columns (if they exist)
  data <- data %>%
    select(any_of(c("text", "likeCount", "reply")))
  
  # Clean the text column
  data <- data %>%
    mutate(
      text = str_squish(text),  # remove extra spaces
      
      # --- Remove usernames or replies like "@小明", "回覆 @小美", etc. ---
      text = str_replace_all(text, "@[A-Za-z0-9_\\u4e00-\\u9fa5]+", ""),  # remove @username
      text = str_replace_all(text, "回覆\\s*@?[A-Za-z0-9_\\u4e00-\\u9fa5]+", ""),  # remove "回覆 小明" or "回覆@小明"
      
      # --- Remove URLs ---
      text = str_replace_all(text, "https?://[A-Za-z0-9./_-]+", ""),
      
      # remove emojis and special symbols, but keep Chinese, English, numbers, punctuation
      text = str_replace_all(text, "[^\u4e00-\u9fa5a-zA-Z0-9\\p{P}\\p{Z}]", ""),
      
      text = str_trim(text)  # trim leading/trailing spaces
    )
  
  # Create clean file name
  clean_name <- gsub("\\.csv$", "_cleaned.csv", basename(file))
  clean_path <- file.path(folder_path, clean_name)
  
  # Save cleaned version as UTF-8
  write.csv(data, clean_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  
  
  
}

cat("data cleaning complete!.\n")