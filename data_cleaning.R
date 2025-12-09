

# List all CSV files in the folder
files <- list.files("Final_project_data", pattern = "\\.csv$", full.names = TRUE)

for (file in files) {
  # Read the CSV
  data <- read.csv(file, stringsAsFactors = FALSE)
  
  # Keep only the necessary columns (if they exist)
  data <- data %>%
    select(any_of(c("text", "likeCount", "reply")))
  
  # Clean the text column
  data <- data %>%
    mutate(
      text = str_squish(text),                           # remove extra spaces
      # remove emojis and unusual symbols but keep all Chinese characters
      text = str_replace_all(text, "[\\p{So}\\p{Cn}]", ""), 
      text = str_trim(text)                              
      # trim leading/trailing spaces
    )
  
  # Create clean file name
  clean_name <- gsub("\\.csv$", "_cleaned.csv", basename(file))
  clean_path <- file.path("Final_project_data", clean_name)
  
  # Save cleaned version as UTF-8
  write.csv(data, clean_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  
  
  
}

cat("data cleaning complete!.\n")