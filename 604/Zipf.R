# 1. Load Required Libraries
library(dplyr)
library(ggplot2)
library(readr) # Recommended for reading CSV files

# --- Data Preparation ---

# 2. Read the CSV file
# Ensure the file name is correct.
word_frequency_data <- read.csv("Final_project_data/traditional_common_words_combined.csv")

# 3. Sort by frequency and assign ranks
zipf_data_ranked <- word_frequency_data %>%
  arrange(desc(total_count)) %>%
  mutate(rank = row_number())

# Optional: Print the top 15 ranked words to confirm the data structure
print(head(zipf_data_ranked, 15))

# --- Plot 1: Linear Scale (As Requested) ---

ggplot(zipf_data_ranked, aes(x = rank, y = total_count)) +
  geom_line(color = "steelblue") +
  geom_point(color = "darkorange", size = 1.5) +
  geom_text(
    # Label the top 8 words
    aes(label = ifelse(rank <= 5, traditional_text, "")),
    vjust = -0.8,
    size = 3.5,
    check_overlap = TRUE # Prevents overlapping labels
  ) +
  labs(
    title = "Zipf’s Law: Word Rank vs Frequency (Linear Scale)",
    subtitle = "A steep curve is characteristic of the Zipf distribution on linear axes.",
    x = "Rank of Word",
    y = "Frequency (Total Count)"
  ) +
  theme_minimal(base_size = 13)

