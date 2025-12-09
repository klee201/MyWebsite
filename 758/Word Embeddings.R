# ================================
# 0. Load libraries
# ================================
library(word2vec)
library(text2vec)

# ================================
# 1. Read CSV
# ================================
df <- read.csv("Final_project_data/traditional_common_words_combined.csv", stringsAsFactors = FALSE)

# Example structure:
# traditional_text total_count
# "為什麼"         254
# "不知道"         172

# ================================
# 2. Create pseudo-document
# ================================
# Repeat words according to their frequency
pseudo_doc <- rep(df$traditional_text, df$total_count)

# Collapse into a single string (space-separated)
text_doc <- paste(pseudo_doc, collapse = " ")

# Write to a temporary text file
temp_file <- tempfile(fileext = ".txt")
writeLines(text_doc, temp_file)

# ================================
# 3. Train Word2Vec model
# ================================
# Using skip-gram, dimension=50, window=5, 50 iterations
word2vec_model <- word2vec(temp_file,
                           type = "skip-gram",
                           dim = 50,
                           window = 5,
                           iter = 50,
                           min_count = 1)

# ================================
# 4. Convert Word2Vec model to matrix
# ================================
word2vec_vectors <- as.matrix(word2vec_model)

# ================================
# 5. Inspect word vectors
# ================================
head(word2vec_vectors[, 1:6])  # first 6 dimensions of first few words

# ================================
# 6. Example: cosine similarity
# ================================
library(text2vec)  # for sim2()

# Pick a keyword
keyword <- "為什麼"
keyword_vector <- word2vec_vectors[keyword, , drop = FALSE]

# Compute cosine similarity between keyword and all words
cos_sim <- sim2(x = word2vec_vectors, y = keyword_vector,
                method = "cosine", norm = "l2")

# Sort top 10 most similar words
top_similar <- sort(cos_sim[,1], decreasing = TRUE)[1:10]
top_similar


# Compute embeddings & top similar words
# Save results
#write.csv(top_similar, "Final_project_data/top_similar_words.csv", row.names = TRUE)
