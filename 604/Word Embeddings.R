# ================================
# 0. Load libraries
# ================================
library(word2vec)
library(text2vec)
library(igraph)
library(ggraph)
library(ggplot2)

# ================================
# 1. Read CSV
# ================================
df <- read.csv("Final_project_data/common_words_across_files.csv", stringsAsFactors = FALSE)

# Example structure:
#   word           total_count
# "people"         1784
# "sam"            940

# ================================
# 2. Create pseudo-document
# ================================
# Repeat words according to their frequency
pseudo_doc <- rep(df$word, df$total_count)

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
keyword <- "people"
keyword_vector <- word2vec_vectors[keyword, , drop = FALSE]

# Compute cosine similarity between keyword and all words
cos_sim <- sim2(x = word2vec_vectors, y = keyword_vector,
                method = "cosine", norm = "l2")

# Sort top 10 most similar words
top_similar <- sort(cos_sim[,1], decreasing = TRUE)[1:10]
top_similar


# Compute embeddings & top similar words
# Save results
write.csv(top_similar, "Final_project_data/top_similar_words.csv", row.names = TRUE)

#-------- if the bottom doesn't go well, smash-----------------

#create the network graph
# ==================================
# 7. Prepare Data for Network
# ==================================

# 7.1. Extract the names of the top 100 words
# 'top_similar' is a named vector where names are the words.
network_words <- names(top_similar)

# 7.2. Extract vectors for these specific words from the Word2Vec matrix
network_vectors <- word2vec_vectors[network_words, ]

# 7.3. Calculate the FULL pairwise cosine similarity matrix for these 100 words
# This is crucial: it measures how similar *each word* is to *every other word* in the list.
similarity_matrix <- sim2(x = network_vectors, y = network_vectors,
                          method = "cosine", norm = "l2")

# Set the diagonal to 0 (we don't want a word connecting to itself)
diag(similarity_matrix) <- 0

# ==================================
# 8. Create and Plot the Network
# ==================================

# 8.1. Set a Similarity Threshold
# Filter out weak connections. A value between 0.6 and 0.7 usually works well.
# Try 0.65 first. If the graph is too dense, increase it (e.g., to 0.7).
threshold <- 0.65 

# 8.2. Filter the Similarity Matrix
# Create a *new* matrix where any similarity below the threshold is set to 0.
# This is the correct way to filter before passing to igraph.
filtered_matrix <- similarity_matrix
filtered_matrix[filtered_matrix < threshold] <- 0

# 8.3. Create the igraph object
# igraph will automatically ignore the edges with a weight of 0.
net <- graph_from_adjacency_matrix(
  filtered_matrix,
  mode = "undirected",
  weighted = TRUE, # This uses the similarity score as the 'weight'
  diag = FALSE
)

# 8.4. Remove Isolated Nodes (Nodes with no connections)
# This step cleans up the graph by removing any word that was too dissimilar to all others.
net <- delete_vertices(net, degree(net) == 0)


# ==================================
# 9. Calculate Node Size and Plot the Network
# ==================================

# 9.1. Calculate Node Size 
# Use 'strength' (sum of connected similarity scores) to size the nodes.
V(net)$size <- strength(net, weights = E(net)$weight) * 0.1 

# 9.2. Plot the Network
library(ggraph)
library(ggplot2)

ggraph(net, layout = "fr") +
  # Draw edges (connections)
  geom_edge_fan(aes(alpha = weight), 
                show.legend = FALSE, 
                edge_width = 0.5, 
                color = "gray60") +
  # Draw nodes (words)
  geom_node_point(aes(size = size), color = "steelblue", alpha = 0.8) +
  # Label nodes (repel ensures labels don't overlap much)
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  
  labs(
    title = paste("Semantic Network of Words Similar to 'people'"),
    subtitle = paste("Connections are Cosine Similarity >", threshold)
  ) +
  theme_graph()

