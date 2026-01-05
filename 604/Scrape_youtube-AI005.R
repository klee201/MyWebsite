library(httr2)
library(jsonlite)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(readr)


key <- "AIzaSyB8ggZO52-y0JHNktxWYwZNmqia-rMOpoA"
videoid <- "e85AxYW0Qyk"
include_replies <- TRUE


# ---- HELPERS ----
yt_get <- function(endpoint, query) {
  req <- request(paste0("https://www.googleapis.com/youtube/v3/", endpoint)) |>
    req_url_query(!!!query, key = key) |>
    req_user_agent("R/httr2 YouTube comments scraper")
  resp <- req_perform(req)
  resp_body_json(resp, simplifyVector = FALSE)
}


## Parse a comment snippet (either top-level or a reply) into a 1-row tibble
parse_comment_snippet <- function(snippet, parentId = NA_character_) {
  tibble(
    videoId      = snippet$videoId %||% NA_character_,
    commentId    = snippet$commentId %||% NA_character_, # replies API fills this; for top-level we’ll overwrite below
    parentId     = parentId,
    author       = snippet$authorDisplayName %||% NA_character_,
    text         = snippet$textOriginal    %||% snippet$textDisplay %||% NA_character_,
    likeCount    = snippet$likeCount %||% 0,
    publishedAt  = snippet$publishedAt %||% NA_character_,
    updatedAt    = snippet$updatedAt %||% NA_character_,
    viewerRating = snippet$viewerRating %||% NA_character_,
    canRate      = snippet$canRate %||% NA
  )
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# ---- 1) Fetch TOP-LEVEL comments (commentThreads.list) with pagination ----
fetch_top_level_comments <- function(video_id) {
  out <- list()
  token <- NULL
  
  repeat {
    dat <- yt_get(
      "commentThreads",
      query = list(
        part       = "snippet,replies",
        videoId    = video_id,
        maxResults = 100,
        pageToken  = token,
        textFormat = "plainText",
        order      = "time"
      )
    )
    
    items <- dat$items %||% list()
    
    # Each item is a thread; top-level comment is at $snippet$topLevelComment$snippet
    top_tbl <- map_dfr(items, function(it) {
      tlc <- it$snippet$topLevelComment
      s   <- tlc$snippet
      row <- parse_comment_snippet(s, parentId = NA_character_) |>
        mutate(
          videoId   = video_id,
          commentId = tlc$id %||% NA_character_,   # set proper id for top-level
          reply     = FALSE
        )
      row
    })
    
    # Also capture any replies that are bundled with the thread (YouTube only returns a few here)
    replies_tbl <- map_dfr(items, function(it) {
      reps <- it$replies$comments %||% list()
      if (length(reps) == 0) return(tibble())
      map_dfr(reps, function(rc) {
        s <- rc$snippet
        parse_comment_snippet(s, parentId = s$parentId %||% NA_character_) |>
          mutate(
            videoId   = video_id,
            commentId = rc$id %||% NA_character_,
            reply     = TRUE
          )
      })
    })
    
    out <- append(out, list(bind_rows(top_tbl, replies_tbl)))
    
    token <- dat$nextPageToken %||% NULL
    if (is.null(token)) break
  }
  
  bind_rows(out)
}

# ---- 2) (Optional) For FULL replies, call comments.list for each parentId with pagination ----
fetch_all_replies_for_parents <- function(parent_ids) {
  parent_ids <- unique(na.omit(parent_ids))
  if (length(parent_ids) == 0) return(tibble())
  
  map_dfr(parent_ids, function(pid) {
    token <- NULL
    out <- list()
    repeat {
      dat <- yt_get(
        "comments",
        query = list(
          part       = "snippet",
          parentId   = pid,
          maxResults = 100,
          pageToken  = token,
          textFormat = "plainText"
        )
      )
      items <- dat$items %||% list()
      tbl <- map_dfr(items, function(it) {
        s <- it$snippet
        parse_comment_snippet(s, parentId = s$parentId %||% pid) |>
          mutate(
            videoId   = s$videoId %||% NA_character_,
            commentId = it$id %||% NA_character_,
            reply     = TRUE
          )
      })
      out <- append(out, list(tbl))
      token <- dat$nextPageToken %||% NULL
      if (is.null(token)) break
    }
    bind_rows(out)
  })
}


# ---- RUN ----
message("Fetching top-level comments (and bundled replies)…")
threads <- fetch_top_level_comments(videoid)

if (include_replies) {
  message("Fetching full reply threads (this can take more quota/time)…")
  # Parent IDs are the top-level comment IDs
  parent_ids <- threads |>
    filter(!reply) |>
    pull(commentId) |>
    unique()
  
  full_replies <- fetch_all_replies_for_parents(parent_ids)
  
  comments <- bind_rows(
    threads |> filter(!reply),               # top-level
    full_replies                             # complete replies
  ) |>
    distinct(commentId, .keep_all = TRUE) |>
    arrange(publishedAt)
} else {
  comments <- threads |>
    arrange(publishedAt)
}

# ---- SAVE ----
# Clean text a touch (optional)
comments <- comments |>
  mutate(text = str_squish(text))


write.csv(comments, file = "Final_project_data/AI005_comments.csv", row.names = FALSE)




