
# ============================================================================
# MASSIVE AUTISM EMPLOYMENT PROGRAM CRAWLER (3000+ URLs, Depth 5, Robust NLP)
# ============================================================================
# Features:
# - Crawls federal, state, local, nonprofit, and corporate pages (3000+ URLs)
# - Resilient to timeouts and skips failed links
# - Depth-5 web crawling with breadth-first queue (not limited to seed URLs)
# - Extracts and saves .txt files for each page
# - Builds CSV index for all successful crawls
# - Applies NLP: summarization, sentiment analysis, topic modeling
# - Produces a JAMA-style report as Word file
# Output location: C:/data/AutismEmployment/
# ============================================================================

# --- 1. Libraries ---
packages <- c("rvest", "httr", "xml2", "stringr", "dplyr", "tidyr", "tidytext", 
              "topicmodels", "text2vec", "NLP", "sentimentr", "textrank", 
              "officer", "magrittr", "ggplot2", "tokenizers", "readr", "urltools")
for(pkg in packages){
  if(!require(pkg, character.only = TRUE)) install.packages(pkg, dependencies=TRUE)
  library(pkg, character.only = TRUE)
}

# --- 2. Output Setup ---
output_dir <- "C:/data/AutismEmployment"
txt_dir <- file.path(output_dir, "pages")
if(!dir.exists(txt_dir)) dir.create(txt_dir, recursive=TRUE)
index_file <- file.path(output_dir, "index.csv")
doc_file <- file.path(output_dir, "Autism_Employment_Report.docx")

# --- 3. Seed URLs ---
seed_urls <- c(
  "https://www.dol.gov/agencies/odep",
  "https://www.ssa.gov/work/",
  "https://www.hhs.gov/programs/disability/",
  "https://www.autismspeaks.org/",
  "https://www.mass.gov/orgs/massachusetts-rehabilitation-commission"
)

# Expand seed list with state-level vocational rehab URLs
state_urls <- paste0("https://", tolower(state.abb), ".gov")
seed_urls <- unique(c(seed_urls, state_urls))

# --- 4. Crawler Setup ---
visited <- character()
results <- list()
queue <- data.frame(url=seed_urls, depth=0, stringsAsFactors=FALSE)
keywords <- c("autism", "vocational rehab", "transition to work", "supported employment", "neurodiverse", "job coach")
max_depth <- 5
max_pages <- 3000
crawl_count <- 0

# --- 5. Crawl Loop ---
while(nrow(queue) > 0 && crawl_count < max_pages){
  row <- queue[1, ]; queue <- queue[-1, ]
  url <- row$url; depth <- row$depth
  if(url %in% visited) next
  visited <- c(visited, url)

  tryCatch({
    Sys.sleep(1.5)
    html <- read_html(httr::GET(url, timeout(8)))
    text <- html %>% html_elements("p, li") %>% html_text2() %>% paste(collapse="\n")
    title <- html %>% html_element("title") %>% html_text()
    if(!any(str_detect(text, regex(keywords, ignore_case=TRUE)))) next

    id <- paste0("page", crawl_count + 1)
    page_txt <- file.path(txt_dir, paste0(id, ".txt"))
    writeLines(text, con=page_txt)

    results[[id]] <- list(url=url, title=title, text=text)
    crawl_count <- crawl_count + 1

    # Queue next-level links
    if(depth < max_depth){
      links <- html %>% html_elements("a") %>% html_attr("href")
      links <- links[grepl("^https?://", links)]
      new_links <- setdiff(links, visited)
      if(length(new_links) > 0){
        queue <- rbind(queue, data.frame(url=head(new_links, 100), depth=depth+1))
      }
    }
  }, error=function(e) NULL)
}

# --- 6. Save CSV Index ---
index_df <- do.call(rbind, lapply(names(results), function(id){
  r <- results[[id]]
  data.frame(id=id, url=r$url, title=r$title, txt_file=file.path("pages", paste0(id, ".txt")), stringsAsFactors=FALSE)
}))
write_csv(index_df, index_file)

# --- 7. NLP: Summary + Sentiment ---
index_df$summary <- sapply(index_df$id, function(id){
  text <- results[[id]]$text
  sents <- tokenizers::tokenize_sentences(text)[[1]]
  paste(head(sents[nchar(sents)>40], 2), collapse=" ")
})

bing <- get_sentiments("bing")
index_df$sentiment <- sapply(index_df$id, function(id){
  words <- tibble(text=results[[id]]$text) %>% unnest_tokens(word, text)
  sc <- words %>% inner_join(bing, by="word") %>% count(sentiment) %>% spread(sentiment, n, fill=0)
  sc$positive - sc$negative
})

# --- 8. Topic Modeling (if enough pages) ---
topic_terms <- NULL
if(nrow(index_df) >= 5){
  docs <- index_df$id
  all_tokens <- tibble(id=rep(docs, each=1), text=sapply(docs, function(i) results[[i]]$text)) %>%
                unnest_tokens(word, text) %>%
                anti_join(get_stopwords())
  dtm <- all_tokens %>% count(id, word) %>% cast_dtm(id, word, n)
  lda <- LDA(dtm, k=5, control=list(seed=123))
  topic_terms <- tidy(lda, matrix="beta") %>% group_by(topic) %>% top_n(5, beta)
}

# --- 9. Generate Report ---
doc <- read_docx()
doc <- doc %>% body_add_par("Autism Employment Programs – Web Crawl Report", style="heading 1")
doc <- doc %>% body_add_par(paste("Generated:", Sys.Date()), style="Normal")
doc <- doc %>% body_add_par(paste("Pages crawled:", nrow(index_df)), style="Normal")

for(cat in index_df$id){
  ent <- index_df[index_df$id == cat, ]
  doc <- doc %>% body_add_par(ent$title, style="heading 2")
  doc <- doc %>% body_add_par(ent$summary, style="Normal")
  doc <- doc %>% body_add_par(paste("Sentiment Score:", ent$sentiment), style="Normal")
  doc <- doc %>% body_add_par(paste("Source:", ent$url), style="Normal")
}

if(!is.null(topic_terms)){
  doc <- doc %>% body_add_par("Top Terms by Topic", style="heading 2")
  for(t in unique(topic_terms$topic)){
    terms <- topic_terms %>% filter(topic == t)
    doc <- doc %>% body_add_par(paste("Topic", t, ":", paste(terms$term, collapse=", ")), style="Normal")
  }
}

print(doc, target=doc_file)
message("✔ Report written to ", doc_file)
