library(tm)
library(tmcn)
library(Matrix)
library(wordcloud)

# segmentation, Term Document Matrix
docs <- readLines("structuralism.txt")
docs <- gsub("\\[[0-9]+\\]", "", docs)
docs.corpus <- Corpus(VectorSource(docs))
docs.seg <- tm_map(docs.corpus, segmentCN)
docs.tdm <- TermDocumentMatrix(docs.seg, control = list())
inspect(docs.tdm)

# TFIDF counting
docs.tf <- apply(as.matrix(docs.tdm), 2, function(doc) {doc / sum(doc)})
idf.function <- function(word_doc) { log2( (length(word_doc)+1) / nnzero(word_doc) ) }
docs.idf <- apply(docs.tdm, 1, idf.function)
docs.tfidf <- docs.tf * docs.idf

# query of words
query.tfidf <- function(q){
  q.position <- which(rownames(docs.tfidf) %in% q)
  q.tfidf <- docs.tfidf[q.position, ]
  return (q.tfidf)
}
query.tfidf(c("結構", "人類學", "整體"))

# cosine similiarity
cos <- function(x, y){
  return (x %*% y / sqrt(x %*% x * y %*% y))[1, 1]
}
# compare with first doc
docs.cos.sim <- apply(docs.tfidf, 2, cos, y = docs.tfidf[, 1])
docs.cos.sim

# wordcloud
f <- sort(rowSums(docs.tfidf), decreasing = T)
docs.df <- data.frame(
  word = names(f),
  freq = f
)
wordcloud(docs.df$word, docs.df$freq, scale=c(5,0.1), colors=brewer.pal(8, "Dark2"))
