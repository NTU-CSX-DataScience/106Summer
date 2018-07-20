library(tm)
library(tmcn)
library(jiebaRD)
library(jiebaR)
library(devtools)
install_github("ggbiplot", "vqv") #下載一次就好了
library(ggbiplot)
library(factoextra)

# INPUT DATA
d.corpus <- Corpus( DirSource("./DATA") )
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
d.corpus <- tm_map(d.corpus, function(word) {
  gsub("[A-Za-z0-9]", "", word)
})

# TDM
mixseg = worker()
jieba_tokenizer = function(d)
{
  unlist( segment(d[[1]], mixseg) )
}
seg = lapply(d.corpus, jieba_tokenizer)

count_token = function(d)
{
  as.data.frame(table(d))
}
tokens = lapply(seg, count_token)

n = length(seg)
TDM = tokens[[1]]
colNames <- names(seg)
colNames <- gsub(".txt", "", colNames)
for( id in c(2:n) )
{
  TDM = merge(TDM, tokens[[id]], by="d", all = TRUE)
  names(TDM) = c('d', colNames[1:id])
}
TDM[is.na(TDM)] <- 0

# TF-IDF
tf <- apply(as.matrix(TDM[,2:(n+1)]), 2, sum)

library(Matrix)
idfCal <- function(word_doc)
{ 
  log2( (n+1) / nnzero(word_doc) ) 
}
idf <- apply(as.matrix(TDM[,2:(n+1)]), 1, idfCal)

doc.tfidf <- TDM
tempY = matrix(rep(c(as.matrix(tf)), each = length(idf)), nrow = length(idf))
tempX = matrix(rep(c(as.matrix(idf)), each = length(tf)), ncol = length(tf), byrow = TRUE)
doc.tfidf[,2:(n+1)] <- (doc.tfidf[,2:(n+1)] / tempY) * tempX

# 如果 idf 算法是用 n+1 當分子，就不會有 0 的 entry 出現
# stopLine = rowSums(doc.tfidf[,2:(n+1)])
# delID = which(stopLine == 0)

# PCA
t = as.data.frame(t(doc.tfidf))
t = t[-1,]
# t = apply(t[,1:10], 2, as.numeric)
t = apply(t, 2, as.numeric)
pcat = prcomp(t)
g <- ggbiplot(pcat, obs.scale = 1, var.scale = 1, ellipse = TRUE, circle = TRUE)
g
fviz_pca_ind(pcat)

# Kmeans
kmeansData = pcat$x[,1:2]
cl <- kmeans(kmeansData, 3)
plot(kmeansData, col = cl$cluster)
points(cl$centers, col = 1:2, pch = 8, cex = 2)