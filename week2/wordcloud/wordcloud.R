library(NLP)
library(tm)
library(jiebaRD)
library(jiebaR)
library(RColorBrewer)
library(wordcloud)

wk <- worker()

text <- readLines("speech.txt", encoding = "UTF-8")

stopwords <- c("的", "與", "在", "和", "會", "是", "我", "都", "來", "要", "們", "一", "第")
stopwords.pattern <- paste0(stopwords, sep = "|", collapse = "") %>%
  substr(1, nchar(.) -1)

text <- gsub(stopwords.pattern, " ", text)

seg <- wk[text]

seg <- as.data.frame(table(seg))
freq <- seg[order(seg$Freq, decreasing = T), ]

wordcloud(freq$seg, freq$Freq,
          min.freq = 2, random.order = F,
          color = brewer.pal(8, "Dark2"))
