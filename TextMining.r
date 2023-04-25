#install tm package
install.packages('tm')
library(tm)

#Create Corpus
docs <- Corpus(DirSource('\\Users\\bibhu\\OneDrive\\Desktop\\Text-Mining-In-R\\TextMining'))
inspect(docs)

#inspect particular document
writeLines(as.character(docs[[2]]))

#preprocessing
#contains text with punctuation that are not separated by spaces for eg. 55-year-old
#content_transformer allows to create a custom function

toSpace <- content_transformer(function(x,pattern){return(gsub(pattern," ",x))})

docs <- tm_map(docs, toSpace, "-")
docs <- tm_map(docs, toSpace, ":")
docs <- tm_map(docs, toSpace, "'")
docs <- tm_map(docs, toSpace, " -")
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "//")

#remove punctuation
docs <- tm_map(docs, removePunctuation)

#transform to lower case
docs<- tm_map(docs, content_transformer(tolower))

#strip digits
docs <- tm_map(docs, removeNumbers)

#remove stopwords from standard stopword list
docs <- tm_map(docs, removeWords, stopwords("english"))

#strip whitespace
docs <- tm_map(docs, stripWhitespace)

#inspect output
writeLines(as.character(docs[[1]]))

#snowball library for stemming
install.packages('SnowballC')
library(SnowballC)

#stem document
docs <- tm_map(docs,stemDocument)

#few clean up
docs <- tm_map(docs, content_transformer(gsub), pattern = "lazi", replacement = "lazy" )
docs <- tm_map(docs, content_transformer(gsub), pattern = "intellig", replacement = "intelligent" )
docs <- tm_map(docs, content_transformer(gsub), pattern = "famili", replacement = "family" )
docs <- tm_map(docs, content_transformer(gsub), pattern = "purpos", replacement = "purpose" )
docs <- tm_map(docs, content_transformer(gsub), pattern = "purpos", replacement = "purpose" )

#Create document-term matrix
dtm <- DocumentTermMatrix(docs)

#inspect segment of document term matrix
inspect(dtm[1:5,50:53])

#total frequency of each word occuring across the documents
freq <- colSums(as.matrix(dtm))

#number of unique words
length(freq)

#sort
ord <- order(freq,decreasing=TRUE)

#most frequently occuring terms
freq[head(ord)]

#least frequently occurring terms
freq[tail(ord)]

#filter out frequently and infrequently occurring words (they are mostly descriptive of particular document)
dtmr <- DocumentTermMatrix(docs, control = list(wordLengths = c(4,20),bounds = list(global = c(2,5))))

freqr <- colSums(as.matrix(dtmr))

length(freqr)

#sort in ascending order
ordr <- order(freqr, decreasing = TRUE)

#most frequently occurring terms
freq[head(ordr)]

#least frequently occurring terms
freq[tail(ordr)]

#list most frequent terms
findFreqTerms(dtmr, lowfreq = 10)

#correlations
findAssocs(dtmr, "forest", 0.6)
findAssocs(dtmr, "year", 0.6)

#histogram
wf = data.frame(term=names(freqr), occurrences=freqr)
library(ggplot2)
p <- ggplot(subset(wf, freqr>3),aes(term,occurrences))
p <- p + geom_bar(stat="identity")
p <- p + theme(axis.text.x = element_text(angle = 45, hjust = 1))
p

#wordcloud
install.packages('wordcloud')
library(wordcloud)

#for consistent look across clouds
set.seed(46)

#limit words by specifying min frequency
wordcloud(names(freqr), freqr, min.freq = 50, random.order = FALSE, colors = brewer.pal(8, "Dark2"), scale = c(4, 0.5), rot.per = 0.35, max.words = 200, min.words = 5, use.r.layout = FALSE, margin = c(2, 2, 2, 2))
