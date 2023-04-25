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

length(freq)

#sort
ord <- order(freq.decreasing=TRUE)

#most frequently occuring terms
freq[head(ord)]

#least frequently occuring terms
freq[tail(ord)]

