library(tm) # Framework for text mining.
library(qdap) # Quantitative discourse analysis of transcripts.
library(qdapDictionaries)
library(dplyr) # Data wrangling, pipe operator %>%().
library(RColorBrewer) # Generate palette of colours for plots.
library(ggplot2) # Plot word frequencies.
library(scales) # Include commas in numbers.
library(Rgraphviz) # Correlation plots.

#The corpus contains a novel titles Treasure Island by R.L. Stevenson
cname <- file.path(".", "Text Mining", "Documents")
cname

#Checking the input
length(dir(cname))
dir(cname)

#Storing the document 
docs <- Corpus(DirSource(cname))
docs

#Exploring the data
inspect(docs[1])

#Text preprocessing
#Converting to lower cases
docs <- tm_map(docs, content_transformer(tolower))

#Removing number
docs <- tm_map(docs, removeNumbers)

#Removing punctuations
docs <- tm_map(docs, removePunctuation)

#Removing English stop words
docs <- tm_map(docs, removeWords, stopwords("english"))

#English stop words
length(stopwords("english"))
stopwords("english")


#Creating a Document Term Matrix
dtm <- DocumentTermMatrix(docs)
dtm

inspect(dtm)

class(dtm)

dim(dtm)

tdm <- TermDocumentMatrix(docs)
tdm

#Exploring frequency term document
freq <- colSums(as.matrix(dtm))
length(freq)

#Least frequent terms
ord <- order(freq)
freq[head(ord)]

# Most frequent terms.
freq[tail(ord)]

#Identifying Frequent Items 
findFreqTerms(dtm, lowfreq=100)

#Word frequency
freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 10)

wf <- data.frame(word=names(freq), freq=freq)
head(wf)

#Word cloud
library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=25)

#Word cloud with colors and fewer terms
wordcloud(names(freq), freq, min.freq=50, colors=brewer.pal(6, "Dark2"))




