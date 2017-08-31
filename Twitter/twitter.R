library(twitteR)
library(RCurl)
library(wordcloud)
library(tm)
library(SnowballC)
library(ggplot2)

consumer_key <- ''
consumer_secret<- ''
acess_token <- ''
acess_secret <- ''

setup_twitter_oauth(consumer_key, consumer_secret, acess_token, acess_secret)

number_tweets <- 1500

#busca todos os tweets no periodo de tempo permitido e converte para data.frame
tweets <- twListToDF(searchTwitter('pec', number_tweets, since= '2016-09-17',geocode = '-22.90278,-43.2075,60km'))

#faz todos os tratamentos necessários para limpar os textos dos tweets
tweets$text <- lapply(tweets$text, function(x)gsub(" ?http(s?)://(.*)", '',x))
tweets$text <- lapply(tweets$text, removePunctuation)
tweets$text <- lapply(tweets$text, tolower)
tweets$text <- lapply(tweets$text, removeWords, stopwords("pt"))
tweets$text <- lapply(tweets$text, removeNumbers)
tweets$text <- lapply(tweets$text, stripWhitespace)

#stemming
tweets$text <- lapply(tweets$text, stemDocument, language  = "pt")

#wordcloud com cores e frequencia maior que 20
tweets_corpus <- Corpus(VectorSource(tweets$text))
wordcloud(tweets_corpus, min.freq = 20, colors = rainbow(50))

#DTM
tdm <- TermDocumentMatrix(tweets_corpus, control = list(wordLengths = c(1, Inf)))

term_freq <- rowSums(as.matrix(tdm))
term_freq <- subset(term_freq, term_freq >=40)
df <- data.frame(term = names(term_freq), freq = term_freq)

#maiores frequencias
df[order(-df$freq), ]

#histograma
ggplot(df, aes(x=term, y=freq)) + geom_bar(stat = "identity") + xlab("Termos") + ylab("frequência") +coord_flip()

#termos associados com o termo "população" segundo uma correlação de 60%
findAssocs(tdm, "população", 0.6)

# remove sparse terms
tdm2 <- removeSparseTerms(tdm, sparse = 0.95)
m2 <- as.matrix(tdm2)

# cluster terms
distMatrix <- dist(scale(m2))
fit <- hclust(distMatrix)

plot(fit)
rect.hclust(fit, k = 10) 
