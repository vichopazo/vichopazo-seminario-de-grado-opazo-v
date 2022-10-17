#Library packages

library(pacman)
library(topicmodels)
library(quanteda)
library(readtext)
library(tm)
library(wordcloud)
library(ggplot2)

#Load dataset and readtext, UNGDC_Chile_1971_2022


url <- "https://www.researchgate.net/profile/Vicente_Opazo/publication/363844089_UN_General_Debate_Corpus_Chile_1971-2022/data/634ca5809cb4fe44f32f306d/UNGDC-Chile-1971-2022.zip"

download.file(url, "UNGC_Chile.zip")
unzip("UNGC_Chile.zip")
ungd_chl <- readtext("UNGDC_Chile_1971_2022", 
                     ignore_missing_files = FALSE, 
                     text_field = NULL,
                     docid_field = NULL, 
                     docvarsfrom = "filenames", 
                     dvsep = "_", 
                     docvarnames = c("country", "session", "year"),  
                     encoding = "UTF-8")


#Pre-processing (creating corpus and document term matrix, removing punctuation, least frequent words and stopwords)

corp <- corpus(ungd_chl)

dfm = dfm(corp)
dfm = dfm(corp, remove_punct=T, remove=stopwords("english"))
dfm = dfm_trim(dfm, min_docfreq = 5)
dtm = convert(dfm, to = "topicmodels")

#Setting and running LDA, k = 7, seed = 1 (to secure reproducibility)

set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 7, control = list(alpha = 0.1))

#Top terms per topic overview and potential "environmental issues" topic review 

terms(m, 10)
topic = 1
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing=T), n = 30)
wordcloud(names(topwords), topwords, colors = c("black", "darkgreen", "turquoise2"))

#Top documents per topic, topic 1

topic_1.docs = posterior(m)$topics[, topic]
topic_1.docs = sort(topic_1.docs, decreasing=T)
head(topic_1.docs)

#Topic preferences per year (per topic scores)

docs = docvars(dfm)[match(rownames(dtm), docnames(dfm)),]
tpp = aggregate(posterior(m)$topics, by=docs["year"], mean) 
rownames(tpp) = tpp$year
heatmap(as.matrix(tpp[-1]))

#Other topics review

topic = 2 #potential "Promotion of peaceful, co-operative and rules-based international relations"
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing=T), n = 30)
wordcloud(names(topwords), topwords, colors = c("yellow1", "yellow3", "yellow4"))

topic = 3 #potential "Chilean internal affairs & Chilean government foreign policy"
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing=T), n = 30)
wordcloud(names(topwords), topwords, colors = c("yellow1", "yellow3", "yellow4"))

topic = 4 #potential "Promotion of democracy and freedom as universal values"
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing=T), n = 30)
wordcloud(names(topwords), topwords, colors = c("yellow1", "yellow3", "yellow4"))

topic = 5 #potential "References to UN system and structures"
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing=T), n = 30)
wordcloud(names(topwords), topwords, colors = c("yellow1", "yellow3", "yellow4"))

topic = 6 #potential "Calls for a global outlook on development, security, democracy and human rights"
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing=T), n = 30)
wordcloud(names(topwords), topwords, colors = c("grey70", "grey50", "grey20")) #800*751

topic = 7 #potential "Standard references to international relations"
words = posterior(m)$terms[topic, ]
topwords = head(sort(words, decreasing=T), n = 30)
wordcloud(names(topwords), topwords, colors = c("yellow1", "yellow3", "yellow4"))



