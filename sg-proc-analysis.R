#Cargar paquetes

library(pacman)
pacman::p_load(ggplot2)
library(topicmodels)
library(quanteda)
library(readtext)
library(tm)
library(wordcloud)

#Load dataset and readtext, UNGDC_Chile_1970_2022


url <- "https://www.researchgate.net/profile/Vicente_Opazo/publication/363844089_UN_General_Debate_Corpus_Chile_1970-2022/data/63313d086063772afd92b766/UNGDC-Chile-1970-2022.zip"

download.file(url, "UNGC_Chile.zip")
unzip("UNGC_Chile.zip")
ungd_chl <- readtext("UNGDC_Chile_1970_2022", 
                     ignore_missing_files = FALSE, 
                     text_field = NULL,
                     docid_field = NULL, 
                     docvarsfrom = "filenames", 
                     dvsep = "_", 
                     docvarnames = c("country", "session", "year"),  
                     encoding = "UTF-8")


#Pre-processing (creating corpus and document term matrix)

corp <- corpus(ungd_chl)

ungdcorp_chl = corpus_reshape(corp, to = "paragraphs")

dfm = dfm(ungdcorp_chl, remove_punct=T, remove=stopwords("english"))
dfm = dfm_trim(dfm, min_docfreq = 5)
dtm = convert(dfm, to = "topicmodels")

#Setting and running LDA, k = 20, ver https://www.un.org/sg/en/content/sg/secretary-generals-speeches

set.seed(1)
m = LDA(dtm, method = "Gibbs", k = 20, control = list(alpha = 0.1))

topic_5 = 5
words_5 = posterior(m)$terms[topic_5, ]
topwords_5 = head(sort(words_5, decreasing=T), n = 30)

#Wordcloud, topic_6

wordcloud(names(topwords_5), topwords_5)
