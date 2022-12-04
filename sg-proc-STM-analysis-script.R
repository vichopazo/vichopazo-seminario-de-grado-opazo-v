library(pacman)
pacman::p_load(readr, 
               stm,
               openxlsx, 
               readtext, 
               dplyr, 
               graphics)

#Load data

url <- "https://www.researchgate.net/profile/Vicente_Opazo/publication/365748138_United_Nations_General_Debate_Corpus_Chile_1971-2022/data/638139f0c2cb154d292784b2/UNGDC-Chile-1990-2022.zip"
download.file(url, "UNGC_Chile_1990_2022.zip")
unzip("UNGC_Chile_1990_2022.zip")

url2 <- "https://www.researchgate.net/profile/Vicente_Opazo/publication/365748138_United_Nations_General_Debate_Corpus_Chile_1971-2022/data/63813a027b0e356feb8296af/metadata-UNGDC-Chile-1990-2022.xlsx" 
download.file(url2, "metadata_UNGDC_Chile_1990_2022.xlsx", quiet = TRUE, mode = "wb")

#Data as dataset

documents <- readtext("UNGDC_Chile_1990_2022", 
                      ignore_missing_files = FALSE, 
                      text_field = NULL,
                      docid_field = NULL)

#Metadata as dataset
meta_ungdc_chile <- read.xlsx("metadata_UNGDC_Chile_1990_2022.xlsx", sheet = 1)
meta_ungdc_chile$gov_rating <- as.factor(meta_ungdc_chile$gov_rating)
meta_ungdc_chile <- meta_ungdc_chile %>% arrange(year)

#Data pre-processing
data_proc <- textProcessor(documents$text, metadata = meta_ungdc_chile)
output <- prepDocuments(data_proc$documents, data_proc$vocab, data_proc$meta, 
                        lower.thresh = 1, upper.thresh = 33)  
docs <- output$documents
vocab <- output$vocab
meta <-output$meta


#Exploring optimal K
findingk <- searchK(output$documents, output$vocab, K = c(10:30), 
                    prevalence =~ gov_rating + s(year), 
                    data = meta, verbose=TRUE, init.type = "Spectral", proportion = 0.8)
 plot(findingk) #optimal K=10:20


#Running stm + visualising results
STM <- stm(documents = output$documents, vocab = output$vocab,
           K = 12, prevalence =~ gov_rating + s(year),
           max.em.its = 75, data = output$meta, 
           init.type = "Spectral", verbose = FALSE) 
plot(STM)

#Exploring stm
labelTopics(STM, n = 7) #check probability (prob) + frequency & exclusivity (frex) 
findThoughts(STM, texts = documents$text,
             n = 4, topics = 6)
findThoughts(STM, texts = documents$text,
             n = 1, topics = 2)
findThoughts(STM, texts = documents$text,
             n = 4, topics = 11)
findThoughts(STM, texts = documents$text,
             n = 2, topics = 1)
set.seed(930)
cloud(STM, topic=6, max.words = 30)
cloud(STM, topic=2, max.words = 30)
cloud(STM, topic=11, max.words = 30)

#Covariates effect prediction
covars_effect <- estimateEffect(1:12 ~ gov_rating + s(year), STM, meta = output$meta, 
uncertainty = "Local", documents = output$documents)

plot(covars_effect, covariate = "gov_rating", topics = c(6,2,11,1), model = STM, 
     method = "difference", 
     cov.value1 = "Centre-to-right", cov.value2 = "Centre-to-left", 
     xlab = "More left-leaning ... More right-leaning", 
     main = "Effect of political position (Left/Right axis)", xlim = c(-1,1), width = 15, 
     labeltype = "custom", custom.labels = c("T6. UNSC role in global gov.", 
     "T2. UN devt. goals", 
     "T11. Environmental issues", 
     "T1. Chile-Latam relations & Chile intl. relations"))

STM_content <- stm(documents = output$documents, vocab = output$vocab,
                          K = 12, prevalence =~ gov_rating + s(year), 
                   content =~ gov_rating, 
                   max.em.its = 75, data = output$meta, 
                   init.type = "Spectral", verbose = FALSE) 

plot(STM_content, type = "perspectives", topics = 11)

plot(covars_effect, "year", method = "continuous", topics = c(11),
     model = STM, xlab = "Time in years (1990-2022)", ci.level = .99, 
     labeltype = "custom", custom.labels = "T11. Environmental issues", linecol = "dark green")
plot(covars_effect, "year", method = "continuous", topics = c(2),
     model = STM, xlab = "Time in years (1990-2022)", ci.level = .90, 
     labeltype = "custom", custom.labels = "T2. Un devt. goals", 
     add = TRUE, linecol = "blue")

plot(covars_effect, "year", method = "continuous", topics = c(11),
     model = STM, xlab = "Time in years (1990-2022)", ci.level = .99, 
     labeltype = "custom", custom.labels = "T11. Environmental issues", linecol = "dark green")
plot(covars_effect, "year", method = "continuous", topics = c(1),
     model = STM, xlab = "Time in years (1990-2022)", ci.level = .90, 
     labeltype = "custom", custom.labels = "T1. Chile-Latam relations & Chile intl. relations", 
     add = TRUE, linecol = "red")

#Experiment

STM_2 <- stm(documents = output$documents, vocab = output$vocab,
           K = 20, prevalence =~ gov_rating + s(year),
           max.em.its = 75, data = output$meta, 
           init.type = "Spectral", verbose = FALSE) 
plot(STM_2)
labelTopics(STM, n = 7)

#Time as linear function, log. function

STM_3 <- stm(documents = output$documents, vocab = output$vocab,
           K = 12, prevalence =~ gov_rating + year,
           max.em.its = 75, data = output$meta, 
           init.type = "Spectral", verbose = FALSE) 
covars_effect_3 <- estimateEffect(1:12 ~ gov_rating + year, STM, meta = output$meta, 
                                uncertainty = "Local", documents = output$documents)
plot(covars_effect_3, "year", method = "continuous", topics = c(11),
     model = STM, xlab = "Time in years (1990-2022)", ci.level = .99, 
     labeltype = "custom", custom.labels = "T11. Environmental issues", linecol = "dark green")

STM_4 <- stm(documents = output$documents, vocab = output$vocab,
             K = 12, prevalence =~ gov_rating + log(year),
             max.em.its = 75, data = output$meta, 
             init.type = "Spectral", verbose = FALSE) 
covars_effect_4 <- estimateEffect(1:12 ~ gov_rating + log(year), STM, meta = output$meta, 
                                  uncertainty = "Local", documents = output$documents)
plot(covars_effect_4, "year", method = "continuous", topics = c(11),
     model = STM, xlab = "Time in years (1990-2022)", ci.level = .99, 
     labeltype = "custom", custom.labels = "T11. Environmental issues", linecol = "dark green")
plot(covars_effect_4, "year", method = "continuous", topics = c(1),
     model = STM, xlab = "Time in years (1990-2022)", ci.level = .90, 
     labeltype = "custom", custom.labels = "T1. Chile-Latam relations & Chile intl. relations", 
     add = TRUE, linecol = "red")
