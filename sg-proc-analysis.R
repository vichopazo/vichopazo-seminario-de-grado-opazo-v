#Cargar paquetes

library(pacman)
pacman::p_load(ggplot2)
library(topicmodels)
library(quanteda)
library(readtext)
library(stm)

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


#Reshape corpus into paragraphs and create corpus from ed_dataset 

ungdcorp_chl <- corpus(ungd_chl)

dfm = dfm(ungdcorp_chl, remove_punct=T, remove=stopwords("spanish"))


#Push to github (Mikhaylov, Slava; Baturo, Alexander; Dasandi, Niheer, 2017, "United Nations General Debate Corpus", doi:10.7910/DVN/0TJX8Y, Harvard Dataverse, V3)

