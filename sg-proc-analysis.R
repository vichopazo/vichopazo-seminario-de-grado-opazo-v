#Cargar paquetes

library(pacman)
pacman::p_load(ggplot2)
library(topicmodels)
library(quanteda)
library(readtext)
library(stm)

#Load readtext and ed_dataset, UNGDC_Chile_1970_2015

raw_speeches <- readtext("C:/Users/vitoo/Onedrive/Documents/GitHub/seminario-de-grado-opazo-v/input/data/UNGDC_Chile_1970_2022/*.txt")

                  
ungd_chl <- readtext("C:/Users/vitoo/Onedrive/Documents/GitHub/seminario-de-grado-opazo-v/input/data/UNGDC_Chile_1970_2022/*.txt", 
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

