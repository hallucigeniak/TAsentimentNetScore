library(dplyr)
library(tidytext)
library(RDRPOSTagger)
library(dplyr)
library(readr)
library(rvest)

sentiment_df_en_es <- read_csv("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv", locale = readr::locale(encoding = "latin1")) %>%
  filter(!is.na(Palabra))

models <- rdr_available_models()


rae_url <- "https://dle.rae.es/"
rae_wordQuery <- "?w=tiempo"
rae_endpoint <- paste0(rae_url, wordQuery)

wiki_url <- "https://es.wiktionary.org/wiki/"
wiki_wordQuery <- "absolver"
wiki_endpoint <- paste0(wiki_url, wiki_wordQuery)



wiki_results <- read_html(wiki_endpoint)

wiki_results %>%
  html_nodes("h3 .mw-headline") %>%
  html_text()


rvest::