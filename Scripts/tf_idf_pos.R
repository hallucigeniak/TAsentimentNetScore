library(tidytext)
library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(RDRPOSTagger)
library(stringr)

#--- GLOBALS ----
raw_data <- read_xlsx("data/Responses_20190629_235959_clean.xlsx")

sentiment_lexicon <- read_delim("https://raw.githubusercontent.com/felipebravom/StaticTwitterSent/master/extra/SentiStrength/spanish/EmotionLookupTable.txt", delim = "\t", col_names = F)

models <- rdr_available_models()
tagger_es <- rdr_model(language = "Spanish", annotation = "UniversalPOS")

#--- DATA PROCESSING ----

comments_df <- raw_data %>%
  select(ATM, starts_with("comentario")) %>%
  mutate (comentarios = coalesce(`Comentario NPS ATMs Detractor`,
                                 `Comentario NPS ATMs Pasivo`,
                                 `Comentario NPS ATMs Promotor`),
          tipo_usuario = case_when(!is.na(`Comentario NPS ATMs Detractor`) ~ "Detractor",
                                   !is.na(`Comentario NPS ATMs Pasivo`) ~ "Pasivo",
                                   !is.na(`Comentario NPS ATMs Promotor`) ~ "Promotor"))

my_stopwords <- data.frame(my_stopwords = stopwords::stopwords(language = "es", source = "snowball")) %>%
  mutate(my_stopwords = as.character(my_stopwords))

comments_token_df <- comments_df %>%
  select(tipo_usuario, comentarios) %>%
  unnest_tokens(word_token, comentarios) %>%
  anti_join(my_stopwords, by = c("word_token" = "my_stopwords")) %>%
  count(tipo_usuario, word_token, sort=T) %>%
  arrange(tipo_usuario) 

comments_token_pos <- rdr_pos(tagger_es, x = comments_token_df$word_token) %>%
  select(token, pos) %>%
  distinct()

comments_token_adj_df <- comments_token_df %>%
  left_join(comments_token_pos, by = c("word_token" = "token")) %>%
  filter(pos == "ADJ")

# comments_token_noun_df <- comments_token_df %>%
#   left_join(comments_token_pos, by = c("word_token" = "token")) %>%
#   filter(pos == "NOUN")

total_words <- comments_token_adj_df %>% 
  group_by(tipo_usuario) %>% 
  summarize(total = sum(n)) %>%
  arrange(desc(total))

comments_words <- left_join(comments_token_adj_df, total_words)

comments_words <- comments_words %>%
  bind_tf_idf(word_token, tipo_usuario, n)

top_10 <- comments_words %>%
  select(-total) %>%
  arrange(tipo_usuario, desc(tf_idf)) %>%
  group_by(tipo_usuario) %>%
  top_n(10)

top_10 <- top_10 %>%
  mutate(word_regex = paste0("((\\w+) )+", word_token, "[^.]*"),
         context    = NA)

for (i in 1:NROW(top_10)) {
  i_regex <- top_10$word_regex[i]
  i_context <- vector()
  for (j in 1:NROW(comments_df)){
    if (str_detect(comments_df$comentarios[j], i_regex)){
      i_j_context <- str_extract(comments_df$comentarios[j], i_regex)
      i_context <- append(i_context, i_j_context)
    }
  }
  i_context <- paste0(i_context, collapse = "; ")
  top_10$context[i] <- i_context

}
