library(dplyr)
library(tidytext)
library(readr)
library(tokenizers)
library(udpipe)
library(SnowballC)
library(tidytext)

pos_es <- udpipe_load_model("data/spanish-ancora-ud-2.4-190531.udpipe")
pos_en <- udpipe_load_model("data/english-ewt-ud-2.4-190531.udpipe")
afinn_lex_es <- readRDS("data/afinn_lex_es.rds")

lexicon <- get_sentiments("afinn") %>%
  left_join(afinn_lex_es, by = "word") %>%
  mutate(word = palabra1) %>%
  mutate_at(.vars = "palabra1", ~wordStem(.)) %>%
  select(word, value, stems = palabra1)



wordPolarity <- function(comments, lexicon, pos_language) {
  
  cmnts <- comments
  colnames(cmnts) <- c("doc_id", "comentarios")
  lex <- lexicon
  # lexicon_pos <- udpipe_annotate(pos_language, lexicon$palabra1) %>%
  #   data.frame() %>%
  #   left_join(lexicon, by = c("token" = "word")) %>%
  #   select(token, upos, value, stems)

  # coments_pos <- udpipe_annotate(pos_language, comments) %>%
  #   data.frame() %>%
  #   mutate_at(.vars = "token", funs(stems = wordStem )) %>%
  #   left_join(lexicon_pos, by = c("token" = "token", "upos" = "upos")) 
  
  # new_text <- coments_pos %>%
  #   select(doc_id, sentence_id, token, value) %>%
  #   distinct() %>%
  #   mutate(color_token = case_when(value > 0 ~ paste0('<span style="color:green; font-weight:bold">', token, '</span>'),
  #                                  value == 0 ~ paste0('<span style="color:yellow; font-weight: bold">', token, '</span>'),
  #                                  value < 0 ~ paste0('<span style="color:red; font-weight: bold">', token, '</span>'),
  #                                  is.na(value) ~ token
  #   )
  #   ) %>%
  #   group_by(doc_id) %>%
  #   summarise(comment = paste(color_token, collapse = " "))
  
  #return(new_text)
  
  token_value <- cmnts %>%
    unnest_tokens("word_token", "comentarios") %>%
    left_join(select(lex, palabra1, value), by = c("word_token" = "palabra1")) %>%
    distinct() %>%
    mutate(color_token = case_when(value > 0 ~ paste0('<span style="color:green; font-weight:bold">', word_token, '</span>'),
                                   value == 0 ~ paste0('<span style="color:yellow; font-weight: bold">', word_token, '</span>'),
                                   value < 0 ~ paste0('<span style="color:red; font-weight: bold">', word_token, '</span>'),
                                   is.na(value) ~ word_token
                                   )
           ) %>%
    group_by(doc_id) %>%
    summarise(comment = paste(color_token, collapse = " "))
  

  return(token_value)
  
}

datatable(wordPolarity(f, lexicon), escape = FALSE)

pos_es <- udpipe_load_model("spanish-ancora-ud-2.4-190531.udpipe")
pos_en <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

lexicon <- get_sentiments("afinn") %>%
  mutate_at(.vars = "word", funs(stems = SnowballC::wordStem )) 

lexicon_pos <- udpipe_annotate(pos_en, lexicon$word) %>%
  data.frame() %>%
  left_join(lexicon, by = c("token" = "word")) %>%
  select(token, upos, value, stems)

coments <- read_csv("Documentos/Data/comments.csv") %>%
  mutate(doc_id = paste0("doc", row_number()))

coments_pos <- udpipe_annotate(pos_en, coments$comment) %>%
  data.frame() %>%
  mutate_at(.vars = "token", funs(stems = SnowballC::wordStem )) %>%
  left_join(lexicon_pos, by = c("token" = "token", "upos" = "upos"))

coment_sentiments <- coments_pos %>%
  group_by(doc_id) %>%
  distinct() %>%
  #filter(upos == "ADJ") %>%
  summarise(coment_score = sum(value, na.rm = T)) %>%
  mutate(sentiment = case_when(coment_score < 0 ~ "Negativo",
                               coment_score == 0 ~ "Neutral",
                               coment_score > 0 ~ "Positivo")) %>%
  left_join(coments, by = "doc_id")

# TOPICS

coments_pos$phrase_tag <- as_phrasemachine(coments_pos$upos, type = "upos")

stats <- keywords_phrases(x = coments_pos$phrase_tag, 
                          term = tolower(coments_pos$token), 
                          pattern = c("N", "V", "A"),
                          #pattern = c("N", "P", "D", "N"), 
                          detailed = FALSE,
                          sep =" "
                          )
