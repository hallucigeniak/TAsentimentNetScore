word_coocurrence <- function() {
  
  init <- read_csv("data/comentarios_Banorte_Format.csv", locale = locale(encoding = "WINDOWS-1252"))
  
  comments_etl <- init %>% 
    rename_all(funs(gsub(" ", "_", .))) %>%
    mutate(doc_id = paste0("doc_", row_number()),
           comentarios = coalesce(Comentario_NPS_ATMs_Detractor, Comentario_NPS_ATMs_Pasivo, Comentario_NPS_ATMs_Promotor)) %>%
    select(doc_id, comentarios) 
  
  comments_etl$comentarios <- gsub(pattern = "[[:punct:]]", replacement = "", x = comments_etl$comentarios)
  
  my_stopwords <- data.frame(my_stopwords = stopwords::stopwords(language = "es", source = "snowball")) %>%
    mutate(my_stopwords = as.character(my_stopwords))
  
  annot <- udpipe_annotate(udpipe_load_model("data/pos_models/es_ancora-ud-2.4-190531.udpipe"),
                           comments_etl$comentarios) %>% 
    as.data.frame() %>%
    select(doc_id, sentence_id, token_id, token, lemma, xpos) %>%
    anti_join(my_stopwords, by = c("lemma" = "my_stopwords"))
    
  # cooc_df <- annot %>%
  #   filter(xpos %in% c("ADJ", "NOUN")) 
  
    <- annot %>%
    cooccurrence(group = "doc_id", term = "lemma", relevant = annot$xpos %in% c("ADJ", "NOUN"), skipgram = 2)
     
}
