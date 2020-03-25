library(dplyr)
library(tidytext)
library(readr)
library(udpipe)
library(formattable)
library(wordcloud2)

textAnalysis <- function(comments, lexicon, pos_language) {
  
  cmnts <- comments
  
  colnames(cmnts) <- c("doc_id", "comentarios")
  
  lex <- lexicon %>%
    select(-word) %>%
    distinct() %>%
    group_by(palabra1) %>%
    slice(1)
  
  pos_word_open_tag <- '<span style="color:green; font-weight:bold">'
  neu_word_open_tag <- '<span style="color:yellow; font-weight: bold">'
  neg_word_open_tag <- '<span style="color:red; font-weight: bold">'
  word_close_tag <- '</span>'
  
  tokenized <- cmnts %>%
    unnest_tokens("word_token", "comentarios") %>%
    left_join(select(lex, palabra1, value), by = c("word_token" = "palabra1")) %>%
    distinct() 
  
  comment_scores <- tokenized %>%
    mutate(color_token = case_when(value > 0 ~ paste0(pos_word_open_tag, word_token, word_close_tag),
                                   value == 0 ~ paste0(neu_word_open_tag, word_token, word_close_tag),
                                   value < 0 ~ paste0(neg_word_open_tag, word_token, word_close_tag),
                                   is.na(value) ~ word_token
    )
    ) %>%
    group_by(doc_id) %>%
    summarise(comment = paste(color_token, collapse = " "),
              polarity = sum(value, na.rm = T)) %>%
    select(doc_id, polarity, comment)
  
  doc_scores <- comment_scores %>%
    select(doc_id, polarity)
  
  word_polarity <- comment_scores %>%
    select(-doc_id)
  
  output_word_polarity <- as.datatable(
    formattable(word_polarity,
                align = c("c", "l"),
                list(`polarity` = formatter("span",
                                            x ~ icontext(case_when(x > 0 ~ "ok",
                                                                   x == 0 ~ "minus",
                                                                   x < 0 ~ "remove")
                                            ),
                                            style = x ~ style(color = case_when(x > 0 ~ "green",
                                                                                x == 0 ~ "#CCCC00",
                                                                                x < 0 ~ "red")
                                            )
                )
                )
    )
  )
  
  ####---- NSS ----
  pre_nss <- word_polarity %>%
    mutate(tipo = case_when(polarity > 0  ~ "promotor",
                            polarity == 0 ~ "pasivo",
                            polarity < 0  ~ "detractor")) %>%
    group_by(tipo) %>%
    summarise(count_tipo = n()) %>%
    ungroup() %>%
    mutate(porcentaje = count_tipo / sum(count_tipo) * 100)
  
  nps <- pull(select(filter(pre_nss, tipo == "promotor"), porcentaje)) - pull(select(filter(pre_nss, tipo == "detractor"), porcentaje))
  
  ####---- WORD COOCURRENCE ----
  
  
  
  ####---- TF-IDF ----
  
  my_stopwords <- data.frame(my_stopwords = stopwords::stopwords(language = "es", source = "snowball")) %>%
    mutate(my_stopwords = as.character(my_stopwords))
  
  word_counts <- tokenized %>%
    anti_join(my_stopwords, by = c("word_token" = "my_stopwords")) %>%
    count(word_token, sort = T) 
  
  global_word_cloud <- word_counts %>%
    left_join(tokenized) %>%
    select(word_token, n, value) %>%
    distinct() %>%
    transform(value = case_when(is.na(value) ~ "lightblue",
                                value > 0    ~ "green",
                                value < 0    ~ "red")) %>%
    arrange(desc(n))
  
 
  pos_word_cloud <- udpipe_annotate(pos_model, global_word_cloud$word_token) %>%
    data.frame() %>%
    select(token, lemma, upos) %>%
    right_join(global_word_cloud, by = c("token" = "word_token")) %>%
    group_by(lemma, upos, value) %>%
    summarise(freq = sum(n)) %>%
    arrange(desc(freq))
  
  text_analysis_results <- list(word_polarity = output_word_polarity,
                                global_wc     = global_word_cloud,
                                pos_wc        = pos_word_cloud,
                                nps           = nps,
                                doc_scores    = doc_scores,
                                tokens_by_doc = tokenized
                                #tfidf         = output_tfidf
                                )
  
  return(text_analysis_results)
  
}