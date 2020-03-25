segment_wordcloud <- function(df1, df2, segment_col, segment_rows, language) {
  
  my_stopwords <- data.frame(my_stopwords = stopwords::stopwords(language = language, source = "snowball")) %>%
    mutate(my_stopwords = as.character(my_stopwords))
  
  df_1 <- df1 %>%
    select("doc_id", segmento = segment_col) #%>% 
    #

  wc_output_df <- df2 %>%
    inner_join(df_1, by = "doc_id") %>%
    anti_join(my_stopwords, by = c("word_token" = "my_stopwords")) %>%
    group_by(segmento, value) %>%
    count(word_token, sort = T) %>%
    bind_tf_idf(word_token, segmento, n) %>%
    arrange(segmento, desc(tf_idf)) %>%
    transform(value = case_when(is.na(value) ~ "lightblue",
                                value > 0 ~ "green",
                                value < 0 ~ "red")) %>%
    filter(segmento == segment_rows) %>%
    select(word_token, n, value)

  return(wc_output_df)
}

