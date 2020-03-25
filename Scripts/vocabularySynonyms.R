source("C:/Users/eniak hernandez/Documents/TextAnalytics/Scripts/translation_funs.R")

afinn_lex_es <- googleTranslate(words = get_sentiments("afinn")$word,
                                from_lang = "en",
                                to = "es",
                                port_no = 4447L,
                                retry = 5)

saveRDS(es_afinn_lex, "C:/Users/eniak hernandez/Documents/TextAnalytics/data/es_afinn_LAST.rds")

bing_lex_es <- googleTranslate(words = get_sentiments("bing")$word,
                from_lang = "en",
                to = "es",
                port_no = 4441L,
                retry = 5)

loughran_lex_es <- googleTranslate(words = get_sentiments("loughran")$word,
                            from_lang = "en",
                            to = "es",
                            port_no = 4440L,
                            retry = 5)

afinn_lex_es_syn <- googleSynonyms(words     = afinn_lex_es$palabra1, 
                                   from_lang = "es",
                                   to_lang   = "en",
                                   port_no   = 4436L,
                                   retry     = 5)


afinn_lex_es_syns <- afinn_lex_es %>%
  left_join(get_sentiments("afinn"), by = "word") %>%
  left_join(afinn_lex_es_syn, by = "palabra1") %>% distinct()

saveRDS(afinn_lex_es_syns, "C:/Users/eniak hernandez/Documents/TextAnalytics/data/afinn_lex_es_syns.rds")

#---------------------------------------------------------------------------

# bing_lex_es_syn <- googleSynonyms(words     = bing_lex_es$palabra1, 
#                                   from_lang = "es",
#                                   to_lang   = "en",
#                                   port_no   = 4436L,
#                                   retry     = 5)
# 
# bing_lex_es_syns <- bing_lex_es %>%
#   left_join(get_sentiments("bing"), by = "word") %>%
#   left_join(bing_lex_es_syn, by = "palabra1") %>% distinct()
# 
# saveRDS(bing_lex_es_syns, "C:/Users/eniak hernandez/Documents/TextAnalytics/data/bing_lex_es_syns.rds")
# 
# #----------------------------------------------------------------------------
# 
# loughran_lex_es_syn <- googleSynonyms(words = loughran_lex_es$palabra1, 
#                                       from_lang = "es",
#                                       to_lang   = "en",
#                                       port_no   = 4436L,
#                                       retry     = 5)
# loughran_lex_es_syns <- loughran_lex_es %>%
#   left_join(get_sentiments("loughran"), by = "word") %>%
#   left_join(loughrang_lex_es_syn, by = "palabra1") %>% distinct()
# 
# saveRDS(loughran_lex_es_syns, "C:/Users/eniak hernandez/Documents/TextAnalytics/data/loughran_lex_es_syns.rds")