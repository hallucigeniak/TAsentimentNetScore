library(dplyr)
library(tidytext)
library(RDRPOSTagger)
library(dplyr)
library(readr)
library(rvest)

sentiment_df_en_es <- read_csv("https://raw.githubusercontent.com/jboscomendoza/rpubs/master/sentimientos_afinn/lexico_afinn.en.es.csv", locale = readr::locale(encoding = "latin1")) %>%
  filter(!is.na(Palabra))

rae_url <- "https://dle.rae.es/"
wordQuery <- "?w=a+bordo"

models <- rdr_available_models()
models$POS$language
opinion <- c("Muy bueno y el servicio excelente totalmente será nuestra primera opción al ir a la paz. Las habitaciones muy bien y las instalaciones muy bonitas y cerca del malecón lastima que fuimos de entrada y salida pero ya regresaremos con mucho más tiempo para disfrutar muy bien", "La comida es mala, los meseros nada atentos, al igual que los recepcionistas y las amas de llaves. Solicite botellas de agua que jamás llegaron. Atención en la alberca que no hubo, para toallas, tuve una queja y la recepcionista fue sumamente pesada.")

opinion_df <- data.frame(id      = c(1,2), 
                         opinion = opinion) %>%
  mutate(opinion = as.character(opinion))

opinion_tokens <- tidytext::unnest_tokens(tbl = opinion_df,
                        output = "tokens",
                        input = "opinion")

opinion_tokens %>%
  left_join(sentiment_df_en_es, by = c("tokens" = "Palabra"))

tagger_es <- rdr_model(language = "Spanish", annotation = "UniversalPOS")
tagger_en <- rdr_model(language = "English", annotation = "UniversalPOS")

opinion <- tolower(opinion)

opinion_pos <- rdr_pos(tagger_es, x = opinion_tokens$tokens)

sentiment_es_pos <- rdr_pos(tagger_es, x = sentiment_df_en_es$Palabra)

sentiment_en_pos <- rdr_pos(tagger_en, x = sentiment_df_en_es$Word)

sentiment_df_en_es %>%
  left_join(sentiment_es_pos, by = c("Palabra" = "token")) %>%
  select(Palabra, Puntuacion, Word, pos_es=pos) #%>%
  # left_join(sentiment_en_pos, by = c("Word" = "token")) %>%
  # select(Palabra, Puntuacion, Word, pos_es, pos_en=pos) %>%
  # distinct()


opinion_pos %>%
  select(token, pos) %>%
  distinct()


