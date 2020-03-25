library(dplyr)
library(tidytext)
library(readr)
library(udpipe)
library(formattable)

wordPolarity <- function(comments, lexicon, pos_language) {
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
  
  token_value <- cmnts %>%
    unnest_tokens("word_token", "comentarios") %>%
    left_join(select(lex, palabra1, value), by = c("word_token" = "palabra1")) %>%
    distinct() %>%
    mutate(color_token = case_when(value > 0 ~ paste0(pos_word_open_tag, word_token, word_close_tag),
                                   value == 0 ~ paste0(neu_word_open_tag, word_token, word_close_tag),
                                   value < 0 ~ paste0(neg_word_open_tag, word_token, word_close_tag),
                                   is.na(value) ~ word_token
    )
    ) %>%
    group_by(doc_id) %>%
    summarise(comment = paste(color_token, collapse = " "),
              polarity = sum(value, na.rm = T)) %>%
    select(polarity, comment)
  
  output_dt <- as.datatable(
    formattable(token_value,
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

  return(output_dt)
  
  }