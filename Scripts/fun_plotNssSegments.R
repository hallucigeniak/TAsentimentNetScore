
plot_segments_nss <- function(df1, df2, segment_col){
  
  df1 %>%
    select("doc_id", segment_col) %>%
    na.omit() %>%
    left_join(df2) %>%
    mutate(calificacion = case_when(polarity > 0 ~ "Promotor",
                                    polarity == 0 ~ "Pasivo",
                                    polarity < 0 ~ "Detractor")
    ) %>% 
    group_by_(segment_col, "calificacion") %>%
    count_() %>%
    ungroup() %>%
    group_by_(segment_col) %>%
    mutate(suma_grupo   = sum(n),
           segment_prop = n / suma_grupo * 100) %>%
    filter(calificacion %in% c("Promotor", "Detractor")) %>%
    select_(segment_col, "calificacion", "segment_prop") %>%
    spread(calificacion, segment_prop) %>%
    ungroup() %>%
    mutate(Promotor = replace_na(Promotor, 0),
           Detractor = replace_na(Detractor, 0)) %>%
    mutate(segment_nps = formatC(Promotor - Detractor, 
                                 digits = 2,
                                 format = "f")) %>% 
    select_(Segmento = segment_col, "Detractor", "Promotor", "segment_nps") %>%
    mutate(segment_nps = as.numeric(segment_nps)) %>%
    ggplot(aes(x = as.factor(Segmento),
               y = segment_nps, 
               fill = as.factor(Segmento),
               label = segment_nps)) +
    geom_col( width = 0.5) +
    labs(x = "Segmento",y = "NSS") +
    geom_text(nudge_y = 1) +
    coord_flip() +
    ylim(-100, 100) +
    theme(legend.position = "none", axis.text=element_text(size=12))
  
}