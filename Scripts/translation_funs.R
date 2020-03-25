library(RSelenium)
library(tidytext)
library(dplyr)

#--- TRANSLATION PROCESS ----
setLanguageEndpoint <- function(from = "en", to = "es"){
  
  url1 <- "https://translate.google.com.mx/?hl=es&tab=TT&authuser=0#view=home&op=translate&sl="
  url2 <- "&tl="

  full_url <- paste0(url1, from, url2, to)
  
  return(full_url)
}

getTranslation <- function(word) {
  
  s_i <- word
  trad <- vector()
  
  out <- tryCatch(
    {

      inputBox$sendKeysToElement(list(s_i))
      Sys.sleep(0.5)
      
      tradBox2 <- remDr$findElements(using = "class", "tlid-translation")
      n_terms <- length(tradBox2)
      trad <- vector()
      Sys.sleep(0.5)
   
      for (j in 1:n_terms){
        term_i <- tradBox2[[j]]$getElementText()
        term_i <- unlist(term_i)
        trad <- rbind(trad, term_i)
      }

      inputBox$clearElement()
      
      print(paste0(s_i, ": ", trad))
      
      trad
    },
    
    error=function(cond) {
      message(paste("Error: ", word))
      trad <- vector()
      inputBox$clearElement()
      return("ERROR")
    },
    
    warning=function(cond) {
      message(paste("Advertencia: ", word))
      trad <- vector()
      inputBox$clearElement()
      return("ERROR")
    },
    
    finally={
      trad <- vector()
      inputBox$clearElement()
    }
  )    
  return(out)
}

getSynonyms <- function(word) {
  out <- tryCatch(
    {
      s_i <- word
      
      inputBox$sendKeysToElement(list(s_i))
      Sys.sleep(1)
      synBox <- remDr$findElements(using = "class", "gt-cd-cl")
      Sys.sleep(0.5)
      n_syns <- length(synBox)
      
      synonyms <- vector()
      for (s in 1:n_syns){
        syn_s <- synBox[[s]]$getElementText()
        syn_s <- unlist(syn_s)
        synonyms <- append(synonyms, syn_s)
      }
        if (all(synonyms == "")){
          synonyms_out <- "No encontrado"
        } else {
          
          synonyms <- synonyms[synonyms!=""]
          
          synonyms_df <- data.frame(palabra1= s_i, synonyms=synonyms) %>%
            mutate_if(is.factor, as.character)
          rownames(synonyms_df) <- NULL
        }

      print(paste0(s_i, ": ", paste0(synonyms, collapse = " ")))

      synonyms_df
    },
    
    error=function(cond) {
      message(paste("Error: ", word))
      error_df <- data.frame(palabra1= s_i, synonyms="ERROR") %>%
        mutate_if(is.factor, as.character)
      return(error_df)
    },
    
    warning=function(cond) {
      message(paste("Advertencia: ", word))
      error_df <- data.frame(palabra1= s_i, synonyms="ERROR") %>%
        mutate_if(is.factor, as.character)
      return(error_df)
    },
    
    finally={
      inputBox$clearElement()
    }
  )    
  return(out)
}

googleTranslate <- function(words, from_lang= "en", to_lang = "es", port_no = 4444L, retry = 5) {
  
  rmD <<- rsDriver(port =port_no, browser = "chrome", chromever = "75.0.3770.90")
  remDr <<- rmD$client
  #remDr$open()
  
  words_vec <- words
  lang1 <- from_lang
  lang2 <- to_lang
  
  endpoint <- setLanguageEndpoint(from = lang1, to = lang2)
  #print(endpoint)
  
  remDr$navigate(endpoint)
  Sys.sleep(5)
  inputBox <<- remDr$findElement(using = "id", "source")
  
  trans <<- sapply(words_vec, function(x) getTranslation(x))
  
  dict_1 <<- data.frame(word     = names(trans),
                      palabra1 = sapply(trans, function(x) x[1]),
                      palabra2 = sapply(trans, function(x) x[2])) %>%
    mutate_if(is.factor, as.character)
  
  dict <- dict_1 %>%
    select(word, palabra1) %>%
    union(na.omit(select(dict_1, word, palabra1 = palabra2))) %>%
    arrange(word)
  
  n_try <- 0
  
  while (("ERROR" %in% dict$palabra1 | any(grepl("...", dict$palabra1, fixed = T))) & n_try < retry ){
    
    dict_missing <- dict %>% 
      filter(palabra1 == "ERROR") %>%
      mutate_if(is.factor, as.character) 
    
    trans2 <- sapply(dict_missing$word, function(x) getTranslation(x))
    
    dict2 <- data.frame(word     = names(trans2),
                        palabra1 = sapply(trans2, function(x) x[1]),
                        palabra2 = sapply(trans2, function(x) x[2])) %>%
      filter(palabra1 != "ERROR") %>%
      mutate_if(is.factor, as.character)
    
    dict <<- dict %>%
      anti_join(dict2, by = "word")
    
    dict <<- bind_rows(dict, dict2)
    
    n_try <- n_try + 1
    
    print(paste0("Missing terms: ", length(dict$palabra1 == "ERROR")))
    print(paste0("Atempt: ", n_try, "/", retry))
  }
  
  return(arrange(dict, word))
}

googleSynonyms <- function(words, from_lang= "en", to_lang = "es", port_no = 4444L, retry = 5) {
  
  if (!exists("remDr")) {  
    rmD <<- rsDriver(port = port_no, browser = "chrome", chromever = "75.0.3770.90")
    remDr <<- rmD$client
  } else {
    remDr$open()
  }

  words_vec <- words
  lang1 <- from_lang
  lang2 <- to_lang
  
  endpoint <- setLanguageEndpoint(from = lang1, to = lang2)
  print(endpoint)
  
  remDr$navigate(endpoint)
  Sys.sleep(5)
  
  inputBox <<- remDr$findElement(using = "id", "source")
  
  sinonimos <<- sapply(words_vec, function(x) getSynonyms(x), simplify = FALSE)
  
  dict <<- bind_rows(sinonimos) %>%
    mutate_if(is.factor, as.character)
  
  n_try <- 0
  
  while ("Error" %in% dict$synonyms){
    n_try <- n_try + 1
    
    print(paste0("Missing terms: ", length(dict$synonyms == "Error")))
    print(paste0("Atempt: ", n_try, "/", retry))
    
    dict_missing <- dict %>%
      filter(synonyms == "Error") %>%
      mutate_if(is.factor, as.character)
    
    sinonimos2 <<- sapply(dict_missing$palabra1, function(x) getSynonyms(x), simplify = FALSE)
    
    dict2 <<- bind_rows(sinonimos2) %>%
      mutate_if(is.factor, as.character) %>%
      filter(synonyms != "Error") %>%
      mutate_if(is.factor, as.character)
    
    dict <<- dict %>%
      anti_join(dict2, by = "palabra1")
    
    dict <<- bind_rows(dict, dict2)
   
    if(n_try == retry) {break}
  }

  return(arrange(dict, palabra1))
}  
