library(RSelenium)
library(tidytext)

#--- GLOBALS ----
diccionario <- data.frame()
sentiments_en <- get_sentiments()
endpoint <- "https://translate.google.com.mx/?hl=en#view=home&op=translate&sl=auto&tl=es"

#--- SELENIUM SERVER SETUP ----
rmD <- rsDriver(port =4445L ,browser = "chrome", chromever = "75.0.3770.90")
remDr <- rmD$client
remDr$open()
remDr$navigate(endpoint)
inputBox <- remDr$findElement(using = "id", "source")

#--- TRANSLATION PROCESS ----

inputBox$clearElement()

for (i in 441:NROW(sentiments_en)){
  
  s_i <- sentiments_en$word[i]
  v_i <- sentiments_en$value[i]
  
  inputBox$sendKeysToElement(list(s_i))
  
  Sys.sleep(1.5)
  
  tradBox <- remDr$findElement(using = "class", "gt-baf-word-clickable")
  Sys.sleep(1.5)
  trad <- tradBox$getElementText()
  trad <- unlist(trad)
  
  if (trad == ""){
    trad <- vector()
    tradBox2 <- remDr$findElements(using = "class", "tlid-translation")
    n_terms <- length(tradBox2)
    
    for (j in 1:n_terms){
      term_i <- tradBox2[[j]]$getElementText()
      trad <- rbind(trad, term_i)
    }
  }
  posBox <- remDr$findElement(using = "class", "gt-cd-pos")
  pos <- posBox$getElementText()
  pos <- unlist(pos)
  inputBox$clearElement()
  
  entry_en_es <- data.frame(word=as.character(s_i), palabra=as.character(trad), value=v_i, pos=pos) %>%
    mutate_if(is.factor, as.character)
  rownames(entry_en_es) <- NULL
  diccionario <- rbind(diccionario, entry_en_es)
}


getTranslation <- function(word) {
  out <- tryCatch(
    {
      s_i <- word

      inputBox$sendKeysToElement(list(s_i))
      Sys.sleep(0.5)
      tradBox <- remDr$findElement(using = "class", "gt-baf-word-clickable")
      Sys.sleep(0.5)
      trad <- tradBox$getElementText()
      trad <- unlist(trad)
      
      if (trad == ""){
        trad <- vector()
        tradBox2 <- remDr$findElements(using = "class", "tlid-translation")
        n_terms <- length(tradBox2)
        
        for (j in 1:n_terms){
          term_i <- tradBox2[[j]]$getElementText()
          term_i <- unlist(term_i)
          trad <- rbind(trad, term_i)
        }
      }
      
      posBox <- remDr$findElement(using = "class", "gt-cd-pos")
      pos <- posBox$getElementText()
      pos <- unlist(pos)
      inputBox$clearElement()
      
      print(paste0(s_i, ": ", trad))
      trad
    },
    error=function(cond) {
      message(paste("Error: ", word))
      return("Error")
    },
    warning=function(cond) {
      message(paste("Advertencia: ", word))
      return("Error")
    },
    finally={
      inputBox$clearElement()
    }
  )    
  return(out)
}

y <- sapply(sentiments$word, function(x) getTranslation(x))

z <- data.frame(a=names(y), b= sapply(y, function(x) x[1]), c=sapply(y, function(x) x[2]))

y_missing <- z %>%
  filter(b=="Error") %>%
  select(a)

yy <- sapply(y_missing$a, function(x) getTranslation(x))
