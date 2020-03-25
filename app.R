#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinycssloaders)
library(shinyWidgets)
library(dplyr)
library(stringr)
library(DT)
library(rhandsontable)
library(udpipe)
library(readxl)
library(readr)
library(tidytext)
library(wordcloud2)
library(flexdashboard)
library(lubridate)
library(tidyr)
library(ggplot2)
#library(plotly)

#TRENDING TOPICS -> Positive & Negative [at least n mentions in a given time period & ]
#WORD CO-OCURRENCE


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "Text Analytics",
                  titleWidth = 280
                  ),
  
  dashboardSidebar(width = 280,
                   sidebarMenu(id = "tabs",
                     menuItem(strong("Importar archivos"), 
                              icon=icon("upload"),
                              menuItem("Archivo de comentarios",
                                       tabName = "uploadComments",
                                       badgeLabel = "1",
                                       badgeColor = "blue"),
                              menuItem("Diccionarios",
                                       tabName = "uploadDictionaries",
                                       badgeLabel = "2",
                                       badgeColor = "blue")
                     ),
                     menuItem(strong("Analisis de sentimientos"),
                              menuItem("Resumen",
                                       tabName = "resumen-polaridad"),
                              menuItem("Segmentos",
                                       tabName = "segmentos")
                     ),
                     menuItem(strong("Temas"),
                              menuItem("resumen",
                                       tabName = "resumen-topics"),
                              menuItem("Temas",
                                       tabName = "topics")
                     )
                   )
  ),
  
  dashboardBody(
      
    tags$head( 
      tags$style(HTML(".main-sidebar { font-size: 20px; }
                      .treeview-menu>li>a {font-size: 15px!important;}
                      .html-widget.gauge svg {
                                              height: 400px;
                                              width: 800px;
                                              margin-left: auto;
                                              margin-right: auto;
                      }
                      ")) 

    ),
    tabItems(
      tabItem(tabName = "uploadComments",
              useShinyjs(),
              fluidRow(
                box(title = "Selecciona el archivo de comentarios",
                    status = "primary",
                    width = 4,
                    fileInput(inputId = "choseFile",
                              label = "File Input",
                              buttonLabel =  "explorar archivos")
                 ),
                
                box(title = "Selecciona id",
                    status = "primary",
                    width = 4,
                    solidHeader = F,
                    uiOutput("idSelector")
                ),
                
                box(title = "selecciona campo con la fecha",
                    status = "primary",
                    width = 4,
                    uiOutput("date"))
              ),
              
              fluidRow(
                box(title = "Selecciona los segmentos",
                    status = "primary",
                    width = 6,
                    solidHeader = F,
                    uiOutput("segmentSelector")
                ),
                
                box(title = "Selecciona campos con comentarios",
                    status = "primary",
                    width = 6,
                    solidHeader = F,
                    uiOutput("fieldsSelector")
                )
              ),
              
              fluidRow(
                box(title = "Vista previa",
                    status = "primary",
                    width = 12,
                    solidHeader = T,
                    dataTableOutput("filePreview")
                )
              ),
              
              fluidRow(
                box(
                  width = 12,
                  disabled(actionButton(inputId = "nextBtn",
                               label   = "Next",
                               width   = "100%", 
                               height  = "500px",
                               style   = "background-color:lightgreen;
                                           color:darkgreen;
                                           font-weight: bold;
                                           font-size: 20px;
                                           padding:20px"))
                )
              )
      ),
      tabItem(tabName = "uploadDictionaries",
              
              fluidRow(
                box(title = "Selecciona el diccionario",
                    status = "primary",
                    width = 6,
                    radioButtons(inputId  = "language",
                                 label    = "Selecciona el idioma",
                                 choices  = list(Español = "es", Inglés = "en"),
                                 selected = 0),
                    br(),
                    uiOutput("dictSelect"),
                    uiOutput("posSelect"),
                    br(),
                    actionButton(inputId = "backBtn",
                                 label   = "back",
                                 height  = "500px",
                                 style   = "background-color:lightgreen;
                                           color:darkgreen;
                                           font-weight: bold;
                                           font-size: 20px;
                                           padding:20px"),
                    
                    disabled(actionButton(inputId = "analyzeText",
                                 label   = "Analizar texto",
                                 height  = "500px",
                                 style   = "background-color:lightgreen;
                                           color:darkgreen;
                                           font-weight: bold;
                                           font-size: 20px;
                                           padding:20px"))

                ),
                
                box(title = "Diccionario",
                    status = "primary",
                    width = 6,
                    rHandsontableOutput('dictDT'),
                    br(),
                    uiOutput("useRegexBtn"),
                    textInput("searchField", "Search"),
                    br(),
                    actionButton(inputId = "saveDict",
                                 label   = "guardar cambios",
                                 height  = "500px",
                                 style   = "background-color:lightgreen;
                                           color:darkgreen;
                                           font-weight: bold;
                                           font-size: 20px;
                                           padding:20px")
                )
              ),
              
              fluidRow( )
      ),
      tabItem(tabName = "resumen-polaridad",
              fluidRow(
                gaugeOutput("nps",
                            height = "200%")
              ),
              
              fluidRow(  
                box(title       = "Palabras más mencionadas",
                    status      = "primary",
                    width       = 12,
                    solidHeader = T,
                    
                    sliderInput(inputId = "top_n_wc",
                                label   = "Mostrar palabras",
                                min     = 10,
                                max     = 200,
                                value   = 100,
                                step    = 10
                                ),
                    
                    selectInput(inputId   = "filterPolarity",
                                label     = "Filtros Polaridad",
                                selectize = T,
                                multiple  = T,
                                selected  = c("green", "lightblue", "red"),
                                choices   = list(value = list(positivo = "green",
                                                              neutral  = "lightblue",
                                                              negativo = "red")
                                               )
                                ),

                    selectInput(inputId   = "filterPOS",
                                label     = "Filtro POS",
                                selectize = T,
                                multiple  = T,
                                selected  = c("ADJ", "NOUN"), 
                                choices   = list(POS = list(Adjetivos   = "ADJ",
                                                            Sustantivos = "NOUN")
                                               )
                                ),
                    
                    wordcloud2Output("globalWordCloud")
                )
              ),
              
              fluidRow(
                box(title       = "Comentarios",
                    status      = "primary",
                    width       = 12,
                    solidHeader = T,
                    DT::dataTableOutput("commentsTable") %>% withSpinner()
                )
              )
      ),
      
      tabItem(tabName = "segmentos",
              fluidRow(
                box(title = "NSS por segmento",
                    width = "100%",
                    uiOutput("selectSegmentUI"),  
                    plotly::plotlyOutput("segmentNPS", height = "200%", width = "100%")
                ),
                box(title = "Palabras más frecuentes por segmento",
                    width = "100%",
                    sliderInput(inputId = "top_n_segment_wc",
                                label   = "Mostrar palabras",
                                min     = 10,
                                max     = 200,
                                value   = 100,
                                step    = 10
                    ),
                    selectInput(inputId   = "filterPOSsegment",
                                label     = "Filtro POS",
                                selectize = T,
                                multiple  = T,
                                selected  = c("ADJ", "NOUN"), 
                                choices   = list(POS = list(Adjetivos   = "ADJ",
                                                            Sustantivos = "NOUN")
                                )
                    ),
                    selectInput(inputId   = "filterPolaritySegments",
                                label     = "Filtros Polaridad",
                                selectize = T,
                                multiple  = T,
                                selected  = c("green", "lightblue", "red"),
                                choices   = list(value = list(positivo = "green",
                                                              neutral  = "lightblue",
                                                              negativo = "red")
                                )
                    ),
                    uiOutput("segmentItemUI"),
                    wordcloud2Output("segmentWordCloud")
                    )
              )
              )
      
    )
  )
)
  
  
# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  flags <- reactiveValues(commentsOn = FALSE, dictOn = FALSE, dupCols = FALSE)
  
  observeEvent(input$choseFile,
               {
                 chosenFile <- input$choseFile
                 chosenFile <- gsub('\\\\', '/', chosenFile$datapath)
                 print(chosenFile)
                 chosenFile_ext <- unlist(strsplit(chosenFile, ".", fixed = T))[-1]
                 
                 if(chosenFile_ext == "csv"){
                   comments_raw <<- read_csv(chosenFile, locale = locale(encoding = "WINDOWS-1252")) %>%
                     mutate(doc_id = paste0("doc", row_number())) %>%
                     rename_all(funs(gsub(" ", "_", .)))
                 } else if (chosenFile_ext %in% c("xls", "xlsx")){
                   comments_raw <<- read_xls(chosenFile) %>%
                     mutate(doc_id = paste0("doc", row_number())) %>%
                     rename_all(funs(gsub(" ", "_", .)))
                 }
                 
                 col_names <- colnames(comments_raw)
                 
                  output$fieldsSelector <- renderUI(
                    {
                      
                      selectizeInput(inputId  = "commentCols",
                                     label    = "Selecciona columnas",
                                     choices  = split(col_names, col_names),
                                     multiple = TRUE)
                    }
                  )

                  output$idSelector <- renderUI(
                    {

                      selectizeInput(inputId  = "idCol",
                                     label    = "Selecciona columna id",
                                     choices  = split(col_names, col_names),
                                     selected = 1,
                                     multiple = FALSE)
                    }
                   )
                  
                  output$segmentSelector <- renderUI(
                    {
                      selectizeInput(inputId = "segmentCols",
                                     label = "selecciona campos con los segmntos",
                                     choices = split(col_names, col_names),
                                     multiple = T,
                                     selected = 2)
                    }
                  )
                  
                  output$date <- renderUI(
                    {
                      selectizeInput(inputId = "dateCol",
                                label = "selecciona columna con la fecha",
                                choices = split(col_names, col_names),
                                multiple = F,
                                selected = 3)
                    }
                  )
               }
  )

  observeEvent(input$language,
               {
                 enable("choseDict")

                 lex_path <- "data/lexicons/"
                 lex_ext <- list.files(path = lex_path, pattern = paste0("^", input$language, "_"))
                 lex <- unlist(strsplit(x = lex_ext, split = ".", fixed = T))[1]
                 lex_ext <- paste0(lex_path, lex_ext)

                 output$dictSelect <- renderUI(
                   {
                     selectInput(inputId  = "selectDict",
                                 label    = "selecciona el diccionario",
                                 choices  = split(lex_ext, lex),
                                 selected = 0)
                   }
                 )

                 pos_path <- "data/pos_models/"
                 pos_ext <- list.files(path = pos_path, pattern = paste0("^", input$language, "_"))
                 pos <- unlist(strsplit(x = pos_ext, split = ".", fixed = T))[1]
                 pos_ext <- paste0(pos_path, pos_ext)

                 output$posSelect <- renderUI(
                   {
                     selectInput(inputId  = "selectPOS",
                                 label    = "selecciona el modelo de POS",
                                 choices  = split(pos_ext, pos),
                                 selected = 0)
                   }
                 )
               }
)


  #chosenDict <- reactiveValues()

  observeEvent(input$selectDict,
               {
                 flags$dictOn <- TRUE
                 chosenDict <- input$selectDict
                 lexicon <<- readRDS(chosenDict)
                 
                 lexicon <<- lexicon %>%
                   mutate(index = row_number()) %>%
                   select(index, word, value, palabra1)
                 
                 lexicon_2 <<- lexicon
                 
                 output$dictDT <- renderRHandsontable(
                   {
                     rhandsontable(lexicon, search = TRUE, width = 650, height = 650) %>%
                       hot_table(contextMenu = T, highlightCol = TRUE, highlightRow = TRUE) %>%
                       hot_cols(columnSorting = T)
                     
                   }
                 )
                 
                 output$useRegexBtn <- renderUI(
                   {
                     radioButtons(inputId = "useRegex",
                                  label = "Usar RegEx?",
                                  choiceNames = c("Si", "No"),
                                  choiceValues = c(FALSE, TRUE))
                   }
                 )
               }
  )
  
  observeEvent(input$searchField,
               {
                 text_pattern <- input$searchField

                 if (nchar(text_pattern) >= 1 && flags$dictOn){
                   filter_df <- lexicon_2 %>%
                     filter_all(any_vars(grepl(text_pattern, ., fixed = input$useRegex)))
                   
                   output$dictDT <- renderRHandsontable(
                     {
                       rhandsontable(filter_df, search = TRUE, width = 650, height = 650) %>%
                         hot_table(contextMenu = T, highlightCol = TRUE, highlightRow = TRUE) %>%
                         hot_cols(columnSorting = T)
                     }
                   )
                 } else if (flags$dictOn) {
                   output$dictDT <- renderRHandsontable(
                     {
                       rhandsontable(lexicon_2, search = TRUE, width = 650, height = 650) %>%
                         hot_table(contextMenu = T, highlightCol = TRUE, highlightRow = TRUE) %>%
                         hot_cols(columnSorting = T)
                       
                     }
                   )
                 }
               }
  )
  
  observeEvent(input$dictDT,
               {
                 if(!is.null(input$dictDT$changes$changes)){
                   print(input$dictDT$data)
                   r_i <- input$dictDT$changes$changes[[1]][[1]] + 1 #row
                   print(r_i)
                   edit_col <- input$dictDT$changes$changes[[1]][[2]] + 1#col
                   print(edit_col)
                   row_index <- input$dictDT$data[[r_i]][[1 ]][[1]]
                   print(row_index)

                   cell_replacement <- input$dictDT$changes$changes[[1]][[4]]
                   lexicon_2[row_index, edit_col] <<- cell_replacement
                 }
               }
  )
  
  observeEvent(input$commentCols,
               {
                 flags$commentsOn <- TRUE
                 output$filePreview <- renderDataTable(
                   {
                     col_vector_comments <- c(input$idCol, input$commentCols)
                     col_vector_segments <- c(input$idCol, input$segmentCols, input$dateCol)
                     all_cols <- c(input$idCol, input$segmentCols, input$dateCol, input$commentCols)
                     preview_cols <- c(input$idCol, input$segmentCols, input$dateCol)
                     
                     col_vector <- append(col_vector_comments, col_vector_segments)
                     if (any(duplicated(col_vector)))
                     {
                       col_vector <- unique(col_vector)
                     } 
                     
                     col_vector <- col_vector[!is.null(col_vector)]
                     col_vector <- col_vector[col_vector != ""]

                     if (is.null(col_vector)){
                       return(NULL)
                     } else if (length(input$commentCols) >= 1) {
                       
                       print(input$commentCols)

                       preview_table <- comments_raw[, all_cols] %>%
                         mutate(comentarios := coalesce(!!!as.list(comments_raw[, input$commentCols]))) %>%
                         select(preview_cols, "comentarios")
                       
                       comments_etl <<- comments_raw[, col_vector_comments] %>%
                         mutate(comentarios := coalesce(!!!as.list(comments_raw[, input$commentCols]))) %>%
                         select(input$idCol, "comentarios")
                       
                       segments_df <<- comments_raw[, col_vector_segments]
                       
                     }
                     
                     preview_table %>%
                       head(5)
                   }
                 )
               }
  )
  
  observeEvent(input$commentCols,
               {
                 enable("nextBtn")
               }
  )
  
  observeEvent({
    input$commentCols
    input$selectDict},
               {
                 enable("nextBtn")
                 if(flags$commentsOn && flags$dictOn){
                   enable("analyzeText")
                 }
                 
                 output$selectSegmentUI <- renderUI(
                   {
                     input$segmentCols
                     selectInput(inputId = "selectSegment",
                                 label   = "Selecciona Segmentación",
                                 choices = split(isolate(input$segmentCols), isolate(input$segmentCols)),
                                 selected = 0
                     )
                   }
                 )
               }
  )
  
  observeEvent(input$selectSegment,
               {
                 output$segmentItemUI <- renderUI(
                   {
                     segment_factors <- segments_df %>% 
                       select(input$selectSegment) %>%
                       transmute_at(input$selectSegment, as.factor) %>%
                       pull() %>%
                       levels()
                       
                     selectInput(inputId = "segmentItem",
                                 label = "Selecciona el segmento",
                                 choices = split(segment_factors, segment_factors),
                                 selected = 0)
                   }
                 )
               }
  )
  
  observeEvent(input$nextBtn, {
    updateTabItems(session, "tabs", selected = "uploadDictionaries")
  })
  
  observeEvent(input$backBtn, {
    updateTabItems(session, "tabs", selected = "uploadComments")
  })
  
  observeEvent(input$saveDict, {
    inputSweetAlert(inputId = "fileSave",
                    session = session, 
                    title = "nombra el archivo"
    )
  })
  
   observeEvent(input$fileSave,
                {
                  
                  if (!is.null(input$dictDT)) { 
                    new_dict_name <- paste0("data/lexicons/", input$language, "_", input$fileSave, ".rds")
                    edited_dict <- hot_to_r(input$dictDT)
                    saveRDS(edited_dict, new_dict_name)
                  }
                }
   )
   
   observeEvent(input$selectPOS,
     {
       pos_model <<- udpipe_load_model(input$selectPOS)
     }
   )
   
   observeEvent(input$analyzeText,
                {
                  
                  source("Scripts/fun_textAnalysis.R")
                  source("Scripts/fun_plotNssSegments.R")
                  source("Scripts/fun_segmentWordcloud.R")
                  textAnalysis_results <<- textAnalysis(comments_etl, lexicon_2, pos_model)
                  
                  #--- NSS
                  nps <- textAnalysis_results$nps
                  nps <- formatC(nps, digits = 2, format = "f")
                  output$nps <- renderGauge(
                    {
                      gauge(nps,
                            label = "NSS",
                            min = -100,
                            max = 100,
                            gaugeSectors(success = c(20, 100),
                                         warning = c(-20, 20),
                                         danger  = c(-100, -19)
                                         )
                            )
                    }
                  )
                  #--- WordClouds
                  output$globalWordCloud <- renderWordcloud2(
                    {
                      pos_wc <- textAnalysis_results$pos_wc %>%
                        ungroup() %>% 
                        filter(value %in% input$filterPolarity) %>%
                        filter(upos %in% input$filterPOS) %>%
                        select(word=lemma, freq, value) %>%
                        slice(1:input$top_n_wc)
                      
                    wordcloud2(data = pos_wc, size = 2,
                               color = pos_wc$value,
                               maxRotation = 0,
                               minRotation = 0,
                               gridSize = 10)
                    }
                  )
                  #--- Segment NSS
                  output$segmentNPS <- plotly::renderPlotly(
                    {
                      doc_scores <- textAnalysis_results$doc_scores
                      p <- plot_segments_nss(segments_df, doc_scores, input$selectSegment)
                      plotly::ggplotly(p)
                    }
                  )
                  #--- Segment WordCloud
                  output$segmentWordCloud <- renderWordcloud2(
                    {
                      tokens_by_doc <<- textAnalysis_results$tokens_by_doc
                      segment_wc_df <<- segment_wordcloud(segments_df, tokens_by_doc, input$selectSegment, input$segmentItem, input$language) %>%
                        left_join(filter(textAnalysis_results$pos_wc, upos %in% input$filterPolaritySegments), by = c("word_token" = "lemma")) %>%
                        filter(value.x %in% input$filterPolaritySegments) %>%
                        select(word_token, n, value = value.x) %>%
                        distinct() %>%
                        top_n(n = input$top_n_segment_wc, wt = n) 
                        

                      
                      wordcloud2(data = segment_wc_df, size = 2,
                                 color = segment_wc_df$value)
                    }
                  )
                  #--- WordPolarity
                  output$commentsTable <- DT::renderDataTable(textAnalysis_results$word_polarity, server = T)
                }
   )
   

}

# Run the application 
shinyApp(ui = ui, server = server)

