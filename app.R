library(shiny)
library(readr)
library(utf8)
library(dplyr)
library(tidytext)
library(stringr)
library(tm)
library(tidyr)
library(leaflet)
library(rgdal)
library(shinyalert)
library(textdata)
library(DT)

stripHTML <- function(htmlString) {
    return(gsub("<.*?>", "", htmlString))
}

onlyASCII <- function(stringVector){
    return(gsub('[^\x20-\x7E]', '', stringVector))
}

saatchiTokenize <- function(df){
    
}

options(shiny.maxRequestSize=100*1024^2)

saatchiBigramize <- function(df){
    df %>%
        unnest_tokens(bigram, Comment2, "ngrams", n = 2)
}

bing_sentiments <- tidytext::get_sentiments("bing")
#afinn_sentiments <- tidytext::get_sentiments("afinn")

glossary <- data.frame(Tab = c("Tables", "Tables", "Tables",
                               "Visualizations", "Visualizations",
                               "Visualizations", "Visualizations",
                               "Visualizations", "Visualizations",
                               "Context", "Context", "Context", "Context",
                               "Topics", "Topics", "Topics", "Topics", "Topics",
                               "Maps"), 
                       Term = c("Variables", "Tokenize", "Bigramize",
                                "Top N Contributions", 
                                "Top N Positive/Negative",
                                "Max. Positive Words",
                                "Min. Positive Freq.",
                                "Max Negative Words",
                                "Min. Negative Freq.", 
                                "One Word", "Multiple Words", "One Phrase", "Multiple Phrases",
                                "UDPipe NLP", "LDA", "Naive Bayes", "Decision Trees", "SVM",
                                "Variables"),
                       Definition = c("Select a variable to tokenize or bigramize. This variable will be the subject of analysis in all subsequent tabs. Use this selector to update the variable for analysis.", 
                                      "", "", "", "", "", "", "", "", "", "", "", "", "",
                                      "", "", "", "", ""))

gdal.states <- readRDS("StatesShapeFile.rds")


negative_sentiments <- get_sentiments("bing") %>%
    filter(sentiment == "negative")

positive_sentiments <- get_sentiments("bing") %>%
    filter(sentiment == "positive")

ui <- navbarPage(title = "Saatchi Text Analyzer", 
                 tabPanel("Maps",
                          div(class="outer",
                              tags$head(
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                              ),
                              leafletOutput("leaflet_map", width = "100%", height = "100%"),
                              absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                            draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
                                            width = 330, height = "auto",
                                            h2("Saatchi Maps"),
                                            selectInput("fileType2", "File Type", choices = c(".csv", ".ftr"), selected = ".csv"),
                                            fileInput("file2", "Choose CSV File",
                                                      accept = c(
                                                          "text/csv",
                                                          "text/comma-separated-values,text/plain",
                                                          ".csv", ".ftr")
                                            ),
                                            uiOutput("variables_maps"),
                                            uiOutput("variables_maps_text"),
                                            selectInput("spatialGranularity", label = "Granularity", choices = c("Postcode", "City", "State", "Country")),
                                            actionButton("renderMap", "Go")
                              )
                          )),
                 tabPanel("Glossary",
                          mainPanel(dataTableOutput("glossaryTable"), width = 8)
                 )
)





server <- shinyServer(function(input, output) {
    
 
    output$variables_maps = renderUI({
        selectInput('variables_maps_ui', 'Location', names(maps_data()))
    })
    
    output$variables_maps_text = renderUI({
        selectInput('variables_maps_ui_text', 'Text', names(maps_data()))
    })
    
    maps_data <- reactive({
        inFile <- input$file2
        if(input$fileType2 == ".csv"){
            temp1 <- read.csv(inFile$datapath, header = input$header)
        } else if(input$fileType == ".ftr"){
            temp1 <- read_feather(inFile$datapath)
        }
        temp1
    })
    
    maps_data2 <- reactive({
        uploaded_maps_data_variable_selected <- sym(input$variables_maps_ui)
        uploaded_maps_data_text_field <- sym(input$variables_maps_ui_text)
        
        maps_data_temp <- mutate(maps_data(), 
                                 Cleaned_Location_Text = 
                                     as.character((!!!uploaded_maps_data_variable_selected)),
                                 Cleaned_Hit_Sentence = 
                                     as.character(!!!uploaded_maps_data_text_field))
        
        
        
        maps_data_temp$Cleaned_Location_Text = utf8::utf8_format(maps_data_temp$Cleaned_Location_Text)
        Encoding(maps_data_temp$Cleaned_Location_Text) <- "latin1"
        maps_data_temp$Cleaned_Location_Text <- stripHTML(maps_data_temp$Cleaned_Location_Text)
        maps_data_temp$Cleaned_Location_Text <- onlyASCII(maps_data_temp$Cleaned_Location_Text)
        
        maps_data_temp$Cleaned_Hit_Sentence = utf8::utf8_format(maps_data_temp$Cleaned_Hit_Sentence)
        Encoding(maps_data_temp$Cleaned_Hit_Sentence) <- "latin1"
        maps_data_temp$Cleaned_Hit_Sentence <- stripHTML(maps_data_temp$Cleaned_Hit_Sentence)
        maps_data_temp$Cleaned_Hit_Sentence <- onlyASCII(maps_data_temp$Cleaned_Hit_Sentence)
        
        
        if(input$spatialGranularity == "State"){
            maps_data_temp <- maps_data_temp %>% 
                mutate(State_Cleaned = 
                           case_when(
                               str_detect(Cleaned_Location_Text, ".*queensland.*") ~ "Queensland",
                               str_detect(Cleaned_Location_Text, ".*Queensland.*") ~ "Queensland",
                               str_detect(Cleaned_Location_Text, ".*Qld.*") ~ "Queensland",
                               str_detect(Cleaned_Location_Text, ".*qld.*") ~ "Queensland",
                               str_detect(Cleaned_Location_Text, ".*QLD.*") ~ "Queensland",
                               str_detect(Cleaned_Location_Text, ".*nsw.*") ~ "New South Wales",
                               str_detect(Cleaned_Location_Text, ".*New.*") ~ "New South Wales",
                               str_detect(Cleaned_Location_Text, ".*new.*") ~ "New South Wales",
                               str_detect(Cleaned_Location_Text, ".*NSW.*") ~ "New South Wales",
                               str_detect(Cleaned_Location_Text, ".*Vic.*") ~ "Victoria",
                               str_detect(Cleaned_Location_Text, ".*VIC.*") ~ "Victoria",
                               str_detect(Cleaned_Location_Text, "ACT") ~ "Australian Capital Territory",
                               str_detect(Cleaned_Location_Text, "Act") ~ "Australian Capital Territory",
                               str_detect(Cleaned_Location_Text, ".*Australian Cap.*") ~ "Australian Capital Territory",
                               str_detect(Cleaned_Location_Text, ".*West.*") ~ "Western Australia",
                               str_detect(Cleaned_Location_Text, "WA") ~ "Western Australia",
                               str_detect(Cleaned_Location_Text, ".*South Au.*") ~ "South Australia",
                               str_detect(Cleaned_Location_Text, ".*SA.*") ~ "South Australia",
                               str_detect(Cleaned_Location_Text, ".*South.*") ~ "South Australia",
                               str_detect(Cleaned_Location_Text, ".*south.*") ~ "South Australia",
                               str_detect(Cleaned_Location_Text, ".*Tas.*") ~ "Tasmania",
                               str_detect(Cleaned_Location_Text, ".*TAS.*") ~ "Tasmania",
                               str_detect(Cleaned_Location_Text, ".*North.*") ~ "Northern Territory",
                               str_detect(Cleaned_Location_Text, ".*NT.*") ~ "Northern Territory", 
                               TRUE ~ "NA"
                           ))  
            
            maps_data_temp %>%
                filter(!State_Cleaned == "NA")
        }
        
    })
    
    
    
    percentage_bins = c(0, 1, 5, 10, 20, 40, 60, 80, Inf)
    
    average_sentiment_bins = c(-2, -1, 0, 0.2, 0.5, 0.7, 1, 1.5, Inf)
    
    observeEvent(input$renderMap, {
        if(input$spatialGranularity == "State"){
        #   shinyalert("Mapping", "Your data is being mapped according to Australian states. This should be ready in around 60 seconds!", type = "info")
            maps_data_grouped_subregion <- reactive({
                maps_data2() %>%
                    unnest_tokens(word, Cleaned_Hit_Sentence) %>%
                    inner_join(get_sentiments("nrc")) %>%
                    group_by(State_Cleaned, word) %>%
                    summarise(occurences = n(), `sentiment score` = mean(value)) %>%
                    dplyr::ungroup() %>%
                    dplyr::group_by(State_Cleaned) %>%
                    dplyr::summarise(`Average Sentiment` = round(mean(`sentiment score`), 2)) %>%                
                    dplyr::mutate(STE_NAME16 = State_Cleaned)
            })
            gdal.states <- readRDS("StatesShapeFile.rds")
            gdal.states@data <- dplyr::left_join(gdal.states@data, maps_data_grouped_subregion(), by = "STE_NAME16")
            pal <- colorBin("RdYlGn", domain = maps_data_grouped_subregion()$`Average Sentiment`, bins = average_sentiment_bins)
            leafletProxy("leaflet_map", data = gdal.states) %>%
                clearControls() %>%
                addPolygons(data = gdal.states, #,
                            fillColor = ~pal(gdal.states$`Average Sentiment`),
                            weight = 1,
                            opacity = 1,
                            color = "black",
                            dashArray = "2",
                            fillOpacity = .32,
                            highlight = highlightOptions(
                                weight = 3.5, 
                                color = "white",
                                dashArray = "4",
                                fillOpacity = 0.32,
                                bringToFront = TRUE),
                            layerId = gdal.states@data$STE_NAME16,
                            label = sprintf("<strong>%s</strong><br/>%s",
                                            paste("Average Sentiment: ", gdal.states$`Average Sentiment`, sep = ""),
                                            paste("State: ", gdal.states$STE_NAME16, sep = "")) %>%
                                lapply(htmltools::HTML),
                            labelOptions = labelOptions(
                                style = list("font-weight" = "normal", padding = "3px 8px"),
                                textsize = "15px",
                                direction = "auto")) %>%
                addLegend("bottomright", pal = pal, values = ~average_sentiment_bins,
                          title = "Average Sentiment",
                          opacity = 1)
            
        }
    })
    
    
    output$leaflet_map <- renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            setView(lng = 134.48, lat = -25.73, zoom = 5)
    })
    
    
}
)

shinyApp(ui, server) 

