library(shiny)
library(pdftools)
library(dplyr)
library(tm)
library(stringr)
library(sentimentr)
library(DT)
library(shinyFiles)
library(lubridate)
library(gsubfn)
library(proto)
library(lubridate)
library(ggplot2)
library(tidyr)
alpay <- readRDS("alpay.RDS") ## This is the dictionary file filtered positive negative
duygu <- function(dosya) {
    text <- pdf_text(pdf = dosya)
    text <- paste(text,collapse = " ")
    removeends <- function(x) gsub("\n","",x)
    removeSpecialChars <- function(x) gsub("[^a-zA-Z ]","",x)
    text <- text %>% 
        removeends() %>% 
        tolower() %>% 
        removeSpecialChars() %>% 
        stripWhitespace() %>% 
        PlainTextDocument
    text <- text$content
    rm(removeends,removeSpecialChars)
    emotion <- text %>%
        get_sentences() %>%
        emotion_by(emotion_dt = alpay)
    x <- emotion  %>%
        select(emotion_type, ave_emotion) %>%
        filter (str_detect(emotion_type, "negated")) %>% arrange(as.character(emotion_type))
    y <- emotion  %>%
        select(emotion_type, ave_emotion) %>%
        filter (!str_detect(emotion_type, "negated"))
    emotions <- data.frame("emotion_type" = y$emotion_type,
                           "ave_emotion" = y[,2]-x[,2])
    rates <<- data.frame("emotion_type" = y$emotion_type,
                        "min" = rep(0,7),
                        "max" = c(0.3,0.3,0.2309603,0.3316599,0.3,0.3,0.3),
                        "mean" = c(0.02,0.02,0.04689535,0.07893328,0.01,0.02,0.01))
    index_fun <- function(x) {
        ind <- scales::rescale_mid(emotions[emotions$emotion_type == as.character(x),"ave_emotion"],
                                   from = c(rates[rates$emotion_type == as.character(x),"min"],
                                            rates[rates$emotion_type == as.character(x),"max"]),
                                   to = c(0,100),
                                   mid = rates[rates$emotion_type == as.character(x),"mean"])
        return(ind)
    }
    
    results <- data.frame("Document Name" = basename(dosya),
                          "Total Word Count" = emotion$word_count[1],
                          "Total anticipation Word" = emotion[emotion$emotion_type == "anticipation","emotion_count"][[1]],
                          "Total anticipation Negated Word" = emotion[emotion$emotion_type == "anticipation_negated","emotion_count"][[1]],
                          "Total fear Word" = emotion[emotion$emotion_type == "fear","emotion_count"][[1]],
                          "Total fear Negated Word" = emotion[emotion$emotion_type == "fear_negated","emotion_count"][[1]],
                          "Total Negative Word" = emotion[emotion$emotion_type == "negative","emotion_count"][[1]],
                          "Total Negative Negated Word" = emotion[emotion$emotion_type == "negative_negated","emotion_count"][[1]],
                          "Total Positive Word" = emotion[emotion$emotion_type == "positive","emotion_count"][[1]],
                          "Total Positive Negated Word" = emotion[emotion$emotion_type == "positive_negated","emotion_count"][[1]],
                          "Total surprise Word" = emotion[emotion$emotion_type == "surprise","emotion_count"][[1]],
                          "Total surprise Negated Word" = emotion[emotion$emotion_type == "surprise_negated","emotion_count"][[1]],
                          "Total trust Word" = emotion[emotion$emotion_type == "trust","emotion_count"][[1]],
                          "Total trust Negated Word" = emotion[emotion$emotion_type == "trust_negated","emotion_count"][[1]],
                          "Total uncertainty Word" = emotion[emotion$emotion_type == "uncertainty","emotion_count"][[1]],
                          "Total uncertainty Negated Word" = emotion[emotion$emotion_type == "uncertainty_negated","emotion_count"][[1]],
                          "Raw anticipation Ratio"  =emotions$ave_emotion[1],
                          "Raw fear Ratio"  =emotions$ave_emotion[2],
                          "Raw Negative Ratio"  =emotions$ave_emotion[3],
                          "Raw Positive Ratio"  =emotions$ave_emotion[4],
                          "Raw surprise Ratio"  =emotions$ave_emotion[5],
                          "Raw trust Ratio"  =emotions$ave_emotion[6],
                          "Raw uncertainty Ratio"  =emotions$ave_emotion[7],
                          "anticipation Index" = index_fun(y$emotion_type[1]),
                          "fear Index" = index_fun(y$emotion_type[2]),
                          "Negativity Index" = index_fun(y$emotion_type[3]),
                          "Positivity Index" = index_fun(y$emotion_type[4]),
                          "surprise Index" = index_fun(y$emotion_type[5]),
                          "trust Index" = index_fun(y$emotion_type[6]),
                          "uncertainty Index" = index_fun(y$emotion_type[7]))
    rm(emotions,emotion,x,y,text,dosya)
    return(results)
}
fill_NA <- function(x) {
    which.na <- c(which(!is.na(x)), length(x) + 1)
    values <- na.omit(x)
    
    if (which.na[1] != 1) {
        which.na <- c(1, which.na)
        values <- c(values[1], values)
    }
    
    diffs <- diff(which.na)
    l <- c()
    for (i in 1:length(values)) {
        v <- seq(values[i], 0, length.out = diffs[i])
        l <- c(l,v)
    }
    return(l)
}
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Emotion Index Calculator for Monetary Policy Documents"),
    textOutput("text"),
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h3("Upload your file(s)"),
            fileInput("file1", "Please upload file(s) in pdf format", accept = ".pdf",multiple = T),
            actionButton(inputId="grafikyap","Run")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Table",
           DT::dataTableOutput("sonuc"),
           DT::dataTableOutput("sonuc2"),
           tableOutput("sonuc1")
                ),
           tabPanel("Graph",
                    plotOutput("barplot")
           )
            )
        )
        
        
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    output$text <- renderText({ 
        "The study can be accessed at https://dergipark.org.tr/en/pub/bddkdergisi/issue/58352/841216 'Are CBRT's Monetary Policy Statements Affected by ECB and FED Statements?'; You are at a page where you can make the emotional analysis in that study by uploading your own documents. Filenames should include date with 8 digits (dmy, ymd or mdy, i.e. 01022023, 20230201 or 02012023. If you want to analyse multiple file, please use same dateformat in the filenames."
        
        
    })
    
    
    observeEvent(input$grafikyap,{
            inFile <<- input$file1
            if (is.null(inFile)) {
                return(NULL)
            } else {
                # d <- duygu(inFile$datapath))
                d <- as.data.frame(t(sapply(inFile$datapath, duygu)))
                d[,1] <- sub(".pdf$", "", basename(inFile$name))
                
                if (length(strapplyc(d$Document.Name, "[0-9]{8,}", simplify = TRUE)[[1]]) != 0) {
                 
                d <- d %>% 
                    mutate(Document.Name.Date.Estimated = strapplyc(Document.Name, "[0-9]{8,}", simplify = TRUE)) %>%
                    mutate(Document.Name.Date.Estimated  = as.Date(parse_date_time(Document.Name.Date.Estimated,c("mdy","mdY","Bdy","ymd","dmy", "bd","md","Bdh","mdYHM")))) %>%
                    select(Document.Name,Document.Name.Date.Estimated,everything())
                
                DATE <- data.frame(Document.Name.Date.Estimated =  seq(as.Date(min(d$Document.Name.Date.Estimated)),
                                                                        as.Date(max(d$Document.Name.Date.Estimated)), "days")) } else {d}
                if (nrow(inFile)>1) {
                
                tryCatch({
                d2 <- as.data.frame(sapply(d, unlist))
                d2$Document.Name.Date.Estimated <- as.Date(as.numeric(d2$Document.Name.Date.Estimated),origin = "1970-01-01")
                d2 <- d2 %>% right_join(DATE,by = "Document.Name.Date.Estimated") %>% arrange(Document.Name.Date.Estimated)
                d3 <- bind_cols(as.data.frame(sapply(d2[,c(3:dim(d2)[2])], fill_NA)),
                                 "Document.Name.Date.Estimated"= d2$Document.Name.Date.Estimated,
                                 "Document.Name" = d2$Document.Name)
                d4 <- d3 %>% mutate(YEAR = year(Document.Name.Date.Estimated), 
                                   MONTH = month(Document.Name.Date.Estimated)) %>% 
                    dplyr::select(-Document.Name.Date.Estimated,-Document.Name) %>% group_by(YEAR, MONTH) %>%
                    summarise_all(max)
                d4 <- d4  %>%
                    dplyr::select(YEAR,MONTH, anticipation.Index, fear.Index,
                                  Negativity.Index,
                                  Positivity.Index,
                                  surprise.Index,
                                  trust.Index,
                                  uncertainty.Index)
                },  error = function(e) d4 <<- NULL)} else {d4 <- NULL}
                
            }
            

            
           output$sonuc <- DT::renderDataTable({
                
                DT::datatable(
                    { d },
                    caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
                    extensions = 'Buttons',
                    rownames= FALSE,
                    options = list(
                        paging = FALSE,
                        searching = TRUE,
                        fixedColumns = FALSE,
                        autoWidth = FALSE,
                        ordering = TRUE,
                        dom = 'tB',
                        buttons = c('copy', 'csv', 'excel')
                    ),
                    
                    class = "display"
                ) %>%
                    formatRound(columns=c("Raw.anticipation.Ratio",
                                          "Raw.fear.Ratio",
                                          "Raw.Negative.Ratio",
                                          "Raw.Positive.Ratio",
                                          "Raw.surprise.Ratio",
                                          "Raw.trust.Ratio",
                                          "Raw.uncertainty.Ratio",
                                          "anticipation.Index",
                                          "fear.Index",
                                          "Negativity.Index",
                                          "Positivity.Index",
                                          "surprise.Index",
                                          "trust.Index",
                                          "uncertainty.Index"), digits=3)
            })
           
           output$sonuc2 <- 
               
               if (is.null(d4)) {NULL} else {
               
               DT::renderDataTable({
               
               DT::datatable(
                   { d4 },
                   caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
                   extensions = 'Buttons',
                   rownames= FALSE,
                   options = list(
                       paging = FALSE,
                       searching = TRUE,
                       fixedColumns = FALSE,
                       autoWidth = FALSE,
                       ordering = TRUE,
                       dom = 'tB',
                       buttons = c('copy', 'csv', 'excel')
                   ),
                   
                   class = "display"
               ) %>%
                   formatRound(columns=c(
                       "anticipation.Index",
                       "fear.Index",
                       "Negativity.Index",
                       "Positivity.Index",
                       "surprise.Index",
                       "trust.Index",
                       "uncertainty.Index"), digits=3)
           })
               }
           
           
            output$sonuc1 <- renderTable(rates)
            kk <- length(strapplyc(d$Document.Name, "[0-9]{8,}", simplify = TRUE)[[1]]) == 0
            output$barplot <- 
                
                
                if (nrow(inFile)>1 & kk == FALSE) {
                renderPlot({
                d5 <- d4 %>% ungroup() %>% 
                    mutate(Date = ymd(paste0(YEAR,"-",MONTH,"-","01"))) %>% 
                    select(-YEAR,-MONTH) %>% gather("type", "data",-Date)
                ggplot(d5,aes(Date,data)) + geom_line() + facet_grid(type~.) +
                    labs(x="Document Time", y="uncertainty.Index")
                })} else {NULL}
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
          
    
              
              
              
              
              
              
              
            
            
            
          
        