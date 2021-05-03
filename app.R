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
alpay <- readRDS("alpay.RDS")
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
        filter (str_detect(emotion_type, "negated"))
    y <- emotion  %>%
        select(emotion_type, ave_emotion) %>%
        filter (!str_detect(emotion_type, "negated"))
    emotions <- data.frame("emotion_type" = y$emotion_type,
                           "ave_emotion" = y[,2]-x[,2])
    rates <- data.frame("min" = rep(0,2),"max" = c(0.2309603,0.3316599),"mean" = c(0.04689535,0.07893328))
    negativitiy_index <- scales::rescale_mid(emotions$ave_emotion[1],
                                             from = c(0,rates$max[1]),
                                             to = c(0,100),
                                             mid = rates$mean[1])
    
    positivity_index <- scales::rescale_mid(emotions$ave_emotion[2],
                                            from = c(0,rates$max[2]),
                                            to = c(0,100),
                                            mid = rates$mean[2])
    balance_index <- scales::rescale_mid((emotions$ave_emotion[2]-emotions$ave_emotion[1]),
                                         from = c(-mean(rates$max[1],rates$max[2]),mean(rates$max[1],rates$max[2])),
                                         to = c(0,100),
                                         mid = 0)
    results <- data.frame("Document Name" = basename(dosya),
                          "Total Word Count" = emotion$word_count[1],
                          "Total Negative Word" = emotion[emotion$emotion_type == "negative","emotion_count"][[1]],
                          "Total Negative Negated Word" = emotion[emotion$emotion_type == "negative_negated","emotion_count"][[1]],
                          "Total Positive Word" = emotion[emotion$emotion_type == "positive","emotion_count"][[1]],
                          "Total Positive Negated Word" = emotion[emotion$emotion_type == "positive_negated","emotion_count"][[1]],
                          "Raw Negative Ratio"  =emotions$ave_emotion[1],
                          "Raw Positive Ratio"  =emotions$ave_emotion[2],
                          "Negativity Index" = negativitiy_index,
                          "Positivity Index" = positivity_index,
                          "Balance Index" = balance_index)
    rm(emotions,emotion,rates,x,y,negativitiy_index,positivity_index,text,dosya)
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
                # d <- duygu(inFile$datapath)
                rates <- data.frame("Emotions" = c("Negative","Positive"),
                                    "Historical raw min" = rep(0,2),
                                    "Historical raw max" = c(0.2309603,0.3316599),
                                    "Historical raw mean" = c(0.04689535,0.07893328))
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
                d3 <- bind_cols(as.data.frame(sapply(d2[,c(3:12)], fill_NA)),
                                 "Document.Name.Date.Estimated"= d2$Document.Name.Date.Estimated,
                                 "Document.Name" = d2$Document.Name)
                d4 <- d3 %>% mutate(YEAR = year(Document.Name.Date.Estimated), 
                                   MONTH = month(Document.Name.Date.Estimated)) %>% 
                    dplyr::select(-Document.Name.Date.Estimated,-Document.Name) %>% group_by(YEAR, MONTH) %>%
                    summarise_all(max)
                d4 <- d4  %>%
                    dplyr::select(YEAR,MONTH,Negativity.Index,Positivity.Index,Balance.Index)
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
                    formatRound(columns=c("Raw.Negative.Ratio", 
                                          "Raw.Positive.Ratio",
                                          "Negativity.Index",
                                          "Positivity.Index",
                                          "Balance.Index"), digits=3)
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
                                         "Negativity.Index",
                                         "Positivity.Index",
                                         "Balance.Index"), digits=3)
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
                    labs(x="Document Time", y="Balance Index")
                })} else {NULL}
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
          
    
              
              
              
              
              
              
              
            
            
            
          
        