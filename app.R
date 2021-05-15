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
library(shinydashboard)
library(plotly)
alpay <- readRDS("alpay.RDS") 
rates <- data.frame("emotion_type" = c("anticipation","fear", "negative", "positive", "surprise", "trust", "uncertainty"),
                     "min" = rep(0,7),
                     "max" = c(0.3,0.3,0.2309603,0.3316599,0.3,0.3,0.3),
                     "mean" = c(0.02,0.02,0.04689535,0.07893328,0.01,0.02,0.01))
duygu <- function(dosya) {
    withProgress(message = 'Processing...', value = 10, {
    
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
                          "Total negative Word" = emotion[emotion$emotion_type == "negative","emotion_count"][[1]],
                          "Total negative Negated Word" = emotion[emotion$emotion_type == "negative_negated","emotion_count"][[1]],
                          "Total positive Word" = emotion[emotion$emotion_type == "positive","emotion_count"][[1]],
                          "Total positive Negated Word" = emotion[emotion$emotion_type == "positive_negated","emotion_count"][[1]],
                          "Total surprise Word" = emotion[emotion$emotion_type == "surprise","emotion_count"][[1]],
                          "Total surprise Negated Word" = emotion[emotion$emotion_type == "surprise_negated","emotion_count"][[1]],
                          "Total trust Word" = emotion[emotion$emotion_type == "trust","emotion_count"][[1]],
                          "Total trust Negated Word" = emotion[emotion$emotion_type == "trust_negated","emotion_count"][[1]],
                          "Total uncertainty Word" = emotion[emotion$emotion_type == "uncertainty","emotion_count"][[1]],
                          "Total uncertainty Negated Word" = emotion[emotion$emotion_type == "uncertainty_negated","emotion_count"][[1]],
                          "Raw anticipation Ratio"  =emotions$ave_emotion[1],
                          "Raw fear Ratio"  =emotions$ave_emotion[2],
                          "Raw negative Ratio"  =emotions$ave_emotion[3],
                          "Raw positive Ratio"  =emotions$ave_emotion[4],
                          "Raw surprise Ratio"  =emotions$ave_emotion[5],
                          "Raw trust Ratio"  =emotions$ave_emotion[6],
                          "Raw uncertainty Ratio"  =emotions$ave_emotion[7],
                          "anticipation Index" = index_fun(y$emotion_type[1]),
                          "fear Index" = index_fun(y$emotion_type[2]),
                          "negative Index" = index_fun(y$emotion_type[3]),
                          "positive Index" = index_fun(y$emotion_type[4]),
                          "surprise Index" = index_fun(y$emotion_type[5]),
                          "trust Index" = index_fun(y$emotion_type[6]),
                          "uncertainty Index" = index_fun(y$emotion_type[7]))
    results <-  results %>% mutate_if(is.numeric,  function(x) {(round(x,digits = 5))})
    rm(emotions,emotion,x,y,text,dosya)
    return(results)
    incProgress(1/(length(inFile$datapath)), detail = paste0("Progressed a document"))
    Sys.sleep(1)  
    }
    )
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


ui <- dashboardPage(
    dashboardHeader(title = "Emotion Index Calculator for Monetary Policy Documents",titleWidth = 750),
    dashboardSidebar(
        width = 250,
        sidebarMenu(
            menuItem("Main page", tabName = "anasayfa", icon = icon("igloo")),
                        menuItem("Central Banks", icon = icon("money-bill-wave"),expandedName = "Here, you can find the results CB's MPC statements.",startExpanded = T,
                     # fileInput("file1", "Please upload file(s) in pdf format", accept = ".pdf",multiple = T),
                     # actionButton(inputId="grafikyap","Run"),
                     menuItem("Results", tabName = "sonuclar_cb", icon = icon("book-open"),startExpanded = T,
                              menuItem("FED", tabName = "sonuclar_cb_fed", icon = icon("book-open")),
                              menuItem("ECB", tabName = "sonuclar_cb_ecb", icon = icon("book-open")),
                              menuItem("CBRT", tabName = "sonuclar_cb_cbrt", icon = icon("book-open"))
                              ),
                     menuItem("Time Series", tabName = "zamanserisi_cb", icon = icon("dashboard"),startExpanded = T,
                              menuItem("FED", tabName = "zamanserisi_cb_fed", icon = icon("book-open")),
                              menuItem("ECB", tabName = "zamanserisi_cb_ecb", icon = icon("book-open")),
                              menuItem("CBRT", tabName = "zamanserisi_cb_cbrt", icon = icon("book-open"))),
                     menuItem("Graphics", tabName = "grafikler_cb", icon = icon("bar-chart-o"),startExpanded = T,
                              menuItem("FED", tabName = "grafikler_cb_fed", icon = icon("book-open")),
                              menuItem("ECB", tabName = "grafikler_cb_ecb", icon = icon("book-open")),
                              menuItem("CBRT", tabName = "grafikler_cb_cbrt", icon = icon("book-open")))
            ),
            menuItem("Self-analysis", icon = icon("th"),expandedName = "You can make your own analysis by supplying documents.",startExpanded = T,
                     fileInput("file1", "Please upload file(s) in pdf format", accept = ".pdf",multiple = T),
                     actionButton(inputId="grafikyap","Run"),
                     menuSubItem("Results", tabName = "sonuclar", icon = icon("book-open")),
                     menuSubItem("Time Series", tabName = "zamanserisi", icon = icon("dashboard")),
                     menuSubItem("Graphics", tabName = "grafikler", icon = icon("bar-chart-o"))
            )
        )  
    ),
    dashboardBody(
        tabItems(
            tabItem("anasayfa",
                    fluidRow(
                        box(status = "info",width = 4,
                            h1("Emotion Index Calculator"),
                            p("The study can be accessed at https://dergipark.org.tr/en/pub/bddkdergisi/issue/58352/841216 
                            'Are CBRT's Monetary Policy Statements Affected by ECB and FED Statements?'; 
                            You are at a page where you can make the emotional analysis in that study by uploading your own documents. 
                            Filenames should include date with 8 digits (dmy, ymd or mdy, i.e. 01022023, 20230201 or 02012023. 
                              If you want to analyse multiple file, please use same dateformat in the filenames.")
                            ),
                        
                        box(status = "info",width = 4,
                            h1("Detail Explanation"),
                            p("I consider seven types of sentiments, by following the method suggested by Rinker (2019). 
                              The analysis is performed to obtain sentiments using the dictionaries defined in Hu and Liu (2004), Henry (2008), Loughran and McDonald (2011), Young and Soroka (2012), Mohammad and Turney (2013). 
                              The scores are the ratios of the number of emotionary words to the total number of words in a statement. 
                              I assume the scores to show an exponential decay process running from the date of a document until the next one. 
                              Afterward, I perform the aggregation by taking monthly averages on each score. 
                              The monthly score is calculated by the average of daily sentiments.
                              Finally, I rescale the monthly indexes. The monthly indicators lie between 0 and 100 with 50 mean. 
                              If the index higher than 50, the emotions is favorable on the statements, otherwise unfavorable If the index equals 50, the statements have a neutral sentiment.")
                            ), 
                        
                        box(status = "info",width = 4,
                            h1("Literature"),
                            p("In the literature, several studies have explored the statements’ role in the com- munications strategy of central banks using sentiment analysis. Recently, Bholat (2015), Hansen, McMahon, and Prat (2018), and Stegmann (2019) analyzed the Federal Reserve (FED) statements while Berger, De Haan, and Sturm (2011) and Coenen et al. (2017) examined the European Central Bank (ECB) statements. Kah- veci and Odabaş (2016) and Iglesias, Ortiz, and Rodrigo (2017) studied the Central Bank of the Republic of Turkey (CBRT) statements. The related studies are focused on time series properties of the statements’ sentiment indicators, and a possible relationship between them and economic indicators."),
                            p("There exists a consensus in the literature that the monetary policies of central banks are related to each other symmetrically or asymmetrically (Throop (1994), Siklos and Wohar (1997), Bec, Salem, and Collard (2002)). For instance, Bec, Salem, and Collard (2002) examines non-linear taylor-type monetary reaction functions for US, France and Germany. The authors suggest a model framework which allows one country’s interest rate may have a role in other country’s reaction function which data covers the years between 1982 and 1997. They use Generalized Method of Moments estimation method to estimate threshold models. The study concludes that the inter-related monetary reaction functions for the US, Germany and France can be represented by non-linear models.")
                            )
                        
                    )
            ),
            tabItem("sonuclar", 
                    column(width=10,
                             h1("Results per document"),
                             "Source: The documents that you uploaded. Method(202X)",
                             h2("Raw Results"),
                           selectInput(inputId = "emotions", 
                                       label = "Emotions",
                                       choices = rates$emotion_type,multiple = F,
                                       selected = rates$emotion_type[1],selectize = T),
                           DT::dataTableOutput("raw",width = 1500,height = 250)
                             )
                    ),
            tabItem("sonuclar_cb_fed", 
                    column(width=10,
                           h1("Results per document"),
                           "Source: The documents came from CB's website Method(202X)",
                           h2("Raw Results"),
                           selectInput(inputId = "emotions_cb_fed", 
                                       label = "Emotions",
                                       choices = rates$emotion_type,multiple = F,
                                       selected = rates$emotion_type[1],selectize = T),
                           DT::dataTableOutput("raw_cb_fed",width = 1500,height = 250)
                    )
            ),
            tabItem("sonuclar_cb_cbrt", 
                    column(width=10,
                           h1("Results per document"),
                           "Source: The documents came from CB's website Method(202X)",
                           h2("Raw Results"),
                           selectInput(inputId = "emotions_cb_cbrt", 
                                       label = "Emotions",
                                       choices = rates$emotion_type,multiple = F,
                                       selected = rates$emotion_type[1],selectize = T),
                           DT::dataTableOutput("raw_cb_cbrt",width = 1500,height = 250)
                    )
            ),
            tabItem("sonuclar_cb_ecb", 
                    column(width=10,
                           h1("Results per document"),
                           "Source: The documents came from CB's website Method(202X)",
                           h2("Raw Results"),
                           selectInput(inputId = "emotions_cb_ecb", 
                                       label = "Emotions",
                                       choices = rates$emotion_type,multiple = F,
                                       selected = rates$emotion_type[1],selectize = T),
                           DT::dataTableOutput("raw_cb_ecb",width = 1500,height = 250)
                    )
            ),
            tabItem("zamanserisi", 
                    column(width=10,
                           h1("Results per time indicated"),
                           "Source: The documents that you uploaded. Method(202X)",
                           h2("Emotions time series"),
                           DT::dataTableOutput("timeseries",width = 1500,height = 250)
                           
                    )
                    ),
            tabItem("zamanserisi_cb_fed", 
                    column(width=10,
                           h1("Results per time indicated"),
                           "Source: The documents came from CB's website Method(202X)",
                           h2("Emotions time series"),
                           DT::dataTableOutput("timeseries_cb_fed",width = 1500,height = 250)
                           
                    )
            ),
            tabItem("zamanserisi_cb_cbrt", 
                    column(width=10,
                           h1("Results per time indicated"),
                           "Source: The documents came from CB's website Method(202X)",
                           h2("Emotions time series"),
                           DT::dataTableOutput("timeseries_cb_cbrt",width = 1500,height = 250)
                           
                    )
            ),
            tabItem("zamanserisi_cb_ecb", 
                    column(width=10,
                           h1("Results per time indicated"),
                           "Source: The documents came from CB's website Method(202X)",
                           h2("Emotions time series"),
                           DT::dataTableOutput("timeseries_cb_ecb",width = 1500,height = 250)
                           
                    )
            ),
            tabItem("grafikler", 
                    br(),
                    h1("Graphs per emotion"),
                    "Source: The documents came from CB's website Method(202X)",
                    h2("Time Series Graphs"),
                    br(),
                    column(width=10,
                           plotlyOutput("graph")
                    )
            ),
            tabItem("grafikler_cb_fed", 
                    br(),
                    h1("Graphs per emotion"),
                    "Source: The documents came from CB's website Method(202X)",
                    h2("Time Series Graphs"),
                    br(),
                    column(width=10,
                           plotlyOutput("graph_cb_fed")
                    )
            ),
            tabItem("grafikler_cb_cbrt", 
                    br(),
                    h1("Graphs per emotion"),
                    "Source: The documents came from CB's website Method(202X)",
                    h2("Time Series Graphs"),
                    br(),
                    column(width=10,
                           plotlyOutput("graph_cb_cbrt")
                    )
            ),
            tabItem("grafikler_cb_ecb", 
                    br(),
                    h1("Graphs per emotion"),
                    "Source: The documents came from CB's website Method(202X)",
                    h2("Time Series Graphs"),
                    br(),
                    column(width=10,
                             plotlyOutput("graph_cb_ecb")
                    )
            )
        )
    )
            
)


server <- function(input, output,session) {
    
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
                                  negative.Index,
                                  positive.Index,
                                  surprise.Index,
                                  trust.Index,
                                  uncertainty.Index)
                },  error = function(e) d4 <<- NULL)} else {d4 <- NULL}
                
            }
            

            
           output$raw <- DT::renderDataTable({
                DT::datatable(
                    {d %>% select("Document.Name", "Document.Name.Date.Estimated","Total.Word.Count", contains(input$emotions))},
                    caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
                    extensions = c('Buttons','Scroller'),
                    rownames= FALSE,
                    options = list(
                        deferRender = TRUE,
                        scrollY = 200,
                        scroller = TRUE,
                        lengthMenu = c(10, 25, 50, 100),
                        scrollX = TRUE,
                        pageLength = 1000,
                        paging = TRUE,
                        searching = F,
                        fixedColumns = FALSE,
                        autoWidth = FALSE,
                        ordering = TRUE,
                        dom = 'ftpBRSQ',
                        buttons = c('copy', 'csv', 'excel')
                    ),
                    
                    class = "display"
                ) 
            })
           
           output$timeseries <- 
               
               if (is.null(d4)) {NULL} else {
               
               DT::renderDataTable({
               
               DT::datatable(
                   { d4 },
                   caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
                   extensions = c('Buttons','Scroller'),
                   rownames= FALSE,
                   options = list(
                       deferRender = TRUE,
                       scrollY = 200,
                       scroller = TRUE,
                       lengthMenu = c(10, 25, 50, 100),
                       scrollX = TRUE,
                       pageLength = 1000,
                       paging = TRUE,
                       searching = F,
                       fixedColumns = FALSE,
                       autoWidth = FALSE,
                       ordering = TRUE,
                       dom = 'ftpBRSQ',
                       buttons = c('copy', 'csv', 'excel')
                   ),
                   
                   class = "display"
               ) %>%
                   formatRound(columns=c(
                       "anticipation.Index",
                       "fear.Index",
                       "negative.Index",
                       "positive.Index",
                       "surprise.Index",
                       "trust.Index",
                       "uncertainty.Index"), digits=3)
           })
               }
           
           
            output$sonuc1 <- renderTable(rates)
            kk <- length(strapplyc(d$Document.Name, "[0-9]{8,}", simplify = TRUE)[[1]]) == 0
            output$graph <- 
                
                
                if (nrow(inFile)>1 & kk == FALSE) {
                renderPlotly({
                d5 <- d4 %>% ungroup() %>% 
                    mutate(Date = ymd(paste0(YEAR,"-",MONTH,"-","01"))) %>% 
                    select(-YEAR,-MONTH) %>% gather("type", "data",-Date)
                p <- ggplot(d5,aes(Date,data)) + geom_line() + facet_wrap(type~.) +
                    labs(x="Document Time", y="uncertainty.Index")
                ggplotly(p)
                })} else {NULL}
    })
   
    
    # drop_download(path = "/CBTEXTSFOLDER/Results/data_cb_fed.RDS",overwrite = T)
    data_cb_fed <- readRDS("data_cb_fed.RDS")
    
    output$raw_cb_fed <- DT::renderDataTable({
        DT::datatable(
            {data_cb_fed %>% select("Document.Name", "Document.Name.Date.Estimated","Total.Word.Count", contains(input$emotions_cb_fed))},
            caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
            extensions = c('Buttons','Scroller'),
            rownames= FALSE,
            options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                lengthMenu = c(10, 25, 50, 100),
                scrollX = TRUE,
                pageLength = 1000,
                paging = TRUE,
                searching = F,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'ftpBRSQ',
                buttons = c('copy', 'csv', 'excel')
            ),
            
            class = "display"
        ) 
    })
    
    # drop_download(path = "/CBTEXTSFOLDER/Results/data_cb4_fed.RDS",overwrite = T)
    data_cb4_fed <- readRDS("data_cb4_fed.RDS")
    
    output$timeseries_cb_fed <- DT::renderDataTable({
        DT::datatable(
                    { data_cb4_fed },
                    caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
                    extensions = c('Buttons','Scroller'),
                    rownames= FALSE,
                    options = list(
                        deferRender = TRUE,
                        scrollY = 200,
                        scroller = TRUE,
                        lengthMenu = c(10, 25, 50, 100),
                        scrollX = TRUE,
                        pageLength = 1000,
                        paging = TRUE,
                        searching = F,
                        fixedColumns = FALSE,
                        autoWidth = FALSE,
                        ordering = TRUE,
                        dom = 'ftpBRSQ',
                        buttons = c('copy', 'csv', 'excel')
                    ),
                    
                    class = "display"
                ) %>%
                    formatRound(columns=c(
                        "anticipation.Index",
                        "fear.Index",
                        "negative.Index",
                        "positive.Index",
                        "surprise.Index",
                        "trust.Index",
                        "uncertainty.Index"), digits=3)
            })
        
    output$graph_cb_fed <- renderPlotly({
        data_cb5_fed <- data_cb4_fed %>% ungroup() %>% 
                    mutate(Date = ymd(paste0(YEAR,"-",MONTH,"-","01"))) %>% 
                    select(-YEAR,-MONTH) %>% gather("type", "data",-Date)
               p <- ggplot(data_cb5_fed,aes(Date,data)) + geom_line() + facet_wrap(type~.) +
                    labs(x="Document Time", y="uncertainty.Index")
               ggplotly(p)
            })

    # drop_download(path = "/CBTEXTSFOLDER/Results/data_cb_fed.RDS",overwrite = T)
    data_cb_cbrt <- readRDS("data_cb_cbrt.RDS")
    
    output$raw_cb_cbrt <- DT::renderDataTable({
        DT::datatable(
            {data_cb_cbrt %>% select("Document.Name", "Document.Name.Date.Estimated","Total.Word.Count", contains(input$emotions_cb_cbrt))},
            caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
            extensions = c('Buttons','Scroller'),
            rownames= FALSE,
            options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                lengthMenu = c(10, 25, 50, 100),
                scrollX = TRUE,
                pageLength = 1000,
                paging = TRUE,
                searching = F,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'ftpBRSQ',
                buttons = c('copy', 'csv', 'excel')
            ),
            
            class = "display"
        ) 
    })
    
    # drop_download(path = "/CBTEXTSFOLDER/Results/data_cb4_fed.RDS",overwrite = T)
    data_cb4_cbrt <- readRDS("data_cb4_cbrt.RDS")
    
    output$timeseries_cb_cbrt<- DT::renderDataTable({
        DT::datatable(
            { data_cb4_cbrt },
            caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
            extensions = c('Buttons','Scroller'),
            rownames= FALSE,
            options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                lengthMenu = c(10, 25, 50, 100),
                scrollX = TRUE,
                pageLength = 1000,
                paging = TRUE,
                searching = F,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'ftpBRSQ',
                buttons = c('copy', 'csv', 'excel')
            ),
            
            class = "display"
        ) %>%
            formatRound(columns=c(
                "anticipation.Index",
                "fear.Index",
                "negative.Index",
                "positive.Index",
                "surprise.Index",
                "trust.Index",
                "uncertainty.Index"), digits=3)
    })
    
    output$graph_cb_cbrt<- renderPlotly({
        data_cb5_cbrt <- data_cb4_cbrt %>% ungroup() %>% 
            mutate(Date = ymd(paste0(YEAR,"-",MONTH,"-","01"))) %>% 
            select(-YEAR,-MONTH) %>% gather("type", "data",-Date)
       p <- ggplot(data_cb5_cbrt,aes(Date,data)) + geom_line() + facet_wrap(type~.) +
            labs(x="Document Time", y="uncertainty.Index")
       ggplotly(p)
    })
    
    
    # drop_download(path = "/CBTEXTSFOLDER/Results/data_cb_fed.RDS",overwrite = T)
    data_cb_ecb <- readRDS("data_cb_ecb.RDS")
    
    output$raw_cb_ecb <- DT::renderDataTable({
        DT::datatable(
            {data_cb_ecb %>% select("Document.Name", "Document.Name.Date.Estimated","Total.Word.Count", contains(input$emotions_cb_ecb))},
            caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
            extensions = c('Buttons','Scroller'),
            rownames= FALSE,
            options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                lengthMenu = c(10, 25, 50, 100),
                scrollX = TRUE,
                pageLength = 1000,
                paging = TRUE,
                searching = F,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'ftpBRSQ',
                buttons = c('copy', 'csv', 'excel')
            ),
            
            class = "display"
        ) 
    })
    
    # drop_download(path = "/CBTEXTSFOLDER/Results/data_cb4_fed.RDS",overwrite = T)
    data_cb4_ecb <- readRDS("data_cb4_ecb.RDS")
    
    output$timeseries_cb_ecb <- DT::renderDataTable({
        DT::datatable(
            { data_cb4_ecb },
            caption = paste0("Selected document(s) are analyzed to obtain emotion indicators."),  
            extensions = c('Buttons','Scroller'),
            rownames= FALSE,
            options = list(
                deferRender = TRUE,
                scrollY = 200,
                scroller = TRUE,
                lengthMenu = c(10, 25, 50, 100),
                scrollX = TRUE,
                pageLength = 1000,
                paging = TRUE,
                searching = F,
                fixedColumns = FALSE,
                autoWidth = FALSE,
                ordering = TRUE,
                dom = 'ftpBRSQ',
                buttons = c('copy', 'csv', 'excel')
            ),
            
            class = "display"
        ) %>%
            formatRound(columns=c(
                "anticipation.Index",
                "fear.Index",
                "negative.Index",
                "positive.Index",
                "surprise.Index",
                "trust.Index",
                "uncertainty.Index"), digits=3)
    })
    
    output$graph_cb_ecb <- renderPlotly({
        data_cb5_ecb <- data_cb4_ecb %>% ungroup() %>% 
            mutate(Date = ymd(paste0(YEAR,"-",MONTH,"-","01"))) %>% 
            select(-YEAR,-MONTH) %>% gather("type", "data",-Date)
        p <- ggplot(data_cb5_ecb,aes(Date,data)) + geom_line() + facet_wrap(type~.) +
            labs(x="Document Time", y="uncertainty.Index")
        ggplotly(p)
    })
    
    
    
    }


shinyApp(ui = ui, server = server)
          
    
              
              
              
              
              
              
              
            
            
            
          
        