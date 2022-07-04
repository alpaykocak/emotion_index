#########FED############
library(rdrop2)
library(pdftools)
library(tm)
library(dplyr)
library(sentimentr)
library(stringr)
library(tidyr) 
library(gsubfn)
library(proto)
library(lubridate)
library(mFilter)
outputDir <- "CBTEXTSFOLDER"
filesInfo <- drop_dir(outputDir)
filePath <- as.character(filesInfo[filesInfo$name == "FOMC", "path_display"])
files <- drop_dir(filePath)$path_display
names <- drop_dir(filePath)$name
dir <- tempdir()
for (i in files) {
  drop_download(i, dir, overwrite = T)
}
list.files(dir)
datapath <- paste0(dir,"/",names)

alpay <- readRDS("alpay.RDS") 
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
    # v <- seq(values[i], 0, length.out = diffs[i])
    v <- seq(values[i], values[i], length.out = diffs[i])
    l <- c(l,v)
  }
  return(l)
}
rates <- data.frame("emotion_type" = c("anticipation","fear", "negative", "positive", "surprise", "trust", "uncertainty"),
                    "min" = rep(0,7),
                    "max" = c(0.3,0.3,0.2309603,0.3316599,0.3,0.3,0.3),
                    "mean" = c(0.02,0.02,0.04689535,0.07893328,0.01,0.02,0.01))
duygusal <- function(dosya) {
    text <- pdf_text(pdf = dosya)
    text <- paste(text,collapse = " ")
    removeends <- function(x) gsub("\n"," ",x)
    removeSpecialChars <- function(x) gsub("[^a-zA-Z. ]"," ",x)
    removevoting <- function(x) if (str_detect(x,"federal open market committee") == TRUE) {
      gsub("\\voting.*"," ",x)
    } else {
        x}    
    text <- text %>% 
      removeends() %>% 
      tolower() %>% 
      removeSpecialChars() %>% 
      stripWhitespace() %>% 
      removevoting() %>% 
      PlainTextDocument
    text <- text$content
    rm(removeends,removeSpecialChars,removevoting)
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
}
data_cb <- as.data.frame(t(sapply(datapath, duygusal)))
data_cb <-suppressWarnings(unnest(data_cb))
data_cb <- data_cb %>% 
  mutate(Document.Name.Date.Estimated = strapplyc(Document.Name, "[0-9]{8,}", simplify = TRUE)) %>%
  mutate(Document.Name.Date.Estimated  = as.Date(parse_date_time(Document.Name.Date.Estimated,c("mdy","mdY","Bdy","ymd","dmy", "bd","md","Bdh","mdYHM")))) %>%
  select(Document.Name,Document.Name.Date.Estimated,everything())
DATE <- data.frame(Document.Name.Date.Estimated =  seq(as.Date(min(data_cb$Document.Name.Date.Estimated)),
                                                       as.Date(max(data_cb$Document.Name.Date.Estimated)), "days")) 
data_cb2 <- as.data.frame(sapply(data_cb, unlist))
data_cb2$Document.Name.Date.Estimated <- as.Date(as.numeric(data_cb2$Document.Name.Date.Estimated),origin = "1970-01-01")
data_cb2 <- data_cb2 %>% right_join(DATE,by = "Document.Name.Date.Estimated") %>% arrange(Document.Name.Date.Estimated)
data_cb3 <- bind_cols(as.data.frame(sapply(data_cb2[,c(3:dim(data_cb2)[2])], fill_NA)),
                "Document.Name.Date.Estimated"= data_cb2$Document.Name.Date.Estimated,
                "Document.Name" = data_cb2$Document.Name)
data_cb4 <- data_cb3 %>% mutate(YEAR = year(Document.Name.Date.Estimated), 
                    MONTH = month(Document.Name.Date.Estimated)) %>% 
  dplyr::select(-Document.Name.Date.Estimated,-Document.Name) %>% group_by(YEAR, MONTH) %>%
  summarise_all(max)
data_cb4 <- data_cb4  %>%
  dplyr::select(YEAR,MONTH, anticipation.Index, fear.Index,
                negative.Index,
                positive.Index,
                surprise.Index,
                trust.Index,
                uncertainty.Index) %>% 
  ungroup() %>%
  select(-YEAR,-MONTH) %>%
  as.ts() %>%
  apply(., 2, function(x) {l = hpfilter(c(x),freq=12,type="frequency",drift=F)
  l = l$trend
  return(l)}
  ) %>%
  as_tibble() %>%
  cbind(data_cb4[,c(1,2)],.) %>%
  ungroup()
saveRDS(object = data_cb,"data_cb_fed.RDS")
saveRDS(object = data_cb4,"data_cb4_fed.RDS")
drop_upload(file = "data_cb_fed.RDS", path = "/CBTEXTSFOLDER/Results", mode = "overwrite")
drop_upload(file = "data_cb4_fed.RDS", path = "/CBTEXTSFOLDER/Results", mode = "overwrite")

#########CBRT##################
rstudioapi::restartSession()
library(rdrop2)
library(pdftools)
library(tm)
library(dplyr)
library(sentimentr)
library(stringr)
library(tidyr) 
library(gsubfn)
library(proto)
library(lubridate)
library(mFilter)
outputDir <- "CBTEXTSFOLDER"
filesInfo <- drop_dir(outputDir)
filePath <- as.character(filesInfo[filesInfo$name == "MPC2", "path_display"])
files <- drop_dir(filePath)$path_display
names <- drop_dir(filePath)$name
dir <- tempdir()
for (i in files) {
  drop_download(i, dir, overwrite = T)
}
list.files(dir)
datapath <- paste0(dir,"/",names)

alpay <- readRDS("alpay.RDS") 
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
    # v <- seq(values[i], 0, length.out = diffs[i])
    v <- seq(values[i], values[i], length.out = diffs[i])
    l <- c(l,v)
  }
  return(l)
}
rates <- data.frame("emotion_type" = c("anticipation","fear", "negative", "positive", "surprise", "trust", "uncertainty"),
                    "min" = rep(0,7),
                    "max" = c(0.3,0.3,0.2309603,0.3316599,0.3,0.3,0.3),
                    "mean" = c(0.02,0.02,0.04689535,0.07893328,0.01,0.02,0.01))
duygusal <- function(dosya) {
  text <- pdf_text(pdf = dosya)
  text <- paste(text,collapse = " ")
  removeends <- function(x) gsub("\n"," ",x)
  removeSpecialChars <- function(x) gsub("[^a-zA-Z. ]"," ",x)
  removevoting <- function(x) if (str_detect(x,"federal open market committee") == TRUE) {
    gsub("\\voting.*"," ",x)
    } else if (str_detect(x,"inflation developments") == TRUE) {
      gsub(".*inflation developments", "", x)} else {
        x}
  text <- text %>% 
    removeends() %>% 
    tolower() %>% 
    removeSpecialChars() %>% 
    stripWhitespace() %>% 
    removevoting() %>%
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
}
data_cb <- as.data.frame(t(sapply(datapath, duygusal)))
data_cb <- suppressWarnings(unnest(data_cb))
data_cb <- data_cb %>% 
  mutate(Document.Name.Date.Estimated = strapplyc(Document.Name, "[0-9]{8,}", simplify = TRUE)) %>%
  mutate(Document.Name.Date.Estimated  = as.Date(parse_date_time(Document.Name.Date.Estimated,c("mdy","mdY","Bdy","ymd","dmy", "bd","md","Bdh","mdYHM")))) %>%
  select(Document.Name,Document.Name.Date.Estimated,everything())
DATE <- data.frame(Document.Name.Date.Estimated =  seq(as.Date(min(data_cb$Document.Name.Date.Estimated)),
                                                       as.Date(max(data_cb$Document.Name.Date.Estimated)), "days")) 
data_cb2 <- as.data.frame(sapply(data_cb, unlist))
data_cb2$Document.Name.Date.Estimated <- as.Date(as.numeric(data_cb2$Document.Name.Date.Estimated),origin = "1970-01-01")
data_cb2 <- data_cb2 %>% right_join(DATE,by = "Document.Name.Date.Estimated") %>% arrange(Document.Name.Date.Estimated)
data_cb3 <- bind_cols(as.data.frame(sapply(data_cb2[,c(3:dim(data_cb2)[2])], fill_NA)),
                      "Document.Name.Date.Estimated"= data_cb2$Document.Name.Date.Estimated,
                      "Document.Name" = data_cb2$Document.Name)
data_cb4 <- data_cb3 %>% mutate(YEAR = year(Document.Name.Date.Estimated), 
                                MONTH = month(Document.Name.Date.Estimated)) %>% 
  dplyr::select(-Document.Name.Date.Estimated,-Document.Name) %>% group_by(YEAR, MONTH) %>%
  summarise_all(max)
data_cb4 <- data_cb4  %>%
  dplyr::select(YEAR,MONTH, anticipation.Index, fear.Index,
                negative.Index,
                positive.Index,
                surprise.Index,
                trust.Index,
                uncertainty.Index) %>% 
  ungroup() %>%
  select(-YEAR,-MONTH) %>%
  as.ts() %>%
  apply(., 2, function(x) {l = hpfilter(c(x),freq=12,type="frequency",drift=F)
  l = l$trend
  return(l)}
  ) %>%
  as_tibble() %>%
  cbind(data_cb4[,c(1,2)],.) %>%
  ungroup()
saveRDS(object = data_cb,"data_cb_cbrt.RDS")
saveRDS(object = data_cb4,"data_cb4_cbrt.RDS")
drop_upload(file = "data_cb_cbrt.RDS", path = "/CBTEXTSFOLDER/Results", mode = "overwrite")
drop_upload(file = "data_cb4_cbrt.RDS", path = "/CBTEXTSFOLDER/Results", mode = "overwrite")

##########ECB###########
rstudioapi::restartSession()
library(rdrop2)
library(pdftools)
library(tm)
library(dplyr)
library(sentimentr)
library(stringr)
library(tidyr) 
library(gsubfn)
library(proto)
library(lubridate)
library(readr)
library(xml2)
library(rvest)
library(mFilter)
drop_upload(file = "links.txt", path = "/CBTEXTSFOLDER/ECB", mode = "overwrite")
dir <- tempdir()
list.files(dir)
drop_download("/CBTEXTSFOLDER/ECB/links.txt", dir, overwrite = T)
datapath <- paste0(dir,"/","links.txt")

links <- read_csv(datapath, col_names = TRUE)
link <- links$links
rm(links)
ecb <- data.frame()
for (i in 1:length(link)) {
  ecb[i,1] <-ymd(paste0(substr(gsub("([^0-9])", "\\", link[i]),1,4),"-",
                        substr(gsub("([^0-9])", "\\", link[i]),7,8),"-",
                        substr(gsub("([^0-9])", "\\", link[i]),9,10)))
  ecb[i,2] <- link[i] %>% read_html() %>% html_nodes(xpath="//div[@class='section']") %>% html_text() %>% paste(.,collapse = ",")
}
ecb <- as_tibble(ecb) %>% mutate("ID" = "ECB")
colnames(ecb) <- c("DATE", "TEXT", "ID")
ecb <- select(ecb, "dosya" = DATE, "satir" = TEXT)
alpay <- readRDS("alpay.RDS") 
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
    # v <- seq(values[i], 0, length.out = diffs[i])
    v <- seq(values[i], values[i], length.out = diffs[i])
    l <- c(l,v)
  }
  return(l)
}
rates <- data.frame("emotion_type" = c("anticipation","fear", "negative", "positive", "surprise", "trust", "uncertainty"),
                    "min" = rep(0,7),
                    "max" = c(0.3,0.3,0.2309603,0.3316599,0.3,0.3,0.3),
                    "mean" = c(0.02,0.02,0.04689535,0.07893328,0.01,0.02,0.01))
duygusal <- function(satir,dosya) {
  removeends <- function(x) gsub("\n"," ",x)
  removeSpecialChars <- function(x) gsub("[^a-zA-Z. ]"," ",x)
  # removevoting <- function(x) if (str_detect(x,"federal open market committee") == TRUE) {
  #   gsub("\\voting.*"," ",x)
  # } else if (str_detect(x,"inflation developments") == TRUE) {
  #   gsub(".*inflation developments", "", x)
  # } else if (str_detect(x,"president of the ecb") == TRUE) {
  #   gsub(".*transcript of the questions", "", x)
  # } else {
  #   x
  # }  
  text <- satir %>% 
    removeends() %>%
    tolower() %>%
    removeSpecialChars() %>% 
    stripWhitespace() %>% 
    # removevoting() %>%
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
  
  results <- data.frame("Document Name" = paste0(as.character(dosya),".html"),
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
}
data_cb <- as.data.frame(t(mapply(duygusal,satir = ecb$satir,dosya = ecb$dosya)))
data_cb <- suppressWarnings(unnest(data_cb))
data_cb <- data_cb %>% 
  mutate(Document.Name.Date.Estimated = as.Date(sub(".html$", "", Document.Name))) %>%
  select(Document.Name,Document.Name.Date.Estimated,everything())
DATE <- data.frame(Document.Name.Date.Estimated =  seq(as.Date(min(data_cb$Document.Name.Date.Estimated)),
                                                       as.Date(max(data_cb$Document.Name.Date.Estimated)), "days")) 
data_cb2 <- as.data.frame(sapply(data_cb, unlist))
data_cb2$Document.Name.Date.Estimated <- as.Date(as.numeric(data_cb2$Document.Name.Date.Estimated),origin = "1970-01-01")
data_cb2 <- data_cb2 %>% right_join(DATE,by = "Document.Name.Date.Estimated") %>% arrange(Document.Name.Date.Estimated)
data_cb3 <- bind_cols(as.data.frame(sapply(data_cb2[,c(3:dim(data_cb2)[2])], fill_NA)),
                      "Document.Name.Date.Estimated"= data_cb2$Document.Name.Date.Estimated,
                      "Document.Name" = data_cb2$Document.Name)
data_cb4 <- data_cb3 %>% mutate(YEAR = year(Document.Name.Date.Estimated), 
                                MONTH = month(Document.Name.Date.Estimated)) %>% 
  dplyr::select(-Document.Name.Date.Estimated,-Document.Name) %>% group_by(YEAR, MONTH) %>%
  summarise_all(max)
data_cb4 <- data_cb4  %>%
  dplyr::select(YEAR,MONTH, anticipation.Index, fear.Index,
                negative.Index,
                positive.Index,
                surprise.Index,
                trust.Index,
                uncertainty.Index) %>% 
  ungroup() %>%
  select(-YEAR,-MONTH) %>%
  as.ts() %>%
  apply(., 2, function(x) {l = hpfilter(c(x),freq=12,type="frequency",drift=F)
  l = l$trend
  return(l)}
  ) %>%
  as_tibble() %>%
  cbind(data_cb4[,c(1,2)],.) %>%
  ungroup()
saveRDS(object = data_cb,"data_cb_ecb.RDS")
saveRDS(object = data_cb4,"data_cb4_ecb.RDS")
drop_upload(file = "data_cb_ecb.RDS", path = "/CBTEXTSFOLDER/Results", mode = "overwrite")
drop_upload(file = "data_cb4_ecb.RDS", path = "/CBTEXTSFOLDER/Results", mode = "overwrite")
