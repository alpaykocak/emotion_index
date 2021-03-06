library(dplyr)
library(magrittr)
library(tidytext)
library(qdapTools)
library(quanteda)
library(stringr)
library(SentimentAnalysis)
library(tidyr)
bing <- get_sentiments("bing")
loughran <- get_sentiments("loughran")
nrc <- get_sentiments("nrc")
tidysentiments <- full_join(bing, loughran) %>% full_join(nrc)
tidysentiments <- full_join(bing, nrc)
LSD2015 <- list2df(data_dictionary_LSD2015) %>%
  filter(X2 %in% c("positive", "negative")) %>%
  rename("word" = X1, "sentiment" = X2)
LSD2015$word <- str_replace_all(LSD2015$word, "[[:punct:]]", "")
GI <- list2df(DictionaryGI) %>% rename("word" = X1, "sentiment" = X2)
HE <- list2df(DictionaryHE) %>% rename("word" = X1, "sentiment" = X2)
LM <- list2df(DictionaryLM) %>% rename("word" = X1, "sentiment" = X2)
SentimentAnalysis_sentiments <- full_join(GI, HE) %>% full_join( LM)
alpay <- full_join(tidysentiments, SentimentAnalysis_sentiments) %>%
  full_join(sentiments) %>%
  full_join(LSD2015)
colnames(alpay) <- c('token', 'emotion')
alpay <- alpay[duplicated(alpay) == FALSE,] %>%
  filter(emotion %in% c("positive", "negative","trust",
                        "fear","surprise","anticipation","uncertainty"))
alpay <- data.table::as.data.table(alpay)
alpay <- data.table::setkey(alpay,token)
rm(bing, GI,HE,LM,loughran, LSD2015,nrc,SentimentAnalysis_sentiments,tidysentiments)
saveRDS(object = alpay,"alpay.RDS")
dict <- data.frame(table(alpay)) %>% spread(emotion,Freq)
xlsx::write.xlsx(x = dict,file = "sozluk.xlsx",sheetName = "Sayfa1")
