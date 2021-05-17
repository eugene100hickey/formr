# 

library(tidyverse)
library(quanteda)
library(quanteda.textstats)
library(quanteda.corpora)

student <- read_csv("data/student_tidy.csv")

digital_experience <- student %>% 
  select(text = Usefuldigitalactivity,
         text1 = DTLkeep, 
         id = session) %>% 
  drop_na() %>%
  unite(col = text, text, text1, sep = ": ", remove = T)

corp <- corpus(digital_experience, text_field = "text")
names(corp) <- abbreviate(digital_experience$id, 10)

toks <- tokens(corp, remove_punct = T)
toks <- tokens_select(toks, 
                             pattern = stopwords("en"), 
                             selection = "remove", 
                             padding = TRUE)

kwic_corp <- kwic(toks, pattern = "brightspace")
dfm <- dfm(toks)
topfeatures(dfm, 10)
dfm_tfidf <- dfm_tfidf(dfm)

toks_news_cap <- tokens_select(toks, 
                               pattern = "^[A-Z]",
                               valuetype = "regex",
                               case_insensitive = FALSE, 
                               padding = TRUE)
tstat_col_cap <- textstat_collocations(toks_news_cap, min_count = 1, tolower = FALSE)
head(tstat_col_cap, 20)
