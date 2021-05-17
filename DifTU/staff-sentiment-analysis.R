# https://juliasilge.com/blog/animal-crossing/

library(formr)
library(lubridate)
library(tidyverse)
library(rjson)
library(tidytext)
library(tidymodels)
library(textrecipes)
library(vip)
library(showtext)
library(ggtext)
library(SnowballC)

font_add_google("Lemonada", "Lemonada")
showtext_auto()

theme_set(theme_minimal())
theme_update(text = element_text(family = "Lemonada", size = 20))


login_details <- readRDS("../formr_login")

formr_connect(
  email = login_details$email, #enter your elena email here (email = "yourname@live.ie")
  password = login_details$password # and the password here (password = "abcde12345")
)

staff <- formr_raw_results(survey_name = "StaffSurvey") %>% 
  mutate(created = as_datetime(created)) %>% 
  filter(created > as_date("2021-04-20"),
         !is.na(session))

staff_missing <- rowSums(is.na(staff))
staff <- staff[staff_missing<40,]

staff <- staff %>% 
  mutate(Job = Job %>% 
           str_replace("<h5.<p.*?>", "") %>% 
           str_replace("<.*", "") %>% 
           str_trim(),
         Discipline = Discipline %>% 
           str_replace("<h5.<p.*?>", "") %>% 
           str_replace("<.*", "") %>% 
           str_trim(),
         approachtoTech = approachtoTech %>% 
           str_replace("<h5.<p.*?>", "") %>% 
           str_replace("<.*", "") %>% 
           str_trim()
  )

staff_info <- fromJSON(file = "data/StaffSurvey.json")[[2]][[1]]$survey_data[[2]]

staff_info_names <- map_chr(1:length(staff_info), function(x) staff_info[x][[1]]$name)

staff_extract <- function(column){
  index <- which(staff_info_names == column)
  staff_info[[index]]$choices %>% 
    str_replace(".*?>", "") %>% 
    str_replace("<.*", "") %>% 
    str_trim()
}
fun <- function(var){
  staff_extract({{var}})[as.numeric(staff[,{{var}}])]
}


# options for 'grade' below
# qualdigpriv
# R8Support

# free text boxes
# BetterSupp
# GoodDL
# BadDL
# HindersInitatives

textarea <- "HindersInitatives"

staff %>% 
  select(text = {{textarea}}) %>% 
  drop_na() %>%
  unnest_tokens(word, text) %>% 
  anti_join(stop_words, by = "word") %>% 
  count(word, sort = TRUE) %>%
  head(10)

key_word <- "brightspace"
  
digital_experience <- staff %>% 
  select(grade = R8Support, 
         text = {{textarea}}) %>% 
  drop_na() %>%
  mutate(keyword = str_detect(text, key_word),
         text = tolower(text))

digital_experience %>% 
  ggplot(aes(x = as.factor(grade), y = ..prop.., group = keyword, fill = keyword)) +
  geom_bar(alpha = 0.7, position = "dodge") +
  labs(x = "Digital Experience", y = "",
       title = glue::glue("{textarea} based on<br>Keyword <i style = 'color:#8BAA1A;'>{key_word}</i>")) +
  scale_x_discrete(
    breaks = 1:5,
    labels = c("Best\nImaginable",
               "Good",
               "Neutral",
               "Bad",
               "Worst\nImaginable")) +
  theme(axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_markdown())


digital_experience %>%
  count(grade) %>%
  ggplot(aes(as.factor(grade), n)) +
  labs(x = glue::glue("{textarea}"), y = "") +
  scale_x_discrete(
    breaks = 1:5,
    labels = c("Best\nImaginable",
               "Good",
               "Neutral",
               "Bad",
               "Worst\nImaginable")) +
  geom_col(fill = "midnightblue", alpha = 0.7) +
  theme(axis.text.y = element_blank())

digital_experience <- digital_experience %>% select(-keyword)



## negative comments
digital_experience %>%
  filter(grade > 2) %>%
  sample_n(5) %>%
  pull(text)

## positive comments
digital_experience %>%
  filter(grade < 2) %>%
  sample_n(5) %>%
  pull(text)

reviews_parsed <- digital_experience %>%
  filter(grade != 3) %>% 
  mutate(text = str_remove(text, "Expand$")) %>%
  mutate(rating = case_when(
    grade < 3 ~ "good",
    TRUE ~ "bad"
  ))

words_per_review <- reviews_parsed %>%
  unnest_tokens(word, text) %>%
  count(user_name, name = "total_words")

words_per_review %>%
  ggplot(aes(total_words)) +
  geom_histogram(fill = "midnightblue", alpha = 0.8) +
  labs(x = "Total Words per Review", y = "") +
  theme(axis.text.y = element_blank())


