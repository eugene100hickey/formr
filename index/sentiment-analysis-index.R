library(tidyverse)
library(readxl)
library(formr)
library(lubridate)
library(rjson)
library(tidytext)
library(tidymodels)
library(textrecipes)
library(vip)
library(showtext)
library(ggtext)

font_add_google("Lemonada", "Lemonada")
showtext_auto()

theme_set(theme_minimal())
theme_update(text = element_text(family = "Lemonada", size = 20))

sheet_names <- excel_sheets("data/INDEx Survey Student Comments.xlsx")

student <- xlsx::read.xlsx(file = "data/INDEx Survey Student Comments.xlsx", 
                           sheetName = "Survey") %>% 
  janitor::clean_names()

sheet_open <- function(x = "Survey"){
  xlsx::read.xlsx(file = "data/INDEx Survey Student Comments.xlsx", 
                           sheetName = x) %>% 
  janitor::clean_names()
}

z <- map(sheet_names[3:11], sheet_open)


key_word <- "lecture"
digital_experience <- student %>% 
  select(grade = x16_overall_how_would_you_rate_the_quality_of_this_institution_s_digital_provision_software_hardware_learning_environment, 
         text = x22_what_one_thing_should_the_institution_do_or_do_better_to_improve_your_experience_of_digital_teaching_and_learning,
         user_name = unique_response_number) %>% 
  drop_na() %>%
#  unite(col = text, text, text1, sep = ", ", remove = T) %>% 
  mutate(grade = str_remove(grade, " imaginable"), 
         grade = factor(grade, levels = c("Best",
                                          "Excellent",
                                          "Good",
                                          "Average", 
                                          "Poor",
                                          "Awful",
                                          "Worst"))) %>% 
  mutate(keyword = str_detect(text, key_word),
         text = tolower(text))

digital_experience %>% 
  ggplot(aes(x = as.factor(grade), y = ..prop.., group = keyword, fill = keyword)) +
  geom_bar(alpha = 0.7, position = "dodge") +
  labs(x = "", y = "",
       title = glue::glue("Digital Experience based on<br>Keyword <i style = 'color:#8BAA1A;'>{key_word}</i>")) +
  theme(axis.text.y = element_blank(),
        legend.title = element_blank(),
        plot.title = element_markdown())


digital_experience %>%
  count(grade) %>%
  ggplot(aes(as.factor(grade), n)) +
  labs(title = "Digital Experience", x = "", y = "") +
  geom_col(fill = "midnightblue", alpha = 0.7) +
  theme(axis.text.x = element_blank()) +
  coord_flip()

digital_experience <- digital_experience %>% select(-keyword)

## negative comments
digital_experience %>%
  filter(grade %in% c("Worst", "Awful", "Poor")) %>%
  sample_n(5) %>%
  pull(text)

## positive comments
digital_experience %>%
  filter(grade %in% c("Best", "Excellent", "Good")) %>%
  sample_n(5) %>%
  pull(text)

reviews_parsed <- digital_experience %>%
  filter(grade != "Average") %>% 
  mutate(text = str_remove(text, "Expand$")) %>%
  mutate(rating = case_when(
    grade %in% c("Best", "Excellent", "Good") ~ "good",
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


set.seed(123)
review_split <- initial_split(reviews_parsed, strata = rating)
review_train <- training(review_split)
review_test <- testing(review_split)

review_rec <- recipe(rating ~ text, data = review_train) %>%
  step_tokenize(text) %>%
  step_stopwords(text) %>%
  step_stem(text) %>% 
  step_tokenfilter(text, max_tokens = 500) %>%
  step_tfidf(text) %>%
  step_normalize(all_predictors())

review_prep <- prep(review_rec)

review_prep

lasso_spec <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lasso_wf <- workflow() %>%
  add_recipe(review_rec) %>%
  add_model(lasso_spec)

lasso_wf

lambda_grid <- grid_regular(penalty(), levels = 40)

set.seed(123)
review_folds <- bootstraps(review_train, strata = rating)
review_folds

doParallel::registerDoParallel()

set.seed(2020)
lasso_grid <- tune_grid(
  lasso_wf,
  resamples = review_folds,
  grid = lambda_grid,
  metrics = metric_set(roc_auc, ppv, npv)
)


lasso_grid %>%
  collect_metrics()

lasso_grid %>%
  collect_metrics() %>%
  ggplot(aes(penalty, mean, color = .metric)) +
  geom_line(size = 1.5, show.legend = FALSE) +
  facet_wrap(~.metric) +
  scale_x_log10()

best_auc <- lasso_grid %>%
  select_best("roc_auc")
best_auc

final_lasso <- finalize_workflow(lasso_wf, best_auc)

final_lasso

final_lasso %>%
  fit(review_train) %>%
  pull_workflow_fit() %>%
  vi(lambda = best_auc$penalty) %>%
  group_by(Sign) %>%
  top_n(20, wt = abs(Importance)) %>%
  ungroup() %>%
  mutate(
    Importance = abs(Importance),
    Variable = str_remove(Variable, "tfidf_text_"),
    Variable = fct_reorder(Variable, Importance)
  ) %>%
  ggplot(aes(x = Importance, y = Variable, fill = Sign)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Sign, scales = "free_y") +
  labs(y = NULL)

review_final <- last_fit(final_lasso, review_split)

review_final %>%
  collect_metrics()

review_final %>%
  collect_predictions() %>%
  conf_mat(rating, .pred_class)


