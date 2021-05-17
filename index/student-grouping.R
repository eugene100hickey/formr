library(tidyverse)
library(readxl)
library(lubridate)
library(showtext)
library(ggtext)
library(cluster)
library(dendextend)
library(Rtsne)

font_add_google("Lemonada", "Lemonada")
showtext_auto()

theme_set(theme_minimal())
theme_update(text = element_text(family = "Lemonada", size = 20))

sheet_names <- excel_sheets("data/INDEx Survey Student Comments.xlsx")

student <- xlsx::read.xlsx(file = "data/INDEx Survey Student Comments.xlsx", 
                           sheetName = "Survey") %>% 
  janitor::clean_names()
names(student) <- names(student) %>% 
  str_sub(1, 12)

sheet_open <- function(x = "Survey"){
  xlsx::read.xlsx(file = "data/INDEx Survey Student Comments.xlsx", 
                  sheetName = x) %>% 
    janitor::clean_names() %>% 
    pull(var = 1)
}

comments <- map(sheet_names[4:11], sheet_open)
names(comments) <- sheet_names[4:11]

comment_df <- comments %>% 
  enframe() %>% 
  unnest(cols = c("name", "value")) %>% 
  drop_na()

student <- student %>% 
  left_join(comment_df, by = c("x22_what_one" = "value")) %>% 
  unique() %>% 
  filter(!is.na(name))


student_missing <- colSums(is.na(student))
student <- student[, student_missing<100]

student_model <- student %>% 
  select(x2_how_many_:x21_overall_, 
         x23_which_of:x29_which_tu) %>% 
  mutate_if(is.character,as.factor)

# calculate dissimilarity matrix
# see https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
# uses cluster library

gower_student <- daisy(student_model, metric = "gower")
aggl.clust.c <- hclust(gower_student, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")

dendro <- as.dendrogram(aggl.clust.c)
dendro.col <- dendro %>%
  set("branches_k_color", k = 7, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3", "gold3")) %>%
  set("branches_lwd", 0.6) %>%
  set("labels_colors",
      value = c("darkslategray")) %>%
  set("labels_cex", 0.5)
ggd1 <- as.ggdend(dendro.col)
ggplot(ggd1, theme = theme_minimal()) +
  labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")

# Radial plot looks less cluttered (and cooler)
ggplot(ggd1, labels = T) +
  scale_y_reverse(expand = c(0.2, 0)) +
  coord_polar(theta="x")

# https://towardsdatascience.com/clustering-on-mixed-type-data-8bbd0a2569c3

gower_mat <- as.matrix(gower_student)

k <- 3
pam_fit <- pam(gower_student, diss = TRUE, k)
pam_results <- student_model %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))

tsne_obj <- Rtsne(gower_student, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = student$name), show.legend = F)


