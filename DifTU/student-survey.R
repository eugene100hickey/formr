# https://www.rstudio.com/resources/rstudioglobal-2021/using-formr-to-create-r-powered-surveys-with-individualized-feedback/
# https://github.com/jhelvy/surveys-with-formr

library(tidyverse)
library(formr)
library(rjson)
library(lubridate)
library(cluster)
library(dendextend)
library(Rtsne)
library(likert)
library(showtext)

font_add_google("Lemonada", "Lemonada")

showtext_auto()

login_details <- readRDS("../formr_login")

formr_connect(
  email = login_details$email, #enter your elena email here (email = "yourname@live.ie")
  password = login_details$password # and the password here (password = "abcde12345")
)


student <- formr_raw_results(survey_name = "StudentSurvey") %>% 
  mutate(created = as_datetime(created)) %>% 
  filter(created > as_date("2021-04-20"),
         !is.na(session))

student_missing <- rowSums(is.na(student))
student <- student[student_missing<40,]

student <- student %>% 
  mutate(Study = Study %>% 
           str_replace("<p.*?>", "") %>% 
           str_replace("<.*", "") %>% 
           str_trim()
         )

# student_column_types <- lapply(student, class) %>% unlist()
# student_integer_columns <- tibble(column = student_column_types) %>% 
#   filter(column == "integer") %>% 
#   pull(column) %>% 
#   names()

student_info <- fromJSON(file = "../data/StudentSurvey.json")[[2]][[1]]$survey_data[[2]]
best_options <- list(`1` = "Best", 
                     `2` = "Good", 
                     `3` = "Neutral", 
                     `4` = "Poor", 
                     `5` = "Worst")

student_info[45][[1]]$choices <- best_options
student_info[82][[1]]$choices <- best_options
student_info[104][[1]]$choices <- best_options
student_info[105][[1]]$choices <- best_options
student_info[106][[1]]$choices <- best_options
student_info[107][[1]]$choices <- best_options
student_info_names <- map_chr(1:length(student_info), function(x) student_info[x][[1]]$name)

student_extract <- function(column){
  index <- which(student_info_names == column)
  student_info[[index]]$choices %>% 
    str_replace(".*?>", "") %>% 
    str_replace("<.*", "") %>% 
    str_trim()
}


fun <- function(var){
  student_extract({{var}})[as.numeric(student[,{{var}}])]
}


student <- student %>% 
  mutate(Informationcheck = fun('Informationcheck'),
         Feedback = fun('Feedback'),
         Campus = fun('Campus'),
         Age = fun('Age'),
         Gender = fun('Gender'),
         Years = fun('Years'),
         LearningNeeds = fun('LearningNeeds'),
         LearningNeedmet = fun('LearningNeedmet'),
         Reference = fun('Reference'),
         StudyTimwe = fun('StudyTimwe'),
         NotesT = fun('NotesT'),
         AdditionalResources = fun('AdditionalResources'),
         AccessLectures = fun('AccessLectures'),
         InstSupport = fun('InstSupport'),
         InstHealth = fun('InstHealth'),
         SocietyAccess = fun('SocietyAccess'),
         DataPrivacy = fun('DataPrivacy'),
         Support = fun('Support'),
         DigitalExperience = fun('DigitalExperience'),
         FindInfo = fun('FindInfo'),
         Workonline = fun('Workonline'),
         Useeducationalgame = fun('Useeducationalgame'),
         UsePolling = fun('UsePolling'),
         Cr8DigPortfolio = fun('Cr8DigPortfolio'),
         Usepptword = fun('Usepptword'),
         VLEeasyuse = fun('VLEeasyuse'),
         Vlereliable = fun('Vlereliable'),
         Vlemobile = fun('Vlemobile'),
         Vleusebylecturers = fun('Vleusebylecturers'),
         Onlineassesments = fun('Onlineassesments'),
         GoodTeachingSpaces = fun('GoodTeachingSpaces'),
         RelevantSoftware = fun('RelevantSoftware'),
         PersoalDataStorage = fun('PersoalDataStorage'),
         DigitalSkill4Jobs = fun('DigitalSkill4Jobs'),
         Opp2updateDSkills = fun('Opp2updateDSkills'),
         DSkillsneededinCareer = fun('DSkillsneededinCareer'),
         Dworkplaceprep = fun('Dworkplaceprep'),
         InvolinDskillsdecisions = fun('InvolinDskillsdecisions'),
         DTeachingSkills = fun('DTeachingSkills'),
         Usefulresources = fun('Usefulresources'),
         BetterUndertstanding = fun('BetterUndertstanding'),
         Enjoymore = fun('Enjoymore'),
         Independent = fun('Independent'),
         Fitseasily = fun('Fitseasily'),
         LearningPref = fun('LearningPref'),
         Useful = fun('Useful'),
         DigitalLearningPref = fun('DigitalLearningPref'),
         QualityL = fun('QualityL'),
         QualityOL = fun('QualityOL'),
         QualityW = fun('QualityW'),
         QualityLib = fun('QualityLib'),
         Connected2college = fun('Connected2college'),
         Connected2peers = fun('Connected2peers')) %>% 
  mutate(Device = str_split(Device, pattern = ", "),
         AccessDL = str_split(AccessDL, pattern = ", ")) 

student$laptop <- sapply(1:nrow(student), function(x) {"1" %in% student$Device[[x]]})
student$smartphone <- sapply(1:nrow(student), function(x) {"2" %in% student$Device[[x]]})
student$printer <- sapply(1:nrow(student), function(x) {"3" %in% student$Device[[x]]})
student$desktop <- sapply(1:nrow(student), function(x) {"4" %in% student$Device[[x]]})
student$tablet <- sapply(1:nrow(student), function(x) {"5" %in% student$Device[[x]]})
student$other <- sapply(1:nrow(student), function(x) {"6" %in% student$Device[[x]]})
student$no_device <- sapply(1:nrow(student), function(x) {"7" %in% student$Device[[x]]})

student$online <- sapply(1:nrow(student), function(x) {"1" %in% student$AccessDL[[x]]})
student$wifi <- sapply(1:nrow(student), function(x) {"2" %in% student$AccessDL[[x]]})
student$e_books <- sapply(1:nrow(student), function(x) {"3" %in% student$AccessDL[[x]]})
student$file_storage <- sapply(1:nrow(student), function(x) {"4" %in% student$AccessDL[[x]]})
student$recorded_lectures <- sapply(1:nrow(student), function(x) {"5" %in% student$AccessDL[[x]]})
student$internet_training <- sapply(1:nrow(student), function(x) {"6" %in% student$AccessDL[[x]]})
student$none_access <- sapply(1:nrow(student), function(x) {"7" %in% student$AccessDL[[x]]})

student <- student %>% 
  mutate(Support = ifelse(Support == "Family and Friends", 
                          "Friends and family",
                          Support),
         Support = ifelse(Support == "Online video and resources", 
                          "Online videos and resources",
                          Support),
         city = sum(Campus == "City Centre"),
         tallaght = sum(Campus == "Tallaght"),
         blanch = sum(Campus == "Blanchardstown"))

student <- student %>% 
  select(-c(Device, AccessDL))

write_csv(student, file = "../data/student-diftu-tidy.csv")

student_model <- student %>% 
  select(Campus:LearningNeedmet, 
         Reference:Usepptword, 
         VLEeasyuse:DTeachingSkills,
         BetterUndertstanding:Connected2peers,
         laptop:none_access) %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(Years = fct_relevel(Years, levels = c("Less than a year", "1-2 years", "2-3 years", "More than 3 years")),
         FindInfo = fct_relevel(FindInfo, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Workonline = fct_relevel(Workonline, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Useeducationalgame = fct_relevel(Useeducationalgame, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         UsePolling = fct_relevel(UsePolling, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Cr8DigPortfolio = fct_relevel(Cr8DigPortfolio, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Usepptword = fct_relevel(Usepptword, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Reference = fct_relevel(Reference, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         StudyTimwe = fct_relevel(StudyTimwe, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         NotesT = fct_relevel(NotesT, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         AdditionalResources = fct_relevel(AdditionalResources, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         AccessLectures = fct_relevel(AccessLectures, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")))

# https://slcladal.github.io/surveys.html#4_Visualizing_survey_data

student_model %>%
  select(FindInfo:Usepptword) %>% 
  rename("Work online with others" = Workonline,
         "Create a digital record/ portfolio of your learning" = Cr8DigPortfolio,
         "Find information online" = FindInfo,
         "Use an educational game or simulation learning" = Useeducationalgame,
         "Use a polling device / online quiz to give answers in class" = UsePolling,
         "Produce work in digital formats other than Word/PowerPoint" = Usepptword) %>% 
  likert(grouping = student_model %>% pull(Years)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "As part of your course, how often do you…") +
  theme(text = element_text(family = "Lemonada", size = 18),
        legend.title = element_blank())

student_model %>% dplyr::filter(Gender %in% c("Female", "Male")) %>% 
  select(Reference:AccessLectures) %>%
  rename("Manage links or references" = Reference,
         "Organise your study time" = StudyTimwe,
         "Make notes or recordings" = NotesT,
         "Access lecture notes or recorded lectures" = AccessLectures,
         "Look for extra resources not recommended by your lecturer" = AdditionalResources) %>% 
  likert(grouping = student_model %>% 
           dplyr::filter(Gender %in% c("Female", "Male")) %>% 
           pull(Gender)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "In your own learning time, how often do you use digital tools or apps to…") +
  theme(text = element_text(family = "Lemonada", size = 18),
        legend.title = element_blank())

# Wed May  5 14:38:44 2021 ------------------------------
# calculate dissimilarity matrix
# see https://towardsdatascience.com/hierarchical-clustering-on-categorical-data-in-r-a27e578f2995
# uses cluster library
# 
# gower_student <- daisy(student_model, metric = "gower")
# aggl.clust.c <- hclust(gower_student, method = "complete")
# plot(aggl.clust.c,
#      main = "Agglomerative, complete linkages")
# 
# dendro <- as.dendrogram(aggl.clust.c)
# dendro.col <- dendro %>%
#   set("branches_k_color", k = 7, value =   c("darkslategray", "darkslategray4", "darkslategray3", "gold3", "darkcyan", "cyan3", "gold3")) %>%
#   set("branches_lwd", 0.6) %>%
#   set("labels_colors", 
#       value = c("darkslategray")) %>% 
#   set("labels_cex", 0.5)
# ggd1 <- as.ggdend(dendro.col)
# ggplot(ggd1, theme = theme_minimal()) +
#   labs(x = "Num. observations", y = "Height", title = "Dendrogram, k = 7")
# 
# # Radial plot looks less cluttered (and cooler)
# ggplot(ggd1, labels = T) + 
#   scale_y_reverse(expand = c(0.2, 0)) +
#   coord_polar(theta="x")
# 
# # https://towardsdatascience.com/clustering-on-mixed-type-data-8bbd0a2569c3
# 
# gower_mat <- as.matrix(gower_student)
# 
# k <- 3
# pam_fit <- pam(gower_student, diss = TRUE, k)
# pam_results <- student_model %>%
#   mutate(cluster = pam_fit$clustering) %>%
#   group_by(cluster) %>%
#   do(the_summary = summary(.))
# 
# tsne_obj <- Rtsne(gower_student, is_distance = TRUE)
# tsne_data <- tsne_obj$Y %>%
#   data.frame() %>%
#   setNames(c("X", "Y")) %>%
#   mutate(cluster = factor(pam_fit$clustering))
# ggplot(aes(x = X, y = Y), data = tsne_data) +
#   geom_point(aes(color = cluster))
