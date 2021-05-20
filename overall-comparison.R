library(tidyverse)
library(likert)
library(showtext)
library(ggtext)

font_add_google("Tillana", "my_font")

showtext_auto()

fill_colour <- "gray80"
index_colour <- "#7DAF9C"
index_colour <- "#B2CEDE"
diftu_colour <- "#C38964"
font_colour <- "#093824"
font_colour <- "gray20"
font_colour <- "#095D6B"

best_options <- list(`1` = "Best", 
                     `2` = "Good", 
                     `3` = "Neutral", 
                     `4` = "Poor", 
                     `5` = "Worst")

theme_set(theme_minimal())
theme_update(text = element_text(family = "my_font", size = 20),
             legend.title = element_blank(),
             axis.text.y = element_text(colour = font_colour),
             plot.title.position = "plot")

student_index <- read_csv("../data/student-index-tidy.csv") %>% 
  select(Campus, everything()) %>% 
  mutate(survey = "INDEx-2019")
student_diftu <- read_csv("../data/student-diftu-tidy.csv") %>% 
  select(-c(Other, DTLkeep)) %>% 
  select(-c(QualityL:none_access)) %>% 
  mutate(survey = "DifTU-2021")
student_diftu_devices <- read_csv("../data/student-diftu-tidy.csv") %>% 
  select(session, laptop:no_device) %>% 
  pivot_longer(cols = -session, 
               names_to = "device", 
               values_to = "possesses") %>% 
  filter(possesses) %>% 
  mutate(device = recode_factor(device,
                                laptop = "Laptop computer",
                                smartphone = "Smartphone",
                                printer = "Printer",
                                desktop = "Desktop computer",
                                tablet = "Tablet/iPad",
                                other = "Other",
                                no_device = "None of the above")) %>% 
  mutate(survey = "DifTU-2021",
         number_students = nrow(student_diftu))

student_index_devices <- read_csv("../data/student-index-devices.csv") %>% 
  select(session, laptop:no_device) %>% 
  pivot_longer(cols = -session, 
               names_to = "device", 
               values_to = "possesses") %>% 
  filter(possesses) %>% 
  mutate(device = recode_factor(device,
                                laptop = "Laptop computer",
                                smartphone = "Smartphone",
                                printer = "Printer",
                                desktop = "Desktop computer",
                                tablet = "Tablet/iPad",
                                other = "Other",
                                no_device = "None of the above")) %>% 
  mutate(survey = "INDEx-2019",
         number_students = nrow(student_index))

student <- bind_rows(student_diftu, student_index)
student_devices <- bind_rows(student_diftu_devices, student_index_devices)


student_model <- student %>% 
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
         AccessLectures = fct_relevel(AccessLectures, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Study = fct_lump_min(Study, min = 50)
         )


student_model_2 <- student %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(Years = fct_relevel(Years, levels = c("Less than a year", "1-2 years", "2-3 years", "More than 3 years")),
         AccessLectures = fct_relevel(AccessLectures, levels = c("Agree", "Neutral", "Disagree")),
         InstSupport = fct_relevel(InstSupport, levels = c("Agree", "Neutral", "Disagree")),
         InstHealth = fct_relevel(InstHealth, levels = c("Agree", "Neutral", "Disagree")),
         SocietyAccess = fct_relevel(SocietyAccess, levels = c("Agree", "Neutral", "Disagree")),
         DataPrivacy = fct_relevel(DataPrivacy, levels = c("Agree", "Neutral", "Disagree")),
         VLEeasyuse = fct_relevel(VLEeasyuse, levels = c("Agree", "Neutral", "Disagree")),
         Vlereliable = fct_relevel(Vlereliable, levels = c("Agree", "Neutral", "Disagree")),
         Vlemobile = fct_relevel(Vlemobile, levels = c("Agree", "Neutral", "Disagree")),
         Vleusebylecturers = fct_relevel(Vleusebylecturers, levels = c("Agree", "Neutral", "Disagree")),
         Onlineassesments = fct_relevel(Onlineassesments, levels = c("Agree", "Neutral", "Disagree")),
         GoodTeachingSpaces = fct_relevel(GoodTeachingSpaces, levels = c("Agree", "Neutral", "Disagree")),
         RelevantSoftware = fct_relevel(RelevantSoftware, levels = c("Agree", "Neutral", "Disagree")),
         PersoalDataStorage = fct_relevel(PersoalDataStorage, levels = c("Agree", "Neutral", "Disagree")),
         DigitalSkill4Jobs = fct_relevel(DigitalSkill4Jobs, levels = c("Agree", "Neutral", "Disagree")),
         Opp2updateDSkills = fct_relevel(Opp2updateDSkills, levels = c("Agree", "Neutral", "Disagree")),
         DSkillsneededinCareer = fct_relevel(DSkillsneededinCareer, levels = c("Agree", "Neutral", "Disagree")),
         Dworkplaceprep = fct_relevel(Dworkplaceprep, levels = c("Agree", "Neutral", "Disagree")),
         InvolinDskillsdecisions = fct_relevel(InvolinDskillsdecisions, levels = c("Agree", "Neutral", "Disagree")),
         BetterUndertstanding = fct_relevel(BetterUndertstanding, levels = c("Agree", "Neutral", "Disagree")),
         Enjoymore = fct_relevel(Enjoymore, levels = c("Agree", "Neutral", "Disagree")),
         Independent = fct_relevel(Independent, levels = c("Agree", "Neutral", "Disagree")),
         Fitseasily = fct_relevel(Fitseasily, levels = c("Agree", "Neutral", "Disagree")),)

# Question 2

q02 <- student_model %>% 
  filter(!is.na(Years)) %>% 
  mutate(Years = Years %>% fct_recode("Less than 1 year" = "Less than a year",
                                      "1 to 2 years" = "1-2 years",
                                      "2 to 3 years" = "2-3 years")) %>% 
  count(Years, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = fct_rev(survey)) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(Years), fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 5), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q02. How many years have you studied at this institution?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q02

# Question 5

q05 <- student_model %>% 
  filter(survey == "DifTU-2021") %>% 
  mutate(Study = fct_infreq(Study)) %>% 
  count(Study) %>% 
  ggplot(aes(n, fct_rev(Study))) +
  geom_col(fill = fill_colour, width = 0.8) +
  geom_text(aes(label = glue::glue("{n}  ({round(n / sum(n) * 100, 0)} %)"), 
                x = min(n/2)), 
            size = 6, colour = font_colour,
            family = "my_font",
            fontface = "bold") +
  scale_y_discrete(labels = scales::wrap_format(30)) +
  labs(title = "Q5. What area is your programme of study?") +
  theme(plot.title.position = "plot",
        title = element_text(size = 20),
        axis.text.x = element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_text(lineheight = 0.8),
        panel.grid = element_blank())
q05


# Question 6
q06 <- student_model %>%  
  filter(survey == "DifTU-2021", !is.na(Age)) %>% 
  mutate(Age = glue::glue("{Age} years")) %>% 
  count(Age) %>% 
  ggplot(aes(n, fct_rev(Age))) +
  geom_col(fill = fill_colour, width = 0.8) +
  geom_text(aes(label = glue::glue("{n}  ({round(n / sum(n) * 100, 0)} %)"), 
                x = 100), 
            size = 6, colour = font_colour,
            family = "my_font",
            fontface = "bold") +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) + 
  labs(title = "Q6. How old are you?") +
  theme(plot.title.position = "plot",
        title = element_text(size = 30),
        text = element_text(size = 24),
        panel.grid = element_blank())
q06

# Question 7
q07 <- student_model %>% 
  filter(!is.na(Gender)) %>% 
  ggplot(aes(fct_rev(Gender), fill = survey)) + 
  geom_bar(show.legend = F, position = "dodge") +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) + 
  scale_x_discrete(labels = scales::wrap_format(30)) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  labs(title = glue::glue("Q7. What gender do you identify as? (<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown(size = 28))
q07

# Question 8

q08 <- student_model %>% 
  filter(!is.na(LearningNeeds)) %>% 
  mutate(LearningNeeds = ifelse(str_detect(LearningNeeds, "Yes"), "Yes", "No")) %>% 
  count(LearningNeeds, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = fct_rev(survey)) %>%
  ungroup() %>% 
  ggplot(aes(percentage, LearningNeeds, fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 8), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q08. Do you use any assistive technologies to meet your learning needs?<br>(e.g. screen readers, voicerecognition, switches)<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q08

# Question 9

q09 <- student_model %>% 
  filter(!is.na(LearningNeedmet)) %>% 
  mutate(LearningNeeds = ifelse(str_detect(LearningNeedmet, "Yes"), "Yes", "No")) %>% 
  count(LearningNeedmet, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = fct_rev(survey)) %>%
  ungroup() %>% 
  ggplot(aes(percentage, LearningNeedmet, fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 8), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q09. f YES, has your institution provided you with any support with assistive technologies?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q09


# Question 11
q11 <- student_devices %>% 
  filter(!(device == "Other")) %>% 
  mutate(device = fct_infreq(device)) %>% 
  count(device, survey, number_students) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/number_students*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(device), fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 10), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q11. Which of these personally-owned devices do you use to support your learning?<br>(Choose all that apply) (<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q11

# Question 12
likert <- student_model %>%
  as.data.frame %>% 
  select(Reference:AccessLectures, survey) %>% 
  drop_na() 

q12 <- likert %>% 
  select(Reference:AccessLectures) %>%
  rename("Manage links or references" = Reference,
         "Organise your study time" = StudyTimwe,
         "Make notes or recordings" = NotesT,
         "Access lecture notes or recorded lectures" = AccessLectures,
         "Look for extra resources not recommended by your lecturer" = AdditionalResources) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = toupper("Q12. In your own learning time, how often do you use digital tools or apps to…")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20))
q12


# Question 13
q13 <- read_csv("../data/student-diftu-tidy.csv") %>% 
  select(session, online:none_access) %>% 
  pivot_longer(cols = -session, 
               names_to = "access", 
               values_to = "possesses") %>% 
  filter(possesses) %>% 
  mutate(access = recode_factor(access,
                                online = "Online course materials",
                                wifi = "Reliable Wi-Fi",
                                e_books = "e-books and e-journals",
                                file_storage = "File storage and back-up",
                                recorded_lectures = "Recorded lectures",
                                internet_training = "Internet-based skills training",
                                none_access = "None of the above")) %>% 
  mutate(access = fct_infreq(access)) %>% 
  count(access) %>% 
  ggplot(aes(n, fct_rev(access))) + 
  geom_col(show.legend = F, position = "dodge", fill = fill_colour, width = 0.7) +
  geom_text(aes(label = glue::glue("{n} ({round(n/nrow(student_diftu)*100, 0)} %)")), x = 200, colour = font_colour, family = "my_font", size = 6) +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot",
        plot.title = element_markdown(),
        panel.grid = element_blank()) + 
  labs(title = "Q13. Which	of	these	do	you	have	access	to	at	your	institution whenever	you	need	them?<br>(Choose all that apply)")
q13

# Question 14
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(InstSupport:DataPrivacy, survey) %>% 
  drop_na() %>% 
  mutate(DataPrivacy = fct_recode(DataPrivacy, Neutral = "Neutural"))

q14 <- likert %>% 
  select(InstSupport:DataPrivacy) %>%
  rename("This institution supports me to use my own digital devices" = InstSupport,
         "I can access institution health and wellbeing services online" = InstHealth,
         "I can participate in student union / club / society activities online" = SocietyAccess,
         "This institution protects my data privacy" = DataPrivacy) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q14. How much do you agree with the following statements?")

q14

# Question 15
q15 <- student_model %>% 
  filter(!is.na(Support)) %>% 
  mutate(Support = fct_infreq(Support)) %>% 
  count(Support, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(Support), fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 5), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q15. Who supports you most to use digital technology in your learning?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q15
# https://slcladal.github.io/surveys.html#4_Visualizing_survey_data


# Question 16
likert <- student_model %>%
  as.data.frame %>% 
  select(DigitalExperience, survey) %>% 
  mutate(DigitalExperience = recode_factor(DigitalExperience,
                                           'Worst imaginable' = 'Worst',
                                           'Best imaginable' = 'Best',
                                           'Neutral' = 'Average'),
         DigitalExperience = DigitalExperience %>% fct_relevel(c("Best", 
                                                                 "Excellent",
                                                                 "Good",
                                                                 "Average",
                                                                 "Poor",
                                                                 "Awful",
                                                                 "Worst"))) %>% 
  drop_na() 

q16 <- likert %>% 
  select(DigitalExperience) %>% 
  rename(" Overall, how would you rate the quality of this institution's digital provision\n(software, hardware,learning environment)?" = DigitalExperience) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q16.")  +
  guides(fill = guide_legend(nrow = 1))
q16

# Question 17
likert <- student_model %>%
  as.data.frame %>% 
  select(FindInfo:Usepptword, survey) %>% 
  drop_na() 

q17 <- likert %>% 
  select(FindInfo:Usepptword) %>% 
  rename("Work online with others" = Workonline,
         "Create a digital record/ portfolio of your learning" = Cr8DigPortfolio,
         "Find information online" = FindInfo,
         "Use an educational game or simulation learning" = Useeducationalgame,
         "Use a polling device / online quiz to give answers in class" = UsePolling,
         "Produce work in digital formats other than Word/PowerPoint" = Usepptword) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = "Q17. As part of your course, how often do you…") 
q17



# Question 18
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(VLEeasyuse:Vleusebylecturers, survey) %>% 
  drop_na() 

q18 <- likert %>% 
  select(VLEeasyuse:Vleusebylecturers) %>%
  rename("I can easily find things on the VLE" = VLEeasyuse,
         "I rely on it to do my coursework" = Vlereliable,
         "I regularly access it on a mobile device" = Vlemobile,
         "I would like it to be used more by my tutors/instructors" = Vleusebylecturers) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = "Q18. How much do you agree with the following statements?")
q18


# Question 19
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(Onlineassesments:PersoalDataStorage, survey) %>% 
  drop_na() 

q19 <- likert %>% 
  select(Onlineassesments:PersoalDataStorage) %>%
  rename("Online assessments are delivered and managed well" = Onlineassesments,
         "Teaching spaces are well designed for the technologies we use" = GoodTeachingSpaces,
         "The software used on my course is industry standard and up-to-date" = RelevantSoftware,
         "I am told how my personal data is stored and used" = PersoalDataStorage) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q19. How much do you agree with the following statements?")
q19


# Question 20
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(DigitalSkill4Jobs:InvolinDskillsdecisions, survey) %>% 
  drop_na() 

q20 <- likert %>% 
  select(DigitalSkill4Jobs:InvolinDskillsdecisions) %>%
  rename("Before I started my course I was told what digital skills I would need" = DigitalSkill4Jobs,
         "I have regular opportunities to review and update my digital skills" = Opp2updateDSkills,
         "Digital skills are important in my chosen career" = DSkillsneededinCareer,
         "My course prepares me for the digital workplace" = Dworkplaceprep,
         "Learners are given the chance to be involved in decisions about digital services" = InvolinDskillsdecisions) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q20. How much do you agree with the following statements?")
q20

# Question 21
likert <- student_model %>%
  as.data.frame %>% 
  select(DTeachingSkills, survey) %>% 
  mutate(DTeachingSkills = recode_factor(DTeachingSkills,
                                           'Worst imaginable' = 'Worst',
                                           'Best imaginable' = 'Best',
                                           'Neutral' = 'Average'),
         DTeachingSkills = DTeachingSkills %>% fct_relevel(c("Best", 
                                                                 "Excellent",
                                                                 "Good",
                                                                 "Average",
                                                                 "Poor",
                                                                 "Awful",
                                                                 "Worst"))) %>% 
  drop_na() 

q21 <- likert %>% 
  select(DTeachingSkills) %>% 
  rename(" Overall, how would you rate the quality of digital teaching and learning on your course?" = DTeachingSkills) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q21.")  +
  guides(fill = guide_legend(nrow = 1))
q21

# Question 23
q23 <- student_model %>% 
  select(Usefulresources, survey) %>% 
  filter(!is.na(Usefulresources), !(Usefulresources == "Other")) %>% 
  mutate(Usefulresources = Usefulresources %>% recode_factor("Course-related videos" = "... course-related videos",
                                                             "Interactive polls/quizzes in class" = "... interactive polls/quizzes in class",
                                                             "Proactive questions available online" = "... practice questions available online",
                                                             "References and readings" = "... references and readings",
                                                             "Time working online with other students" = "... time working online with other students")) %>% 
  mutate(Usefulresources = fct_infreq(Usefulresources)) %>% 
  count(Usefulresources, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(Usefulresources), fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 5), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q23. Which of these would be most useful to you as a learner? More ...<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q23

# Question 24
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(BetterUndertstanding:Fitseasily, survey) %>% 
  drop_na() 

q24 <- likert %>% 
  select(BetterUndertstanding:Fitseasily) %>%
  rename("I understand things better" = BetterUndertstanding,
         "I enjoy learning more" = Enjoymore,
         "I am more independent in my learning" = Independent,
         "I can fit learning into my life more easily" = Fitseasily) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = "Q24. When digital technologies are used on my course ...")
q24

# Question 25
q25 <- student_model %>% 
  select(LearningPref, survey) %>% 
  filter(!is.na(LearningPref), !(LearningPref == "Other")) %>% 
  mutate(LearningPref = LearningPref %>% recode_factor("I prefer learning on my own" = "I prefer to learn on my own")) %>% 
  mutate(LearningPref = fct_infreq(LearningPref)) %>% 
  count(LearningPref, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(LearningPref), fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 10), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q25. Which best describes your preferences as a learner?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q25


# Question 26
q26 <- student_model %>% 
  select(Useful, survey) %>% 
  filter(!is.na(Useful)) %>% 
  mutate(Useful = Useful %>% recode_factor("More laptops and tablets available in class" = "More laptops/tablets available in class",
                                           "More laptops and tablets available on long-term loan" = "More laptops/tablets on long-term loan.")) %>% 
  mutate(Useful = fct_infreq(Useful)) %>% 
  count(Useful, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(Useful), fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 10), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q26. Which of these would be most useful to you?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q26
