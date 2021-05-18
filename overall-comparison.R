library(tidyverse)
library(likert)
library(showtext)

font_add_google("Neucha", "Neucha")

showtext_auto()

fill_colour <- "#881144"

best_options <- list(`1` = "Best", 
                     `2` = "Good", 
                     `3` = "Neutral", 
                     `4` = "Poor", 
                     `5` = "Worst")

theme_set(theme_minimal())
theme_update(text = element_text(family = "Neucha", size = 20),
          legend.title = element_blank())

student_index <- read_csv("../data/student-index-tidy.csv") %>% 
  select(Campus, everything()) %>% 
  mutate(survey = "index")
student_diftu <- read_csv("../data/student-diftu-tidy.csv") %>% 
  select(-c(Other, DTLkeep)) %>% 
  select(-c(QualityL:none_access)) %>% 
  mutate(survey = "diftu")
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
                                no_device = "None of the above"))

student <- bind_rows(student_diftu, student_index)


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




# Question 5
student_model %>% 
  filter(survey == "diftu") %>% 
  ggplot(aes(fct_infreq(Study), fill = survey)) + 
  geom_bar(show.legend = F, position = "dodge") +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) + 
  scale_x_discrete(labels = scales::wrap_format(30)) +
  labs(title = "Q5. What area is your programme of study?") +
  theme(plot.title.position = "plot",
        title = element_text(size = 30))
ggsave("images/Q05.png")


# Question 6
student_model %>% 
  filter(!is.na(Age), survey == "diftu") %>%
  mutate(Age = glue::glue("{Age} years")) %>% 
  ggplot(aes(fct_rev(Age), fill = survey)) + 
  geom_bar(show.legend = F, position = "dodge") +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) + 
  scale_x_discrete(labels = scales::wrap_format(30)) +
  labs(title = "Q6. How old are you?") +
  theme(plot.title.position = "plot",
        title = element_text(size = 30),
        text = element_text(size = 24))
ggsave("images/Q06.png")

# Question 7
student_model %>% 
  filter(!is.na(Gender)) %>% 
  ggplot(aes(fct_rev(Gender), fill = survey)) + 
  geom_bar(show.legend = F, position = "dodge") +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) + 
  scale_x_discrete(labels = scales::wrap_format(30)) +
  labs(title = "Q7. What gender do you identify as?") +
  theme(plot.title.position = "plot",
        title = element_text(size = 30),
        text = element_text(size = 24))
ggsave("images/Q07.png")


# Question 11
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
                                no_device = "None of the above"))

student_diftu_devices %>% 
  ggplot(aes(fct_infreq(device) %>% fct_rev)) + 
  geom_bar(show.legend = F, position = "dodge", fill = fill_colour) +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank()) + 
  scale_x_discrete(labels = scales::wrap_format(30)) +
  labs(title = "Q11. Which of these personally-owned devices do you use to support your learning? (Choose all that apply)") +
  theme(plot.title.position = "plot",
        title = element_text(size = 30),
        text = element_text(size = 24)) 
ggsave("images/Q11.png")

# Question 12
likert <- student_model %>%
  as.data.frame %>% 
  select(Reference:AccessLectures, survey) %>% 
  drop_na() 

likert %>% 
  select(Reference:AccessLectures) %>%
  rename("Manage links or references" = Reference,
         "Organise your study time" = StudyTimwe,
         "Make notes or recordings" = NotesT,
         "Access lecture notes or recorded lectures" = AccessLectures,
         "Look for extra resources not recommended by your lecturer" = AdditionalResources) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "Q12. In your own learning time, how often do you use digital tools or apps to…")
ggsave("images/Q12.png")


# Question 13
read_csv("../data/student-diftu-tidy.csv") %>% 
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
  ggplot(aes(fct_infreq(access) %>% fct_rev)) + 
  geom_bar(show.legend = F, position = "dodge", fill = fill_colour) +
  coord_flip() +
  theme(axis.text.x = element_blank(),
        axis.title = element_blank(),
        plot.title.position = "plot") + 
  scale_x_discrete(labels = scales::wrap_format(30)) + 
  labs(title = "Q13. Which	of	these	do	you	have	access	to	at	your	institution whenever	you	need	them?	Tick	all	that apply")
ggsave("images/Q13.png")


# Question 14
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(InstSupport:DataPrivacy, survey) %>% 
  drop_na() %>% 
  mutate(DataPrivacy = fct_recode(DataPrivacy, Neutral = "Neutural"))

likert %>% 
  select(InstSupport:DataPrivacy) %>%
  rename("This institution supports me to use my own digital devices" = InstSupport,
         "I can access institution health and wellbeing services online" = InstHealth,
         "I can participate in student union / club / society activities online" = SocietyAccess,
         "This institution protects my data privacy" = DataPrivacy) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "Q14. How much do you agree with the following statements?")
ggsave("images/Q14.png")

# Question 15
student %>% 
  filter(!is.na(Support)) %>% 
  ggplot(aes(Support, fill = survey)) +
  geom_bar(show.legend = F, position = "dodge", aes(y = (..count..)/sum(..count..))) +
  geom_text(aes(group = survey,
                label = scales::percent((..count..)/sum(..count..)),
                y = (..count..)/sum(..count..)), 
            stat= "count", 
            hjust = -.5,
            position = position_dodge(width = 1)) +
  coord_flip() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = "Q15. Who supports you most to use digital technology in your learning?") +
  theme(plot.title.position = "plot")
ggsave("images/Q15.png")


# https://slcladal.github.io/surveys.html#4_Visualizing_survey_data


# Question 17
likert <- student_model %>%
  as.data.frame %>% 
  select(FindInfo:Usepptword, survey) %>% 
  drop_na() 

likert %>% 
  select(FindInfo:Usepptword) %>% 
  rename("Work online with others" = Workonline,
         "Create a digital record/ portfolio of your learning" = Cr8DigPortfolio,
         "Find information online" = FindInfo,
         "Use an educational game or simulation learning" = Useeducationalgame,
         "Use a polling device / online quiz to give answers in class" = UsePolling,
         "Produce work in digital formats other than Word/PowerPoint" = Usepptword) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "Q17. As part of your course, how often do you…") 
ggsave("images/Q17.png")



# Question 18
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(VLEeasyuse:Vleusebylecturers, survey) %>% 
  drop_na() 

likert %>% 
  select(VLEeasyuse:Vleusebylecturers) %>%
  rename("I can easily find things on the VLE" = VLEeasyuse,
         "I rely on it to do my coursework" = Vlereliable,
         "I regularly access it on a mobile device" = Vlemobile,
         "I would like it to be used more by my tutors/instructors" = Vleusebylecturers) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "Q18. How much do you agree with the following statements?")
ggsave("images/Q18.png")


# Question 19
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(Onlineassesments:PersoalDataStorage, survey) %>% 
  drop_na() 

likert %>% 
  select(Onlineassesments:PersoalDataStorage) %>%
  rename("Online assessments are delivered and managed well" = Onlineassesments,
         "Teaching spaces are well designed for the technologies we use" = GoodTeachingSpaces,
         "The software used on my course is industry standard and up-to-date" = RelevantSoftware,
         "I am told how my personal data is stored and used" = PersoalDataStorage) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "Q19. How much do you agree with the following statements?")
ggsave("images/Q19.png")


# Question 20
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(DigitalSkill4Jobs:InvolinDskillsdecisions, survey) %>% 
  drop_na() 

likert %>% 
  select(DigitalSkill4Jobs:InvolinDskillsdecisions) %>%
  rename("Before I started my course I was told what digital skills I would need" = DigitalSkill4Jobs,
         "I have regular opportunities to review and update my digital skills" = Opp2updateDSkills,
         "Digital skills are important in my chosen career" = DSkillsneededinCareer,
         "My course prepares me for the digital workplace" = Dworkplaceprep,
         "Learners are given the chance to be involved in decisions about digital services" = InvolinDskillsdecisions) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "Q20. How much do you agree with the following statements?")
ggsave("images/Q20.png") 


# Question 24
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(BetterUndertstanding:Fitseasily, survey) %>% 
  drop_na() 

likert %>% 
  select(BetterUndertstanding:Fitseasily) %>%
  rename("I understand things better" = BetterUndertstanding,
         "I enjoy learning more" = Enjoymore,
         "I am more independent in my learning" = Independent,
         "I can fit learning into my life more easily" = Fitseasily) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels) + 
  labs(title = "Q24. When digital technologies are used on my course ...")
ggsave("images/Q24.png")

