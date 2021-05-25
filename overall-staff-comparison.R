library(tidyverse)
library(likert)
library(showtext)
library(ggtext)
library(patchwork)

font_add_google("Tillana", "my_font")

showtext_auto()

fill_colour <- "gray80"
index_colour <- "#81A88D"
diftu_colour <- "#F2300F"
font_colour <- "#24281A"

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

staff_index <- read_csv("../data/staff-index-tidy.csv") %>% 
  mutate(survey = "INDEx-2019",
         R8Support = ifelse(R8Support == "Excellent", "Best", R8Support),
         R8Support = ifelse(R8Support == "Average", "Neutral", R8Support),
         R8Support = ifelse(R8Support == "Awful", "Worst", R8Support),
         qualdigpriv = ifelse(qualdigpriv == "Excellent", "Best", qualdigpriv),
         qualdigpriv = ifelse(qualdigpriv == "Average", "Neutral", qualdigpriv),
         qualdigpriv = ifelse(qualdigpriv == "Awful", "Worst", qualdigpriv))


staff_diftu <- read_csv("../data/staff-diftu-tidy.csv") %>% 
  select(Campus, Job, Discipline:BetterSupp) %>% 
  select(-c(RecordL, DToolsandApps)) %>% 
  mutate(survey = "DifTU-2021")


staff <- bind_rows(staff_diftu, staff_index %>% select(-c(wifi:none_access)))
# student_devices <- bind_rows(student_diftu_devices, student_index_devices)
# student_access <- bind_rows(student_diftu_access, student_index_access)

staff_diftu_access <- read_csv("../data/staff-diftu-tidy.csv") %>% 
  select(Campus, vle:wifi) %>% 
  pivot_longer(cols = -Campus, 
               names_to = "access", 
               values_to = "possesses") %>% 
  filter(possesses) %>% 
  mutate(access = recode_factor(access,
                                vle = "A virtual learning environment",
                                wifi = "Reliable Wi-Fi",
                                e_books = "e-books and e-journals",
                                file_storage = "File storage and back-up",
                                lecture_capture = "Lecture capture",
                                internet_training = "Internet-based skills training"))%>% 
  mutate(survey = "DifTU-2021",
         number_students = nrow(staff_diftu))

staff_index_access <- read_csv("../data/staff-index-tidy.csv") %>% 
  select(Campus, wifi:internet_training) %>% 
  pivot_longer(cols = -c(Campus), 
               names_to = "access", 
               values_to = "possesses") %>% 
  filter(possesses) %>% 
  mutate(access = recode_factor(access,
                                vle = "A virtual learning environment",
                                wifi = "Reliable Wi-Fi",
                                e_books = "e-books and e-journals",
                                file_storage = "File storage and back-up",
                                lecture_capture = "Lecture capture",
                                internet_training = "Internet-based skills training")) %>% 
  mutate(survey = "INDEx-2019",
         number_students = nrow(staff_index))

staff_access <- bind_rows(staff_diftu_access, staff_index_access)


staff_model <- staff %>% 
  mutate_if(is.character,as.factor) %>% 
  mutate(Webinars = fct_relevel(Webinars, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         CreateDigForms = fct_relevel(CreateDigForms, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Polls = fct_relevel(Polls, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         DigFeedback = fct_relevel(DigFeedback, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         OnlineDTeachRes = fct_relevel(OnlineDTeachRes, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         DisTeachWPeers = fct_relevel(DisTeachWPeers, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         DTeachDevResearch = fct_relevel(DTeachDevResearch, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         DevelopDTSkills = fct_relevel(DevelopDTSkills, levels = c("Daily", "Weekly or more", "Monthly or less", "Never")),
         Discipline = fct_lump_min(Discipline, min = 20),
         R8Support = fct_relevel(R8Support, levels = c("Best", "Excellent", "Good", "Neutral", "Poor", "Awful", "Worst")),
         qualdigpriv = fct_relevel(qualdigpriv, levels = c("Best", "Excellent", "Good", "Neutral", "Poor", "Awful"))) %>% 
  mutate(across(c(RELIABLE:onlinemarking, Dskills4Lect:HealthandWellbeing), ~ fct_relevel(., levels = c("Agree", "Neutral", "Disagree")))) %>% 
  mutate(approachtoTech = fct_recode(approachtoTech, "I tend to be an early adopter where I can see clear benefits" = "I tend to be an early adopter where I see clear benefits"),
         Audio = fct_recode(Audio, "Neutral" = "Neutural"),
         StudentSafetyOnline = fct_recode(StudentSafetyOnline, "Neutral" = "Neutural"),
         AssitndAdaptTech = fct_recode(AssitndAdaptTech, "Agree" = "Yes"),
         AssitndAdaptTech = fct_recode(AssitndAdaptTech, "Disagree" = "No")) %>% 
  mutate(DTechUseFuture = fct_relevel(DTechUseFuture, levels = c("More than they are now", "Same as they are now", "Less than they are now"))) %>% 
  mutate(approachtoTech = fct_relevel(approachtoTech, levels = c("I am usually among the first to adopt new technologies", 
                                                                 "I tend to be an early adopter where I can see clear benefits",
                                                                 "I tend to adopt new technologies at the pace of my peers",
                                                                 "I tend to adopt new technologies after my peers")))


# Question 6
q04 <- staff_model %>% 
  filter(!is.na(Job)) %>% 
  mutate(Job = fct_infreq(Job)) %>% 
  count(Job, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(Job), fill = survey)) +
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
  labs(title = glue::glue("Q4. What	best	describes	your	role?	(NOTE:	If	you	have	more	than	one	role<br>in	the	institution,	please
choose	the	one	in	which	you	spend	the	most time<br>on	a	FTE	(Full	Time	Equivalent)	basis)<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q04

# Question 5

q05 <- staff_model %>% 
  select(Discipline, survey) %>% 
  mutate(Discipline = as.character(Discipline)) %>% 
  mutate(Discipline = ifelse(Discipline == "Business and Administration and Law", "Business, Administration and Law", Discipline),
         Discipline = ifelse(Discipline == "Arts and Humanities and Languages", "Arts, Humanities and Languages", Discipline),
         Discipline = ifelse(Discipline == "Computing, Information and Communication Technologies", "Computing and ICT", Discipline),
         Discipline = ifelse(Discipline == "Engineering and Manufacturing and Architecture and Construction", "Engineering, Manufacturing, Architecture and Construction", Discipline),
         Discipline = ifelse(Discipline == "Health (e.g. Medicine, Dentistry, Nursing, Pharmacy, Physiotherapy and Sports in Health)", "Health", Discipline),
         Discipline = ifelse(Discipline == "Natural Sciences (e.g. Biology, Chemistry, Physics) and Mathematics", "Natural Sciences and Mathematics", Discipline)) %>% 
  filter(!is.na(Discipline)) %>% 
  mutate(Discipline = fct_infreq(Discipline)) %>% 
  count(Discipline, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         Discipline = fct_rev(Discipline)) %>%
  ungroup() %>% 
  ggplot(aes(percentage, Discipline, fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 3), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  scale_y_discrete(labels = scales::wrap_format(30)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  labs(title = glue::glue("Q5. In	what	discipline	or	unit	do	you	teach	or	support	learning	and	teaching?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q05



# Question 6
q06 <- staff_model %>% 
  filter(!is.na(Gender)) %>% 
  count(Gender, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(Gender), fill = survey)) +
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
  labs(title = glue::glue("Q6. What gender do you identify as? (<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q06

# Question 7

q07 <- staff_model %>% 
  filter(!is.na(AssistTech)) %>% 
  mutate(AssistTech = ifelse(str_detect(AssistTech, "Yes"), "Yes", "No")) %>% 
  count(AssistTech, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, AssistTech, fill = survey)) +
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
  labs(title = glue::glue("Q07. Do you use any assistive technologies to meet your learning needs?<br>(e.g. screen readers, voicerecognition, switches)<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q07

# Question 9

q09 <- staff_model %>% 
  filter(!is.na(approachtoTech)) %>% 
  count(approachtoTech, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(approachtoTech), fill = survey)) +
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
  labs(title = glue::glue("Q09. Which best describes your approach to adopting new technologies for teaching?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q09

# Question 10
q10 <- staff_model %>% 
  filter(!is.na(SupportDTech)) %>% 
  mutate(SupportDTech = fct_infreq(SupportDTech)) %>% 
  count(SupportDTech, survey) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/sum(n)*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(SupportDTech), fill = survey)) +
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
  labs(title = glue::glue("Q10. Who supports you most to use digital technologies in your teaching?<br>(<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)")) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q10

# Question 11

q11 <- staff_access %>% 
  filter(!(access == "Other")) %>% 
  mutate(access = fct_infreq(access)) %>% 
  count(access, survey, number_students) %>% 
  group_by(survey) %>%
  mutate(percentage = round(n/number_students*100, 0),
         survey = survey) %>%
  ungroup() %>% 
  ggplot(aes(percentage, fct_rev(access), fill = survey)) +
  geom_col(width = 0.8, position = "dodge", show.legend = F) +
  scale_fill_manual(values = c(diftu_colour, index_colour)) +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank()) +
  theme(plot.title.position = "plot",
        plot.title = element_markdown())
q11 + 
  geom_text(aes(label = glue::glue("{n}  ({percentage} %)"), 
                x = 10), 
            size = 6, colour = font_colour,
            family = "my_font", 
            position = position_dodge(width = 0.7),
            fontface = "bold") +
  labs(title = glue::glue("Q11. Which of these do you have access to at your institution whenever you need them?<br>(Tick all that apply)  (<i style = 'color:{index_colour};'>INDex-2019</i>, <i style = 'color:{diftu_colour};'>DifTU-2021</i>)"))
q11

# Question 12
likert <- staff_model %>%
  as.data.frame %>% 
  select(RELIABLE:VLEmobile, survey) %>% 
  drop_na() 

q12 <- likert %>% 
  select(RELIABLE:VLEmobile) %>%
  mutate_if(is.factor, fct_rev) %>% 
  rename("I rely on the VLE for my teaching" = RELIABLE,
         "It is easy to design and organise course materials" = organiseVLE,
         "I regularly use it for student collaboration" = CollabVLE,
         "It encourages me to try different activities" = VLEexplore,
         "I regularly access it on a mobile device" = VLEmobile) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = toupper("Q12. How much do you agree with the following statements about your VLE\n(Virtual Learning Environment)?")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20))
q12

# Question 13
likert <- staff_model %>%
  as.data.frame %>% 
  select(Audio:onlinemarking, survey) %>% 
  drop_na() 

q13 <- likert %>% 
  select(Audio:onlinemarking) %>%
  mutate_if(is.factor, fct_rev) %>% 
  rename("Audio visual equipment is reliable and easy to use" = Audio,
         "Teaching spaces are well designed for digital technology use" = Welldesigned,
         "The software available to teach with is industry standard and up-to-date" = uptostandard,
         "Digital media production facilities (e.g. video) are available if I need them" = prodfacil,
         "The system for online marking and giving feedback is easy for me to use" = onlinemarking) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = toupper("Q13. How much do you agree with the following statements?")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20))
q13

# Question 14
likert <- staff_model %>%
  as.data.frame %>% 
  select(qualdigpriv, survey) %>% 
  drop_na() 

q14 <- likert %>% 
  select(qualdigpriv) %>%
  mutate_if(is.factor, fct_rev) %>% 
  rename("Overall, how would you rate the quality of this institution's digital provision (software, hardware,
learning environment)?" = qualdigpriv) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = toupper("Q14")) +
  theme(plot.title.position = "plot",
        plot.title = element_text(size = 20))
q14

# Question 15
likert <- staff_model %>%
  as.data.frame() %>% 
  select(Polls:DigFeedback, survey) %>% 
  drop_na()

q15 <- likert %>% 
  select(Polls:DigFeedback) %>%
  mutate_if(is.factor, fct_rev) %>% 
  rename("Carry out live polls or quizzes in class" = Polls,
         "Teach in a live online environment e.g. a webinar" = Webinars,
         "Create learning materials in a digital format (not just text or PowerPoint)" = CreateDigForms,
         "Use a digital system to give personalised feedback" = DigFeedback) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q15. In your teaching practice, how often do you:")
q15

# Question 16
likert <- staff_model %>%
  as.data.frame() %>% 
  select(DTechUseFuture, survey) %>% 
  drop_na()

q16 <- likert %>% 
  select(-survey) %>%
  mutate_if(is.factor, fct_rev) %>% 
  rename("Ideally, how much would you like digital technologies to be used in your teaching practice?" = DTechUseFuture) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q16. ")
q16

# https://slcladal.github.io/surveys.html#4_Visualizing_survey_data


# Question 17
likert <- staff_model %>%
  as.data.frame %>% 
  select(OnlineDTeachRes:DevelopDTSkills, survey) %>% 
  drop_na()

q17 <- likert %>% 
  select(OnlineDTeachRes:DevelopDTSkills) %>% 
  mutate_if(is.factor, fct_rev) %>% 
  rename("Search online for digital teaching resources" = OnlineDTeachRes,
         "Discuss teaching with peers via an online network or forum" = DisTeachWPeers,
         "Read up on developments and issues relating to digital education" = DTeachDevResearch,
         "Develop your digital teaching skills (formally or informally)" = DevelopDTSkills) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q17. How often do you do the following to support your teaching?")  +
  guides(fill = guide_legend(nrow = 1))
q17

# Question 18
likert <- staff_model %>%
  as.data.frame %>% 
  select(Dskills4Lect:InvolDServices, survey) %>% 
  drop_na()

q18 <- likert %>% 
  select(Dskills4Lect:InvolDServices) %>% 
  mutate_if(is.factor, fct_rev) %>% 
  rename("Guidance	about	the	digital	skills	you	need	as	a	teacher" = Dskills4Lect,
         "Regular	opportunities	to	develop	your	digital	skills" = Opps2DevDSkills,
         "Time and support to innovate" = TimeSupport,
         "Reward/recognition when you develop digital aspects of your role" = RecogRole,
         "Opportunity to be involved in decisions about digital services" = InvolDServices) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = "Q18. How much do you agree that your institution provides you with ...") 
q18



# Question 19
likert <- staff_model %>%
  as.data.frame %>% 
  select(Datasecurity:HealthandWellbeing, survey) %>% 
  drop_na()

q19 <- likert %>% 
  select(Datasecurity:HealthandWellbeing) %>%
  mutate_if(is.factor, fct_rev) %>% 
  rename("Managing learner data securely" = Datasecurity,
         "Digital copyright and licensing" = CopyrightandLicen,
         "Assistive and adaptive technologies" = AssitndAdaptTech,
         "Ensuring students behave safely online" = StudentSafetyOnline,
         "Your	health	and	wellbeing	in	the	digital	workplace" = HealthandWellbeing) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = "Q19. How much do you agree that you are informed about your responsibilities\nwith regard to:")
q19


# Question 20
likert <- staff_model %>%
  as.data.frame %>% 
  select(R8Support, survey) %>% 
  drop_na()

q20 <- likert %>% 
  select(R8Support) %>%
  mutate_if(is.factor, fct_rev) %>% 
  rename(" Overall, how would you rate the support you receive from your institution to develop the digital aspects of your role?" = R8Support) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q20.")
q20


# Question 20
likert <- student_model_2 %>%
  as.data.frame %>% 
  select(DigitalSkill4Jobs:InvolinDskillsdecisions, survey) %>% 
  drop_na() 

q20 <- likert %>% 
  select(DigitalSkill4Jobs:InvolinDskillsdecisions) %>%
  mutate_if(is.factor, fct_rev) %>% 
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
likert <- staff_model %>%
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
  mutate_if(is.factor, fct_rev) %>% 
  rename(" Overall, how would you rate the quality of digital teaching and learning on your course?" = DTeachingSkills) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 100, labeller = labels, text.size = 5) + 
  labs(title = "Q21.")  +
  guides(fill = guide_legend(nrow = 1))
q21

# Question 23
q23 <- staff_model %>% 
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
  mutate_if(is.factor, fct_rev) %>% 
  rename("I understand things better" = BetterUndertstanding,
         "I enjoy learning more" = Enjoymore,
         "I am more independent in my learning" = Independent,
         "I can fit learning into my life more easily" = Fitseasily) %>% 
  likert(grouping = likert %>% pull(survey)) %>% 
  plot(ordered = F, wrap= 60, labeller = labels, text.size = 5) + 
  labs(title = "Q24. When digital technologies are used on my course ...")
q24

# Question 25
q25 <- staff_model %>% 
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
q26 <- staff_model %>% 
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



# Mon May 24 13:38:44 2021 ------------------------------


student_diftu_devices <- read_csv("../data/student-diftu-tidy.csv") %>% 
  select(session, Campus, laptop:no_device, city:blanch) %>% 
  pivot_longer(cols = -c(session, Campus, city:blanch), 
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
         number_students = nrow(staff_diftu))


student_index_devices <- read_csv("../data/student-index-devices.csv") %>% 
  select(session, Campus = campus, city:no_device) %>% 
  pivot_longer(cols = -c(session, Campus, city:blanch), 
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
         number_students = nrow(staff_index))

student_diftu_access <- read_csv("../data/student-diftu-tidy.csv") %>% 
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
                                none_access = "None of the above"))%>% 
  mutate(survey = "DifTU-2021",
         number_students = nrow(staff_diftu))

student_index_access <- read_csv("../data/student-index-access.csv") %>% 
  select(session, wifi:none_access) %>% 
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
                                none_access = "None of the above"))%>% 
  mutate(survey = "INDEx-2019",
         number_students = nrow(staff_index))



