library(tidyverse)
library(readxl)
library(lubridate)


sheet_names <- excel_sheets("data/INDEx Survey Student Comments.xlsx")

student <- xlsx::read.xlsx(file = "data/INDEx Survey Student Comments.xlsx", 
                           sheetName = "Survey") %>% 
  janitor::clean_names()
names(student) <- names(student) %>% str_sub(1, 12)
student <- student %>% 
  mutate(x5_in_what_a = ifelse(x5_in_what_a == "Other", 
                               x5_a_if_you_, 
                               x5_in_what_a)) %>% 
  select(-x5_a_if_you_) %>% 
  select(Years = x2_how_many_,
         Campus = x29_which_tu,
         Study = x5_in_what_a,
         Age = x6_how_old_a,
         Gender = x7_what_gend,
         LearningNeeds = x8_do_you_us,
         LearningNeedmet = x9_if_yes_ha,
         Exampleassitivetech = x10_please_g,
         Reference = x12_1_a_mana,
         StudyTimwe = x12_2_a_orga,
         NotesT = x12_3_a_make,
         AdditionalResources = x12_4_a_look,
         AccessLectures = x12_5_a_acce,
         InstSupport = x14_1_a_this,
         InstHealth = x14_2_a_i_ca,
         SocietyAccess = x14_3_a_i_ca,
         DataPrivacy = x14_5_a_this,
         Support = x15_who_supp,
         DigitalExperience = x16_overall_,
         FindInfo = x17_1_a_find,
         Workonline = x17_2_a_work,
         Useeducationalgame = x17_3_a_use_,
         UsePolling = x17_4_a_use_,
         Cr8DigPortfolio = x17_5_a_crea,
         Usepptword = x17_6_a_prod,
         Usefuldigitalactivity = x17_a_please,
         VLEeasyuse = x18_1_a_i_ca,
         Vlereliable = x18_2_a_i_re,
         Vlemobile = x18_3_a_i_re,
         Vleusebylecturers = x18_4_a_i_wo,
         Onlineassesments = x19_1_a_onli,
         GoodTeachingSpaces = x19_2_a_teac,
         RelevantSoftware = x19_3_a_the_,
         PersoalDataStorage = x19_4_a_i_am,
         DigitalSkill4Jobs = x20_1_a_befo,
         Opp2updateDSkills = x20_2_a_i_ha,
         DSkillsneededinCareer = x20_3_a_digi,
         Dworkplaceprep = x20_4_a_my_c,
         InvolinDskillsdecisions = x20_5_a_lear,
         DTeachingSkills = x21_overall_,
         Usefulresources = x23_which_of,
         BetterUndertstanding = x24_1_a_i_un,
         Enjoymore = x24_2_a_i_en,
         Independent = x24_3_a_i_am,
         Fitseasily = x24_4_a_i_ca,
         LearningPref = x25_which_be,
         Useful = x26_which_of,
         DigitalLearningPref = x27_in_class
  )

write_csv(student, "data/student-index-tidy.csv")

sheet_open <- function(x = "Survey"){
  xlsx::read.xlsx(file = "data/INDEx Survey Student Comments.xlsx", 
                  sheetName = x) %>% 
    janitor::clean_names()
}

z <- map(sheet_names[3:11], sheet_open)


