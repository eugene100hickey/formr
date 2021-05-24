library(tidyverse)
library(readxl)
library(lubridate)

sheet_names <- excel_sheets("../data/INDEx Survey Staff Comments.xlsx")

staff <- xlsx::read.xlsx(file = "../data/INDEx Survey Staff Comments.xlsx", 
                           sheetName = "Survey") %>% 
  janitor::clean_names()

names(staff) <- names(staff) %>% 
  str_sub(1, 12)

staff <- staff %>% 
  select(x4_what_best:x23_which_of) %>% 
  mutate(survey = "INDEx-2019",
         x5_in_what_d = ifelse(x5_in_what_d == "None of the above", 
                               x5_a_if_your, 
                               x5_in_what_d)) %>% 
  select(-x5_a_if_your) %>% 
  select(Campus = x22_which_tu,
         Discipline = x5_in_what_d,
         AssistTech = x7_do_you_pe,
         Gender = x6_what_gend,
         approachtoTech =  x9_which_bes,
         SupportDTech = x10_who_supp,
         RELIABLE = x12_1_a_i_re,
         organiseVLE = x12_2_a_it_i,
         CollabVLE = x12_3_a_i_re,
         VLEexplore = x12_4_a_it_e,
         VLEmobile = x12_5_a_i_re,
         Audio = x13_1_a_audi,
         Welldesigned = x13_2_a_teac,
         uptostandard = x13_3_a_the_,
         prodfacil = x13_4_a_digi,
         onlinemarking = x13_5_a_the_,
         qualdigpriv = x14_overall_,
         Polls = x15_1_a_carr,
         Webinars = x15_2_a_teac,
         CreateDigForms = x15_3_a_crea,
         DigFeedback = x15_4_a_use_,
         DTechUseFuture = x16_ideally_,
         OnlineDTeachRes = x17_1_a_sear,
         DisTeachWPeers = x17_2_a_disc,
         DTeachDevResearch = x17_3_a_read, 
         DevelopDTSkills = x17_4_a_deve,
         Dskills4Lect = x18_1_a_guid,
         Opps2DevDSkills = x18_2_a_regu,
         TimeSupport = x18_3_a_time,
         RecogRole = x18_4_a_rewa,
         InvolDServices = x18_5_a_oppo,
         Datasecurity = x19_1_a_mana,
         CopyrightandLicen = x19_2_a_digi,
         AssitndAdaptTech = x19_3_a_assi,
         StudentSafetyOnline = x19_4_a_ensu,
         HealthandWellbeing = x19_5_a_your,
         R8Support = x20_overall_,
         BetterSupp = x21_what_one
  )

write_csv(staff, "../data/staff-index-tidy.csv")
