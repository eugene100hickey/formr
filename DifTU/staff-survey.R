# .font150[ `r fa("sad-tear", fill = "white")` `r fa("frown", fill = "white")` `r fa("meh", fill = "white")` `r fa("smile", fill = "white")` `r fa("grin-beam", fill = "deepskyblue")`<br>&emsp;&emsp;&emsp;&emsp;&nbsp;`r fa("hand-point-up", fill = "deepskyblue")`]
# https://www.rstudio.com/resources/rstudioglobal-2021/using-formr-to-create-r-powered-surveys-with-individualized-feedback/
# https://github.com/jhelvy/surveys-with-formr

library(tidyverse)
library(lubridate)
library(formr)
library(rjson)

login_details <- readRDS("../formr_login")

formr_connect(
  email = login_details$email, #enter your elena email here (email = "yourname@live.ie")
  password = login_details$password # and the password here (password = "abcde12345")
)

staff <- formr_raw_results(survey_name = "StaffSurvey") %>% 
  mutate(created = as_datetime(created)) %>% 
  filter(created > as_date("2021-04-20"),
         !is.na(session))
# student <- formr_raw_results(survey_name = "StudentSurvey")

staff_missing <- rowSums(is.na(staff))
staff <- staff[staff_missing<20,] %>% 
  select(-c(expired, AssitTechSupport, Other, Informationcheck, Feedback))

staff <- staff %>% 
  mutate(Job = Job %>% str_replace("<h5>.*?>", "") %>% str_replace("<.*", "") %>% str_trim(),
         Discipline = Discipline %>% str_replace("<h5>.*?>", "") %>% str_replace("<.*", "") %>% str_trim(),
         approachtoTech = approachtoTech %>% str_replace("<h5>.*?>", "") %>% str_replace("<.*", "") %>% str_trim()
         )

staff_info <- fromJSON(file = "../data/StaffSurvey.json")[[2]][[1]]$survey_data[[2]]
best_options <- list(`1` = "Best", 
                     `2` = "Good", 
                     `3` = "Neutral", 
                     `4` = "Poor", 
                     `5` = "Worst")

staff_info[41][[1]]$choices <- best_options
staff_info[81][[1]]$choices <- best_options

staff_info_names <- map_chr(1:98, function(x) staff_info[x][[1]]$name)


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

fun_quote <- function(x){
  fun(quote({{x}}) %>% as.character)
}

z <- staff %>% 
  mutate(Teaching = fun("Teaching"),
         Campus = fun("Campus"), 
         Gender = fun("Gender"),
         AssistTech = fun("AssistTech"),
         SupportDTech = fun("SupportDTech"),
         RELIABLE = fun("RELIABLE"),
         organiseVLE = fun("organiseVLE"),
         CollabVLE = fun("CollabVLE"),
         VLEexplore = fun("VLEexplore"),
         VLEmobile = fun("VLEmobile"),
         Audio = fun("Audio"),
         Welldesigned = fun("Welldesigned"),
         uptostandard = fun("uptostandard"),
         prodfacil = fun("prodfacil"),
         onlinemarking = fun("onlinemarking"),
         qualdigpriv = fun("qualdigpriv"),
         Polls = fun("Polls"),
         Webinars = fun("Webinars"),
         CreateDigForms = fun("CreateDigForms"),
         DigFeedback = fun("DigFeedback"),
         RecordL = fun("RecordL"),
         DTechUseFuture = fun("DTechUseFuture"),
         OnlineDTeachRes = fun("OnlineDTeachRes"),
         DisTeachWPeers = fun("DisTeachWPeers"),
         DTeachDevResearch = fun("DTeachDevResearch"),
         DevelopDTSkills = fun("DevelopDTSkills"),
         Dskills4Lect = fun("Dskills4Lect"),
         Opps2DevDSkills = fun("Opps2DevDSkills"),
         TimeSupport = fun("TimeSupport"),
         RecogRole = fun("RecogRole"),
         InvolDServices = fun("InvolDServices"),
         Datasecurity = fun("Datasecurity"),
         CopyrightandLicen = fun("CopyrightandLicen"),
         AssitndAdaptTech = fun("AssistTech"),
         StudentSafetyOnline = fun("StudentSafetyOnline"),
         HealthandWellbeing = fun("HealthandWellbeing"),
         R8Support = fun("R8Support"),
         Concerns = fun("Concerns"),
         OnlineTeachingExper = fun("OnlineTeachingExper"),
         Prefsforfuture = fun("Prefsforfuture"),
         WorkingPref = fun("WorkingPref"))

write_csv(z, file = "../data/staff-survey-tidy.csv")

# z <- staff %>% 
#   mutate(Teaching = fun(quote(Teaching) %>% as.character),
#          Campus = fun_quote <- function(Campus, as.character) {
#   fun(quote(Campus) %>% as.character)
# })
# 
# z <- staff %>% 
#   mutate(across(Teaching, function(x) {fun(quote(x) %>% as.character)}))
