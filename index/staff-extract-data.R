library(tidyverse)
library(readxl)
library(lubridate)

sheet_names <- excel_sheets("data/INDEx Survey Staff Comments.xlsx")

staff <- xlsx::read.xlsx(file = "data/INDEx Survey Staff Comments.xlsx", 
                           sheetName = "Survey") %>% 
  janitor::clean_names()

names(staff) <- names(staff) %>% 
  str_sub(1, 12)

write_csv(staff, "data/staff-index-tidy.csv")
