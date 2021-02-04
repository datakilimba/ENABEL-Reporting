library(tidyverse)
library(xlsx)
library(magrittr)

hhs_general_survey = readxl::read_xlsx(paste0("C:/Users/tnkil/OneDrive/Enabel",
" - SAKiRP/M&E/Data Collection/S01 - HH Survey/Data Analysis/Calculation Sheets ",
"2020/Houshold Survey 2020 Sample Sets.xlsx"),
sheet = "HHS general sample")

hhs_general_survey_beneficiaries_male = hhs_general_survey %>%
  filter(`Is the respondent a member of a group or not?` == "Member of a group",
         `Sex of respondent` == "Male",
         `Phone number of HHH or other member of the HH.`!=999) %>% 
  mutate(`Status (X/V)` = "",`# B` = "") %>% 
  rename(Name = `Select beneficiary name`,
         `Phone 1` = `Phone number of HHH or other member of the HH.`,
         `Phone 2` = `2nd phone number of HHH or other member of the HH.`,
         Sex = `Sex of respondent`) %>% 
  mutate(`Phone 2` = na_if(`Phone 2`,999)) %>% 
  select(Name,`Phone 1`,`Phone 2`,Sex ,District, Ward, Village,WAEO,
         `Status (X/V)`,`# B`)




