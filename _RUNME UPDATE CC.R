# packages ----------------------------------------------------------------


library(here) # for finding files easier
library(haven) # for importing sav files
library(tidyverse) # for data cleaning
library(zoo) # additional data cleaning tools
library(dplyr)

# load data ---------------------------------------------------------------

master_2020 <- read_sav(here("data/MasterFile_groupings_2020.sav"))
master_2021 <- read_sav(here("data/MasterFile_groupings_2021.sav"))
master_2022 <- read_sav(here("data/MasterFile_groupings_2022.sav"))
cc_master <- read_sav(here("data/CC.MasterFile_groupings.sav"))

ec_master <- full_join(full_join (master_2020, master_2021), master_2022)
rm(master_2020, master_2021)



# clean data --------------------------------------------------------------

## Download source file, unzip and extract into table
ZipCodeSourceFile = "http://download.geonames.org/export/zip/US.zip"
temp <- tempfile()
download.file(ZipCodeSourceFile , temp)
ZipCodes <- read.table(unz(temp, "US.txt"), sep="\t")
unlink(temp)
names(ZipCodes) = c("CountryCode", "zip", "PlaceName", 
                    "AdminName1", "State", "AdminName2", "AdminCode2", 
                    "AdminName3", "AdminCode3", "latitude", "longitude", "accuracy")
ZipCodes$zip = as.character(ZipCodes$zip)
ZipCodes_r <- ZipCodes %>%
    mutate (zip = case_when (nchar(zip) == 5 ~ zip, 
                             nchar(zip) == 4 ~ paste("0",zip,sep = ""),
                             nchar(zip) == 3 ~ paste("00",zip,sep = "")))

## Matching ec data with zipcode and clean 
ec_response_table = ec_master %>%
    rename(zip = DEMO.001) %>%
    left_join(select(ZipCodes_r, zip, State)) %>%
    #filter(UserLanguage == "EN") %>%
    mutate(child_age03 = case_when(
               DEMO.004.a.2 >= 1 ~ "Yes",
               !is.na(DEMO.004.a.2) ~ "No",
               TRUE ~ NA_character_), 
           Language = case_when (UserLanguage == "EN" ~ "English", 
                                 UserLanguage == "SPA" ~ "Spanish")) %>%
    select(CaregiverID, Week, OPEN.001, OPEN.002, OPEN.003, OPEN.004, OPEN.005, OPEN.006, OPEN.007, OPEN.008, OPEN.009, OPEN.010,
           COVID.007, COVID.008, CTAX.008, CTAX.016, DEBT.005, DEBT.006, FamCon.016, FamCon.017, FamCon.018, GRAND.016, HEALTH.024, 
           HEALTH.030, JOB.030, MH.013, MH.014, POLICY.030, PREG.028, PREG.029, PREG.038, PREG.040, PREG.044, STIM.001.d, STIM.002.d, STIM.003.d,STIM.004.d, STIM.005.d, WKFORCE.011, 
           OPEN.011, POLICY.033, NEEDS.007, WIC.009, CTAX.022, 
           POLICY.036, FSTR.008, JOB.040.a, HEALTH.038, OPEN.012, OPEN.013, CTAX.026, CLIM.002, CLIM.004, 
           CLIM.012, 
           OPEN.014, OPEN.015, OPEN.016, Fam.003, # NEW VARIABLE
           State, FPL.2019.150, zip, Language,
           RaceGroup, CaregiverAge, child_age03) %>%
    filter(OPEN.006 == 1) %>%
    arrange(Week) %>%
    group_by(CaregiverID) %>%
    mutate_if(is.labelled, as_factor, levels = "labels") %>%
    mutate_at(vars(State, FPL.2019.150, RaceGroup, CaregiverAge, child_age03, zip, Language), na.locf0) %>%
    ungroup() %>%
    select(-OPEN.006) %>%
    rename(`Below 1.5xFPL` = FPL.2019.150,
           `Child 0-3` = child_age03) %>%
    gather("Question", "Response", OPEN.001, OPEN.002, OPEN.003, OPEN.004, OPEN.005, OPEN.007, OPEN.008, OPEN.009, OPEN.010,
           COVID.007, COVID.008, CTAX.008, CTAX.016, DEBT.005, DEBT.006, FamCon.016, FamCon.017, FamCon.018, GRAND.016, HEALTH.024, 
           HEALTH.030, JOB.030, MH.013, MH.014, POLICY.030, PREG.028, PREG.029, PREG.038, PREG.040, PREG.044, STIM.001.d, STIM.002.d, STIM.003.d, STIM.004.d, STIM.005.d, WKFORCE.011,
           OPEN.011, POLICY.033, NEEDS.007, WIC.009, CTAX.022,
           POLICY.036, FSTR.008, JOB.040.a, HEALTH.038, OPEN.012, OPEN.013, CTAX.026, CLIM.002, CLIM.004, CLIM.012, 
           OPEN.014, OPEN.015, OPEN.016, Fam.003) %>%
    filter(Response != "") 


ec_questions = ec_master %>% 
    select(OPEN.001, OPEN.002, OPEN.003, OPEN.004, OPEN.005, OPEN.007, OPEN.006, OPEN.008, OPEN.009, OPEN.010,
           COVID.007, COVID.008, CTAX.008, CTAX.016, DEBT.005, DEBT.006, FamCon.016, FamCon.017, FamCon.018, GRAND.016, HEALTH.024, 
           HEALTH.030, JOB.030, MH.013, MH.014, POLICY.030, PREG.028, PREG.029, PREG.038, PREG.040, PREG.044, STIM.001.d, STIM.002.d, STIM.003.d, STIM.004.d, STIM.005.d,  WKFORCE.011,
           OPEN.011, POLICY.033, NEEDS.007, WIC.009, CTAX.022,
           POLICY.036, FSTR.008, JOB.040.a, HEALTH.038, OPEN.012, OPEN.013, CTAX.026, CLIM.002, CLIM.004, CLIM.012,
           OPEN.014, OPEN.015, OPEN.016, Fam.003) %>% 
    select(-OPEN.006)
ec_q_text = sjlabelled::get_label(ec_questions)
ec_q_names = names(ec_questions)
#q_nums = as.numeric(str_remove(q_names, "OPEN."))
ec_q_nums = seq(1, 55)
#rm(questions)

## Matching cc data with zipcode and clean 
cc_response_table = cc_master %>%
    rename(zip = CC.DEMO.001) %>%
    left_join(select(ZipCodes_r, zip, State)) %>%
   # filter(UserLanguage == "EN") %>%
    mutate (provider_type = case_when (CC.DEMO.013_1 == 1 ~ "Center teacher", 
                                       CC.DEMO.013_2 == 1 ~ "Center director",
                                       CC.DEMO.013_3 == 1 | CC.DEMO.013_4 == 1|CC.DEMO.013_8 ==1 | CC.DEMO.013_12 == 1| CC.DEMO.013_9 ==1 | CC.DEMO.013_13 == 1  ~ "Home-based provider", 
                                       CC.DEMO.013_5 == 1 ~ "FFN", 
                                       CC.DEMO.013_6 == 1|CC.DEMO.013_10 == 1 | CC.DEMO.013_11 == 1 ~ "Babysitter/nanny"), 
            Language = case_when (UserLanguage == "EN" ~ "English", 
                                  UserLanguage == "SPA" ~ "Spanish")) %>%
    select(ProviderID, WEEK, CC.OPEN.001, CC.OPEN.002, CC.OPEN.003, CC.OPEN.004, CC.OPEN.005, CC.OPEN.006, CC.OPEN.007, CC.OPEN.008, 
           CC.CCS.004, CC.CCS.005, CC.CTAX.008, CC.CTAX.016, CC.DEBT.005, CC.DEBT.006, CC.FamCon.016, CC.FamCon.017, CC.FamCon.018, CC.MH.013, CC.SF.010, CC.Staff.009, 
           CC.STIM.001.d, CC.STIM.002.d, CC.STIM.003.d, CC.STIM.004.d, CC.STIM.005.d, CC.WellB.002, CC.VACC.003, CC.VACC.003.1, CC.OPEN.009, CC.OPEN.010, CC.OPEN.011,
           CC.Access.002, CC.CTAX.029, CC.FoodAid.002, CC.FoodAid.005, CC.FoodAid.007, CC.HEALTH.039, CC.Inflation.004, CC.ProvDebt.007, CC.Violence.002, 
           CC.Fam.003,# NEW VARIABLES
           State, FPL.2019.150, zip, RaceGroup, provider_type, Language) %>%
    filter(CC.OPEN.007 == 1) %>%
    arrange(WEEK) %>%
    group_by(ProviderID) %>%
    mutate_if(is.labelled, as_factor, levels = "labels") %>%
    mutate_at(vars(State, FPL.2019.150, zip, RaceGroup, provider_type, Language), na.locf0) %>%
    ungroup() %>%
    select(-CC.OPEN.007) %>%
    rename(`Below 1.5xFPL` = FPL.2019.150) %>%
    gather("Question", "Response", CC.OPEN.001, CC.OPEN.002, CC.OPEN.003, CC.OPEN.004, CC.OPEN.005, CC.OPEN.006, CC.OPEN.008, 
           CC.CCS.004, CC.CCS.005, CC.CTAX.008, CC.CTAX.016, CC.DEBT.005, CC.DEBT.006, CC.FamCon.016, CC.FamCon.017, CC.FamCon.018, CC.MH.013, CC.SF.010, CC.Staff.009, 
           CC.STIM.001.d, CC.STIM.002.d, CC.STIM.003.d, CC.STIM.004.d, CC.STIM.005.d, CC.WellB.002, CC.VACC.003, CC.VACC.003.1,
           CC.OPEN.009, CC.OPEN.010, CC.OPEN.011,
           CC.Access.002, CC.CTAX.029, CC.FoodAid.002, CC.FoodAid.005, CC.FoodAid.007, CC.HEALTH.039, CC.Inflation.004, CC.ProvDebt.007, CC.Violence.002, CC.Fam.003) %>%
    filter(Response != "") 


cc_questions = cc_master %>% 
    select(CC.OPEN.001, CC.OPEN.002, CC.OPEN.003, CC.OPEN.004, CC.OPEN.005, CC.OPEN.006, CC.OPEN.007,CC.OPEN.008, 
           CC.CCS.004, CC.CCS.005, CC.CTAX.008, CC.CTAX.016, CC.DEBT.005, CC.DEBT.006, CC.FamCon.016, CC.FamCon.017, CC.FamCon.018, CC.MH.013, CC.SF.010, CC.Staff.009, 
           CC.STIM.001.d, CC.STIM.002.d, CC.STIM.003.d, CC.STIM.004.d, CC.STIM.005.d, CC.WellB.002, CC.VACC.003, CC.VACC.003.1,
           CC.OPEN.009, CC.OPEN.010, CC.OPEN.011,
           CC.Access.002, CC.CTAX.029, CC.FoodAid.002, CC.FoodAid.005, CC.FoodAid.007, CC.HEALTH.039, CC.Inflation.004, CC.ProvDebt.007, CC.Violence.002, CC.Fam.003) %>% 
    select(-CC.OPEN.007)
cc_q_text = sjlabelled::get_label(cc_questions)
cc_q_names = names(cc_questions)
#q_nums = as.numeric(str_remove(q_names, "OPEN."))
cc_q_nums = seq(1, 40)
#rm(questions)

# loop bigram  ------------------------------------------------------------

## ec loop bigram

for(i in 1:length(ec_q_nums)){
    
    rmarkdown::render(input = "_template_bigram_EC.Rmd",
                      output_file = paste0("ec_bigram_", ec_q_nums[i], ".html"), 
                      params = list(
                          title = paste("Question", ec_q_nums[i]),
                          question = ec_q_text[i],
                          variable = ec_q_names[i],
                          data = ec_response_table))
}


## cc loop bigram

for(i in 1:length(cc_q_nums)){
    
    rmarkdown::render(input = "_template_bigram_CC.Rmd",
                      output_file = paste0("cc_bigram_", cc_q_nums[i], ".html"), 
                      params = list(
                          title = paste("Question", cc_q_nums[i]),
                          question = cc_q_text[i],
                          variable = cc_q_names[i],
                          data = cc_response_table))
}


# loop response table -----------------------------------------------------

## ec response table
for(i in 1:length(ec_q_nums)){
    
    rmarkdown::render(input = "_template_responses_EC.Rmd",
                      output_file = paste0("ec_responses_", ec_q_nums[i], ".html"), 
                      params = list(
                          title = paste("Question", ec_q_nums[i]),
                          question = ec_q_text[i],
                          variable = ec_q_names[i],
                          data = ec_response_table))
}

## cc response table
for(i in 1:length(cc_q_nums)){
    
    rmarkdown::render(input = "_template_responses_CC.Rmd",
                      output_file = paste0("cc_responses_", cc_q_nums[i], ".html"), 
                      params = list(
                          title = paste("Question", cc_q_nums[i]),
                          question = cc_q_text[i],
                          variable = cc_q_names[i],
                          data = cc_response_table))
}


# reminders ---------------------------------------------------------------

print("Check that the _site.yaml file looks as follows:")
paste0(ec_q_nums,": ", ec_q_text)
paste0(cc_q_nums,": ", cc_q_text)
