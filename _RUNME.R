# packages ----------------------------------------------------------------


library(here) # for finding files easier
library(haven) # for importing sav files
library(tidyverse) # for data cleaning
library(zoo) # additional data cleaning tools
library(dplyr)

# load data ---------------------------------------------------------------

master_2020 <- read_sav(here("data/MasterFile_groupings_2020.sav"))
master_2021 <- read_sav(here("data/MasterFile_groupings_2021.sav"))

master <- full_join (master_2020, master_2021)
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

response_table = master %>%
    rename(zip = DEMO.001) %>%
    left_join(select(ZipCodes_r, zip, State)) %>%
    filter(UserLanguage == "EN") %>%
    mutate(child_age03 = case_when(
               DEMO.004.a.2 >= 1 ~ "Yes",
               !is.na(DEMO.004.a.2) ~ "No",
               TRUE ~ NA_character_)) %>%
    select(CaregiverID, Week, starts_with("OPEN"),COVID.007, COVID.008, CTAX.008, DEBT.005, DEBT.006, FamCon.016, FamCon.017, FamCon.018, HEALTH.024, HEALTH.030, 
           JOB.030, MH.013, POLICY.030, PREG.028, PREG.029, PREG.038, PREG.040, STIM.001.d, STIM.002.d, STIM.003.d, STIM.004.d, WKFORCE.011,
           State, FPL.2019.UM.150, zip,
           RaceGroup, CaregiverAge, child_age03) %>%
    filter(OPEN.006 == 1) %>%
    arrange(Week) %>%
    group_by(CaregiverID) %>%
    mutate_if(is.labelled, as_factor, levels = "labels") %>%
    mutate_at(vars(State, FPL.2019.UM.150, RaceGroup, CaregiverAge, child_age03, zip), na.locf0) %>%
    ungroup() %>%
    select(-OPEN.006) %>%
    rename(`Below 1.5xFPL` = FPL.2019.UM.150,
           `Child 0-3` = child_age03) %>%
    gather("Question", "Response", starts_with("OPEN"), COVID.007, COVID.008, CTAX.008, DEBT.005, DEBT.006, FamCon.016, FamCon.017, FamCon.018, HEALTH.024, HEALTH.030, 
           JOB.030, MH.013, POLICY.030, PREG.028, PREG.029, PREG.038, PREG.040, STIM.001.d, STIM.002.d, STIM.003.d, STIM.004.d, WKFORCE.011) %>%
    filter(Response != "") 


questions = master %>% select(starts_with("OPEN"), COVID.007, COVID.008, CTAX.008, DEBT.005, DEBT.006, FamCon.016, FamCon.017, FamCon.018, HEALTH.024, HEALTH.030, 
                              JOB.030, MH.013, POLICY.030, PREG.028, PREG.029, PREG.038, PREG.040, STIM.001.d, STIM.002.d, STIM.003.d, STIM.004.d, WKFORCE.011) %>% select(-OPEN.006)
q_text = sjlabelled::get_label(questions)
q_names = names(questions)
#q_nums = as.numeric(str_remove(q_names, "OPEN."))
q_nums = seq(1, 31)
#rm(questions)

# loop bigram  ------------------------------------------------------------

for(i in 1:length(q_nums)){
    
    rmarkdown::render(input = "_template_bigram.Rmd",
                      output_file = paste0("bigram_", q_nums[i], ".html"), 
                      params = list(
                          title = paste("Question", q_nums[i]),
                          question = q_text[i],
                          variable = q_names[i],
                          data = response_table))
}


# loop response table -----------------------------------------------------

for(i in 1:length(q_nums)){
    
    rmarkdown::render(input = "_template_responses.Rmd",
                      output_file = paste0("responses_", q_nums[i], ".html"), 
                      params = list(
                          title = paste("Question", q_nums[i]),
                          question = q_text[i],
                          variable = q_names[i],
                          data = response_table))
}


# reminders ---------------------------------------------------------------

print("Check that the _site.yaml file looks as follows:")
paste0(q_nums,": ", q_text)

