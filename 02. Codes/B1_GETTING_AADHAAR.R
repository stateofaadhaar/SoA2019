# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outfile <- file.path(v_loc["logs"], "B1_GETTING_AADHAAR.txt")
unlink(outfile)
logmsg("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 1. GETTING AADHAAR SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", cursor = "")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading datasets and local variables ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!(exists("random_df") && is.data.frame(random_df))) {
  random_df <- import(file = file.path(v_loc[["datasets"]], 
                                        "02. State of Aadhaar In-depth 2019", 
                                        "20191118_State of Aadhaar_in-depth survey_random_sample_hh.csv"))
  logmsg("Random DF loaded from local")}

if (!(exists("purposive_df") && is.data.frame(purposive_df))) {
  purposive_df <- import(file = file.path(v_loc[["datasets"]], 
                                           "02. State of Aadhaar In-depth 2019", 
                                           "20191118_State of Aadhaar_in-depth survey_over_sample_hh.csv"))
  logmsg("Purposive DF loaded from local")}

if (!(exists("random_mem_df") && is.data.frame(random_mem_df))) {
  random_mem_df <- import(file = file.path(v_loc[["datasets"]], 
                                            "02. State of Aadhaar In-depth 2019", 
                                            "20191118_State of Aadhaar_in-depth survey_random_sample_mem.csv"))
  logmsg("Random member DF loaded from local")}

if (!(exists("purposive_mem_df") && is.data.frame(purposive_mem_df))){
  purposive_mem_df <- import(file = file.path(v_loc[["datasets"]], 
                                               "02. State of Aadhaar In-depth 2019", 
                                               "20191118_State of Aadhaar_in-depth survey_over_sample_mem.csv"))
  logmsg("Purposive member DF loaded from local")}

summary_list <- list()
analysis_number <- 0
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 001 | % of individuals with aadhaar ----
descr <- "% of individuals with aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar)

logmsg("Note: There are 610 individuals, including household members, who have an invalid response, and are dropped")
logmsg("Please see CMIE analysis for larger sample")

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 002 | % of adults with aadhaar ----
descr <- "% of adults with aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar),
         xr_age_final >= 18) %>%
  summariser_mem(xr_has_aadhaar)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 003 | % of children with aadhaar ----
descr <- "% of children with aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar),
         xr_age_final < 18) %>%
  summariser_mem(xr_has_aadhaar)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 004 | % of adults with voter ID ----
descr <- "% of adults with voter ID"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  summariser(xr_has_voter_card_flag)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 005 | % of individuals with aadhaar by state ----
descr <- "% of individuals with aadhaar by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_state, type = "flipped")

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 006 | % of individuals with voter ID by state ----
descr <- "% of individuals with voter ID by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  summariser(xr_has_voter_card_flag, zh_state, type = "flipped")

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH | # of states with greater Aadhaar than voter ID (footnote) ----
descr <- "# of states with greater Aadhaar than voter ID"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak("ADH", descr)

summary <- random_df %>%
  summariser(xr_has_voter_card_flag, zh_state, type = "flipped") %>%
  select(zh_state, perc_voter = perc_yes)

random_mem_df %>%
  filter(!is.na(xr_has_aadhaar), 
         xr_age_final >= 18) %>%
  summariser_mem(xr_has_aadhaar, zh_state, type = "flipped") %>%
  select(zh_state, perc_aadhaar = perc_yes) %>%
  left_join(summary, by = "zh_state") %>%
  filter(zh_state != "Overall") %>%
  mutate_at(vars(-zh_state), perc_to_dec) %>%
  mutate(temp = perc_aadhaar > perc_voter) %>%
  pull(temp) %>%
  sum() %>%
  paste0(" out of 17 states had Aadhaar penetration greater that that of voter ID") %>%
  logmsg()

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 007 | estimated number of individuals with no aadhaar ----
descr <- "estimated number of individuals with no aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, stat = "total")

logmsg("Note: There are 632 individuals, including household members, who have an invalid response, and are dropped")
logmsg("Please see CMIE analysis for larger sample")

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 008 | estimated number of children with no aadhaar ----
descr <- "estimated number of children with no aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar),
         xr_age_final < 18) %>%
  summariser_mem(xr_has_aadhaar, stat = "total")

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 009 | estimated number of adults with no aadhaar ----
descr <- "estimated number of adults with no aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar),
         xr_age_final >= 18) %>%
  summariser_mem(xr_has_aadhaar, stat = "total")

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 010 | estimated number of adults with no aadhaar by state  ----
descr <- "estimated number of adults with no aadhaar by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_state, stat = "total", type = "flipped")

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 011 | % of adults who want aadhaar ----
descr <- "Among those who dont have Aadhaar, % of adults who want aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "No") %>%
  mutate(in_assam = ifelse(zh_state %in% c("Assam"), "Assam", "Non-Assam")) %>% 
  summariser(zr_want_to_enrol, in_assam)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 012 | Amongst those who do not have Aadhaar, % adults who want but did/could not try to enrol ----
descr <- "Amongst those who don't have Aadhaar, % of individuals who want but did not try"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>% 
  filter(xr_has_aadhaar == "No") %>% 
  mutate(want_and_did_not_try = ifelse(zr_want_to_enrol == "Yes" & 
                                         zr_enrolment_flag == "No", 
                                       "Yes", "No")) %>% 
  summariser(want_and_did_not_try)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
logmsg("'No' is meaningless. Look at 'Yes'")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH | TO BE MOVED TO FIGURE CALCULATIONS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak("ADH")

summary <- random_df %>% 
  filter(xr_has_aadhaar == "No") %>% 
  mutate(category = case_when(
    zr_want_to_enrol == "No" ~"Don't wish to enrol",
    zr_enrolment_flag == "Yes" ~  "Tried but failed",
    TRUE ~ "did not try"
  )) %>% 
  summariser(category, stat = "total") %>% 
  mutate_at(vars(starts_with("estimated")), ~ . / 1000000)

logtable(summary, "break up of people with want to enrol")

rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 013 | reasons for not trying to apply for Aadhaar ----
descr <- "reasons for not trying for Aadhaar among adults who wanted"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp_df <- random_df %>%
  filter(zr_want_to_enrol == "Yes",
         zr_enrolment_flag == "No") %>%
  mutate_at(vars(matches("xr_not_getting_aadhaar_reason_.*")), ~ ifelse(
    str_detect(., paste("bandho", "bondho", "bandha", "centre", "bondh", "far", sep = "|")),
    "there are no enrolment centers nearby",
    .
  )) %>%
  unite(temp_not_getting_aadhaar, matches("xr_not_getting_aadhaar_reason_.*"), sep = "|")

list_of_response <- temp_df %>%
  pull(temp_not_getting_aadhaar) %>%
  str_split("\\|") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  tabyl() %>%
  filter(n > 1) %>% # Filtering for responses which occured more than once
  pull(".")

summary <- map_dfr(list_of_response, function(x) {
  temp_df %>%
    mutate(temp = split_and_match(temp_not_getting_aadhaar, x),
           temp = ifelse(temp == TRUE, "Yes", "No")) %>%
    summariser(temp) %>%
    filter(temp == "Yes") %>%
    mutate(response = x) %>%
    select(-temp)
}) %>%
  mutate(temp = perc_to_dec(perc_overall)) %>%
  arrange(-temp) %>%
  select(-temp, response, everything())

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, list_of_response, temp_df, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 014 | Among those adults who dont have Aadhaar, number/% of individuals who tried ----
descr <- "Among those adults who dont have Aadhaar, number/% of individuals who tried"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "No") %>%
  mutate(want_and_tried = ifelse(zr_want_to_enrol == "Yes" & 
                                   zr_enrolment_flag == "Yes", 
                                 "Yes", "No")) %>%
  summariser(want_and_tried)

summary <- random_df %>%
  filter(xr_has_aadhaar == "No") %>%
  mutate(want_and_tried = ifelse(zr_want_to_enrol == "Yes" & 
                                   zr_enrolment_flag == "Yes", 
                                 "Yes", "No")) %>%
  summariser(want_and_tried, stat = "total") %>%
  select(-overall) %>%
  left_join(summary, by = "want_and_tried")

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 015 | average number of trips for those who tried and failed to get Aadhaar (footnote) ----
descr <- "Average # trips for those who tried but failed to enroll into Aadhaar (excluding 0 trips responses)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(zr_enrolment_flag == "Yes",
         xr_has_aadhaar == "No",
         zr_enrolment_trips_to_center > 0) %>% # NOTE: We are removing zero trips from the calculation of average number of trips
  summariser(zr_enrolment_trips_to_center)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 016 | Enrolment by religion ----
descr <- "% of enrolment by religion"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_religion, type = "flipped")

logmsg("Note: There are 632 individuals, including household members, who have an invalid response, and are dropped")
logmsg("Please see CMIE analysis for larger sample")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 017 | Enrolment by caste ----
descr <- "% of enrolment by caste"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_caste, type = "flipped")

logmsg("Note: There are 632 individuals, including household members, who have an invalid response, and are dropped")
logmsg("Please see CMIE analysis for larger sample")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 018 | Enrolment by gender ----
descr <- "% of enrolment by gender"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, xr_gender_final, type = "flipped")

logmsg("Note: There are 632 individuals, including household members, who have an invalid response, and are dropped")
logmsg("Please see CMIE analysis for larger sample")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 019 | Enrolment by rural/urban ----
descr <- "% of enrolment by rural/urban"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_urban_rural2, type = "flipped", compare = "Rural")

logmsg("Note: There are 632 individuals, including household members, who have an invalid response, and are dropped")
logmsg("Please see CMIE analysis for larger sample")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 020 | Enrolment and % urban population by state ----
descr <- "Enrolment and % urban population by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_state, type = "flipped")

summary <- random_mem_df %>%
  summariser_mem(zh_urban_rural2, zh_state, type = "flipped") %>% 
  select(zh_state, perc_urban) %>% 
  left_join(summary, by = "zh_state")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 021 | % of third gender who have Aadhaar  ----
descr <- "% of third gender who have Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_df %>%
  filter(zr_third_gender == "Yes") %>%
  summariser(xr_has_aadhaar)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 022 | Among those third gender who do not have Aadhaar, % who want ----
descr <- "Among those third gender who dont have aadhaar, % who want"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_df %>%
  filter(zr_third_gender == "Yes",
         xr_has_aadhaar == "No") %>%
  summariser(zr_want_to_enrol)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 023 | reasons for not trying for Aadhaar among third gender adults who wanted ----
descr <- "reasons for not trying for Aadhaar among third gender adults who wanted"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp_df <- purposive_df %>%
  filter(zr_want_to_enrol == "Yes",
         zr_enrolment_flag == "No",
         zr_third_gender == "Yes") %>%
  mutate_at(vars(matches("xr_not_getting_aadhaar_reason_.*")), ~ ifelse(
    str_detect(., paste("bandho", "bondho", "bandha", "centre", "bondh", "far", sep = "|")),
    "there are no enrolment centers nearby",
    .
  )) %>%
  unite(temp_not_getting_aadhaar, matches("xr_not_getting_aadhaar_reason_.*"), sep = "|")

list_of_response <- temp_df %>%
  pull(temp_not_getting_aadhaar) %>%
  str_split("\\|") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  tabyl() %>%
  filter(n > 1) %>% # Filtering for responses which occured more than once
  pull(".")

summary <- map_dfr(list_of_response, function(x) {
  temp_df %>%
    mutate(temp = split_and_match(temp_not_getting_aadhaar, x),
           temp = ifelse(temp == TRUE, "Yes", "No")) %>%
    summariser(temp) %>%
    filter(temp == "Yes") %>%
    mutate(response = x) %>%
    select(-temp)
}) %>%
  mutate(temp = perc_to_dec(perc_overall)) %>%
  arrange(-temp) %>%
  select(-temp, response, everything())

logtable(summary, descr)
summary_list[[descr]] <- summary
  
rm(summary, list_of_response, temp_df, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 024 | % of homeless without Aadhaar  ----
descr <- "% of homeless without Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_mem_df %>%
  filter(zh_homeless == "Yes") %>%
  summariser_mem(xr_has_aadhaar)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 025 | Among those homeless who dont have aadhaar, % who want  ----
descr <- "Among those homeless adults who dont have aadhaar, % who want"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_df %>%
  filter(zh_homeless == "Yes",
         xr_has_aadhaar == "No") %>%
  summariser(zr_want_to_enrol)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 026 | reasons for not trying for Aadhaar among homeless adults who wanted ----
descr <- "reasons for not trying for Aadhaar among homeless adults who wanted"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp_df <- purposive_df %>%
  filter(zr_want_to_enrol == "Yes",
         zr_enrolment_flag == "No",
         zh_homeless == "Yes") %>%
  mutate_at(vars(matches("xr_not_getting_aadhaar_reason_.*")), ~ ifelse(
    str_detect(., paste("bandho", "bondho", "bandha", "centre", "bondh", "far", sep = "|")),
    "there are no enrolment centers nearby",
    .
  )) %>%
  unite(temp_not_getting_aadhaar, matches("xr_not_getting_aadhaar_reason_.*"), sep = "|")

list_of_response <- temp_df %>%
  pull(temp_not_getting_aadhaar) %>%
  str_split("\\|") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  tabyl() %>%
  filter(n > 1) %>% # Filtering for responses which occured more than once
  pull(".")

summary <- map_dfr(list_of_response, function(x) {
  temp_df %>%
    mutate(temp = split_and_match(temp_not_getting_aadhaar, x),
           temp = ifelse(temp == TRUE, "Yes", "No")) %>%
    summariser(temp) %>%
    filter(temp == "Yes") %>%
    mutate(response = x) %>%
    select(-temp)
}) %>%
  mutate(temp = perc_to_dec(perc_overall)) %>%
  arrange(-temp) %>%
  select(-temp, response, everything())

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, list_of_response, temp_df, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 027 | Amongst homeless without Aadhaar, % who made multiple trips (footnote) ----
descr <- "Amongst homeless adults without Aadhaar, % who made multiple trips (footnote)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_df %>%
  filter(zh_homeless == "Yes",
         xr_has_aadhaar == "No") %>% 
  mutate(multiple_trips = case_when(
    zr_enrolment_trips_to_center == 0 ~ "No trips",
    zr_enrolment_trips_to_center == 1 ~ "Single trip",
    zr_enrolment_trips_to_center >= 2 ~ "2 or more trips",
    TRUE ~ NA_character_)) %>%
  filter(!is.na(multiple_trips)) %>%
  summariser(multiple_trips)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 028 | Amongst homeless without Aadhaar, % who have any ID including aadhaar (footnote) ----
descr <- "Among homeless adults without Aadhaar, % who have any other ID (footnote)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_df %>%
  filter(zh_homeless == "Yes",
         xr_has_aadhaar == "No",
  ) %>%
  summariser(zr_has_any_id)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 029 | % of SECC vulnerable who have Aadhaar outside Assam ----
descr <- "% of SECC included members outside Assam, who have Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>% 
  filter(!is.na(xr_has_aadhaar), zh_state != "Assam", xr_age_final >= 18) %>% 
  summariser_mem(xr_has_aadhaar, zh_secc_classification)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 030 | % of SECC vulnerable amongst total population outside Assam ----
descr <- "% of SECC included amongst total population outside Assam"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(zh_state != "Assam", xr_age_final >= 18) %>% 
  summariser_mem(zh_secc_classification)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 031 | % of SECC vulnerable amongst individuals without aadhaar outside Assam ----
descr <- "% of SECC vulnerable amongst individuals without aadhaar outside Assam"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(zh_state != "Assam", xr_has_aadhaar == "No", xr_age_final >= 18) %>% 
  summariser_mem(zh_secc_classification)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 032 | % of indivuals with incorrect information printed on their aadhaar ----
descr <- "Amongst adults with Aadhaar, % of individuals who have incorrect info ON THEIR CARD"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_aadhaar_has_error_on_card)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 033 | Amongst individuals with Aadhaar, % of individuals who have incorrect info ON THEIR CARD ----
descr <- "Amongst individuals with Aadhaar, % of individuals who have incorrect info ON THEIR CARD"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_aadhaar_has_error_on_card)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 034 | Amongst individuals with Aadhaar, % of individuals who have incorrect info ----
descr <- "Amongst individuals with Aadhaar, % of individuals who have incorrect info"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_aadhaar_has_error)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 035 | Amongst individuals with Aadhaar, % of individuals who have incorrect DATA ----
descr <- "Amongst individuals with Aadhaar, % of individuals who have incorrect DATA"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- data.frame()

for (var in c("name", "dob", "address", "photo", "biometrics", "gender", "mob_num")){
  summary <- random_df %>% 
    filter(xr_has_aadhaar == "Yes") %>% 
    summariser(!!(paste0("xr_aadhaar_", var, "_error_flag"))) %>% 
    filter(get(paste0("xr_aadhaar_", var, "_error_flag")) == "Yes") %>% 
    mutate(error_in = var) %>%
    select(everything(), -contains("xr_aadhaar_")) %>% 
    rbind(summary)
}

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH | Amongst individuals with Aadhaar and have biometric, % of individuals who wanted to update ----
descr <- "Amongst individuals with Aadhaar and have biometric, % of individuals who wanted to update"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak("ADH", descr)

summary <- data.frame()

for (var in c("name", "dob", "address", "photo", "biometrics", "gender", "mob_num")){
  summary <- random_df %>% 
    filter(xr_has_aadhaar == "Yes", get(paste0("xr_aadhaar_", var, "_error_flag")) == "Yes") %>% 
    summariser(!!(paste0("xr_aadhaar_", var, "_wanted_to_update_flag"))) %>% 
    filter(get(paste0("xr_aadhaar_", var, "_wanted_to_update_flag")) == "Yes") %>% 
    mutate(wanted_to_update = var) %>%
    select(everything(), -contains("xr_aadhaar_")) %>% 
    rbind(summary)
}

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 036 | Among third gender adults % with inaccurate aadhaar ----
descr <- "Among third gender adults, % with inaccurate aadhaar (on card)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_df %>% 
  filter(zr_third_gender == "Yes",
         xr_has_aadhaar == "Yes") %>% 
  summariser(zr_aadhaar_has_error_on_card)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 037 | Among third gender adults % with inaccurate gender ----
descr <- "Among third gender adults, % with inaccurate gender"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- purposive_df %>%
  filter(zr_third_gender == "Yes",
         xr_has_aadhaar == "Yes") %>% 
  mutate(has_gender_error = ifelse(xr_aadhaar_gender_error_flag == "Yes",
                                           "Yes",
                                           "No")) %>%
  summariser(has_gender_error)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 038 | % of adults with incorrect voter ID and Aadhaar amongst those who have it ----
descr <- "Amongst those who have them, % of adults with incorrect voter ID and Aadhaar (card)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

voter_id <- random_df %>%
  filter(xr_has_voter_card_flag == "Yes") %>%
  summariser(xr_voter_id_correct, zh_state, type = "flipped") %>%
  select(zh_state, n_voter_id_error = n_no, perc_voter_id_error = perc_no)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_aadhaar_has_error_on_card, zh_state, type = "flipped") %>%
  select(zh_state, n_aadhaar_error = n_yes, perc_aadhaar_error_on_card = perc_yes) %>%
  left_join(voter_id, by = "zh_state")

summary <- summary %>% 
  mutate(more_error_in_aadhaar = ifelse(perc_to_dec(perc_aadhaar_error_on_card) > perc_to_dec(perc_voter_id_error), "Yes!", "-"))

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr, voter_id)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 039 | % of indivuals with incorrect information printed on their aadhaar by state ----
descr <- "Among adults who have aadhaar, % with incorrect information printed by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_aadhaar_has_error_on_card, zh_state, type = "flipped")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 040 | Average years of education for individuals with incorrect data on Aadhaar ----
descr <- "Average years of education for individuals with incorrect data on Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- data.frame()

for (var in c("name", "dob", "address", "photo", "biometrics", "gender", "mob_num")){
  summary <- random_df %>% 
    filter(xr_has_aadhaar == "Yes") %>% 
    summariser(zr_education_level_years, !!(paste0("xr_aadhaar_", var, "_error_flag"))) %>% 
    filter(get(paste0("xr_aadhaar_", var, "_error_flag")) == "Yes") %>% 
    mutate(error_in = var) %>%
    select(everything(), -contains("xr_aadhaar_")) %>% 
    rbind(summary)
}

summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_education_level_years, zr_aadhaar_has_error) %>% 
  filter(zr_aadhaar_has_error == "Yes") %>% 
  mutate(error_in = "any error") %>%
  select(everything(), -contains("zr_aadhaar_")) %>% 
  rbind(summary)

summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_education_level_years, zr_aadhaar_has_error_on_card) %>% 
  filter(zr_aadhaar_has_error_on_card == "Yes") %>% 
  mutate(error_in = "any error on card") %>%
  select(everything(), -contains("zr_aadhaar_")) %>% 
  rbind(summary)

summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_education_level_years, zr_aadhaar_has_error_on_card) %>% 
  filter(zr_aadhaar_has_error_on_card == "No") %>% 
  mutate(error_in = "no error on card") %>%
  select(everything(), -contains("zr_aadhaar_")) %>% 
  rbind(summary)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 041 | Among adults who have aadhaar, % who have given their mobile number ----
descr <- "Among adults who have aadhaar, % who have given their mobile number"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_link_aadhaar_to_phone)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 042 | Among adults who have given their mobile number, % who have incorrect mobile number ----
descr <- "Among adults who have aadhaar, % who have incorrect mobile number"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_aadhaar_mob_num_error_flag)

logtable(summary, descr)
summary_list[[descr]] <- summary

logmsg("Please see CMIE analysis")
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 043 | Amongst those who needed to update, % adults who tried to update ----
descr <- "Amongst those who needed to update, % adults who tried to update"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(zr_needed_to_update_final == "Yes") %>%
  summariser(zr_ever_tried_to_update)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 044 | Amongst those who needed to update and tried to update, % who were successful ----
descr <- "Amongst those who needed to update and tried to update, % who were successful"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(zr_ever_tried_to_update == "Yes") %>%
  summariser(xr_aadhaar_update_successful_flag)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 045 | Amongst those who needed to update, % who wants to update ----
descr <- "Amongst those who needed to update, % who wants to update"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(zr_needed_to_update_final == "Yes") %>%
  summariser(zr_wanted_to_update_final)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 046 | Amongst those who needed to update and want to update, % that have ever tried ----
descr <- "Amongst those who needed to update and want to update, % that have ever tried" 
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(zr_needed_to_update_final == "Yes") %>% 
  mutate(want_to_update_havent_tried = 
           ifelse(zr_wanted_to_update_final == "Yes" & zr_ever_tried_to_update == "No", 
                  "Yes", "No")) %>%
  summariser(want_to_update_havent_tried)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 047 | reasons for not trying to update for adults who ever had error, wanted to update, but did not try ----
descr <- "reasons for not trying to update for adults who needed to update, wanted to update, but did not try"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

list_of_response <- random_df %>%
  select(matches("xr_not_update_aadhaar_reason_.*")) %>%
  gather() %>%
  filter(value != "", value != "other") %>%
  pull(value) %>%
  trimws() %>%
  tolower() %>%
  unique()

temp_df <- random_df %>%
  filter(zr_needed_to_update_final == "Yes",
         zr_wanted_to_update_final == "Yes",
         zr_ever_tried_to_update == "No")

summary <- map_dfr(list_of_response, function(x) {
  
  temp_df %>%
    mutate(temp = split_and_match(zr_not_update_aadhaar_reason, x),
           temp = ifelse(temp == TRUE, "Yes", "No")) %>%
    summariser(temp) %>%
    filter(temp == "Yes") %>%
    mutate(response = x) %>%
    select(-temp)
}) %>%
  mutate(temp = perc_to_dec(perc_overall)) %>%
  arrange(-temp) %>%
  select(-temp)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(temp_df, list_of_response, summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 048 | Amongst those who tried to update, % who found the process to be difficult ----
descr <- "Amongst those who tried to update, % who found the process to be difficult" 
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(zr_ever_tried_to_update == "Yes") %>% 
  summariser(xr_update_ease_of_process)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
logmsg("Refer to figure 9")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 049 | Amongst those who tried to update, expenditure based on ease/difficulty ----
descr <- "Amongst those who tried to update, expenditure based on ease/difficulty"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- data.frame()
for (var in c("documents", "travel", "process_fee", "agent", "bribes")){
  summary <- random_df %>%
    filter(zr_ever_tried_to_update == "Yes",
           get(paste0("xr_update_expense_on_", var, "_flag")) == "Rs") %>%
    summariser(!!paste0("xr_update_expense_on_", var, "_value"), xr_update_ease_of_process) %>% 
    mutate(metric = var, 
           easy_pay = .[.$xr_update_ease_of_process == "Easy", "value"] %>% unique() %>% unlist(),
           difficult_pay = .[.$xr_update_ease_of_process == "Difficult", "value"] %>% unique() %>% unlist(),
           `-` = " ",
           n_easy = .[.$xr_update_ease_of_process == "Easy", "overall"] %>% unique() %>% unlist(),
           easy_pay_low = .[.$xr_update_ease_of_process == "Easy", "value_low"] %>% unique() %>% unlist(),
           easy_pay_upp = .[.$xr_update_ease_of_process == "Easy", "value_upp"] %>% unique() %>% unlist(),
           n_difficult = .[.$xr_update_ease_of_process == "Difficult", "overall"] %>% unique() %>% unlist(),
           difficult_pay_low = .[.$xr_update_ease_of_process == "Difficult", "value_low"] %>% unique() %>% unlist(),
           difficult_pay_upp = .[.$xr_update_ease_of_process == "Difficult", "value_upp"] %>% unique() %>% unlist()) %>% 
    select(metric, easy_pay, difficult_pay, `-`, 
           n_easy, easy_pay_low, easy_pay_upp,
           n_difficult, difficult_pay_low, difficult_pay_upp) %>% 
    grouper() %>% 
    select(-n) %>% 
    rbind(summary)
}

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 050 | Amongst those who tried to update, # adults who had to pay based on ease/difficulty ----
descr <- "Amongst those who tried to update, # adults who had to pay based on ease/difficulty"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- data.frame()
for (var in c("documents", "travel", "process_fee", "agent", "bribes")){
  summary <- random_df %>%
    filter(zr_ever_tried_to_update == "Yes") %>%
    summariser(!!paste0("xr_update_expense_on_", var, "_flag"), xr_update_ease_of_process, compare = "difficult") %>%
    filter(get(paste0("xr_update_expense_on_", var, "_flag")) == "Rs") %>% 
    mutate (metric = var) %>% 
    select(metric, contains("difficult"), contains("easy"), -contains("neither_easy_nor_difficult")) %>% 
    rbind(summary)
}

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary

rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 051 | Ease of update over time ----
descr <- "Ease of update over time amongst those who have tried to update"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(zr_ever_tried_to_update == "Yes") %>%
  mutate(max_update_date = pmax(zr_aadhaar_name_tried_to_update_date,
                                zr_aadhaar_dob_tried_to_update_date,
                                zr_aadhaar_address_tried_to_update_date,
                                zr_aadhaar_photo_tried_to_update_date,
                                zr_aadhaar_biometrics_tried_to_update_date,
                                zr_aadhaar_gender_tried_to_update_date,
                                zr_aadhaar_mob_num_tried_to_update_date, na.rm = TRUE),
         max_update_year = as.character(lubridate::year(lubridate::ymd(max_update_date)))) %>%
  filter(!is.na(max_update_year)) %>%
  summariser(xr_update_ease_of_process, max_update_year, type = "flipped")

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 052 | Number of updates by time ----
descr <- "Number of updates over time"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
list_of_response <- random_df %>%
  select(matches("zr_aadhaar_.*_tried_to_update_date")) %>%
  gather() %>%
  filter(!is.na(value)) %>%
  mutate(year = lubridate::year(lubridate::ymd(value))) %>%
  pull(year) %>%
  unique()

temp_df <- random_df %>%
  filter(zr_ever_tried_to_update == "Yes")

summary <- map_dfr(list_of_response, function(x) {

  temp_df %>%
    mutate(temp = split_and_match(zr_years_update, x),
           temp = ifelse(temp == TRUE, "Yes", "No")) %>%
    summariser(temp) %>%
    filter(temp == "Yes") %>%
    mutate(response = x) %>%
    select(-temp)
}) %>%
  mutate(temp = perc_to_dec(perc_overall)) %>%
  arrange(-response) %>%
  select(-temp)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(temp_df, list_of_response, summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 053 | % of adults with correct aadhaar at time of issuance ----
descr <- "Amongst those adults who have Aadhaar, % with correct aadhaar at time of issuance"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_aadhaar_issued_info_accurate_flag)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 054 | % of adults with Aadhaar, for whom any data changed after issuance ----
descr <- "% of adults with Aadhaar, for whom any data changed after issuance"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_any_aadhaar_data_changed)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~

########################### ADHOC: ##########################-----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH | # of states for whom error in Aadhaar is greater than error in voter ID (footnote) ----
descr <- "# of states for whom error in Aadhaar is greater than error in voter ID"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak("ADH", descr)

summary <- random_df %>%
  filter(xr_has_voter_card_flag == "Yes") %>%
  summariser(xr_voter_id_correct, zh_state, type = "flipped") %>%
  select(zh_state, perc_voter = perc_no)

random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_aadhaar_has_error_on_card, zh_state, type = "flipped") %>%
  select(zh_state, perc_aadhaar = perc_yes) %>%
  left_join(summary, by = "zh_state") %>%
  filter(zh_state != "Overall") %>%
  mutate_at(vars(-zh_state), perc_to_dec) %>%
  mutate(temp = perc_aadhaar > perc_voter) %>%
  pull(temp) %>%
  sum() %>%
  paste0(" out of 17 states showed greater error in Aadhaar than that in voter ID") %>%
  logmsg()

random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_aadhaar_has_error_on_card, zh_state, type = "flipped") %>%
  select(zh_state, perc_aadhaar = perc_yes) %>%
  left_join(summary, by = "zh_state") %>%
  filter(zh_state != "Overall") %>%
  mutate_at(vars(-zh_state), perc_to_dec) %>%
  mutate(temp = perc_aadhaar > perc_voter) %>%
  filter(temp) %>%
  pull(zh_state) %>%
  paste0(collapse = ", ") %>%
  paste0("These states are: ", .) %>%
  logmsg()


rm(summary, descr)
# ~~~~~~~~~


########################### FIGURES: ##########################-----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 03 | % of individuals with Aadhaar by state ----
logbreak(3, "% of respondents with aadhaar by state", suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 04 | %, and estimated number, of individuals without Aadhaar ----
logbreak(4, "%, and estimated number, of individuals without Aadhaar", suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 05 | reasons for not having Aadhaar ----
logbreak(5, "reasons for not trying for Aadhaar among adults who wanted", suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** pain points for the amount of travel ....
temp <- random_df %>%
  filter(xr_has_aadhaar == "No", zr_enrolment_flag == "Yes") %>%
  summariser(zr_enrolment_trips_to_center) %>%
  mutate(metric = "Trips to enrolment centre") %>%
  select(metric, value)

temp <- random_df %>%
  filter(xr_has_aadhaar == "No", zr_enrolment_flag == "Yes") %>%
  summariser(zr_enrolment_mins_to_center) %>%
  mutate(metric = "Travel time to enrolment centre (mins)") %>%
  select(metric, value) %>%
  bind_rows(temp)

temp <- random_df %>%
  filter(xr_has_aadhaar == "No", zr_enrolment_flag == "Yes") %>%
  summariser(zr_enrolment_expense_total) %>%
  mutate(metric = "Average money spent on enrolment (Rs)") %>%
  select(metric, value) %>%
  bind_rows(temp)

figure5 <- list() 

figure5[["Pain points for tried"]] <- random_df %>%
  filter(xr_has_aadhaar == "No", zr_enrolment_flag == "Yes") %>%
  mutate(used_redressal = ifelse(xr_tried_to_enrol_unsuccessfully_called_helpline == "Yes" |
                                   xr_tried_to_enrol_unsuccessfully_filed_complaint == "Yes", 
                                 "Yes", 
                                 "Nopes")) %>%
  summariser(used_redressal) %>%
  filter(used_redressal == "Yes") %>%
  mutate(metric = "% of individuals who used grevience redressal",
         value = perc_to_dec(perc_overall)) %>%
  select(metric, value) %>%
  bind_rows(temp)

figure5[["Pain points for others"]] <- summary_list[["reasons for not trying for Aadhaar among adults who wanted"]]

export(figure5,
       file.path(v_loc["figures_data"], 
                 "Figure 05. Reasons given for not having Aadhaar.xlsx"),
       colWidths = "auto")

logmsg("data exported for figure 5")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 06 | % of individuals without Aadhaar by demographic groups ----
logbreak(6, "% of individuals without Aadhaar by demographic groups", suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
third_gender <- purposive_df %>%
  filter(zr_third_gender == "Yes") %>%
  summariser(xr_has_aadhaar) %>%
  mutate(output = "Third gender*",
         value = perc_to_dec(perc_overall)) %>%
  filter(xr_has_aadhaar == "No") %>%
  select(output, value)

export(third_gender, 
       file.path(v_loc["figures_data"], 
                 "Figure 06. Share of people who do not have Aadhaar_third gender.xlsx"))

logmsg("data exported for figure 6", gap = 0)
logmsg("Please see CMIE analysis for additional cuts")
rm(third_gender)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 007 | % of individuals with incorrect information printed on their Aadhaar by state ----
logbreak(7, "Of those who have Aadhaar, % of individuals with incorrect on their Aadhaar CARD, by state", suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 008 | % of individuals with incorrect Aadhaar by type of data ----
logbreak(8, "% of individuals with incorrect Aadhaar by type of data", suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# Figure 009 | share of people who describe Aadhaar related processes as difficult ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(9, "share of people who describe Aadhaar related processes as difficult", suffix = "F")

output_variable <- c("zr_enrolment_ease_of_process",
                     "xr_update_ease_of_process",
                     "xr_linking_pds_with_aadhaar_ease",
                     "xr_linking_mnrega_with_aadhaar_ease",
                     "xr_linking_social_pension_with_aadhaar_ease",
                     "xr_linking_dbt_with_bank_account_ease")


label_text <- c("Enrol in Aadhaar",
                "Update Aadhaar",
                "Link to PDS",
                "Link to MGNREGS",
                "Link to social pensions",
                "Link to DBT and bank account")

temp <- map2_dfr(output_variable, label_text, function(x, y) {
  to_drop <- random_df %>%
    select(output = x, everything()) %>%
    filter(!output %in% c(" ", ""),
           !is.na(output))
  
  if (y == "Update Aadhaar") to_drop <- filter(to_drop, zr_ever_tried_to_update == "Yes")
  
  number_of_responses <- nrow(to_drop) %>% 
    format(big.mark = ",", scientific = FALSE)
  
  to_drop %>%
    mutate(output = case_when(
      output == "Didn't give Aadhaar to link to MNREGA job card" ~ "Didn't give Aadhaar",
      output == "Don't Know" ~ "Don't know",
      TRUE ~ output
    )) %>%
    summariser(output) %>%
    select(value = output, perc_overall) %>%
    mutate(category = str_wrap(y, 30),
           perc_overall = perc_to_dec(perc_overall),
           number_of_responses = number_of_responses)
}) %>%
  filter(value == "Difficult") %>%
  mutate(category = fct_reorder(category, perc_overall, .desc = TRUE)) 

export(temp, 
       file.path(v_loc["figures_data"],
                 "Figure 09. Share of people who found Aadhaar-related processes difficult.xlsx"))

rm(temp, label_text, output_variable)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save summary list ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# saveRDS(summary_list, file.path(v_loc[["outputs"]], "B1_Summary tables.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPEN OUTPUT LOG FILE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("Finished building 'B1_GETTING AADHAAR.R'")