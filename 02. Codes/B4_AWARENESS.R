# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outfile <- file.path(v_loc["logs"], "B4_AWARENESS.txt")
unlink(outfile)
logmsg("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 4. AWARENESS SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", cursor = "")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading datasets and local vars ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (!(exists("random_df") && is.data.frame(random_df))) {
  random_df <- readRDS(file = file.path(v_loc[["datasets"]], 
                                        "02. State of Aadhaar In-depth 2019", 
                                        "20191118_State of Aadhaar_in-depth survey_random_sample_hh.Rda"))
  logmsg("Random DF loaded from local")}

if (!(exists("purposive_df") && is.data.frame(purposive_df))) {
  purposive_df <- readRDS(file = file.path(v_loc[["datasets"]], 
                                           "02. State of Aadhaar In-depth 2019", 
                                           "20191118_State of Aadhaar_in-depth survey_over_sample_hh.Rda"))
  logmsg("Purposive DF loaded from local")}

if (!(exists("random_mem_df") && is.data.frame(random_mem_df))) {
  random_mem_df <- readRDS(file = file.path(v_loc[["datasets"]], 
                                            "02. State of Aadhaar In-depth 2019", 
                                            "20191118_State of Aadhaar_in-depth survey_random_sample_mem.Rda"))
  logmsg("Random member DF loaded from local")}

if (!(exists("purposive_mem_df") && is.data.frame(purposive_mem_df))){
  purposive_mem_df <- readRDS(file = file.path(v_loc[["datasets"]], 
                                               "02. State of Aadhaar In-depth 2019", 
                                               "20191118_State of Aadhaar_in-depth survey_over_sample_mem.Rda"))
  logmsg("Purposive member DF loaded from local")}

summary_list <- list()
analysis_number <- 0
# ~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 001 | Among individuals who tried to enrol, % who fully or mostly understand Aadhaar enrolment ----
descr <- "Among individuals who tried to enrol, % who fully or mostly understand Aadhaar enrolment"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(zr_enrolment_flag == "Yes") %>%
  summariser(zr_enrolment_process_knowledge)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 002 | Among individuals who tried to enrol, % who fully or mostly understand Aadhaar updating ----
descr <- "Among individuals who tried to update, % who fully or mostly understand Aadhaar updating"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(zr_ever_tried_to_update == "Yes") %>%
  summariser(xr_update_process_knowledge)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 003 | Among individuals who tried but failed to enrol, % of individuals who understand Aadhaar enrolment ----
descr <- "Among individuals who tried but failed to enrol, % of individuals who understand Aadhaar enrolment"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "No",
         zr_enrolment_flag == "Yes") %>%
  summariser(zr_enrolment_process_knowledge)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 004 | Among individuals who tried but failed to update, % of individuals who understand Aadhaar update ----
descr <- "Among individuals who tried but failed to update, % of individuals who understand Aadhaar update"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(zr_ever_tried_to_update == "Yes",
         xr_aadhaar_update_successful_flag == "No") %>%
  summariser(xr_update_process_knowledge)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 005 | Among those who tried to enrol, knowledge of enrolment/update process by demographics ----
descr <- "Among those who tried to enrol/update, knowledge of enrolment/update process by demographics"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

# ** variables needing member data ----
y_var1 <- c()

# ** variables needing random and purposive data ----
y_var2 <- c("zr_enrolment_process_knowledge", "xr_update_process_knowledge")

# ** numeric variables ----
y_var3 <- c()

# ** from mem vs segment ---
seg_rand_resp <- c("zr_disability_flag", "zr_education_level_years_bucket", "zr_aadhaar_has_error")
seg_purp_resp <- c("zh_laborer", "zh_migrant_laborer")
seg_rand_memb <- c("xr_gender_final",  "zh_secc_classification", "zh_caste", "zh_religion",
                   "zr_age_bucket1", "zh_urban_rural2", "zh_state")
seg_purp_memb <- c("zr_elderly", "zh_homeless", "zr_third_gender")

benchmark_adult <- c(seg_rand_resp, seg_purp_resp, "zr_elderly", "zr_third_gender")

flip <- c("zh_caste", "zh_religion", "zr_age_bucket1", "zh_state")
# ~~~~~~~~~


# ** cross grid ----

i = 1
for(segment in c(seg_rand_resp, seg_purp_resp, seg_rand_memb, seg_purp_memb)){

  j = 1
  for (var in c(y_var1, y_var2, y_var3)){

    # ** for variable to be extracted from member level data, i.e. penetration ----

    if (var %in% y_var1) {

      benchmark_mem_df <- random_mem_df %>%
        filter(zr_age_bucket3 %in% ifelse(segment %in% benchmark_adult,
                                          c("Adults", "Elderly (71 and above)"),
                                          c("Children", "Adults", "Elderly (71 and above)")))

      if (segment %in% c(seg_rand_resp)){
        df <- random_df
        compare_val <- "overall"
        file_name <- "random_df"
        
      } else if (segment %in% c(seg_rand_memb)){
        df <- random_mem_df %>%
          select(xh_resp_weight = xh_mem_weight, everything())
        compare_val <- "overall"
        file_name <- "random_mem_df"
        
      } else if (segment %in% c(seg_purp_memb)){
        df <- purposive_mem_df %>%
          select(intersect(colnames(purposive_mem_df), colnames(benchmark_mem_df))) %>%
          rbind(select(benchmark_mem_df, intersect(colnames(purposive_mem_df), colnames(benchmark_mem_df)))) %>%
          select(xh_resp_weight = xh_mem_weight, everything())
        compare_val <- "random"
        file_name <- "purposive_mem_df"
        
      } else if (segment %in% c(seg_purp_resp)){

        benchmark_mem_df <- benchmark_mem_df %>%
          select(xh_resp_weight = xh_mem_weight, everything())

        df <- rbind(
          purposive_df %>%
            select(intersect(colnames(benchmark_mem_df), colnames(purposive_df))),
          benchmark_mem_df %>%
            select(intersect(colnames(benchmark_mem_df), colnames(purposive_df)))
        )
        compare_val <- "random"
        file_name <- "purposive_df"
      }

      rm(benchmark_mem_df)

    } else if (var %in% c(y_var2, y_var3)){

      if (segment %in% c(seg_rand_resp, seg_rand_memb)){
        df <- random_df
        compare_val <- "overall"
        file_name <- "random_df"

      } else if (segment %in% c(seg_purp_resp, seg_purp_memb)){
        df <- rbind(purposive_df, random_df)
        compare_val <- "random"
        file_name <- "purposive_df"
      }
    }

    type_val <- "default"
    if (segment %in% flip) {
      type_val <- "flipped"
    }
    descr <- paste0("summarise ", toupper(var), " :by: ", toupper(segment), " | ", file_name)

    logmsg(paste0("[Analysis: ", i, "]==================================================================================="),
           cursor = "", gap = 0)
    logmsg(descr, cursor = "")

    summary <- df %>%
      select(segment_temp = segment, var_temp = var, everything()) %>%
      filter(trim(var_temp) != "", !is.na(var_temp), !is.na(segment_temp)) %>%
      mutate(var_temp = case_when(
        var_temp %in% c("Yes, I knew exactly", "Yes, I mostly knew") ~ "Exactly or mostly knew",
        var_temp %in% c("I don't remember", "I partially knew",
                        "No, I didn't know at all", "No, I knew very little") ~ "Little or did not know",
        TRUE ~ NA_character_)) %>%
      summariser(var_temp, segment_temp, vartype = "ci", compare = compare_val, type = type_val)

    if (compare_val == "random" & var %in% c(y_var1, y_var2)){
      summary <- summary %>%
        select(-overall, -perc_overall, -perc_overall_low, -perc_overall_upp)
    }

    print(paste0("Completed: ",
                 floor(i*100/(length(c(seg_rand_resp, seg_rand_memb, seg_purp_resp, seg_purp_memb))*
                                length(c(y_var1, y_var2, y_var3)))),
                 "% overall | ",
                 floor(j*100/(length(c(y_var1, y_var2, y_var3)))),
                 "% for the segment..."))
    i = i + 1
    j = j + 1

    logtable(summary)
    summary_list[[descr]] <- summary

    rm(df, compare_val, summary, descr)
  }
}
rm(i, j)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 006 | PREP Among those who are eligible, individuals who found all processes easy/difficult ----
descr <- "PREP Among those who are eligible, individuals who found all processes easy/difficult"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  mutate(
    eligible_dbt = ifelse(xr_dbt_flag == "Yes", "Yes", "No"),
    eligible_pds = ifelse(xr_has_aadhaar == "Yes" & 
                            xr_has_pds_card == "Yes" &
                            xh_tried_getting_pds_in_last_3months_flag == "Yes" &
                            !is.na(xh_link_aadhaar_to_pds_num_hh_member) &
                            xh_link_aadhaar_to_pds_num_hh_member > 0, 
                          "Yes", "No"),
    eligible_mnrega = ifelse(xr_has_aadhaar == "Yes" & xr_has_mnrega_card == "Yes", "Yes", "No"),
    eligible_pension =  ifelse(xr_has_aadhaar == "Yes" & xr_has_social_pension == "Yes", "Yes", "No"),
    eligible_enrolment = ifelse(zr_enrolment_flag == "Yes", "Yes", "No"),
    eligible_update = ifelse(zr_ever_tried_to_update == "Yes", "Yes", "No"))

summary %>% 
  select(eligible_dbt, xr_dbt_flag) %>% 
  grouper() %>% 
  logtable("QC: eligible_dbt")
    
summary %>% 
  select(eligible_pds, xr_has_aadhaar, xr_has_pds_card, xh_link_aadhaar_to_pds_num_hh_member) %>% 
  grouper() %>% 
  logtable("QC: eligible_pds")

summary %>% 
  select(eligible_mnrega, xr_has_aadhaar, xr_has_mnrega_card) %>% 
  grouper() %>% 
  logtable("QC: eligible_mnrega")

summary %>% 
  select(eligible_pension, xr_has_aadhaar, xr_has_social_pension) %>% 
  grouper() %>% 
  logtable("QC: eligible_pension")

summary %>% 
  select(eligible_enrolment, zr_enrolment_flag) %>% 
  grouper() %>% 
  logtable("QC: eligible_enrolment")

summary %>% 
  select(eligible_update, zr_ever_tried_to_update) %>% 
  grouper() %>% 
  logtable("eligible_update")


summary <- summary %>% 
  mutate(
    conside_in_base = case_when(
      eligible_dbt == "Yes" ~ "Yes",
      eligible_pds == "Yes" ~ "Yes",
      eligible_mnrega == "Yes" ~ "Yes",
      eligible_pension == "Yes" ~ "Yes",
      eligible_enrolment == "Yes" ~ "Yes",
      eligible_update == "Yes" ~ "Yes",
      TRUE ~ "No"))

summary %>% 
  select(conside_in_base, eligible_dbt, eligible_pds, eligible_mnrega, 
         eligible_pension, eligible_enrolment, eligible_update) %>% 
  grouper() %>% 
  logtable("QC: Calculating eligibility for base flag")

summary <- summary %>% 
  mutate(all_easy = case_when(
    
    conside_in_base == "No" ~ "No",
    
    eligible_dbt == "Yes" & xr_linking_dbt_with_bank_account_ease != "Easy" ~ "No",
    eligible_pds == "Yes" & xr_linking_pds_with_aadhaar_ease != "Easy" ~ "No",
    eligible_mnrega == "Yes" & xr_linking_mnrega_with_aadhaar_ease != "Easy" ~ "No",
    eligible_pension == "Yes" & xr_linking_social_pension_with_aadhaar_ease != "Easy" ~ "No",
    eligible_enrolment == "Yes" & zr_enrolment_ease_of_process != "Easy" ~ "No",
    eligible_update == "Yes" & xr_update_ease_of_process != "Easy" ~ "No",
    
    TRUE ~ "Yes")) %>%
  
  mutate(all_difficult = case_when(
    
    conside_in_base == "No" ~ "No",
    
    eligible_dbt == "Yes" & xr_linking_dbt_with_bank_account_ease != "Difficult" ~ "No",
    eligible_pds == "Yes" & xr_linking_pds_with_aadhaar_ease != "Difficult" ~ "No",
    eligible_mnrega == "Yes" & xr_linking_mnrega_with_aadhaar_ease != "Difficult" ~ "No",
    eligible_pension == "Yes" & xr_linking_social_pension_with_aadhaar_ease != "Difficult" ~ "No",
    eligible_enrolment == "Yes" & zr_enrolment_ease_of_process != "Difficult" ~ "No",
    eligible_update == "Yes" & xr_update_ease_of_process != "Difficult" ~ "No",
    
    TRUE ~ "Yes"))

summary %>% 
  
  mutate(xr_linking_dbt_with_bank_account_ease = 
           ifelse(xr_linking_dbt_with_bank_account_ease %in% c("Easy", "Difficult"), 
                  xr_linking_dbt_with_bank_account_ease, "Others"),
         
         xr_linking_pds_with_aadhaar_ease = 
           ifelse(xr_linking_pds_with_aadhaar_ease %in% c("Easy", "Difficult"), 
                  xr_linking_pds_with_aadhaar_ease, "Others"),
         
         xr_linking_mnrega_with_aadhaar_ease = 
           ifelse(xr_linking_mnrega_with_aadhaar_ease %in% c("Easy", "Difficult"), 
                  xr_linking_mnrega_with_aadhaar_ease, "Others"),
         
         xr_linking_social_pension_with_aadhaar_ease = 
           ifelse(xr_linking_social_pension_with_aadhaar_ease %in% c("Easy", "Difficult"), 
                  xr_linking_social_pension_with_aadhaar_ease, "Others"),
         
         zr_enrolment_ease_of_process = 
           ifelse(zr_enrolment_ease_of_process %in% c("Easy", "Difficult"), 
                  zr_enrolment_ease_of_process, "Others"),
         
         xr_update_ease_of_process = 
           ifelse(xr_update_ease_of_process %in% c("Easy", "Difficult"), 
                  xr_update_ease_of_process, "Others")) %>%
  
  mutate(xr_linking_dbt_with_bank_account_ease = ifelse(eligible_dbt == "No", 
                                                        "", xr_linking_dbt_with_bank_account_ease),
         
         xr_linking_pds_with_aadhaar_ease = ifelse(eligible_pds == "No",
                                                   "", xr_linking_pds_with_aadhaar_ease),
         
         xr_linking_mnrega_with_aadhaar_ease = ifelse(eligible_mnrega == "No",
                                                      "", xr_linking_mnrega_with_aadhaar_ease),
         
         xr_linking_social_pension_with_aadhaar_ease = ifelse(eligible_pension == "No",
                                                              "", xr_linking_social_pension_with_aadhaar_ease),
         
         zr_enrolment_ease_of_process = ifelse(eligible_enrolment == "No",
                                               "", zr_enrolment_ease_of_process),
         
         xr_update_ease_of_process = ifelse(eligible_update == "No",
                                            "", xr_update_ease_of_process)) %>% 
  
  select(all_easy, all_difficult, 
         eligible_dbt, ease_dbt = xr_linking_dbt_with_bank_account_ease,
         eligible_pds, ease_pds = xr_linking_pds_with_aadhaar_ease,
         eligible_mnrega, ease_mnrega = xr_linking_mnrega_with_aadhaar_ease,
         eligible_pension, ease_pension = xr_linking_social_pension_with_aadhaar_ease,
         eligible_enrolment, ease_enrolment = zr_enrolment_ease_of_process,
         eligible_update, ease_update = xr_update_ease_of_process) %>% 
  grouper() %>% 
  logtable("QC: Calculating all process easy/difficult flag")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 007 | Among those who are eligible, % individuals who found all processes easy ----
descr <- "Among those who are eligible, % individuals who found all processes easy"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary1 <- summary %>%
  filter(conside_in_base == "Yes") %>% 
  summariser(all_easy)

logbreak(analysis_number, descr)
logtable(summary1, descr)
summary_list[[descr]] <- summary1
rm(summary1, descr)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 008 | Among those who are eligible, % individuals who found all processes difficult ----
descr <- "Among those who are eligible, % individuals who found all processes difficult"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary1 <- summary %>%
  filter(conside_in_base == "Yes") %>% 
  summariser(all_difficult)

logbreak(analysis_number, descr)
logtable(summary1, descr)
summary_list[[descr]] <- summary1
rm(summary1, descr)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 009 | Among those who are eligible, % individuals who found enrolment easy by state ----
descr <- "Among those who are eligible, % individuals who found enrolment processes easy by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary1 <- random_df %>%
  filter(zr_enrolment_flag == "Yes") %>% 
  summariser(zr_enrolment_ease_of_process, zh_state, type = "flipped")

logbreak(analysis_number, descr)
logtable(summary1, descr)
summary_list[[descr]] <- summary1
rm(summary1, descr)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 010 | Among those who have Aadhaar, % who refused to provide Aadhaar when asked ----
descr <- "Among those who have Aadhaar, % who refused to provide Aadhaar when asked"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_refused_to_give_aadhaar_flag)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 011 | Among those who knew if they have aadhaar or not, % of individuals who knew about Aadhaar helpline ----
descr <- "Among those who knew if they have aadhaar or not, % of individuals who knew about Aadhaar helpline"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar %in% c("Yes", "No")) %>%
  summariser(xr_knowledge_update_helpline)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 012 | Among those who knew if they have aadhaar or not and faced problems with enrolment or update, % of individuals who knew about Aadhaar helpline ----
descr <- "Among those who knew if they have aadhaar or not and faced problems with enrolment or update, % of individuals who knew about Aadhaar helpline"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar %in% c("Yes", "No") &
           (xr_update_trips_to_center > 1 |
              zr_enrolment_trips_to_center > 1)) %>%
  summariser(xr_knowledge_update_helpline)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 013 | Among those who knew if they have aadhaar or not and faced problems with enrolment or update, % of individuals who knew about Aadhaar helpline and called ----
descr <- "Among those who knew if they have aadhaar or not and faced problems with enrolment or update, % of individuals who knew about Aadhaar helpline and called"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NUM1: xr_knowledge_update_helpline %in% c("Regional UIDAI/Aadhaar office number", "1947") & 
#       xr_tried_to_enrol_unsuccessfully_called_helpline == "Yes"
# DEN1: zr_enrolment_flag == "Yes" & xr_has_aadhaar == "No"
# 
# NUM2: xr_knowledge_update_helpline %in% c("Regional UIDAI/Aadhaar office number", "1947") & 
#       xr_update_called_helpline == "Yes"
# DEN2: zr_ever_tried_to_update == "Yes"

summary <- random_df %>%
  filter(xr_has_aadhaar %in% c("Yes", "No") &
           (xr_update_trips_to_center > 1 |
              zr_enrolment_trips_to_center > 1)) %>%
  mutate(knew_about_and_called_helpline = 
           ifelse(xr_knowledge_update_helpline %in% c(" Regional UIDAI/Aadhaar office number", "1947") & 
                    ((xr_tried_to_enrol_unsuccessfully_called_helpline == "Yes") | 
                       (xr_update_called_helpline == "Yes")), "Yes", "Nopes")) %>% 
  summariser(knew_about_and_called_helpline)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 014 | Among those who knew if they have aadhaar or not and faced problems with enrolment or update, % of individuals who filed ----
descr <- "Among those who knew if they have aadhaar or not and faced problems with enrolment or update, % of individuals who filed"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NUM1: xr_tried_to_enrol_unsuccessfully_filed_complaint == "Yes"
# DEN1: zr_enrolment_flag == "Yes" & xr_has_aadhaar == "No"
# 
# NUM2: xr_update_filed_complaint == "Yes"
# DEN2: zr_ever_tried_to_update == "Yes"

summary <- random_df %>%
  filter(xr_has_aadhaar %in% c("Yes", "No") &
           (xr_update_trips_to_center > 1 |
              zr_enrolment_trips_to_center > 1)) %>%
  mutate(filed_complaint = 
           ifelse(xr_tried_to_enrol_unsuccessfully_filed_complaint == "Yes" |
                    xr_update_filed_complaint == "Yes", "Yes", "Nopes")) %>% 
  summariser(filed_complaint)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 015 | Among those who have a PDS card and HH tried it in last 3 months, % who knew of atleast one alternative in case of fingerprint failure ----
descr <- "Among those who have a PDS card and HH tried it in last 3 months, % who knew of atleast one alternative in case of fingerprint failure"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_pds_card == "Yes") %>%
  mutate(dont_know_alternative = split_and_match(zr_knowledge_pds_if_fingerprint_fails_, c("Don't know", "nhi", "dont now", "finger", "fingar")),
         dont_know_alternative = ifelse(dont_know_alternative == TRUE, "Dont know any alternative", "Nopes")) %>%
  summariser(dont_know_alternative)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 011 | % of individuals who believe Aadhaar is required for all public and private services ----
descr <- "% of individuals who believe Aadhaar is required for all public and private services"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  mutate_at(vars(starts_with("xr_knowledge_aadhaar_mandatory_for_")), ~ ifelse(. == "Yes", 1, 0)) %>%
  mutate(
    aadhaar_is_necessary = rowSums(
      select(., starts_with("xr_knowledge_aadhaar_mandatory_for_")), na.rm = T),
    aadhaar_is_necessary = ifelse(aadhaar_is_necessary == 6, "Yes", "No")
  ) %>%
  summariser(aadhaar_is_necessary)

logtable(summary, descr)

summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 016 | Top alternatives in case of fingerprint failure ----
descr <- "Top alternatives in case of fingerprint failure"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

list_of_responses <- random_df %>%
  pull(zr_knowledge_pds_if_fingerprint_fails_) %>%
  str_split("\\|") %>%
  unlist() %>%
  tabyl() %>%
  filter(n > 10) %>%
  {.[, 1]} %>%
  {.[!. %in% c("", "Don't know")]} %>%
  ifelse(. == "take photo", "photo", .)

summary <- map_dfr(list_of_responses, function(variable) {
  random_df %>%
    filter(xr_has_pds_card == "Yes") %>%
    mutate(alternative = split_and_match(zr_knowledge_pds_if_fingerprint_fails_, variable, sep = ","),
           alternative = ifelse(alternative == TRUE, variable, "Nopes")) %>%
    summariser(alternative) %>%
    filter(alternative != "Nopes")
}) %>%
  mutate(temp = perc_to_dec(perc_overall)) %>%
  arrange(-temp) %>%
  select(-temp)

logtable(summary, descr)

summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 017 | % of individuals who knew of OTP on phone in case of fingerprint failure ----
descr <- "% of individuals who knew of OTP on phone in case of fingerprint failure"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

logmsg("Please see Analysis 016")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 018 | % of DBT users who knew filling out a bank form was important ----
descr <- "% of DBT users who knew filling out a bank form was important"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_dbt_flag == "Yes") %>%
  summariser(xr_knowledge_bank_form_requesting_dbtl)

logtable(summary, descr)

summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 019 | % of DBT users who experienced misdirected payments ----
descr <- "% of DBT users who experienced payments in desired account"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_dbt_flag == "Yes") %>%
  summariser(xr_all_dbt_in_desired_account)

logtable(summary, descr)

summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~

########################### FIGURES: ##########################-----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 017 | Share of people who answered mandatory when asked wether Aadhaar is mandatory for specific services ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(17, "share of people who answered correctly when asked wether Aadhaar is mandatory for specific services", suffix = "F")

output_variable <- c("xr_knowledge_aadhaar_mandatory_for_pds_card",
                     "xr_knowledge_aadhaar_mandatory_for_school_enrolment",
                     "xr_knowledge_aadhaar_mandatory_for_mnrega_card",
                     "xr_knowledge_aadhaar_mandatory_for_sim_card",
                     "xr_knowledge_aadhaar_mandatory_for_social_pension",
                     "xr_knowledge_aadhaar_mandatory_for_bank_account")

map_dfr(output_variable, function(service) {
  random_df %>%
    filter(xr_has_aadhaar %in% c("Yes", "No")) %>% 
    select(output = service, everything()) %>%
    summariser(output) %>%
    filter(output == "Yes") %>%
    mutate(category = str_remove(service, "xr_knowledge_aadhaar_mandatory_for_"),
           category = str_replace_all(category, "_", " "),
           category = simple_cap(category),
           value = perc_to_dec(perc_overall))
}) %>%
  export(file.path(v_loc["figures_data"], 
         "Figure 17. Share of people who believe Aadhaar is mandatory by law.xlsx"))

logmsg("data for figure exported")
rm(output_variable)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 018 | Reasons why people provide Aadhaar to access private-sector services, after the SC judgement ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(18, "Reasons why people provide Aadhaar to access private-sector services, after the SC judgement", suffix = "F")

list_of_responses_for_bank_account <- random_df %>%
  pull(zr_used_aadhaar_for_bank_account_as) %>%
  str_split("\\|") %>% 
  unlist() %>% 
  trimws() %>% 
  unique() %>%
  {.[. != ""]}

temp <- random_df %>%
  filter(zr_to_get_bank_account_used_aadhaar == "Yes") %>% 
  filter(lubridate::ymd(zr_most_recent_bank_account_date) >= v_global["sc_verdict_date"])

temp_df <- map_dfr(list_of_responses_for_bank_account, function(x) {
  temp %>%
    mutate(output = split_and_match(zr_used_aadhaar_for_bank_account_as, x),
           output = ifelse(output == TRUE, "Yes", "No")) %>%
    summariser(output) %>%
    mutate(overall = max(overall, na.rm = TRUE)) %>%
    filter(output == "Yes") %>%
    mutate(category = x,
           value = perc_to_dec(perc_overall)) %>%
    select(category, value, overall)
})  %>%
  mutate(service = "Bank Account")

list_of_responses_for_sim_card <- random_df %>%
  pull(zr_used_aadhaar_for_sim_as) %>%
  str_split("\\|") %>% 
  unlist() %>% 
  trimws() %>% 
  unique() %>%
  {.[. != ""]}

temp <- random_df %>%
  filter(zr_to_get_sim_used_aadhaar == "Yes")

temp_df <- map_dfr(list_of_responses_for_sim_card, function(x) {
  temp %>%
    filter(lubridate::ymd(zr_most_recent_sim_purchase_date) >= v_global["sc_verdict_date"]) %>%
    mutate(output = split_and_match(zr_used_aadhaar_for_sim_as, x),
           output = ifelse(output == TRUE, "Yes", "No")) %>%
    summariser(output) %>%
    mutate(overall = max(overall)) %>%
    filter(output == "Yes") %>%
    mutate(category = x,
           value = perc_to_dec(perc_overall)) %>%
    select(category, value, overall)
}) %>%
  mutate(service = "SIM Card") %>%
  bind_rows(temp_df) %>%
  filter(value >= 0.001,
         category != "photo") %>%
  mutate(category = case_when(
    category %in% c("The bank only accepted Aadhaar", 
                    "The telephone operator only accepted Aadhaar") ~ "The service provider only accepted Aadhaar",
    category %in% c("I chose to give Aadhaar because it meant that I got my bank acc faster", 
                    "I chose to give Aadhaar because it meant that I got my SIM faster") ~ "I chose to give Aadhaar because it meant that I got my service faster",
    TRUE ~ category
  ),
  category = str_wrap(category, 30)) %>%
  filter(category != "Don't know")

export(temp_df, file.path(v_loc["figures_data"],  
                 "Figure 18. Reasons why people provide Aadhaar to access private services.xlsx"))

logmsg("data for figure exported")
rm(temp_df, list_of_responses_for_bank_account, list_of_responses_for_sim_card)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save summary list ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# saveRDS(summary_list, file.path(v_loc[["outputs"]], "B4_Summary tables.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPEN OUTPUT LOG FILE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("Finished building 'B4_AWARENESS.R'")
