# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outfile <- file.path(v_loc["logs"], "B9_HIGHLIGHTS.txt")
unlink(outfile)
logmsg("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 9. HIGHLIGHTS SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", cursor = "")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading datasets ----
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
# ~~~~~~~~~

if (F){
  
  if (!(exists("summaries_d1") && is.data.frame(summaries_d1))){
    summaries_d1 <- readRDS(file = file.path(v_loc[["datasets"]], "summaries_d1.Rda"))
    logmsg("Summaries for D1 loaded from local")}
  
  if (!(exists("summaries_d2") && is.data.frame(summaries_d2))){
    summaries_d2 <- readRDS(file = file.path(v_loc[["datasets"]], "summaries_d2.Rda"))
    logmsg("Summaries for D2 loaded from local")}

  if (!(exists("summaries_d3") && is.data.frame(summaries_d3))){
    summaries_d3 <- readRDS(file = file.path(v_loc[["datasets"]], "summaries_d3.Rda"))
    logmsg("Summaries for D3 loaded from local")}
  
  if (!(exists("summaries_d4") && is.data.frame(summaries_d4))){
    summaries_d4 <- readRDS(file = file.path(v_loc[["datasets"]], "summaries_d4.Rda"))
    logmsg("Summaries for D4 loaded from local")}

  if (!(exists("summaries_d5") && is.data.frame(summaries_d5))){
    summaries_d5 <- readRDS(file = file.path(v_loc[["datasets"]], "summaries_d5.Rda"))
    logmsg("Summaries for D5 loaded from local")}

  if (!(exists("summaries_d6") && is.data.frame(summaries_d6))){
    summaries_d6 <- readRDS(file = file.path(v_loc[["datasets"]], "summaries_d6.Rda"))
    logmsg("Summaries for D6 loaded from local")}
}
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Local vars and Setting up QCs to ensure no changes to the dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
qc_check <- list(
  data = random_df,
  ncols = ncol(random_df),
  nrows = nrow(random_df))

summary_list <- list()
analysis_number <- 0
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# FUNCTION TO PULL DATA FROM EXISTING ANALYSIS
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
getter <- function(summaries, desc){
  # browser() 
  val <- eval(summaries)
  val <- val[[desc]]
  analysis_number <<- analysis_number + 1
  logbreak(analysis_number, desc)
  logtable(val, desc)
  summary_list[[desc]] <<- val
}

payloader <- function(val, desc){
  analysis_number <<- analysis_number + 1
  logbreak(analysis_number, desc)
  logtable(val, desc)
  summary_list[[desc]] <<- val
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01. Aadhaar is deeply embedded in residents' lives ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar) %>%
  payloader("% of individuals with aadhaar")
logmsg("Refer to CMIE")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_usage_of_aadhaar_num_services) %>%
  payloader("Average number of services that adults used Aadhaar for")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_min) %>%
  payloader("Average number of times (MIN) that adults used Aadhaar in the last 3 months")

random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_max) %>%
  payloader("Average number of times (MAX) that adults used Aadhaar in the last 3 months")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(xr_preferred_id_card) %>% 
  mutate(temp_col = perc_to_dec(perc_overall)) %>% 
  arrange(-temp_col) %>% 
  select(-temp_col) %>% 
  payloader("Among all adults, % for whom its their prefered id")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 02. Still, there are many who still do not have Aadhaar ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zr_age_bucket4) %>%
  payloader("% Adults and children who have Aadhaar")
logmsg("Refer to CMIE")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zr_age_bucket4, stat = "total") %>%
  payloader("Number of Adults and children who have Aadhaar")
logmsg("Refer to CMIE")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_caste, type = "flipped") %>%
  payloader("% of HH members who have Aadhaar, based on caste")

purposive_mem_df %>%
  filter(!is.na(xr_has_aadhaar)) %>%
  summariser_mem(xr_has_aadhaar, zh_homeless) %>%
  payloader("% of HH members who have Aadhaar, based on homelessness")

purposive_df %>%
  summariser(xr_has_aadhaar, zr_third_gender) %>%
  payloader("% of adults who have Aadhaar, based on whether they identify as third gender")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03. Aadhaar has supported new inclusion and improved service delivery ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  summariser(zr_used_aadhaar_for_first) %>%
  payloader("% of adults who used Aadhaar for first time access to a service - SIM, BANK, MNREGA, PDS, PENSION")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  summariser(zr_aadhaar_first_id) %>%
  payloader("% of adults for whom Aadhaar was their first ID card")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter((xr_has_pds_card == "Yes" & xh_tried_getting_pds_in_last_3months_flag == "Yes") |
           (xr_has_mnrega_card == "Yes" & trim(xr_reliability_mnrega_job_with_aadhaar) != "") |
           (xr_has_social_pension == "Yes")) %>%
  mutate(more_reliable_atleast_one = case_when(
    xr_has_pds_card == "Yes" & 
      xh_tried_getting_pds_in_last_3months_flag == "Yes" & 
      xh_reliability_of_pds_with_aadhaar == "More reliably with Aadhaar"  ~ "Yes",
    xr_has_mnrega_card == "Yes" & 
      trim(xr_reliability_mnrega_job_with_aadhaar)  != "" & 
      xr_reliability_mnrega_job_with_aadhaar == "More reliably with Aadhaar" ~ "Yes",
    xr_has_social_pension == "Yes" & 
      xr_reliability_of_getting_social_pension_using_aadhaar == "More reliably with Aadhaar" ~ "Yes",
    TRUE ~ "No"
  )) %>%
  summariser(more_reliable_atleast_one) %>%
  payloader("% of adults who are eligible for atleast one service and have received atleast one service MORE reliably")

random_df %>%
  filter((xr_has_pds_card == "Yes" & xh_tried_getting_pds_in_last_3months_flag == "Yes") |
           (xr_has_mnrega_card == "Yes" & trim(xr_reliability_mnrega_job_with_aadhaar) != "") |
           (xr_has_social_pension == "Yes")) %>%
  mutate(less_reliable_atleast_one = case_when(
    xr_has_pds_card == "Yes" & 
      xh_tried_getting_pds_in_last_3months_flag == "Yes" & 
      xh_reliability_of_pds_with_aadhaar == "Less reliably with Aadhaar" ~ "Yes",
    xr_has_mnrega_card == "Yes" & 
      trim(xr_reliability_mnrega_job_with_aadhaar)  != "" & 
      xr_reliability_mnrega_job_with_aadhaar == "Less reliably with Aadhaar" ~ "Yes",
    xr_has_social_pension == "Yes" & 
      xr_reliability_of_getting_social_pension_using_aadhaar == "Less reliably with Aadhaar" ~ "Yes",
    TRUE ~ "No"
  )) %>%
  summariser(less_reliable_atleast_one) %>%
  payloader("% of adults who are eligible for atleast one service and who received atleast one service LESS reliably")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(!is.na(zr_to_get_sim_used_aadhaar)) %>% 
  summariser(xr_ease_of_getting_last_sim, zr_to_get_sim_used_aadhaar, compare = "No") %>% 
  payloader("% of respondents who found it easy/difficult to get last sim, vs. did they use aadhaar for it")

random_df %>%
  filter(!is.na(zr_to_get_bank_account_used_aadhaar)) %>% 
  summariser(xr_ease_of_opening_last_bank_account, zr_to_get_bank_account_used_aadhaar, compare = "No") %>% 
  payloader("% of respondents who found it easy/difficult to open last bank account, vs. did they use aadhaar for it")

random_df %>%
  filter(!is.na(zr_to_get_sim_used_aadhaar)) %>% 
  summariser(xr_days_to_get_sim, zr_to_get_sim_used_aadhaar, compare = "No") %>% 
  payloader("time to get last sim, vs. did they use aadhaar for it")

random_df %>%
  filter(!is.na(zr_to_get_bank_account_used_aadhaar)) %>% 
  summariser(xr_days_to_get_bank_account, zr_to_get_bank_account_used_aadhaar, compare = "No") %>% 
  payloader("time to open last bank account, vs. did they use aadhaar for it")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 04. Despite the Supreme Court ruling, providing Aadhaar is de facto mandatory for SIM cards, bank accounts and school enrolment ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  mutate_at(vars(xr_knowledge_aadhaar_mandatory_for_bank_account, 
                 xr_knowledge_aadhaar_mandatory_for_sim_card, 
                 xr_knowledge_aadhaar_mandatory_for_school_enrolment), ~ ifelse(. == "Yes", 1, 0)) %>%
  mutate(
    aadhaar_is_necessary = rowSums(
      select(., xr_knowledge_aadhaar_mandatory_for_bank_account, 
             xr_knowledge_aadhaar_mandatory_for_sim_card, 
             xr_knowledge_aadhaar_mandatory_for_school_enrolment), na.rm = T),
    aadhaar_is_necessary = ifelse(aadhaar_is_necessary == 3, "Yes", "No")
  ) %>%
  summariser(aadhaar_is_necessary) %>% 
  payloader("% of individuals who believe Aadhaar is required for all private services")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
aadhaar_mandatory <- c("The telephone operator only accepted Aadhaar",
                       "Aadhar se hi de raha tha", 
                       "The bank only accepted Aadhaar",
                       "aadhar ke bina account nhi khol rhe the")
  
random_df %>% 
  filter((zr_to_get_sim_used_aadhaar == "Yes" & 
            lubridate::ymd(zr_most_recent_sim_purchase_date) >= v_global["sc_verdict_date"]) | 
         (zr_to_get_bank_account_used_aadhaar == "Yes" &
            lubridate::ymd(zr_most_recent_bank_account_date) >= v_global["sc_verdict_date"])) %>% 
  mutate(used_aadhaar_after_sc_as_mandatory_bank_sim = 
           ifelse((zr_to_get_sim_used_aadhaar == "Yes" & 
                     lubridate::ymd(zr_most_recent_sim_purchase_date) >= v_global["sc_verdict_date"] & 
                     split_and_match(zr_used_aadhaar_for_sim_as, aadhaar_mandatory)) | 
                  (zr_to_get_bank_account_used_aadhaar == "Yes" &
                     lubridate::ymd(zr_most_recent_bank_account_date) >= v_global["sc_verdict_date"] & 
                     split_and_match(zr_used_aadhaar_for_bank_account_as, aadhaar_mandatory)), "Yes", "No")) %>% 
  summariser(used_aadhaar_after_sc_as_mandatory_bank_sim) %>% 
  payloader("Among those who provided Aadhaar for Sim/Bank account, % of individuals who were demanded")

rm(aadhaar_mandatory)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  summariser(zh_denial_aa_bank_account_hard_flag) %>% 
  payloader("% Adults who were denied SIM card due to aadhaar")

random_df %>% 
  summariser(zh_denial_aa_sim_hard_flag) %>% 
  payloader("% Adults who were denied Bank Account due to aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_mem_df %>%
  filter(between(xr_age_final, 6, 14),
         lubridate::ymd(zh_school_enrolment_date) >= v_global[["sc_verdict_date"]]
  ) %>%
  summariser_mem(xh_school_admission_denied_due_to_aadhaar) %>% 
  payloader("Among 6 to 14, % of children who faced delay or denial")

random_mem_df %>%
  filter(between(xr_age_final, 6, 14)) %>%
  mutate(could_not_enrol = ifelse(
    xh_enroled_in_school_flag == "No" &
      split_and_match(zh_denial_not_enroled_in_school_reason, 
                      c("DonothaveAadhaar", 
                        "Do not have Aadhaar", 
                        "DonothaveAadhaarANDotherrequireddocuments",
                        "Do not have Aadhaar AND other required documents")), "Yes", "No")) %>%
  summariser_mem(could_not_enrol) %>% 
  payloader("Among 6 to 14, % of children who are out of school due to Aadhaar")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05. People experience denial of welfare services, and at times exclusion, due to problems with Aadhaar  ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  summariser(zh_denial_aa_canceled_welfare) %>% 
  payloader("% of people experienced exclusion from a key welfare service, which they had earlier received due to Aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  summariser(zh_denial_na_canceled_welfare) %>% 
  payloader("% of people experienced exclusion from a key welfare service, which they had earlier received due to Non Aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(xr_has_pds_card == "Yes", xh_tried_getting_pds_in_last_3months_flag == "Yes") %>% 
  mutate(completely_denied_aa = ifelse(zr_denial_aa_pds_any_aadhaar_related_above_flag == "Yes" & zh_got_pds_success == "Tried but did not get", "Yes", "No")) %>% 
  summariser(completely_denied_aa) %>% 
  payloader("Among ration card holders who tried to get PDS ration in the last 3 months, % who were completely denied due to problems with Aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_mem_df %>%
  filter(between(xr_age_final, 6, 14)) %>%
  mutate(xh_school_mdm_denied_due_to_aadhaar = str_replace_na(xh_school_mdm_denied_due_to_aadhaar)) %>% 
  summariser_mem(xh_school_mdm_denied_due_to_aadhaar) %>% 
  payloader("Among 6-14 years old, % of children have been denied MDM due to Aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(xr_has_aadhaar %in% c("Yes", "No"),
         zr_enrolment_trips_to_center > 1 | xr_update_trips_to_center > 1) %>%
  summariser(xr_knowledge_update_helpline) %>% 
  payloader("Among those who knew if they have aadhaar or not and faced difficulty, % of individuals who knew about Aadhaar helpline")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06. Many people have incorrect Aadhaar information ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_aadhaar_has_error_on_card) %>% 
  payloader("Amongst those who have Aadhaar, % with error")
logmsg("Refer to CMIE")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_link_aadhaar_to_phone) %>% 
  payloader("Among those who have Aadhaar % of individuals who linked a mobile number with Aadhaar")
logmsg("REMOVE MOBILE ERRORS BASED ON CMIE")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(zr_needed_to_update_final == "Yes", zr_wanted_to_update_final == "Yes", zr_ever_tried_to_update == "Yes") %>%
  summariser(xr_aadhaar_update_successful_flag) %>% 
  payloader("Among those who tried to update Aadhaar, % of individuals who succeeded")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 07. The newer digital features of Aadhaar are yet to be embraced ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_usage_of_digital_features_ever_flag) %>% 
  payloader("Among adults who have Aadhaar, % of use of Aadhaar digital features")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 08. Most people are satisfied with Aadhaar, including those who face difficulties ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  summariser(xh_sentiment_satisfaction_aadhaar) %>% 
  payloader("% of adults who are satisfied with aadhaar")
logmsg("Refer to CMIE")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  filter(zh_denial_aa_any_hard_flag == "Yes") %>% 
  summariser(xh_sentiment_satisfaction_aadhaar) %>% 
  payloader("Among those who faced any hard denial, % of adults who are satisfied with aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  summariser(xh_sentiment_satisfaction_aadhaar, zh_denial_aa_any_welfare_flag) %>% 
  payloader("Satisfaction based on denial flag")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summaries_d3 %>%
  getter("top benefits and challenges with Aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 09. Trust in the Aadhaar system is high ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summaries_d3 %>% 
  getter("Of all adults who responded to having Aadhaar question, % of individuals who believe their data is well protected")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summaries_d3 %>% 
  getter("Among adults who are eligible for atleast one welfare service, % who trust atleast one service MORE")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summaries_d3 %>%
  getter("top benefits and challenges with Aadhaar")

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summaries_d3 %>%
  getter("% faced Aadhaar related frauds")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10. Residents' Aadhaar experience varies noticeably across state lines ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# NA


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH. People without Aadhaar, denied banking services and SIM cards due to Aadhaar-related reasons ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  # filter(xr_has_aadhaar == "No") %>% 
  summariser(zh_denial_aa_bank_account_hard_flag, xr_has_aadhaar, compare = "yes") %>% 
  select(-ends_with("low"), -ends_with("upp"))

random_df %>% 
  # filter(xr_has_aadhaar == "No") %>% 
  summariser(zh_denial_aa_sim_hard_flag, xr_has_aadhaar, compare = "yes") %>% 
  select(-ends_with("low"), -ends_with("upp"))


############################# UPFRONT SCALE SECTION ----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01 | FOOD ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  mutate(user = case_when(
    xr_num_of_fingerprint_trials_to_get_pds > 0 ~ "Yes", 
    is.na(xr_num_of_fingerprint_trials_to_get_pds) ~ "NA",
    TRUE ~ "No")) %>% 
  summariser(user)

random_df %>% 
  mutate(user = case_when(
    xr_num_of_fingerprint_trials_to_get_pds > 0 ~ "Yes", 
    is.na(xr_num_of_fingerprint_trials_to_get_pds) ~ "NA",
    TRUE ~ "No")) %>% 
  summariser(user, stat = "total")

random_df %>% 
  summariser_hh(xh_pds_contribution_to_food_consumption)
  
random_df %>% 
  summariser_hh(xh_tried_getting_pds_in_last_3months_flag, stat = "total")


random_df %>%
  mutate(above_half_food = case_when(
    xh_pds_contribution_to_food_consumption %in% 
      c("Almost all", "Half", "More than half") ~ "Half or more",
    TRUE ~ "Other"
  )) %>%
  summariser_hh(above_half_food)

random_df %>%
    mutate(above_half_food = case_when(
      xh_pds_contribution_to_food_consumption %in% 
        c("Almost all", "Half", "More than half") ~ "Half or more",
      TRUE ~ "Other"
    )) %>% 
  summariser_hh(above_half_food, stat = "total")
  

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 02 | FUEL ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

random_df %>% 
  summariser_hh(xr_usage_of_aadhaar_for_lpg_subsidy)

random_df %>% 
  summariser_hh(xr_usage_of_aadhaar_for_lpg_subsidy, stat = "total")

random_df %>% 
  summariser_hh(xr_usage_of_aadhaar_for_kerosene_subsidy)

random_df %>% 
  summariser_hh(xr_usage_of_aadhaar_for_kerosene_subsidy, stat = "total")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03 | LIVELIHOOD AND INCOME ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  filter(xr_age_final > 65, xr_received_pension_last_period == "Yes") %>% 
  summariser(xr_usage_of_aadhaar_for_social_pension, stat = "total")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 04 | FINANCE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>% 
  summariser(xr_usage_of_aadhaar_for_savings_account)

random_df %>% 
  summariser(xr_usage_of_aadhaar_for_savings_account, stat = "total")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05 | EDUCATION ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_mem_df %>%
  filter(xr_age_final < 18, xr_age_final >= 6) %>%
  mutate(xh_school_admission_produced_aadhaar_flag = 
           str_replace_na(xh_school_admission_produced_aadhaar_flag)) %>% 
  summariser_mem(xh_school_admission_produced_aadhaar_flag)

random_mem_df %>%
  filter(xr_age_final < 18, xr_age_final >= 6) %>% 
  summariser_mem(xh_school_admission_produced_aadhaar_flag, stat = "total")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06 | SIM ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
random_df %>%
  mutate(zr_to_get_sim_used_aadhaar = str_replace_na(zr_to_get_sim_used_aadhaar)) %>% 
  summariser(zr_to_get_sim_used_aadhaar)

random_df %>%
  mutate(zr_to_get_sim_used_aadhaar = str_replace_na(zr_to_get_sim_used_aadhaar)) %>% 
  summariser(zr_to_get_sim_used_aadhaar, stat = "total")



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save summary list ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summaries_d9 <- summary_list


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# QC check to see if we have modified the dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

delta_cols <- ncol(random_df) - qc_check$ncols
delta_rows <- nrow(random_df) - qc_check$nrows
delta_data <- all_equal(random_df, qc_check$data)

if (any(delta_cols != 0, delta_rows != 0, !isTRUE(delta_data))) {
  logmsg("X X X QC Fail: The code for analysis modified the dataset")
} else {
  logmsg("QC Pass: Dataset for analysis has not been modified at the end of the analysis")
}

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPEN OUTPUT LOG FILE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
shell.exec(outfile)
print("Finished building 'D9_HIGHLIGHTS.R'")

