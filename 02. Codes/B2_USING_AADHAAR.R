# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outfile <- file.path(v_loc["logs"], "B2_USING AADHAAR.txt")
unlink(outfile)
logmsg("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 2. USING AADHAAR SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", cursor = "")


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Loading datasets and local variables ----
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
# 001 |  % of individuals who have used Aadhaar in the past three months ----
descr <- "Among adults who have Aadhaar, % who have used it in the past three months"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  mutate(used_aadhaar_atleast_once = case_when(
    is.na(zr_freq_of_usage_3months_all_types_min) ~ "",
    zr_freq_of_usage_3months_all_types_min >= 1 ~ "Used atleast once",
    TRUE ~ "Nopes"
  )) %>%
  summariser(used_aadhaar_atleast_once)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 002 |  Among adults who have Aadhaar, average number of times Aadhaar is used in the past three months ----
descr <- "Among adults who have Aadhaar, average number of times Aadhaar is used in the past three months"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_min)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 003 |  % usage of Aadhaar by age (footnote) ----
descr <- "% usage of Aadhaar by age"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_min, zr_age_bucket1)
logtable(summary, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_min, zr_age_bucket2)
logtable(summary, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_min, zr_age_bucket3)
logtable(summary, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_min, zr_age_bucket4)
logtable(summary, descr)

summary <- purposive_df %>%
  filter(zr_elderly == "Yes") %>%
  summariser(zr_freq_of_usage_3months_all_types_min)
logtable(summary, descr)

summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 004 | Among those who have Aadhaar, average years of education by usage (num services) ----
descr <- "Among those who have Aadhaar, average years of education by usage (num services)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         !is.na(zr_education_level_years)) %>%
  mutate(zr_usage_of_aadhaar_num_services = as.character(zr_usage_of_aadhaar_num_services)) %>%
  summariser(zr_education_level_years, zr_usage_of_aadhaar_num_services) %>%
  mutate(zr_usage_of_aadhaar_num_services = as.numeric(zr_usage_of_aadhaar_num_services)) %>%
  arrange(-zr_usage_of_aadhaar_num_services)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 005 | Among those who have Aadhaar, usage (num services) by average years of education buckets ----
descr <- "Among those who have Aadhaar, usage (num services) by average years of education buckets"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         !is.na(zr_education_level_years)) %>%
  summariser(zr_usage_of_aadhaar_num_services, zr_education_level_years_bucket)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 006 |  top use cases of Aadhaar ----
descr <- "top use-cases of Aadhaar among those who have Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

meta_data <- import(file.path(v_loc["inputs"],
                              "02. Data for figures",
                              "Figure 10. Meta data.xlsx"))

temp <- NULL

for (num in 1:nrow(meta_data)) {
  
  # Moving forward for scholarships
  if (meta_data$variable[num] == "xr_usage_of_aadhaar_for_scholarships") {
    logmsg("There are no valid responses for using Aadhaar for scholarships. Moving forward")
    next
  }
  
  to_drop <- random_df %>%
    filter(xr_has_aadhaar == "Yes") %>%
    select(output = meta_data$variable[num], everything())
  
  temp <- to_drop %>%
    summariser(output) %>%
    filter(output == "Yes") %>%
    mutate(value = perc_to_dec(perc_overall),
           use_case = meta_data$use_case[num],
           mandatory = meta_data$mandatory[num],
           no_of_responses = nrow(to_drop)) %>%
    select(use_case, value, mandatory, no_of_responses) %>%
    rbind(temp)
  
  rm(to_drop)
}

logtable(temp %>% arrange(-value), descr)
summary_list[[descr]] <- temp

rm(descr, temp, meta_data)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH | access to DBT to bank account based on having Aadhaar ----
descr <- "access to DBT to bank account based on having Aadhaar"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak("ADH", descr)

temp <- random_df %>%
  filter(xr_has_savings_account == "Yes") %>%
  summariser(xr_dbt_flag, xr_has_aadhaar)

logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 007 |  use of Aadhaar by feature ----
descr <- "Among those who have Aadhaar, use of Aadhaar by feature"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

output_variable <- c("xr_usage_of_aadhaar_card_flag",
                     "xr_usage_of_masked_aadhaar_flag",
                     "xr_usage_of_aadhaar_num_flag",
                     "xr_usage_of_virtual_aadhaar_num_flag",
                     "xr_usage_of_aadhaar_otp_flag",
                     "xr_usage_of_aadhaar_fingerprint_flag",
                     "xr_usage_of_aadhaar_iris_scan_flag",
                     "xr_usage_of_aadhaar_qr_code_flag",
                     "xr_usage_of_maadhaar_flag",
                     "xr_usage_of_aadhaar_photocopy_flag")

temp <- NULL

for (variable in output_variable) {
  temp <- random_df %>%
    filter(xr_has_aadhaar == "Yes") %>%
    select(output = variable, everything()) %>%
    summariser(output) %>%
    mutate(category = variable,
           category = str_remove(category, "xr_usage_of_"),
           category = str_remove(category, c("flag")),
           category = str_replace_all(category, "_", " "),
           category = trimws(category),
           percent_yes = perc_to_dec(perc_overall),
           overall = max(overall)) %>%
    filter(output %in% c("Yes")) %>%
    select(category, percent_yes, overall) %>%
    rbind(temp)
}

logtable(temp %>% arrange(-percent_yes), descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 008 | Among people who have Aadhaar, % of individuals who did not know of any of Aadhaar's digital features (footnote) ----
descr <- "Among people who have Aadhaar, % of individuals who did not know of any of Aadhaar's digital features"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

unaware <- c("I don't know what that is", "I don't know if I have used it")

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  mutate(did_not_know_of_any_aadhaar_feature = ifelse(
    xr_usage_of_masked_aadhaar_flag %in% unaware &
      xr_usage_of_virtual_aadhaar_num_flag %in% unaware &
      xr_usage_of_aadhaar_qr_code_flag %in% unaware &
      xr_usage_of_maadhaar_flag %in% unaware, "Yes", "No"))

summary %>% select(did_not_know_of_any_aadhaar_feature,
                   xr_usage_of_masked_aadhaar_flag,
                   xr_usage_of_virtual_aadhaar_num_flag,
                   xr_usage_of_aadhaar_qr_code_flag,
                   xr_usage_of_maadhaar_flag) %>%
  grouper() %>%
  logtable()

summary <- summary %>%
  summariser(did_not_know_of_any_aadhaar_feature)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 009 | Among people who have Aadhaar, % of individuals who did not know of Aadhaar's individual digital features (footnote) ----
descr <- "Among people who have Aadhaar, % of individuals who did not know of Aadhaar's individual digital features"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

unaware <- c("I don't know what that is", "I don't know if I have used it")

output_variable <- c("xr_usage_of_masked_aadhaar_flag",
                     "xr_usage_of_virtual_aadhaar_num_flag",
                     "xr_usage_of_aadhaar_qr_code_flag",
                     "xr_usage_of_maadhaar_flag")

temp <- map_dfr(output_variable, function(x) {
  random_df %>%
    filter(xr_has_aadhaar == "Yes") %>%
    select(output = x, everything()) %>%
    mutate(output = ifelse(output %in% unaware, "unaware", output)) %>%
    summariser(output) %>%
    filter(output == "unaware") %>%
    mutate(feature = x)
})

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 010 |  % of individuals who have linked a mobile number with Aadhaar ----
descr <- "Among those who have Aadhaar % of individuals who linked a mobile number with Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_link_aadhaar_to_phone)

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)

logmsg("Only 'Yes' is meaningful")
logmsg("REMOVE MOBILE ERRORS BASED ON CMIE")

# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 011 | Among adults who have Aadhaar and use it for job application, % of use of Aadhaar digital features (footnote) ----
descr <- "Among adults who have Aadhaar and use it for job application, % of use of Aadhaar digital features (footnote)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_usage_of_digital_features_ever_flag, xr_usage_of_aadhaar_for_job_application)

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH | Among those who have Aadhaar, average years of education by usage of digital features (footnote) ----
descr <- "Among those who have Aadhaar, average years of education by usage of digital features (footnote)"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(zr_education_level_years, zr_usage_of_digital_features_ever_flag)

logbreak("ADH", descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 012 | Among those who have Aadhaar and avail a welfare service, % adults who found it easy to link with Aadhaar ----
descr <- "Among those who have Aadhaar and avail a welfare service, % adults who found it easy to link with Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

welfare_services <- c("xr_has_pds_card",
                      "xr_has_mnrega_card",
                      "xr_has_social_pension")

ease_of_linking <- c("xr_linking_pds_with_aadhaar_ease",
                     "xr_linking_mnrega_with_aadhaar_ease",
                     "xr_linking_social_pension_with_aadhaar_ease")

temp <- map2_dfr(welfare_services, ease_of_linking, function(x, y) {
  random_df %>%
    filter(xr_has_aadhaar == "Yes",
           !!sym(x) == "Yes") %>%
    summariser(!!y) %>%
    rename(response = y) %>%
    mutate(service = case_when(
      x == "xr_has_pds_card" ~ "PDS",
      x == "xr_has_mnrega_card" ~ "MNREGA",
      x == "xr_has_social_pension" ~ "Social Pension"
    ))
}) %>%
  filter(response == "Easy")

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 013 | Average number of trips to link aadhaar with welfare services, for Aadhaar enrolment and update ----
descr <- "Among those who have Aadhaar and avail a welfare service, Average number of trips to link aadhaar with welfare services"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes" |
           xr_has_mnrega_card == "Yes" |
           xr_has_social_pension == "Yes") %>%
  mutate(average_number_of_trips = rowMeans(
    select(., xh_linking_pds_with_aadhaar_hh_trips,
           xr_linking_mnrega_with_aadhaar_trips,
           xr_linking_social_pension_with_aadhaar_trips), na.rm = TRUE)
    # ,
    # correct_mobile = case_when(
    #   xr_link_aadhaar_to_phone == "No" ~ "Not linked to mobile",
    #   xr_link_aadhaar_to_phone == "Yes" &
    #     xr_aadhaar_mob_num_error_flag == "Yes" ~  "wrong mobile",
    #   xr_link_aadhaar_to_phone == "Yes" &
    #     xr_aadhaar_mob_num_error_flag == "No" ~  "correct mobile",
    #   TRUE ~ "god know what"
    # )
  ) %>%
  summariser(average_number_of_trips) %>%
  rename(trips = average_number_of_trips) %>%
  mutate(service = "Linking to welfare services")

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes" |
           xr_has_mnrega_card == "Yes" |
           xr_has_social_pension == "Yes") %>%
  summariser(zr_enrolment_trips_to_center) %>%
  rename(trips = zr_enrolment_trips_to_center) %>%
  mutate(service = "Enrollment") %>%
  bind_rows(temp)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes" |
           xr_has_mnrega_card == "Yes" |
           xr_has_social_pension == "Yes") %>%
  summariser(xr_update_trips_to_center) %>%
  rename(trips = xr_update_trips_to_center) %>%
  mutate(service = "Updates") %>%
  bind_rows(temp)

logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 014 | reasons for not trying to link aadhaar with pds ----
descr <- "reasons for not trying to link aadhaar with pds"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp_df <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes")

list_of_response <- temp_df %>%
  pull(zh_aadhaar_to_pds_not_linked_as) %>%
  str_split("\\|") %>%
  unlist() %>%
  trimws() %>%
  tolower() %>%
  tabyl() %>%
  filter(n > 1) %>% # Filtering for responses which occured more than once
  pull(".")

summary <- map_dfr(list_of_response, function(x) {
  temp_df %>%
    mutate(temp = split_and_match(zh_aadhaar_to_pds_not_linked_as, x),
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
# 015 | Among those adults who have Aadhaar and use the relevant service, % for whom first authentication attempt did not work ----
descr <- "Among those adults who have Aadhaar and use the relevant service, % for whom first authentication attempt did not work"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes" |
           xr_has_mnrega_card == "Yes" |
           xr_has_social_pension == "Yes" |
           xr_has_sim_card == "Yes") %>%
  mutate(first_attempt_did_not_work = case_when(
    xr_num_of_fingerprint_trials_to_get_pds > 1                                               ~ "First attempt did not work",
    xr_num_of_fingerprint_trials_to_get_pension > 1                                           ~ "First attempt did not work",
    xr_num_of_fingerprint_trials_to_get_sim > 1                                               ~ "First attempt did not work",
    zr_fingerprint_failure == "Yes"                                                           ~ "First attempt did not work",
    xh_linking_pds_with_aadhaar_gave_biometric == "Yes, but they did not work"                ~ "First attempt did not work",
    xr_linking_mnrega_with_aadhaar_gave_biometric == "Yes, but they did not work"             ~ "First attempt did not work",
    xr_linking_social_pension_with_aadhaar_gave_biometric == "Yes, but they did not work"     ~ "First attempt did not work",
    xh_problem_at_pds_shop_fingerprint_failed == "Yes"                                        ~ "First attempt did not work",
    xh_problem_at_pds_shop_iris_failed == "Yes"                                               ~ "First attempt did not work",
    TRUE                                                                                      ~ "Nopes"
  )) %>%
  summariser(first_attempt_did_not_work)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 016 | average number of authentication attempts required for pension, pds and sim  ----
descr <- "Among those adults who have Aadhaar and use the relevant service, average authentication attempts required for pension, pds and sim"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes" |
           xr_has_social_pension == "Yes" |
           xr_has_sim_card == "Yes") %>%
  mutate(average_number_of_attempts = rowMeans(
    select(., xr_num_of_fingerprint_trials_to_get_pds,
           xr_num_of_fingerprint_trials_to_get_pension,
           xr_num_of_fingerprint_trials_to_get_sim), na.rm = TRUE
  )) %>%
  summariser(average_number_of_attempts) %>%
  rename(service = average_number_of_attempts)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes") %>%
  summariser(xr_num_of_fingerprint_trials_to_get_pds) %>%
  rename(service = xr_num_of_fingerprint_trials_to_get_pds) %>%
  mutate(service = "PDS") %>%
  bind_rows(temp)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_social_pension == "Yes") %>%
  summariser(xr_num_of_fingerprint_trials_to_get_pension) %>%
  rename(service = xr_num_of_fingerprint_trials_to_get_pension) %>%
  mutate(service = "Pensions") %>%
  bind_rows(temp)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_sim_card == "Yes") %>%
  summariser(xr_num_of_fingerprint_trials_to_get_sim) %>%
  rename(service = xr_num_of_fingerprint_trials_to_get_sim) %>%
  mutate(service = "SIM") %>%
  bind_rows(temp)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ADH | failure rate for biometrics in SIM and PDS  ----
descr <- "failure rate for biometrics in SIM and PDS"
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak("ADH", descr)

summary <- random_df %>% 
  filter(xr_has_pds_card == "Yes" &
           xh_tried_getting_pds_in_last_3months_flag == "Yes") %>% 
  mutate(biometric_failure = case_when(
    zr_fingerprint_failure_to_get_pds == "Yes" ~ "Yes",
    xh_problem_at_pds_shop_device_failure == "Yes" ~ "Yes",
    xh_problem_at_pds_shop_internet_server_failure == "Yes" ~ "Yes",
    xh_problem_at_pds_shop_fingerprint_failed == "Yes" ~ "Yes", 
    xh_problem_at_pds_shop_iris_failed == "Yes" ~ "Yes",
    TRUE ~ "No"
  )) %>% 
  summariser(biometric_failure) %>%
  mutate(service = "PDS", overall = max(overall)) %>% 
  filter(biometric_failure == "Yes") %>% 
  select(service, overall, perc_overall)

summary <- random_df %>% 
  filter(xr_has_sim_card == "Yes") %>% 
  summariser(zr_fingerprint_failure_to_get_sim) %>%
  mutate(service = "SIM card", overall = max(overall)) %>% 
  filter(zr_fingerprint_failure_to_get_sim == "Yes") %>% 
  select(service, overall, perc_overall) %>% 
  bind_rows(summary)

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~



# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 017 | Of adults who have aadhaar and use the services, % who needed 2 or more attempts at authentication  ----
descr <- "Of adults who have aadhaar and use the services, % who needed 2 or more attempts at authentication"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes" |
           xr_has_social_pension == "Yes" |
           xr_has_sim_card == "Yes") %>%
  mutate(average_number_of_attempts = rowMeans(
    select(., xr_num_of_fingerprint_trials_to_get_pds, 
           xr_num_of_fingerprint_trials_to_get_pension, 
           xr_num_of_fingerprint_trials_to_get_sim), na.rm = TRUE)) %>% 
  mutate(average_number_of_attempts_bucket = ifelse(average_number_of_attempts >= 2,
                                                    "High|>=2", "Low|<2")) %>%
  summariser(average_number_of_attempts_bucket)

logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 018 |  % of individuals for whom fingerprint/biometric authentication did not go through ----
descr <- "Among adults who have aadhaar and use the service, % for whom fingerprint did not go through"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  mutate(finger_prints_failed = case_when(
    zr_fingerprint_failure == "Yes"                                                           ~ "Yes",
    xh_problem_at_pds_shop_fingerprint_failed == "Yes"                                        ~ "Yes",
    xh_problem_at_pds_shop_iris_failed == "Yes"                                               ~ "Yes",
    xh_linking_pds_with_aadhaar_gave_biometric == "Yes, but they did not work"                ~ "Yes",
    xr_linking_mnrega_with_aadhaar_gave_biometric == "Yes, but they did not work"             ~ "Yes",
    xr_linking_social_pension_with_aadhaar_gave_biometric == "Yes, but they did not work"     ~ "Yes",
    TRUE ~ "Nopes"
  )) %>%
  summariser(finger_prints_failed)

logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 019 | % of adults facing biometric failure and unable to authenticate by any other means ----
descr <- "% of adults facing biometric failure and unable to authenticate by any other means"
analysis_number <- analysis_number + 1
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         (xr_has_pds_card == "Yes" &
            (xh_problem_at_pds_shop_fingerprint_failed == "Yes" | 
               xh_problem_at_pds_shop_iris_failed == "Yes")) |
           (xr_has_social_pension == "Yes" & 
              zr_fingerprint_failure_to_get_pension == "Yes")) %>%
  mutate(alternative_worked = case_when(
    xr_has_pds_card == "Yes" &
      (xh_problem_at_pds_shop_fingerprint_failed == "Yes" | 
         xh_problem_at_pds_shop_iris_failed == "Yes") &
      xh_problem_at_pds_shop_biometric_failure_alternative_flag %in% c("No , asked me to update biometrics at the Aadhaar update center",
                                                                       "Yes, tried getting help from a local leader (Sarpanch)",
                                                                       "Yes, tried OTP on the phone",
                                                                       "ragister se rasan mila", 
                                                                       "rajister se rasan milta h",
                                                                       "register") &
      xh_problem_at_pds_shop_biometric_failure_alternative_worked == "Yes" ~ "Yes",
    
    xr_has_social_pension == "Yes" & 
      zr_fingerprint_failure_to_get_pension == "Yes" &
      xr_pension_biometric_failure_alternative %in% c("I gave a fingerprint scan somewhere else",
                                                      "I got pension using iris scan",
                                                      "I got pension using OTP on the phone") ~ "Yes",
    TRUE ~ "Nopes"
  )) %>%
  summariser(alternative_worked)

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 020 | % people who missed out on service while enroling ----
descr <- "% people who missed out on service while enroling"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  filter(zr_enrolment_flag == "Yes") %>% 
  summariser(xr_enroled_missed_service_during_process_flag)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 021 | % people who missed out on service while updating ----
descr <- "% people who missed out on service while updating"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  filter(zr_ever_tried_to_update == "Yes") %>%
  summariser(xr_update_missed_service_during_process_flag)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 022 | % people experienced exclusion from a key welfare service due to aadhaar ----
descr <- "% people experienced exclusion from a key welfare service due to aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  summariser(zh_denial_aa_welfare_hard_flag)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 023 | Among those who experienced exclusion, % people who lost a key welfare service due to aadhaar ----
descr <- "Among those who experienced exclusion, % people who lost a key welfare service due to aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  summariser(zh_denial_aa_canceled_welfare )

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 024 | % people experienced exclusion from a key welfare service due to non aadhaar ----
descr <- "% people experienced exclusion from a key welfare service due to non aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  summariser(zh_denial_na_welfare_hard_flag)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 025 | Among non Aadhaar holders, % people experienced any exclusion ----
descr <- "Among non Aadhaar holders, % people experienced any exclusion"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  summariser(zh_denial_aa_any_hard_flag, xr_has_aadhaar, compare = "yes") %>% 
  select(-ends_with("low"), -ends_with("upp"))

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 026 | Among non Aadhaar holders, % people experienced any denial (footnote) ----
descr <- "Among non Aadhaar holders, % people experienced any denial (footnote)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)
summary <- random_df %>% 
  summariser(zh_denial_aa_any_hard_flag, xr_has_aadhaar, compare = "yes") %>% 
  select(-ends_with("low"), -ends_with("upp"))

summary <- random_df %>% 
  summariser(zh_denial_aa_any_welfare_flag, xr_has_aadhaar, compare = "yes") %>% 
  select(-ends_with("low"), -ends_with("upp"))
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 027 | Among non Aadhaar holders, number people experienced any exclusion ----
descr <- "Among non Aadhaar holders, number people experienced any exclusion"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  filter(xr_has_aadhaar == "No") %>% 
  summariser(zh_denial_aa_any_hard_flag, stat = "total") %>% 
  mutate_at(vars(starts_with("estimated")), ~ . / 1000000)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 028 | Among individuals with inaccurate aadhaar, % of individuals who used it in the last 3 months (footnote) ----
descr <- "Among individuals with inaccurate aadhaar, % of individuals who used it in the last 3 months (footnote)"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         zr_aadhaar_has_error == "Yes") %>%
  mutate(used_in_three_months = case_when(
    zr_freq_of_usage_3months_all_types_min > 1 ~ "Yes", 
    is.na(zr_freq_of_usage_3months_all_types_min) ~ "",
    TRUE ~ "Nopes")) %>%
  summariser(used_in_three_months)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 029 | Among those with errors on card, % people experienced any exclusion ----
descr <- "Among those with errors on card, % people experienced any exclusion"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)
summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes", 
         zh_denial_aa_any_welfare_flag != "Not a user") %>% 
  summariser(zh_denial_aa_any_welfare_flag, zr_aadhaar_has_error, compare = "no") %>% 
  select(-ends_with("low"), -ends_with("upp"))

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 030 | Among those with errors on card, number people experienced any exclusion ----
descr <- "Among those with errors on card, number people experienced any exclusion"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)
summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes", 
         zh_denial_aa_any_welfare_flag != "Not a user",
         zr_aadhaar_has_error == "Yes") %>% 
  summariser(zh_denial_aa_any_welfare_flag, stat = "total") %>% 
  mutate_at(vars(starts_with("estimated")), ~ . / 1000000)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 031 | Among those with no errors on card, number people experienced any exclusion ----
descr <- "Among those with no errors on card, % people experienced any exclusion"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)
summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes", 
         zh_denial_aa_any_welfare_flag != "Not a user") %>% 
  summariser(zh_denial_aa_any_welfare_flag, zr_aadhaar_has_error, compare = "no") %>% 
  select(-ends_with("low"), -ends_with("upp"))
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 032 |  % of secc, homeless, thirdgender who has access to PDS, by access to Aadhaar ----
descr <- "% of secc, homeless, thirdgender, who has access to PDS, by access to Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>% 
  filter(zh_secc_classification == "Automatically included") %>% 
  summariser(xr_has_pds_card, xr_has_aadhaar, type = "flipped") %>%
  clean_names() %>%
  select(xr_has_aadhaar, n_no, perc_no, n_yes, perc_yes) %>%
  mutate(category = "SECC included (including homeless)")

temp <- purposive_df %>% 
  filter(zh_homeless == "Yes") %>% 
  summariser(xr_has_pds_card, xr_has_aadhaar, type = "flipped") %>%
  clean_names() %>%
  select(xr_has_aadhaar, n_no, perc_no, n_yes, perc_yes) %>%
  mutate(category = "Homeless (purposive sample)") %>%
  bind_rows(temp)

temp <- purposive_df %>% 
  filter(zr_third_gender == "Yes") %>% 
  summariser(xr_has_pds_card, xr_has_aadhaar, type = "flipped") %>%
  clean_names() %>%
  select(xr_has_aadhaar, n_no, perc_no, n_yes, perc_yes) %>%
  mutate(category = "Third gender (purposive sample)") %>%
  bind_rows(temp)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 033 |  % of SECC, homeless, third gender, who has access to MGNREGA, by access to Aadhaar (footnote) ----
descr <- "% of SECC, homeless, third gender, who has access to MGNREGA, by access to Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>% 
  filter(zh_secc_inclusion_flag != "Others") %>% 
  summariser(xr_has_mnrega_card, xr_has_aadhaar, type = "flipped") %>%
  select(xr_has_aadhaar, n_no, perc_no, n_yes, perc_yes) %>%
  mutate(category = "SECC included (including homeless)")

temp <- purposive_df %>% 
  filter(zh_homeless == "Yes") %>% 
  summariser(xr_has_mnrega_card, xr_has_aadhaar, type = "flipped") %>%
  select(xr_has_aadhaar, n_no, perc_no, n_yes, perc_yes) %>%
  mutate(category = "Homeless (purposive sample)") %>%
  bind_rows(temp)

temp <- purposive_df %>% 
  filter(zr_third_gender == "Yes") %>% 
  summariser(xr_has_mnrega_card, xr_has_aadhaar, type = "flipped") %>%
  select(xr_has_aadhaar, n_no, perc_no, n_yes, perc_yes) %>%
  mutate(category = "Third gender (purposive sample)") %>%
  bind_rows(temp)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 034 | Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to aadhaar biometric related reasons ----
descr <- "Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to aadhaar biometric related reasons"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_pds_card == "Yes", xh_tried_getting_pds_in_last_3months_flag == "Yes") %>% 
  mutate(var = ifelse(zr_denial_aa_pds_biometric_failure_flag == "Yes" & zh_got_pds_success == "Tried but did not get", "Yes", "No")) %>% 
  summariser(var) 
  
logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 035 | Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to aadhaar linking related reasons ----
descr <- "Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to aadhaar linking related reasons"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_pds_card == "Yes", xh_tried_getting_pds_in_last_3months_flag == "Yes") %>% 
  mutate(var = ifelse(zr_denial_aa_pds_linking_failure_flag == "Yes" & zh_got_pds_success == "Tried but did not get", "Yes", "No")) %>% 
  summariser(var) 
  
logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 036 | Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to aadhaar no member working fp present ----
descr <- "Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to aadhaar no member working fp present"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_pds_card == "Yes", xh_tried_getting_pds_in_last_3months_flag == "Yes") %>% 
  mutate(var = ifelse(zr_denial_aa_pds_fp_member_absent_flag == "Yes" & zh_got_pds_success == "Tried but did not get", "Yes", "No")) %>% 
  summariser(var)
  

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 037 | Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to any aadhaar related reasons ----
descr <- "Among PDS card holders who tried to get ration in last 3 mons, % who did not get it due to aadhaar related reasons"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_pds_card == "Yes", xh_tried_getting_pds_in_last_3months_flag == "Yes") %>% 
  mutate(var = ifelse(zr_denial_aa_pds_any_aadhaar_related_above_flag == "Yes" & zh_got_pds_success == "Tried but did not get", "Yes", "No")) %>% 
  summariser(var) 
  
logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 038 | Among PDS card holders who tried to get ration in last 3 mons and got in full, % who faced aadhaar related difficulties ----
descr <- "Among PDS card holders who tried to get ration in last 3 mons and got in full, % who faced aadhaar related difficulties"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_pds_card == "Yes", 
         xh_tried_getting_pds_in_last_3months_flag == "Yes", 
         zh_got_pds_success == "Tried and got, full") %>% 
  summariser(zr_denial_aa_pds_any_aadhaar_related_above_flag)
  
logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 039 | Among MNREGA card holders, % who did not get a job last time they tried due to aadhaar ----
descr <- "Among MNREGA card holders, % who did not get a job last time they tried due to aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_mnrega_card == "Yes") %>% 
  summariser(zh_denial_aa_mnrega_job) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 040 | Among MNREGA card holders, % who did not get a job last time they tried due to non aadhaar ----
descr <- "Among MNREGA card holders, % who did not get a job last time they tried due to non aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_mnrega_card == "Yes") %>% 
  summariser(zh_denial_na_mnrega_job) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 041 | Among MNREGA card holders who got job, % who did not get wages ----
descr <- "Among MNREGA card holders who got job, % who did not get wages"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_mnrega_card == "Yes", xr_got_mnrega_job_in_last_try_flag == "Yes") %>% 
  summariser(zh_got_mnrega_wages_success) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 042 | Among MNREGA workers, % who did not get wages due to aadhaar ----
descr <- "Among MNREGA card holders, % who did not get wages due to aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_mnrega_card == "Yes", xr_got_mnrega_job_in_last_try_flag == "Yes") %>% 
  mutate(var = ifelse(zh_denial_aa_mnrega_wages_failed == "Yes" & 
                        zh_got_mnrega_wages_success == "Did not get wages for my job", "Yes", "No")) %>% 
  summariser(var) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 043 | Among MNREGA workers, % who did not get wages due to non aadhaar ----
descr <- "Among MNREGA card holders, % who did not get wages due to non aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_mnrega_card == "Yes", xr_got_mnrega_job_in_last_try_flag == "Yes") %>% 
  mutate(var = ifelse(zh_denial_na_mnrega_wages_failed == "Yes" & 
                        zh_got_mnrega_wages_success == "Did not get wages for my job", "Yes", "No")) %>% 
  summariser(var) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 044 | Among social pension holders, % who did not get last pension ----
descr <- "Among social pension holders, % who did not get last pension"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_social_pension == "Yes") %>% 
  summariser(xr_received_pension_last_period) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 045 | Among social pension holders, % who did not get last pension due to aadhaar ----
descr <- "Among social pension holders, % who did not get last pension due to aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_social_pension == "Yes") %>% 
  mutate(temp = ifelse(xr_received_pension_last_period == "No" &
                         zh_denial_aa_pension_failed == "Yes",
                       "Yes",
                       "No")) %>% 
  summariser(temp) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 046 | Among social pension holders, % who did not get last pension due to non aadhaar ----
descr <- "Among social pension holders, % who did not get last pension due to non aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- random_df %>% 
  filter(xr_has_social_pension == "Yes") %>% 
  mutate(temp = ifelse(xr_received_pension_last_period == "No" &
                         zh_denial_na_pension_failed == "Yes",
                       "Yes",
                       "No")) %>% 
  summariser(temp) 

logbreak(analysis_number, descr)
logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 047 | Among adults who have Aadhaar and their hh has tried collecting pds in last 3 months, % of individuals who found it easier to access PDS due to aadhaar ----
descr <- "Among adults who have Aadhaar and their hh has tried collecting pds in last 3 months, % of individuals who found it easier to access PDS due to aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_aadhaar == "Yes",
         xr_has_pds_card == "Yes",
         xh_tried_getting_pds_in_last_3months_flag == "Yes") %>%
  summariser(xh_ease_due_to_aadhaar_getting_pds)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
logmsg("Refer CMIE data")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 048 | Among adults whose hh has tried for pds in 3 mons, % who used PDS portability basis they have Aadhaar ----
descr <- "Among adults whose hh has pds card and tried for pds in 3 mons, % who used PDS portability basis they have Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_pds_card == "Yes",
         xh_tried_getting_pds_in_last_3months_flag == "Yes") %>%
  summariser(xh_visited_multiple_pds_shop_nearby, xr_has_aadhaar)

logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 049 |  % usage of DBT and ease of linking Aadhaar  ----
descr <- "ease of DBT to bank account based on usage of Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_savings_account == "Yes",
         xr_dbt_flag == "Yes") %>%
  summariser(xr_linking_dbt_with_bank_account_ease, xr_has_aadhaar, compare = "no")

logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 050 | Among welfare recipients with Aadhaar, % who recieve their services more reliably ----
descr <- "Among welfare recipients with Aadhaar, % who recieve their services more reliably"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

welfare_services <- c("xh_tried_getting_pds_in_last_3months_flag",
                      "xr_has_mnrega_card",
                      "xr_has_social_pension")

reliability <- c("xh_reliability_of_pds_with_aadhaar",
                 "xr_reliability_mnrega_job_with_aadhaar",
                 "xr_reliability_of_getting_social_pension_using_aadhaar")

temp <- map2_dfr(welfare_services, reliability, function(x, y) {
  random_df %>%
    filter(xr_has_aadhaar == "Yes",
           !!sym(x) == "Yes") %>%
    summariser(!!y) %>%
    rename(response = y) %>%
    mutate(service = case_when(
      x == "xh_tried_getting_pds_in_last_3months_flag" ~ "PDS user",
      x == "xr_has_mnrega_card" ~ "MNREGA",
      x == "xr_has_social_pension" ~ "Social Pension"
    ))
})

logtable(temp, descr)
summary_list[[descr]] <- temp
rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 051 | Among Aadhaar holders, % who did not have a service before and used Aadhaar to gain first time access to a service ----
descr <- "Among Aadhaar holders, % who did not have a service before and used Aadhaar to gain first time access to a service"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") 

temp <- summary %>%
  summariser(zr_used_aadhaar_for_first_sim) %>%
  rename(used_aadhaar_for_first_access = zr_used_aadhaar_for_first_sim) %>%
  filter(used_aadhaar_for_first_access == "Yes") %>% 
  mutate(service = "SIM")

temp <- summary %>%
  summariser(zr_used_aadhaar_for_first_bank) %>%
  rename(used_aadhaar_for_first_access = zr_used_aadhaar_for_first_bank) %>%
  filter(used_aadhaar_for_first_access == "Yes") %>% 
  mutate(service = "Bank Account") %>%
  bind_rows(temp)

temp <- summary %>%
  summariser(zh_used_aadhaar_for_first_mnrega) %>%
  rename(used_aadhaar_for_first_access = zh_used_aadhaar_for_first_mnrega) %>%
  filter(used_aadhaar_for_first_access == "Yes") %>% 
  mutate(service = "MNREGA") %>%
  bind_rows(temp)

temp <- summary %>%
  summariser(zh_used_aadhaar_for_first_pds) %>%
  rename(used_aadhaar_for_first_access = zh_used_aadhaar_for_first_pds) %>%
  filter(used_aadhaar_for_first_access == "Yes") %>% 
  mutate(service = "PDS") %>%
  bind_rows(temp)

temp <- summary %>%
  summariser(zh_used_aadhaar_for_first_pension) %>%
  rename(used_aadhaar_for_first_access = zh_used_aadhaar_for_first_pension) %>%
  filter(used_aadhaar_for_first_access == "Yes") %>% 
  mutate(service = "Pension") %>%
  bind_rows(temp)

# ** only for sim and bank ----
summary1 <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>% 
  mutate(used_aadhaar_for_first_sim_bank = case_when(
    zr_used_aadhaar_for_first_bank == "Yes" ~ "Yes",
    zr_used_aadhaar_for_first_sim == "Yes" ~ "Yes",
    TRUE ~ "No")) 

summary1 %>% 
  select(used_aadhaar_for_first_sim_bank, 
         zr_used_aadhaar_for_first_bank, 
         zr_used_aadhaar_for_first_sim) %>%
  grouper() %>% 
  logtable("creation of First access to bank account and sim using Aadhaar")

summary1 <- summary1 %>% 
  summariser(used_aadhaar_for_first_sim_bank)

# ** variation by age group ----
summary2 <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(zr_used_aadhaar_for_first, zr_age_bucket1, type = "flipped")

logtable(temp, descr)
logtable(summary1, descr)
logtable(summary2, descr)
summary_list[[descr]] <- temp
rm(descr, summary1, summary2, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 052 | Among those who have SIM / BANK, % of individuals who found it easy to gain access based on used Aadhaar ----
descr <- "Among those who have SIM / BANK, % of individuals who found it easy to gain access based on used Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_savings_account == "Yes") %>%
  summariser(xr_ease_of_opening_last_bank_account, zr_to_get_bank_account_used_aadhaar, compare = "no") %>%
  rename(ease_of_opening = xr_ease_of_opening_last_bank_account) %>%
  select(-overall, -starts_with("percent_overall")) %>%
  mutate(service = "Bank Account")

temp <- random_df %>%
  filter(xr_has_sim_card == "Yes") %>%
  summariser(xr_ease_of_getting_last_sim, zr_to_get_sim_used_aadhaar) %>%
  rename(ease_of_opening = xr_ease_of_getting_last_sim) %>%
  select(-overall, -starts_with("percent_overall")) %>%
  mutate(service = "SIM Card") %>%
  bind_rows(temp)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 053 | Among those who have SIM / BANK, speed of gaining access to the service based on used Aadhaar ----
descr <- "Among those who have SIM / BANK, speed of gaining access to the service based on used Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  filter(xr_has_savings_account == "Yes") %>%
  summariser(xr_days_to_get_bank_account, zr_to_get_bank_account_used_aadhaar) %>%
  rename(days_to_open = xr_days_to_get_bank_account) %>%
  mutate(service = "Bank Account")

temp <- random_df %>%
  filter(xr_has_sim_card == "Yes") %>%
  summariser(xr_days_to_get_sim, zr_to_get_sim_used_aadhaar) %>%
  rename(days_to_open = xr_days_to_get_sim) %>%
  mutate(service = "SIM Card") %>%
  bind_rows(temp)

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 054 | Among all adults, % of individuals for whom accessing services became more difficult ----
descr <- "Among all adults, % of individuals for whom accessing services became more difficult"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp <- random_df %>%
  mutate(difficult_due_to_aadhaar = case_when(
    xr_ease_due_to_aadhaar_getting_other_id_card    == "Difficult with Aadhaar"  ~ "Yes", 
    xh_ease_due_to_aadhaar_getting_pds              == "More difficult"          ~ "Yes", 
    xr_ease_due_to_aadhaar_getting_school_admission == "More difficult"          ~ "Yes", 
    xr_ease_due_to_aadhaar_getting_mnrega_job       == "More difficult"          ~ "Yes", 
    xr_ease_due_to_aadhaar_getting_sim              == "More difficult"          ~ "Yes", 
    xr_ease_due_to_aadhaar_getting_social_pension   == "More difficult"          ~ "Yes", 
    xr_ease_due_to_aadhaar_proof_of_life            == "More difficult"          ~ "Yes", 
    xr_ease_due_to_aadhaar_opening_bank_account     == "More difficult"          ~ "Yes", 
    xr_ease_due_to_aadhaar_accessing_bank_deposit   == "More difficult"          ~ "Yes",
    TRUE ~ "Nopes")) %>%
  summariser(difficult_due_to_aadhaar) 

logtable(temp, descr)
summary_list[[descr]] <- temp

rm(descr, temp)
# ~~~~~~~~~


########################### FIGURES: ##########################-----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 010 | Usage of Aadhaar by mandation ----
descr <- "top use cases of Aadhaar, among people who have Aadhaar and use a service"
logbreak(10, descr, suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
meta_data <- import(file.path(v_loc["inputs"],
                           "02. Data for figures", 
                           "Figure 10. Meta data.xlsx")
                    )

temp <- NULL

for (num in 1:nrow(meta_data)) {
  
  # Moving forward for scholarships
  if (meta_data$variable[num] == "xr_usage_of_aadhaar_for_scholarships") {
    logmsg("There are no valid responses for using Aadhaar for scholarships. Moving forward")
    next
  }
  
  to_drop <- random_df %>%
    filter(xr_has_aadhaar == "Yes") %>%
    select(output = meta_data$variable[num], everything())
  
  if (!is.na(meta_data$base[num])) {
    to_drop <- to_drop %>%
      select(base = meta_data$base[num], everything()) %>% 
      filter(base == "Yes")}
  
  temp <- to_drop %>%
    summariser(output) %>%
    filter(output == "Yes") %>%
    mutate(value = perc_to_dec(perc_overall),
           use_case = meta_data$use_case[num],
           mandatory = meta_data$mandatory[num],
           no_of_responses = nrow(to_drop)) %>%
    select(use_case, value, mandatory, no_of_responses) %>%
    rbind(temp)
  
  rm(to_drop)
}

export(temp, file.path(v_loc["figures_data"], 
                    "Figure 10. Share of service users who have provided Aadhaar.xlsx"))
rm(descr, temp, meta_data)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 011 | Usage of Aadhaar by feature ----
descr <- "Among those who have Aadhaar, use of Aadhaar by feature"
logbreak(11, descr, suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logmsg("Please see analysis 007")

summary_list[[descr]] %>%
  mutate(type = ifelse(category %in% c("aadhaar photocopy", "aadhaar fingerprint", 
                                       "aadhaar num", "aadhaar card", "aadhaar iris scan", "aadhaar otp"),
                       "Traditional features", "Newer digital features")) %>%
  export(file.path(v_loc["figures_data"], 
                   "Figure 11. Use of different forms and features of Aadhaar.xlsx"))
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure ADH | Denial and difficulties ----
descr <- "Denial and difficulties of using Aadhaar"
logbreak("ADH", descr, suffix = "F")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# Section A: Break up of aadhaar related exclusion by type of service
exclusion_variables <- c("zh_denial_aa_invalid_pds_card",
                         "zh_denial_aa_no_pds_card",
                         "zh_denial_aa_invalid_mnrega_card",
                         "zh_denial_aa_no_mnrega_card",
                         "zh_denial_aa_cancel_pension")

temp_df <- map_dfr(exclusion_variables, function(x) {
  random_df %>% 
    summariser(!!x) %>% 
    rename(output = x) %>% 
    filter(output == "Yes") %>% 
    mutate(service = case_when(
      x %in% c("zh_denial_aa_invalid_pds_card",
               "zh_denial_aa_no_pds_card") ~  "PDS",
      x %in% c("zh_denial_aa_invalid_mnrega_card",
               "zh_denial_aa_no_mnrega_card") ~  "MNREGA",
      TRUE ~ "Pension"
    ),
    type = case_when(
      x %in% c("zh_denial_aa_invalid_pds_card",
               "zh_denial_aa_invalid_mnrega_card",
               "zh_denial_aa_cancel_pension") ~  "Thrown out",
      x %in% c("zh_denial_aa_no_pds_card",
               "zh_denial_aa_no_mnrega_card") ~  "Did not recieve",
      TRUE ~ "none"
    )) %>% 
    select(service, type, perc_overall)
  }) %>% 
  mutate(perc_overall = perc_to_dec(perc_overall)*100)


# Section B: Break up of exclusion by service and category
exclusion_variables <- c("zh_denial_aa_pds_hard_flag", 	
                         "zh_denial_na_pds_hard_flag", 
                         "zh_denial_aa_mnrega_hard_flag", 	
                         "zh_denial_na_mnrega_hard_flag", 
                         "zh_denial_aa_pension_hard_flag", 	
                         "zh_denial_na_pension_hard_flag", 
                         "zh_denial_aa_sim_hard_flag", 	
                         "zh_denial_na_sim_hard_flag", 
                         "zh_denial_aa_bank_account_hard_flag", 	
                         "zh_denial_na_bank_account_hard_flag")

temp_df <- map_dfr(exclusion_variables, function(x) {
  random_df %>% 
    summariser(!!x) %>% 
    rename(output = x) %>% 
    filter(output == "Yes") %>% 
    mutate(service = str_remove_all(x, "_hard_flag|zh_denial_aa_|zh_denial_na_"),
    type = ifelse(str_detect(x, "_aa_"), "Aadhaar", "Non-Aadhaar")) %>% 
    select(service, type, perc_overall)
}) %>% 
  mutate(perc_overall = perc_to_dec(perc_overall)*100)

# Section C: problems faced by PDS users who tried but did not recieve a service
exclusion_variables <- c("zr_denial_aa_pds_biometric_failure_flag", 	
                         "zr_denial_aa_pds_linking_failure_flag", 
                         "zr_denial_aa_pds_fp_member_absent_flag", 	
                         "zr_denial_aa_pds_any_aadhaar_related_above_flag")

temp_df <- map_dfr(exclusion_variables, function(x) {
  random_df %>% 
    filter(xr_has_pds_card == "Yes") %>% 
    mutate(output = ifelse(zh_got_pds_success == "Tried but did not get" &
                             !!sym(x) == "Yes", "Yes", "Nopes")) %>% 
    summariser(output) %>% 
    filter(output == "Yes") %>% 
    mutate(failure = str_remove_all(x, "zr_denial_aa_pds_|_flag")) %>% 
    select(failure, perc_overall)
}) %>% 
  mutate(perc_overall = perc_to_dec(perc_overall)*100)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save summary list ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# saveRDS(summary_list, file.path(v_loc[["outputs"]], "B2_Summary tables.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPEN OUTPUT LOG FILE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("Finished building 'B2_USING AADHAAR.R'")
