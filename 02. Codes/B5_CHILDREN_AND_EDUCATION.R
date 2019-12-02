# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outfile <- file.path(v_loc["logs"], "B5_CHILDREN AND EDUCATION.txt")
unlink(outfile)

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
# 001 |  % of children for whom providing Aadhaar was mandatory ----
descr <- "% of children for whom providing Aadhaar was mandatory"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)


summary <- random_mem_df %>% 
  filter(between(xr_age_final, 6, 14),
         lubridate::ymd(zh_school_enrolment_date) >= v_global[["sc_verdict_date"]]
  ) %>%
  mutate(aadhaar_was_mandatory = case_when(
    xh_school_admission_aadhaar_mandatory == "Mandatory" ~ "yes",
    TRUE ~ "nopes"
  )) %>% 
  summariser_mem(aadhaar_was_mandatory)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 002 |  % of children for whom Aadhaar was mandatory after SC ruling, by state ----
descr <- "among children who enroled in school after SC ruling, % for whom Aadhaar was mandatory, by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(between(xr_age_final, 6, 14),
         lubridate::ymd(zh_school_enrolment_date) >= v_global[["sc_verdict_date"]]) %>%
  summariser_mem(xh_school_admission_aadhaar_mandatory, zh_state, type = "flipped")

logtable(summary, descr)

logmsg(paste0("number of sates/UT for which over 75% people said Aadhaar was mandatory (lower bound): ", 
              summary %>% filter(perc_to_dec(perc_mandatory_low) > 0.75, zh_state != "Overall") %>% nrow()))
summary %>% 
  filter(perc_to_dec(perc_mandatory_low) > 0.75) %>% 
  logtable("List of states")

logmsg(paste0("number of sates/UT for which over 75% people said Aadhaar was mandatory (average bound): ", 
              summary %>% filter(perc_to_dec(perc_mandatory) > 0.75, zh_state != "Overall") %>% nrow()))
summary %>% 
  filter(perc_to_dec(perc_mandatory) > 0.75) %>% 
  logtable("List of states")

summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 003 |  % of children for whom Aadhaar was mandatory before SC ruling, by state ----
descr <- "among children who enroled in school before SC ruling, % for whom Aadhaar was mandatory, by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(between(xr_age_final, 6, 14),
         lubridate::ymd(zh_school_enrolment_date) < v_global[["sc_verdict_date"]]
  ) %>%
  summariser_mem(xh_school_admission_aadhaar_mandatory, zh_state, type = "flipped")

logtable(summary, descr)
logmsg(paste0("number of sates/UT for which over 75% people said Aadhaar was mandatory (lower bound): ", 
              summary %>% filter(perc_to_dec(perc_mandatory_low) > 0.75, zh_state != "Overall") %>% nrow()))
logmsg(paste0("number of sates/UT for which over 75% people said Aadhaar was mandatory (average bound): ", 
              summary %>% filter(perc_to_dec(perc_mandatory) > 0.75, zh_state != "Overall") %>% nrow()))
summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 004 |  % of children for whom Aadhaar was mandatory before vs after ruling, by state ----
descr <- "FOOTNOTE: among children who enroled in school before and after SC ruling, % for whom Aadhaar was mandatory, by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- summary_list[[analysis_number - 1]] %>% 
  select(zh_state, perc_mandatory_bef = perc_mandatory, perc_mandatory_low_bef = perc_mandatory_low, perc_mandatory_upp_bef = perc_mandatory_upp) %>% 
  left_join(summary_list[[analysis_number - 2]] %>% select(zh_state, 
                                                           perc_mandatory_aft = perc_mandatory, 
                                                           perc_mandatory_low_aft = perc_mandatory_low, 
                                                           perc_mandatory_upp_aft = perc_mandatory_upp), by = NULL) %>% 
  mutate_at(vars(starts_with("perc")), perc_to_dec) %>% 
  mutate(flag = ifelse(perc_mandatory_low_aft > perc_mandatory_upp_bef, "increase", 
                       ifelse(perc_mandatory_upp_aft < perc_mandatory_low_bef, "decrease",
                              "insignificant")))

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 005 |  % of children denied access to schooling due to Aadhaar ----
descr <- "among children who enroled in school after SC ruling, % who faced delay or denial of schooling due to Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(between(xr_age_final, 6, 14),
         lubridate::ymd(zh_school_enrolment_date) >= v_global[["sc_verdict_date"]]
  ) %>%
  summariser_mem(xh_school_admission_denied_due_to_aadhaar)


random_mem_df %>%
  filter(between(xr_age_final, 6, 14)) %>%
  mutate(could_not_enrol = ifelse(
    xh_enroled_in_school_flag == "No" &
      split_and_match(zh_denial_not_enroled_in_school_reason, 
                    c("DonothaveAadhaar", 
                      "Do not have Aadhaar", 
                      "DonothaveAadhaarANDotherrequireddocuments",
                      "Do not have Aadhaar AND other required documents")),
    "yes",
    "no")) %>%
  summariser_mem(could_not_enrol) %>% 
  logtable("Out of school cause of Aadhaar")

random_mem_df %>%
  filter(between(xr_age_final, 6, 14)) %>%
  mutate(could_not_enrol = ifelse(
    xh_enroled_in_school_flag == "No" &
      split_and_match(zh_denial_not_enroled_in_school_reason, 
                      c("DonothaveAadhaar", 
                        "Do not have Aadhaar", 
                        "DonothaveAadhaarANDotherrequireddocuments",
                        "Do not have Aadhaar AND other required documents")),
    "yes",
    "no")) %>%
  summariser_mem(could_not_enrol, stat = "total") %>% 
  logtable("Number of children out of school cause of Aadhaar")

random_mem_df %>%
  filter(between(xr_age_final, 6, 14), xh_want_to_enrol_in_school == "Yes") %>%
  mutate(could_not_enrol = ifelse(
    xh_enroled_in_school_flag == "No" &
      split_and_match(zh_denial_not_enroled_in_school_reason, 
                      c("DonothaveAadhaar", 
                        "Do not have Aadhaar", 
                        "DonothaveAadhaarANDotherrequireddocuments",
                        "Do not have Aadhaar AND other required documents")),
    "yes",
    "no")) %>%
  summariser_mem(could_not_enrol) %>% 
  logtable("Among those school going children who want to enrol, % out of school cause of Aadhaar")

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 006 |  % of children denied access to MDM due to Aadhaar ----
descr <- "Amongst all school going children, % who were denied access to MDM due to Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(between(xr_age_final, 6, 14)) %>%
  mutate(xh_school_mdm_denied_due_to_aadhaar = 
           ifelse(is.na(xh_school_mdm_denied_due_to_aadhaar), 
                  "", xh_school_mdm_denied_due_to_aadhaar)) %>% 
  summariser_mem(xh_school_mdm_denied_due_to_aadhaar)

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 007 |  estimated number of children denied access to MDM due to Aadhaar in 17 states ----
descr <- "estimated number of children denied access to MDM due to Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(between(xr_age_final, 6, 14)) %>%
  mutate(xh_school_mdm_denied_due_to_aadhaar = ifelse(is.na(xh_school_mdm_denied_due_to_aadhaar), "", xh_school_mdm_denied_due_to_aadhaar)) %>% 
  summariser_mem(xh_school_mdm_denied_due_to_aadhaar, stat = "total")

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 008 |  % of children who do not have Aadhaar ----
descr <- "% of children who do not have Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_mem_df %>%
  filter(xr_age_final < 18,
         xr_has_aadhaar != "") %>%
  summariser_mem(xr_has_aadhaar)

# NOTE: Removing invalid responses before summarising

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 009 |  % of families who did not get full allotment of PDS ----
descr <- "% of families who did not get full allotment of PDS"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>% 
  filter(xr_has_aadhaar == "Yes",
         xh_tried_getting_pds_in_last_3months_flag == "Yes",
         !is.na(xh_link_aadhaar_to_pds_num_hh_member),
         !is.na(zh_got_pds_success)) %>% 
  mutate(has_fewer_members = ifelse(xh_link_aadhaar_to_pds_num_hh_member < zh_hh_num_members,
                                    "fewer", "equal")) %>% 
  summariser(zh_got_pds_success, has_fewer_members, compare = "fewer")

logtable(summary, descr)
summary_list[[descr]] <- summary

rm(descr, summary)
# ~~~~~~~~~


########################### FIGURES: ##########################-----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 19 | Share of children for whom providing Aadhaar was mandatory, after the SC verdict ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(19, "Amongst children who took admission after SC ruling, % of children for whom providing Aadhaar was mandatory", suffix = "F")

temp_df <- random_mem_df %>%
  filter(between(xr_age_final, 6, 14),
         lubridate::ymd(zh_school_enrolment_date) >= v_global[["sc_verdict_date"]]
  )

summary <- temp_df %>%
  mutate(xh_school_admission_aadhaar_mandatory = str_replace_na(xh_school_admission_aadhaar_mandatory)) %>% 
  summariser_mem(xh_school_admission_aadhaar_mandatory, zh_state, type = "flipped") %>%
  select(zh_state, value = perc_mandatory_low, no_of_children = overall) %>% 
  mutate(value = perc_to_dec(value))

temp_df %>%
  group_by(zh_state) %>%
  summarise(no_of_households = length(unique(xr_person_id))) %>%
  right_join(summary, by = "zh_state") %>%
  export(file.path(v_loc["figures_data"],
                   "Figure 19. Share of children who provided Aadhaar for school enrolment.xlsx"))
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 20 | Share of children who have missed a meal due to aadhaar related reason ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(20, "Among children who have mid day meals in school, % who were denied MDM due to aadhaar related reason", suffix = "F")

temp_df <- random_mem_df %>%
  filter(between(xr_age_final, 6, 14))
  
summary <- temp_df %>%
  mutate(xh_school_mdm_denied_due_to_aadhaar = str_replace_na(xh_school_mdm_denied_due_to_aadhaar)) %>% 
  summariser_mem(xh_school_mdm_denied_due_to_aadhaar, zh_state, type = "flipped") %>%
  mutate(no_of_children = nrow(temp_df),
         no_of_households = length(unique(temp_df$xr_person_id)),
         value = perc_to_dec(perc_yes)) %>%
  select(zh_state, value, no_of_children, no_of_households)

export(summary, file.path(v_loc["figures_data"],
                          "Figure 20. Share of children who missed out on MDM.xlsx"))
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save summary list ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# saveRDS(summary_list, file.path(v_loc[["outputs"]], "B5_Summary tables.rds"))

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPEN OUTPUT LOG FILE ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("Finished building 'D5_CHILDREN AND EDUCATION.R'")

