# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
outfile <-  file.path(v_loc["logs"], "B3_SENTIMENT_AND_TRUST.txt")
unlink(outfile)
logmsg("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 3. SENTIMENT AND TRUST SECTION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", cursor = "")


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
# 001 | % of adults who say that their life has improved due to Aadhaar ----
descr <- "% of adults who say that their life has improved due to Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  summariser(xh_sentiment_life_better_worse)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 002 | Top benefits and challenges for Aadhaar ----
descr <- "Top benefits and challenges for Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

temp_df <- random_df %>%
  mutate_at(vars(starts_with("xr_benefit_matter_most_")), clean_text) %>%
  unite(benefits, starts_with("xr_benefit_matter_most_"), sep = "|") %>%
  mutate_at(vars(starts_with("xr_challenge_matter_most_")), clean_text) %>%
  unite(challenges, starts_with("xr_challenge_matter_most_"), sep = "|")

meta_data <- import(file.path(v_loc["inputs"], 
                              "02. Data for figures", 
                              "Figure 15. Meta data.xlsx")) %>%
  clean_names() %>%
  mutate(challenges = ifelse(is.na(challenges), "", challenges),
         cleaned_challenges = clean_text(challenges),
         cleaned_benefits = clean_text(benefits))

list_of_benefits <- meta_data %>%
  pull(cleaned_benefits)

benefits_df <- map_dfr(list_of_benefits, function(x) {
  print(paste("Working on benefit: ", x))

  temp_df %>%
    mutate(output = split_and_match(benefits, x),
           output = ifelse(output == TRUE, "Yes", "No")) %>%
    summariser(output) %>%
    filter(output == "Yes") %>%
    mutate(category = x)
})

# Creating data for challenges
list_of_challenges <- meta_data %>%
  pull(cleaned_challenges) %>%
  {.[. != ""]}

challenges_df <- map_dfr(list_of_challenges, function(x) {
  print(paste("Working on challenge: ", x))

  temp_df %>%
    mutate(output = split_and_match(challenges, x),
           output = ifelse(output == TRUE, "Yes", "No")) %>%
    summariser(output) %>%
    filter(output == "Yes") %>%
    mutate(category = x)
})

# Creating an overlap between challenges and benefits
overlap_df <- NULL

for (x in 1:nrow(meta_data)) {
  challenge_variable <- meta_data[x, "cleaned_challenges"]
  benefits_variable <- meta_data[x, "cleaned_benefits"]

  print(paste("Working on overlap between:", benefits_variable, "&", challenge_variable))

  if (challenge_variable == "") next

  overlap_df <- temp_df %>%
    mutate(output_challenge = split_and_match(challenges, challenge_variable),
           output_benefits = split_and_match(benefits, benefits_variable),
           output = ifelse(output_benefits == TRUE & output_challenge == TRUE,
                           "Yes",
                           "No")) %>%
    summariser(output) %>%
    filter(output == "Yes") %>%
    mutate(cleaned_benefits = benefits_variable) %>%
    rbind(overlap_df)

  rm(challenge_variable, benefits_variable)
}

# merging meta data for figure with benefits, challenges and overlap
summary <- meta_data %>%
  # Merging Benefits
  left_join(benefits_df, by = c("cleaned_benefits" = "category")) %>%
  select(benefits, challenges, cleaned_challenges, perc_benefit = perc_overall, cleaned_benefits) %>%
  # Merging Challenges
  left_join(challenges_df, by = c("cleaned_challenges" = "category")) %>%
  select(benefits, perc_benefit, challenges, perc_challenge = perc_overall, cleaned_benefits) %>%
  # Merging Overlap
  left_join(overlap_df, by = "cleaned_benefits") %>%
  select(benefits, perc_benefit, challenges, perc_challenge, perc_overlap = perc_overall)

logtable(summary, descr)

summary_list[[descr]] <- summary
rm(benefits_df, meta_data, list_of_challenges, challenges_df, overlap_df, summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 003 | % of marginalised groups for whom Aadhaar was their first ID ----
descr <- "% of marginalised for whom Aadhaar was their first ID"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

# ** variables needing member data ----
y_var1 <- c()

# ** variables needing random and purposive data ----
y_var2 <- c("zr_aadhaar_first_id")

# ** numeric variables ----
y_var3 <- c()

# ** from mem vs segment ---
seg_rand_resp <- c("zr_disability_flag")
seg_purp_resp <- c("zh_laborer", "zh_migrant_laborer")
seg_rand_memb <- c("xr_gender_final",  "zh_secc_classification")                      
seg_purp_memb <- c("zr_elderly", "zh_homeless", "zr_third_gender")                    

benchmark_adult <- c(seg_rand_resp, seg_purp_resp, "zr_elderly", "zr_third_gender")

# ** cross grids ----
i = 1
for(segment in c(seg_rand_resp, seg_purp_resp, seg_rand_memb, seg_purp_memb)){
  
  j = 1
  for (var in c(y_var1, y_var2, y_var3)){
    
    if (var %in% y_var1) {
      
      # ** for variable to be extracted from member level data, i.e. penetration ----
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
      
      # ** for variable to be extracted from respondent level data ----
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
    
    descr <- paste0("summarise ", toupper(var), " :by: ", toupper(segment), " | ", file_name)
    
    logmsg(paste0("[Analysis: ", i, "]==================================================================================="), 
           cursor = "", gap = 0)  
    logmsg(descr, cursor = "")
    
    summary <- df %>% 
      select(segment_temp = segment, var_temp = var, everything()) %>% 
      filter(trim(var_temp) != "", !is.na(var_temp), !is.na(segment_temp)) %>% 
      summariser(var_temp, segment_temp, vartype = "ci", compare = compare_val) 
    
    if (compare_val == "random" & var %in% c(y_var1, y_var2)){
      summary <- summary %>% 
        select(-overall, -perc_overall, -perc_overall_low, -perc_overall_upp)
    }
    
    print(paste0("Completed: ",
                 round(i*100/(length(c(seg_rand_resp, seg_rand_memb, seg_purp_resp, seg_purp_memb))*
                                length(c(y_var1, y_var2, y_var3))),0), 
                 "% overall | ",
                 round(j*100/(length(c(y_var1, y_var2, y_var3))),0),
                 "% for the segment..."))
    i = i + 1
    j = j + 1
    
    logtable(summary)
    summary_list[[descr]] <- summary
    
    rm(df, compare_val, summary, descr)
  }
}
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 004 | % of individuals who believe their data is well protected ----
descr <- "Of all adults who responded to having Aadhaar question, % of individuals who believe their data is well protected"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar != "Don't know") %>%
  summariser(xr_trust_personal_data_with_aadhaar)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 005 | Among adults who are eligible for atleast one welfare service, % who trust atleast one service MORE ----
descr <- "Among adults who are eligible for atleast one welfare service, % who trust atleast one service MORE"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  
  filter((xr_has_pds_card == "Yes" & xh_tried_getting_pds_in_last_3months_flag =="Yes") |
           (xr_has_mnrega_card == "Yes") |
           (xr_has_social_pension == "Yes")) %>%
  
  mutate(trust_prevents_other_get_benefits_in_my_name_atleast_one = case_when(
    xr_trust_aadhaar_prevents_other_get_pds_in_my_name == "Yes" ~ "Yes",
    xr_trust_aadhaar_prevents_other_get_wages_in_my_name == "Yes" ~ "Yes",
    xr_trust_aadhaar_prevents_other_get_pension_in_my_name == "Yes" ~ "Yes",
    TRUE ~ "No"
  )) %>%
  summariser(trust_prevents_other_get_benefits_in_my_name_atleast_one)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 006 | Among adults who are eligible for the welfare service, % who trust ALL service MORE ----
descr <- "Among adults who are eligible for the welfare service, % who trust ALL service MORE"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  
  mutate(trust_all_service_more = case_when(
    (xr_has_pds_card == "Yes" & 
       xh_tried_getting_pds_in_last_3months_flag =="Yes" & 
       xr_trust_aadhaar_prevents_other_get_pds_in_my_name != "Yes") |
      (xr_has_mnrega_card == "Yes" & 
         xr_trust_aadhaar_prevents_other_get_wages_in_my_name != "Yes") |
      (xr_has_social_pension == "Yes" &
         xr_trust_aadhaar_prevents_other_get_pension_in_my_name != "Yes") ~ "No",
    (xr_has_pds_card == "Yes" & 
       xh_tried_getting_pds_in_last_3months_flag =="Yes") |
      (xr_has_mnrega_card == "Yes") |
      (xr_has_social_pension == "Yes") ~ "Yes", 
    TRUE ~ NA_character_))

summary %>% 
  select(trust_all = trust_all_service_more,
         has_pds = xr_has_pds_card,
         tried_getting_pds = xh_tried_getting_pds_in_last_3months_flag,
         trust_pds = xr_trust_aadhaar_prevents_other_get_pds_in_my_name,
         has_mnrega = xr_has_mnrega_card,
         trust_mnrega = xr_trust_aadhaar_prevents_other_get_wages_in_my_name,
         has_pension = xr_has_social_pension,
         trust_pension = xr_trust_aadhaar_prevents_other_get_pension_in_my_name) %>% 
  grouper() %>% 
  logtable("QC: TRUST ALL FLAG")

summary <- summary %>% 
  filter(!is.na(trust_all_service_more)) %>% 
  summariser(trust_all_service_more)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 007 | Among adults who are eligible for atleast one welfare service, % who trust atleast one service LESS ----
descr <- "Among adults who are eligible for atleast one welfare service, % who trust atleast one service LESS"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  
  filter((xr_has_pds_card == "Yes" & xh_tried_getting_pds_in_last_3months_flag =="Yes") |
           (xr_has_mnrega_card == "Yes") |
           (xr_has_social_pension == "Yes")) %>%
  
  mutate(dont_trust_prevents_other_get_benefits_in_my_name_atleast_one = case_when(
    xr_trust_aadhaar_prevents_other_get_pds_in_my_name == "No" ~ "Yes",
    xr_trust_aadhaar_prevents_other_get_wages_in_my_name == "No" ~ "Yes",
    xr_trust_aadhaar_prevents_other_get_pension_in_my_name == "No" ~ "Yes",
    TRUE ~ "No"
  )) %>%
  summariser(dont_trust_prevents_other_get_benefits_in_my_name_atleast_one)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 008 | Among those without Aadhaar, % who believe their data is well protected ----
descr <- "Among those without Aadhaar, % who believe their data is well protected"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "No") %>%
  summariser(xr_trust_personal_data_with_aadhaar)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 009 | Among those with Aadhaar, % who believe their data is well protected ----
descr <- "Among those with Aadhaar, % who believe their data is well protected"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_trust_personal_data_with_aadhaar)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 010 | % faced Aadhaar related frauds ----
descr <- "% faced Aadhaar related frauds"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  # filter(xr_has_aadhaar == "Yes") %>%
  summariser(xr_aadhaar_fraud_flag)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 011 | Amongst those who faced Aadhaar related frauds, % of individuals who believe thier data is not well protected ----
descr <- "Amongst those who faced Aadhaar related frauds, % of individuals who believe thier data is not well protected"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_aadhaar_fraud_flag %in% c("Yes", "No")) %>%
  summariser(xr_trust_personal_data_with_aadhaar, xr_aadhaar_fraud_flag, type = "flipped")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 012 | % of adults satisfied with aadhaar ----
descr <- "% of adults who are satisfied with aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  summariser(xh_sentiment_satisfaction_aadhaar)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)

logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 013 | % of adults satisfied with aadhaar, by state ----
descr <- "% of adults satisfied with aadhaar, by state"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  summariser(xh_sentiment_satisfaction_aadhaar, zh_state, type = "flipped")

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)

logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 014 | Among Aadhaar holders, % for whom its their prefered id ----
descr <- "Among Aadhaar holders, % for whom its their prefered id"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(xr_preferred_id_card) %>% 
  mutate(temp_col = perc_to_dec(perc_overall)) %>% 
  arrange(-temp_col) %>% 
  select(-temp_col)
  

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 015 | Among all adults, % of adults satisfied with aadhaar, by if they faced aadhaar related denial ----
descr <- "Among all adults, % of adults satisfied with aadhaar, by if they faced aadhaar related denial"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- rbind( 
  
  random_df %>%
    summariser(xh_sentiment_satisfaction_aadhaar) %>% 
    mutate(denial_type = "ALL ADULTS"),
  
  data.frame(xh_sentiment_satisfaction_aadhaar = "-", 
             overall = "-",
             perc_overall = "-", 
             perc_overall_low = "-",
             perc_overall_upp = "-",
             denial_type = "-"),
  
  random_df %>%
    filter(zh_denial_aa_any_hard_flag == "Yes") %>% 
    summariser(xh_sentiment_satisfaction_aadhaar) %>% 
    mutate(denial_type = "HARD EXCLUSION"),
  
  data.frame(xh_sentiment_satisfaction_aadhaar = "-", 
             overall = "-",
             perc_overall = "-", 
             perc_overall_low = "-",
             perc_overall_upp = "-",
             denial_type = "-"),
  
  random_df %>%
    filter(zh_denial_aa_any_welfare_flag == "Yes") %>% 
    summariser(xh_sentiment_satisfaction_aadhaar) %>% 
    mutate(denial_type = "SOFT DENIAL"),
  
  data.frame(xh_sentiment_satisfaction_aadhaar = "-", 
             overall = "-",
             perc_overall = "-", 
             perc_overall_low = "-",
             perc_overall_upp = "-",
             denial_type = "-"),
  
  random_df %>%
    filter(zh_denial_aa_any_welfare_flag == "No") %>% 
    summariser(xh_sentiment_satisfaction_aadhaar) %>% 
    mutate(denial_type = "NO DENIAL")
  
  )

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 016 | % of adults satisfied with aadhaar, by number of services denied ----
descr <- "% of adults satisfied with aadhaar, by number of services denied"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  mutate(zh_denial_aa_any_hard_flag_num_service = as.character(zh_denial_aa_any_hard_flag_num_service)) %>% 
  summariser(xh_sentiment_satisfaction_aadhaar, zh_denial_aa_any_hard_flag_num_service, type = "flipped") %>% 
  logtable(paste0(descr, "- OVERALL"))

summary <- random_df %>%
  summariser(xh_sentiment_satisfaction_aadhaar, zh_num_services_denied_bucket, type = "flipped") %>%
  logtable(paste0(descr, "- OVERALL"))

summary <- random_df %>%
  mutate(zh_denial_aa_no_any_num_service = as.character(zh_denial_aa_no_any_num_service)) %>%
  summariser(xh_sentiment_satisfaction_aadhaar, zh_denial_aa_no_any_num_service, type = "flipped") %>%
  logtable(paste0(descr, "- HINDERED ACCESS"))

summary <- random_df %>%
  mutate(zh_denial_aa_canceled_any_num_service = as.character(zh_denial_aa_canceled_any_num_service)) %>%
  summariser(xh_sentiment_satisfaction_aadhaar, zh_denial_aa_canceled_any_num_service, type = "flipped") %>%
  logtable(paste0(descr, "- LOST ACCESS"))

summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 017 | Among all aadhaar holders, % satisfaction with Aadhaar based on error in their Aadhaar ----
descr <- "Among all aadhaar holders, % satisfaction with Aadhaar based on error in their Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes") %>% 
  summariser(xh_sentiment_satisfaction_aadhaar, zr_aadhaar_has_error)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 018 | Adult satisfaction with Aadhaar by gender ----
descr <- "Adult satisfaction with Aadhaar by gender"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(analysis_number, descr)

summary <- random_df %>%
  summariser(xh_sentiment_satisfaction_aadhaar, xr_gender_final, compare = "Female")

logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)

logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 019 | % of individuals satisfied with aadhaar, by access to Aadhaar ----
descr <- "% of adults satisfied with aadhaar, by access to Aadhaar"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>% 
  summariser(xh_sentiment_satisfaction_aadhaar, xr_has_aadhaar)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary, descr)

logmsg("Please see CMIE analysis")
# ~~~~~~~~~

########################### ADHOCS: ##########################-----

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 020 | satisfaction based on years ----
descr <- "Among adults who have Aadhaar and reported their year of enrolment, change in satisfaction over time"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes", !is.na(zr_days_since_aadhaar)) %>%
  mutate(zr_years_since_aadhaar = round(zr_days_since_aadhaar/365, 0)) %>% 
  summariser(zr_years_since_aadhaar, xh_sentiment_satisfaction_aadhaar)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 021 | affect on HH based on years ----
descr <- "Among adults who have Aadhaar and reported their year of enrolment, change in impact on HH over time"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes", !is.na(zr_days_since_aadhaar)) %>%
  mutate(zr_years_since_aadhaar = round(zr_days_since_aadhaar/365, 0)) %>% 
  summariser(zr_years_since_aadhaar, xh_sentiment_affect_on_hh)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 022 | life better based on years ----
descr <- "Among adults who have Aadhaar and reported their year of enrolment, change in life impact over time"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes", !is.na(zr_days_since_aadhaar)) %>%
  mutate(zr_years_since_aadhaar = round(zr_days_since_aadhaar/365, 0)) %>% 
  summariser(zr_years_since_aadhaar, xh_sentiment_life_better_worse)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 023 | number of services based on years ----
descr <- "Among adults who have Aadhaar and reported their year of enrolment, change in # services over time"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  filter(xr_has_aadhaar == "Yes", !is.na(zr_days_since_aadhaar)) %>%
  mutate(zr_years_since_aadhaar = round(zr_days_since_aadhaar/365, 0)) %>% 
  mutate(zr_years_since_aadhaar = ifelse(zr_years_since_aadhaar == 0, "Very recent", "Older")) %>% 
  summariser(zr_usage_of_aadhaar_num_services, zr_years_since_aadhaar)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 024 | % respondents for whom Aadhaar is first ID ----
descr <- "% respondents for whom Aadhaar is first ID"
analysis_number <- analysis_number + 1
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summary <- random_df %>%
  summariser(zr_aadhaar_first_id)

logbreak(analysis_number, descr)
logtable(summary, descr)
summary_list[[descr]] <- summary
rm(summary)
# ~~~~~~~~~


########################### FIGURES: ##########################-----


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 13 | top benefits and challenges with Aadhaar ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(13, "top benefits and challenges with Aadhaar", suffix = "F")
export(summary_list[["Top benefits and challenges for Aadhaar"]],
       file.path(v_loc["figures_data"],
                 "Figure 13. Benefits and challenges of Aadhaar.xlsx"))
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 14 | Share of people who used aadhaar to get access to first time service ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(14, "Share of people who used aadhaar to get access to first time service", suffix = "F")

# Creating empty list to hold the list of services
sequence_service <- list()

sequence_service[["Ration"]] <- random_df %>% 
  filter(zr_aadhaar_first_id == "Yes") %>%
  summariser(zh_used_aadhaar_for_first_pds) %>%
  mutate(value = perc_to_dec(perc_overall)) %>%
  rename(category = zh_used_aadhaar_for_first_pds)

sequence_service[["MGNREGS"]] <- random_df %>% 
  filter(zr_aadhaar_first_id == "Yes") %>%
  summariser(zh_used_aadhaar_for_first_mnrega) %>%
  mutate(value = perc_to_dec(perc_overall)) %>%
  rename(category = zh_used_aadhaar_for_first_mnrega)

sequence_service[["Pensions"]] <- random_df %>% 
  filter(zr_aadhaar_first_id == "Yes") %>%
  summariser(zh_used_aadhaar_for_first_pension) %>%
  mutate(value = perc_to_dec(perc_overall)) %>%
  rename(category = zh_used_aadhaar_for_first_pension)

sequence_service[["Bank Account"]] <- random_df %>% 
  filter(zr_aadhaar_first_id == "Yes") %>%
  summariser(zr_used_aadhaar_for_first_bank) %>%
  mutate(value = perc_to_dec(perc_overall)) %>%
  rename(category = zr_used_aadhaar_for_first_bank)

sequence_service[["SIM Card"]] <- random_df %>% 
  filter(zr_aadhaar_first_id == "Yes") %>%
  summariser(zr_used_aadhaar_for_first_sim) %>%
  mutate(value = perc_to_dec(perc_overall)) %>%
  rename(category = zr_used_aadhaar_for_first_sim)

imap_dfr(sequence_service, function(x, y) {
  x %>%
    mutate(service = y)
}) %>%
  group_by(service) %>%
  mutate(no_of_users = sum(overall)) %>%
  filter(category == "Yes") %>%
  select(service, value, no_of_users) %>%
  arrange(-value) %>%
  export(file.path(v_loc["figures_data"], 
                   "Figure 14. Share of people who used Aadhaar to gain first time access.xlsx"))

rm(sequence_service)
# ~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 15 | % of individuals satisfied with Aadhaar ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(15, "% of individuals with aadhaar by state", suffix = "F")
logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Figure 16 | % of individuals satisfied with Aadhaar by state ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
logbreak(16, "% of individuals satisfied with aadhaar by state", suffix = "F")
logmsg("Please see CMIE analysis")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save summary list ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# saveRDS(summary_list, file.path(v_loc[["outputs"]], "B3_Summary tables.rds"))


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# OPEN OUTPUT LOG FILE
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
print("Finished building 'B3_SENTIMENT_AND_TRUST.R'")
