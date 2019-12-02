# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outfile <- file.path(v_loc["logs"], "B6_INCLUSION_EXCLUSION.txt")
unlink(outfile)
logmsg("~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ 6. INCLUSION EXCLUSION ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~", cursor = "")


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

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# LOCAL VARS AND SETTING UP QC TO ENSURE NO CHANGES TO THE DATASET ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
qc_check <- list(
  data = random_df,
  ncols = ncol(random_df),
  nrows = nrow(random_df))

summary_list <- list()

# ** variables needing member data ----
y_var1 <- c("xr_has_aadhaar")

# ** variables needing random and purposive data ----
y_var2 <- random_df %>%
  select(zr_enrolment_flag,
         
         zr_aadhaar_first_id,
         zr_aadhaar_first_personal_id,
         
         xr_hh_decision_making_authority_shift, 
         xr_reliability_of_getting_social_pension_using_aadhaar,
         zr_enrolment_ease_of_process, xr_update_ease_of_process, 
         zr_enrolment_ease_of_documents, xr_update_ease_of_documents, 
         xh_ease_due_to_aadhaar_getting_pds,
         xr_ease_due_to_aadhaar_getting_school_admission,
         xr_ease_due_to_aadhaar_getting_mnrega_job,
         xr_ease_due_to_aadhaar_getting_sim,
         xr_ease_due_to_aadhaar_getting_social_pension,
         xr_ease_due_to_aadhaar_proof_of_life,
         xr_ease_due_to_aadhaar_opening_bank_account,
         xr_ease_due_to_aadhaar_accessing_bank_deposit,
         xr_ease_due_to_aadhaar_getting_other_id_card,
         
         zr_freq_of_usage_3months_all_types_min_bucket, 
         zr_freq_of_usage_3months_all_types_max_bucket, 
         zr_usage_of_aadhaar_num_services_bucket,
         zr_usage_of_digital_features_bucket, 
         zr_avg_biometric_trials_per_service_bucket,
         
         xh_sentiment_satisfaction_aadhaar, xh_sentiment_affect_on_hh, 
         xh_sentiment_life_better_worse,
         
         zr_aadhaar_has_error, xr_aadhaar_name_error_flag, xr_aadhaar_address_error_flag,
         xr_aadhaar_dob_error_flag, xr_aadhaar_photo_error_flag, 
         xr_aadhaar_biometrics_error_flag, xr_aadhaar_gender_error_flag,
         xr_aadhaar_mob_num_error_flag,
         
         xh_visited_multiple_pds_shop_nearby,
         xr_refused_to_give_aadhaar_flag,
         
         zh_denial_aa_canceled_any, 
         zh_denial_aa_no_any, 
         zh_denial_aa_any_hard_flag, 

         zh_years_in_area_bucket2,
         
         starts_with("xr_usage_of_aadhaar_for_"),
         -xr_usage_of_aadhaar_for_scholarships,
         matches("z._used_aadhaar_for_first_")) %>% 
  colnames()

# ** numeric variables ----
y_var3 <- c("zr_ease_integration", "zr_ease_delivery", "zr_freq_of_usage_3months_all_types_min", 
            "zr_freq_of_usage_3months_all_types_max", "zr_usage_of_aadhaar_num_services",
            "zr_usage_of_digital_features", "zr_avg_biometric_trials_per_service", 
            "zr_trips_average", "xr_linking_social_pension_with_aadhaar_trips", 
            "xh_years_in_area", "zh_denial_aa_canceled_any_num_service",
            "zh_denial_aa_no_any_num_service", "zh_denial_aa_any_hard_flag_num_service")

# ** from mem vs segment ---
seg_rand_resp <- c("zr_disability_flag", 
                   "zh_scheduled_caste",
                   "zh_scheduled_tribe_vulnerable",
                   "zh_scheduled_tribe")
seg_purp_resp <- c("zh_laborer", "zh_migrant_laborer")
seg_rand_memb <- c("xr_gender_final",  "zh_secc_classification")                      
seg_purp_memb <- c("zr_elderly", "zh_homeless", "zr_third_gender")                    

benchmark_adult <- c(seg_rand_resp, seg_purp_resp, "zr_elderly", "zr_third_gender")
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CROSS GRIDS ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# ADHOC: TRIPS VS. MOBILE ERRORS----

descr <- paste0("summarise ", toupper("zr_trips_average"), " :by: ", toupper("xr_aadhaar_mob_num_error_flag"), " | ", "random_df")

logmsg(paste0("[Analysis: ", i, "]==================================================================================="), 
       cursor = "", gap = 0)  
logmsg(descr, cursor = "")

summary <- random_df %>% 
  select(xr_aadhaar_mob_num_error_flag, zr_trips_average, everything()) %>% 
  summariser(zr_trips_average, xr_aadhaar_mob_num_error_flag, vartype = "ci")

logtable(summary)
summary_list[[descr]] <- summary

rm(i, descr, j)


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Save summary list ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
summaries_d6 <- summary_list


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
saveRDS(summaries_d6, file = file.path(v_loc[["datasets"]], "summaries_d6.Rda"))
print("Finished building 'D6_INCLUSION_EXCLUSION.R'")
