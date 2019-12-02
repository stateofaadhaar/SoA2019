# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 01 | START ----
# Initialises R and sets up basic options
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
system2("taskkill", args = "/im notepad.exe")
rm(list = ls())
if (!is.null(dev.list())) dev.off()
cat("\014") # Clears the console
options(scipen = 999, survey.lonely.psu = "adjust")
echo_val = F
time_capsule <- list()
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 02 | FILES AND FOLDERS ----
# Sets up folder names for import and export of data
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (T) {
  # File path list
  v_loc <- list()
  v_loc["working"] <- dirname(rstudioapi::getSourceEditorContext()$path)
  v_loc["inputs"]  <- file.path(v_loc[["working"]],"01. Inputs")
  v_loc["codes"]   <- file.path(v_loc[["working"]],"02. Codes")
  v_loc["outputs"] <- file.path(v_loc[["working"]],"03. Outputs")
  
  # Additional paths
  v_loc["datasets"]  <- file.path(v_loc[["inputs"]],"01. Datasets")
  v_loc["figures_data"]  <- file.path(v_loc[["outputs"]],"01. Data for figures")
  v_loc["logs"]  <- file.path(v_loc[["outputs"]],"03. Logs")
}
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 03 | GLOBAL CONSTANTS ----
# Defines certain constants throughout the analysis
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (T) {
  v_global <- list()
  
  v_global["sc_verdict_date"] <- as.Date("2018-Sep-30", format = "%Y-%B-%d")
  v_global["significance"] <- 0.95
  v_global["spread"] <- 0.10

  v_global["low_age_grp_1"] <- 0
  v_global["high_age_grp_1"] <- 5
  v_global["low_age_grp_2"] <- 6
  v_global["high_age_grp_2"] <- 17
  v_global["low_age_grp_3"] <- 18
  v_global["high_age_grp_3"] <- 70
  v_global["low_age_grp_4"] <- 71
  v_global["high_age_grp_4"] <- 150
}  
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 04 | LIBRARIES ----
# Load required packages
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (T) {
  pkgs <- c("extrafont", "gdata", "reshape2", "janitor", "rpart", "caret", "mlbench", 
            "data.table", "rio", "gridExtra", "grid", "srvyr", "analyze.stuff", 
            "DataCombine", "stargazer", "tidyverse")
  
  loaded <- unlist(lapply(pkgs, require, character.only = TRUE))
  
  if (!all(loaded)) stop("Some packages have not be loaded. Please install before proceeding")
  
  rm(pkgs, loaded)
  # loadfonts(device = "win")
}
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 05 | FUNCTION CODES ----
# Source custom functions 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(file.path(v_loc[["codes"]],"A1_ANALYSIS_FUNCTIONS.R"), print.eval = TRUE, echo = echo_val)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 06 | INITIALISING SUMMARISERS ----
# Setup the summariser function with default weights and survey design
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
if (T) {
  # ** SoA Indepth survey ----
  
  # general summariser, used for respondent level data
  summariser <- partial(summariser_base, survey_design = list(
    id = c("zh_state_district"),
    strata = c("xh_urban_rural"),      
    weight = "xh_resp_weight",                      
    nest = TRUE))
  
  # Used for member level data
  summariser_mem <- partial(summariser_base, survey_design = list(
    id = c("zh_state_district"), 
    strata = c("xh_urban_rural"),      
    weight = "xh_mem_weight",      
    nest = TRUE))
  
  # Used for household level estimations
  summariser_hh <- partial(summariser_base, survey_design = list(
    id = c("zh_state_district"), 
    strata = c("xh_urban_rural"),      
    weight = "xh_hh_weight",      
    nest = TRUE))
  
  summariser_resp <- summariser
  
  # ** SoA pulse survey ----
  
  # Used for pulse estimates
  summariser_pulse <- partial(summariser_base, survey_design = list(
    id = c("town_village"), 
    strata = c("hr_regtype_strata"),      
    weight = "hh_weight_extn_hr_regtype_strata_sample_wave",      
    nest = TRUE))
  
  # Used for member level estimations
  summariser_pulse_mem <- partial(summariser_base, survey_design = list(
    id = c("town_village"), 
    strata = c("hr_regtype_strata"),      
    weight = "mem_weight_extn_all_hr_regtype_strata_sample_wave",      
    nest = TRUE))
  
  }
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 07 | ANALYSIS CODES ----
# Run complete analysis conducted for State of Aadhaar, 2019 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source_time(v_loc[["codes"]],"B1_GETTING_AADHAAR.R"       , print.eval = TRUE, echo = echo_val)
source_time(v_loc[["codes"]],"B2_USING_AADHAAR.R"         , print.eval = TRUE, echo = echo_val)
source_time(v_loc[["codes"]],"B3_SENTIMENT_AND_TRUST.R"   , print.eval = TRUE, echo = echo_val)
source_time(v_loc[["codes"]],"B4_AWARENESS.R"             , print.eval = TRUE, echo = echo_val)
source_time(v_loc[["codes"]],"B5_CHILDREN_AND_EDUCATION.R", print.eval = TRUE, echo = echo_val)
source_time(v_loc[["codes"]],"B6_INCLUSION_EXCLUSION.R"   , print.eval = TRUE, echo = echo_val)
source_time(v_loc[["codes"]],"B9_HIGHLIGHTS.R"            , print.eval = TRUE, echo = echo_val)
# ~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 08 | VISUALISATION CODES ----
# Generate visuals featured in State of Aadhaar, 2019 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source_time(v_loc[["codes"]],"C2_VISUALISATION.R"       , print.eval = TRUE, echo = echo_val)
# ~~~~~~~~~


print(time_capsule)
# END ~~~~

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 09 | EXAMPLES ----
# Examples to get started
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# ** Setting up the survey ----
# 
# For both the SoA pulse and in-depth survey, please ensure that you setup the survey to reflect the 
# correct ids, strata and weights using the `survey` package. The design can be provided using the 
# `svydesign` function as below;
#
# svydesign(
#   ids = ~town_village, 
#   strata = ~hr_regtype_strata, 
#   weights = ~mem_weight_extn_all_hr_regtype_strata_sample_wave, 
#   data = <-SOA PULSE SURVEY->
# )
# 
# svydesign(
#   ids = ~zh_state_district, 
#   strata = ~xh_urban_rural, 
#   weights = ~xh_resp_weight, 
#   data = <-SOA INDEPTH SURVEY->
# )
# 
# Alternatively, please use `summariser` - a custom family of function built to analyse the SoA survey 
# Section 6 sets up the summarisers for different configurations - household, member or respondent
# level analysis
#
# ** Analysing data ----
# 
# The `survey` package offers a couple of different methods (proportions, cross-tables, GLMs)
# that can be utilised after a survey has been set. For example, in order to estimate the 
# share of individuals with Aadhaar, run the following
# 
# svytable(~has_aadhaar, <-survey design object->) %>%
#   as.data.frame() %>%
#   mutate(Percent = Freq/sum(Freq))
#
# In comparison, the `summariser` class of functions, is much easier to use. The same command translated
# to use `summariser` would read
#
# summariser(<name of dataset>, has_aadhaar)
#
# The `summariser` functions are tidyverse compliant, i.e. the function takes, and returns a dataframe, 
# and works with quasi-quatation (i.e. does not need you to provide the column name in quotes). The default
# setting for the function is to 1) provide proportions, 2) provide confidence intervals and 3) test for
# significance only when cross-tabulating. For more options please see A1_ANALYSIS_FUNCTIONS.R  or
# contact the authors.
# 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 10 | CONTACT US ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#
# Email: info@stateofaadhaar.in
