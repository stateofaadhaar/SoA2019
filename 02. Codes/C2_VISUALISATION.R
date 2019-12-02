# START ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

outfile <- file.path(v_loc["logs"], "C2_VISUALISATION.txt")
unlink(outfile)

logmsg("~~~~~~~~~~~~~~~~~~~~ THIS SCRIPT CONTAINS VISUALS CREATED AS A PART OF THE MAIN REPORT ~~~~~~~~~~~~~~~~~~~~~~", cursor = "")

library(gridExtra)
library(grid)
library(patchwork)
library(scales)
library(ggforce)
library(ggrepel)
library(waffle)


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


# Setting up QCs to ensure no changes to the dataset ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

qc_check <- list(
  data = random_df,
  ncols = ncol(random_df),
  nrows = nrow(random_df)
)

# Loading India state boundries ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
proj <- "+proj=lcc +datum=WGS84 +lat_1=35.172805555556 +lat_2=12.472944444 +x_0=4000000 +y_0=4000000 +lat_0=24 +lon_0=80"
# Sourced from: https://swat.tamu.edu/docs/swat/india-dataset/2012/Data_sources.txt

india_state_boundries <- sf::read_sf(file.path(v_loc["inputs"], 
                                               "04. India boundary", 
                                               "States", 
                                               "Admin2.shx")) %>%
  rename(psu_state = ST_NM) %>% 
  sf::st_transform(crs = proj)

# Sourcing visualisation functions and themes ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
source(file.path(v_loc["codes"], "C1_VISUAL_FUNCTIONS.R"))

# Updating font face on geom text
update_geom_defaults("text", list(
  font = "Proxima Nova A",
  face = "plain",
  color = "#707274",
  size = 0.3514598 * 7
))

update_geom_defaults("label", list(
  font = "Proxima Nova A",
  face = "plain",
  color = "#707274",
  size = 0.3514598 * 7
))

source_core_survey <-  "Source: State of Aadhaar in-depth survey, 2019"
source_cmie_survey <- "Source: State of Aadhaar pulse survey, 2019"

visual_path <- file.path(v_loc["outputs"], "02. Figures")

# Figure 001 | Coverage of the user surveys ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created externally
# ~~~~~~~~~

# Figure 002 | The Aadhaar user journey ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created externally
# ~~~~~~~~~

# Figure 003 | Share of people who have Aadhaar, by state ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ** caption ----
notes <- c("Note: Respondents and their household members present during the interview were asked to respond on behalf of themselves and all absent household members.",
           "Numbers in the text and the chart may not match due to rounding.") %>% 
  paste(collapse = " ") %>% 
  str_wrap(100)

caption_text <- paste0(
  source_cmie_survey,
  " (N = ",
  format(575127, big.mark = ",", scientific = FALSE),
  ")\n",
  notes
)

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], "Figure 03. Share of residents enroled in Aadhaar.xlsx")) %>%
  filter(has_aadhaar == "Yes") %>%
  mutate(percent_yes_num = Percent*100,
         State = case_when(
           State == "Jammu and Kashmir" ~ "Jammu & Kashmir", 
           State == "Delhi" ~ "NCT of Delhi",
           TRUE ~ State
         )) %>%
  rename(psu_state = State) %>%
  select(psu_state, percent_yes_num) %>%
  right_join(india_state_boundries, by = "psu_state") %>%
  mutate(percent_yes = case_when(
           percent_yes_num <= 80 ~ "0% to 80%",
           percent_yes_num <= 90 ~ "81% to 90%",
           percent_yes_num > 90 ~ "91% to 100%"
         ),
         percent_yes = fct_relevel(percent_yes,
                                   "0% to 80%",
                                   "81% to 90%",
                                   "91% to 100%"
         ),
         percent_yes = fct_rev(percent_yes)
  )

# ** plotting image ----
png(file.path(visual_path, "03. Share of residents enroled in Aadhaar.png"),
    width = 79.6, 
    height = 88,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  geom_sf(aes(fill = percent_yes, 
              geometry = geometry),
          colour = "white",
          size = 0.2) +
  scale_fill_soa(na.value = "#d6d6d6",
                 palette = "main",
                 reverse = TRUE,
                 drop = FALSE,
                 labels = c("91% to 100%",
                            "81% to 90%",
                            "0% to 80%",
                            "Not surveyed"
                 )
  ) +
  scale_x_continuous(expand = expansion(0.15)) +
  labs(fill = "Legend",
       caption = caption_text) +
  theme_soa() +
  theme(
    axis.text = element_blank(),
    
    # LEGEND
    legend.position = c(0.11, 0.20)
  )

dev.off()
graphics.off()

rm(caption_text, temp)
# ~~~~~~~~~~~~~~~~


# Figure 004 | Main groups of people who do not have Aadhaar ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created externally
# ~~~~~~~~~


# Figure 005 | Reasons for not having Aadhaar ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created externally
# ~~~~~~~~~


# Figure 006 | Share of people who do not have Aadhaar, in each demographic group ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ** caption ----
caption_text <- paste0(
  source_cmie_survey,
  " (N = ",
  format(575127, big.mark = ",", scientific = FALSE),
  "); *",
  source_core_survey,
  " (Individuals who indentify as third gender, N = 399) \n",
  str_wrap(
    paste(
      "Note: Respondents and their household members present during the interview were asked to respond on behalf of themselves and all absent household members.",
      "For 1.9% of respondents and their household members, we received either nor response or a response of 'don't know'.",
      "* Individuals who identify as third-gender were over-sampled in the State of Aadhaar in-depth survey 2019; results are less representative ",
      "than for a random sample. Numbers in the text and the chart may not match due to rounding."
    ),
    210
  ),
  "\n"
) 

third_gender <- import(file.path(v_loc["figures_data"], "Figure 06. Share of people who do not have Aadhaar_third gender.xlsx"))

national_average_saturation <- 0.077

# Importing CMIE dataset
temp <- import_list(file.path(v_loc["figures_data"], "Figure 06. Share of people who do not have Aadhaar_all others.xlsx")) %>%
  map(function(x) {
    x %>% 
      filter(has_aadhaar == "No") %>%
      rename(sample_size = `Sample Size`)
  })

plot_list <- list() 

# ** Gender ----
plot_list[["GENDER"]] <- temp[[1]] %>%
  mutate(value = Percent,
         output = if_else(gender == "F", "Female", "Male")
         ) %>%
  select(output, value) %>%
  ggplot() +
  geom_col(data = third_gender,
           aes(x = output, 
               y = value),
           fill = "#fde8e6",
           width = 0.6) +
  geom_text(data = third_gender,
            aes(x = output,
                y = value + 0.03,
                label = dec_to_perc(value, 0)),
            colour = "#707274") +
  geom_col(aes(x = output, 
               y = value),
           fill = soa_light[["orange"]],
           width = 0.6) +
  geom_text(aes(x = output,
                y = value + 0.03,
                label = dec_to_perc(value, 0)),
            colour = "#707274") +
  geom_hline(aes(yintercept = national_average_saturation),
             linetype = 2,
             colour = "#707274",
             size = 0.2) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05)),
                     limits = c(0, 0.62),
                     oob = function(x, limits) x) +
  theme_soa() +
  # labs(subtitle = "GENDER") +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # plot subtitle
    plot.subtitle = element_text(
      colour = "#707274",
      hjust = 0.01,
      vjust = 1,
      face = "bold",
      size = 13
    )
  )

# ** Region ----
plot_list[["REGION"]] <- temp[[3]] %>%
  mutate(category = "REGION",
         value = Percent,
         output = if_else(region_type == "RURAL", "Rural", "Urban")) %>%
  select(category, output, value) %>%
  ggplot() +
  geom_col(aes(x = output, 
               y = value),
           fill = soa_light[["orange"]],
           width = 0.6) +
  geom_text(aes(x = output,
                y = ifelse(output %in% c("Urban"), 0.12,  value + 0.03),
                label = dec_to_perc(value, 0)),
            colour = "#707274") +
  geom_hline(aes(yintercept = national_average_saturation),
             linetype = 2,
             colour = "#707274",
             size = 0.2) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05)),
                     limits = c(0, 0.62),
                     oob = function(x, limits) x) +
  theme_soa() +
  # labs(subtitle = "REGION") +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # plot subtitle
    plot.subtitle = element_text(
      colour = "#707274",
      hjust = 0.01,
      vjust = 1,
      face = "bold",
      size = 13
    )
  )

# ** Age ----
plot_list[["AGE"]] <- temp[[4]] %>%
  mutate(category = "AGE",
         value = Percent,
         output = case_when(
           cal_age_type == "elderly" ~ "> 70 years",
           cal_age_type == "adult" ~ "18 to 69",
           TRUE ~ cal_age_type
         ),
         output = str_remove(output, " years"),
         output = fct_relevel(output,
                              "0 to 5",
                              "6 to 17",
                              "18 to 69")) %>%
  select(category, output, value) %>%
  ggplot() +
  geom_col(aes(x = output, 
               y = value),
           fill = soa_light[["orange"]],
           width = 0.6) +
  geom_text(aes(x = output,
                y = ifelse(output %in% c("18 to 69", "> 70"), 0.061,  value + 0.03),
                label = dec_to_perc(value, 0)),
            colour = "#707274") +
  geom_hline(aes(yintercept = national_average_saturation),
             linetype = 2,
             colour = "#707274",
             size = 0.2) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05)),
                     limits = c(0, 0.62),
                     oob = function(x, limits) x) +
  theme_soa() +
  # labs(subtitle = "AGE") +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # plot subtitle
    plot.subtitle = element_text(
      colour = "#707274",
      hjust = 0.01,
      vjust = 1,
      face = "bold",
      size = 13
    )
  )

# ** Caste ----
plot_list[["CASTE"]] <- temp[[2]] %>%
  mutate(category = "CASTE",
         value = Percent,
         output = caste_category) %>%
  filter(!output %in% c("Not Applicable", "Not Stated")) %>%
  select(category, output, value) %>%
  mutate(output = case_when(
    output == "Intermediate Caste" ~ "Intermediate\ncaste", 
    output == "Upper Caste" ~ "Upper caste", 
    TRUE ~ output
    ),
    output = fct_relevel(output,
                         "Upper caste",
                         "Intermediate\ncaste",
                         "OBC",
                         "SC",
                         "ST")) %>%
  ggplot() +
  geom_col(aes(x = output, 
               y = value),
           fill = soa_light[["orange"]],
           width = 0.6) +
  geom_text(aes(x = output,
                y = case_when(
                  output == "OBC" ~ 0.09,
                  TRUE ~ value + 0.01
                  ),
                label = dec_to_perc(value, 0)),
            colour = "#707274") +
  geom_hline(aes(yintercept = national_average_saturation),
             linetype = 2,
             colour = "#707274",
             size = 0.2) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05)),
                     limits = c(0, 0.15),
                     oob = function(x, limits) x) +
  theme_soa() +
  # labs(subtitle = "CASTE") +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # plot subtitle
    plot.subtitle = element_text(
      colour = "#707274",
      hjust = 0.01,
      vjust = 1,
      face = "bold",
      size = 13
    )
  )

# ** Religion ----
plot_list[["RELIGION"]] <- import(file.path(v_loc["figures_data"], "Figure 06. Share of people who do not have Aadhaar_religion.csv")) %>%
  filter(has_aadhaar == "No",
         !religion %in% c("Not Applicable", "Other Religion", "Religion not stated", "Khasi")) %>%
  mutate(category = "RELIGION",
         value = Percent,
         output = fct_reorder(religion, value, .desc = TRUE)) %>%
  select(category, output, value) %>%
  ggplot() +
  geom_col(aes(x = output, 
               y = value),
           fill = soa_light[["orange"]],
           width = 0.6) +
  geom_text(aes(x = output,
                y = case_when(
                  output %in% c("Sikh", "Buddhist") ~ 0.05,  
                  output %in% c("Hindu") ~ 0.09,  
                  TRUE ~ value + 0.01),
                label = dec_to_perc(value, 0)),
            colour = "#707274") +
  geom_hline(aes(yintercept = national_average_saturation),
             linetype = 2,
             colour = "#707274",
             size = 0.2) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05)),
                     limits = c(0, 0.15),
                     oob = function(x, limits) x) +
  theme_soa() +
  # labs(subtitle = "RELIGION") +
  theme(
    axis.text.y = element_blank(),
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # plot subtitle
    plot.subtitle = element_text(
      colour = "#707274",
      hjust = 0.01,
      vjust = 1,
      face = "bold",
      size = 13
    )
  )

# ** Plotting all graphs together ----
png(file.path(visual_path, "06. Share of people who do not have Aadhaar.png"),
    width = 163, 
    height = 88.5,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

(
  ((plot_list[["AGE"]] | plot_spacer() | plot_list[["GENDER"]] | plot_spacer() | plot_list[["REGION"]]) +
     plot_layout(nrow = 1, widths = c(4, 1.25, 3, 1.25, 2))) /
    plot_spacer() /
    ((plot_list[["RELIGION"]] | plot_spacer() | plot_list[["CASTE"]]) + 
       plot_layout(nrow = 1, widths = c(6, 0.5, 5)))
) + 
  plot_layout(ncol = 1, heights = c(5, 0.3, 5)) +
  plot_annotation(caption = caption_text,
                  # title = title_text,
                  # subtitle = subtitle_text,
                  theme = theme_soa())

dev.off()
graphics.off()

rm(temp, national_average_saturation, caption_text, plot_list)
# ~~~~~~~~~~~~~~~~

# Figure 007 | Share of people with Aadhaar who report an error on their Aadhaar card ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** caption ----
notes <- c("Note: Respondent and their household members present during the interview were asked to respond on behalf of themselves and all absent household members over 15 years of age;", 
           "UIDAI recommends that children update their data", 
           "at age 5 and 15, since biometric information tends to change until that age. After age 15,",
           "however, updates are required only in case of error or change in circumstance. Therefore, we focus on error rates",
           "for individuals older than 15. Numbers in the text and the chart may not match due to rounding.") %>% 
  paste(collapse = " ") %>% 
  str_wrap(95)

caption_text <- paste0(
  source_cmie_survey,
  " (N = ",
  format(479099, big.mark = ",", scientific = FALSE),
  ")\n",
  notes
  )

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], "Figure 07. Share of people with Aadhaar who report an error.xlsx")) %>%
  mutate(percent_no = Percent*100,
         psu_state = case_when(
           state == "Jammu and Kashmir" ~ "Jammu & Kashmir", 
           state == "Delhi" ~ "NCT of Delhi",
           TRUE ~ state
         )) %>%
  select(psu_state, percent_no) %>%
  right_join(india_state_boundries, by = "psu_state") %>%
  mutate(percent_no = case_when(
    percent_no <= 5 ~ "0% to 5%",
    percent_no <= 10 ~ "5% to 10%",
    percent_no > 10 ~ "> 10%"
  ),
  percent_no = fct_relevel(percent_no, 
                           "0% to 5%", 
                           "5% to 10%", 
                           "> 10%"),
  percent_no = fct_rev(percent_no))

# ** plotting the figure ----
png(file.path(visual_path, "07. Share of people with Aadhaar who report an error.png"),
    width = 79.6, 
    height = 89,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  # Filling India map data
  geom_sf(aes(fill = percent_no, 
              geometry = geometry),
          colour = "white",
          size = 0.2) +
  scale_fill_soa(palette = "orange",
                 na.value = "#d6d6d6",
                 reverse = TRUE,
                 labels = c("> 10%",
                            "6% to 10%",
                            "0% to 5%",
                            "Not surveyed")) +
  scale_x_continuous(expand = expansion(0.15)) +
  labs(fill = "Legend",
       caption = caption_text) +
  theme_soa() +
  theme(
    axis.text = element_blank(),
    
    # LEGEND
    legend.position = c(0.12, 0.20)
  )

dev.off()
graphics.off()

rm(caption_text, temp)
# ~~~~~~~~~~~~~~~~

# Figure 008 | Error rates, by type of data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created externally
# ~~~~~~~~~

# Figure 009 | Share of individuals who describe Aadhaar related process as difficult ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ** caption ----
caption_text <- paste0(
  "\n\n",
  source_core_survey,
  "\nNote: The relevant number of respondents is noted in parentheses; not all people who have Aadhaar have engaged in all processes. Numbers in the text and the chart may not match due\nto rounding."
)

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], "Figure 09. Share of people who found Aadhaar-related processes difficult.xlsx")) %>%
  mutate(
    category = str_replace(category, "Link to DBT and", "Link to DBT and\n"),
    category = str_replace(category, "Enroll", "Enrol"),
    category = str_replace(category, "Social Pensions", "social pensions"),
    category = fct_inorder(category)
    )

# ** plotting the figure ----
png(file.path(visual_path, "09. Share of people who describe specific Aadhaar-related processes as difficult.png"),
    width = 140, 
    height = 46.5,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  geom_col(aes(x = category, 
               y = perc_overall),
           colour = "white",
           fill = soa_light[["main"]],
           width = 0.6) +
  geom_text(aes(
    x = category, 
    y = perc_overall + 0.02, 
    label = dec_to_perc(perc_overall, 0)
  ),
  show.legend = FALSE) +
  scale_fill_soa() +
  labs(caption = caption_text,
       fill = "Legend") +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  theme_soa() +
  theme(
    axis.text.y = element_blank(), 
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # LEGEND
    legend.position = c(0.2, 1),
    legend.direction = "horizontal"
  ) +
  guides(fill = guide_legend(reverse = TRUE))

dev.off()
graphics.off()

rm(caption_text, temp)
# ~~~~~~~~~

# Figure 010 | Use of Aadhaar by service, grouped by wether Aadhaar is legally mandatory or voluntary ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** caption ----
caption_text <- paste0(
  "\n\n\n",
  source_core_survey,
  "\n",
  str_wrap(
    paste(
      "Note: The relevant number of respondents is noted in parentheses; not all people use all services. ",
      "Requirements for providing Aadhaar for welfare schemes are not uniformly implemented across states and union territories.",
      "We define mandatory according to our understanding of the 2018 Supreme Court ruling on Aadhaar: Aadhaar can be made mandatory", 
      "for targeted government welfare benefits and can no longer be made mandatory for private-sector service delivery. We understand that state-wise", 
      "implementation varies: some states have not implemented mandates for linking with Aadhaar for certain services, and/or they have made efforts", 
      "to reduce Aadhaar-related exclusion from welfare services. We discuss de facto mandates for non-welfare benefits in a later chapter. For simplicity,", 
      "our definition hinges on whether providing Aadhaar can be made mandatory in principle. Numbers in the text and the chart may not match due to rounding."
    ),
    200
  ),
  "\n"
)

temp <- import(file.path(v_loc["figures_data"], "Figure 10. Share of service users who have provided Aadhaar.xlsx")) %>% 
  mutate(mandatory = ifelse(mandatory %in% c("Other voluntary use-cases",
                                             "Voluntary banking services"),
                            "Aadhaar is voluntary", mandatory))

mandatory <- unique(temp$mandatory) %>% rev()

list_of_plots <- list()

# ** making images ----
for (x in mandatory) {
  list_of_plots[[x]] <- temp %>%
    filter(mandatory == x) %>%
    mutate(use_case = simple_cap(use_case),
           use_case = case_when(
             use_case == "Lpg Subsidy" ~ "LPG subsidy",
             use_case == "Sim Card" ~ "SIM card",
             use_case == "Social Pensions" ~ "Social pensions",
             use_case == "Kerosene Subsidy" ~ "Kerosene subsidy",
             use_case == "Ration Card" ~ "Ration card",
             use_case == "Worker Pensions" ~ "Worker pensions",
             use_case == "Savings Account" ~ "Savings account",
             use_case == "Mutual Funds" ~ "Mutual funds",
             use_case == "Age Proof" ~ "Age proof",
             use_case == "Job Application" ~ "Job application",
             use_case == "Another ID" ~ "Applying for another ID",
             use_case == "Land Registration" ~ "Land registration",
             use_case == "Aayushmaan Bharat" ~ "Ayushman\nBharat",
             use_case == "MNREGA" ~ "MGNREGS",             
             TRUE ~ use_case
           ),
           use_case = str_wrap(use_case, 12),
           use_case = fct_reorder(use_case, 
                                  value, 
                                  .desc = TRUE),
           mandatory = toupper(mandatory),
           label_position = value + 0.2) %>%
    ggplot() +
    geom_col(aes(x = use_case, 
                 y = value
    ),
    width = ifelse(x == "Mandatory", 0.5, 0.5),
    fill = soa_light[["main"]],
    show.legend = FALSE) +
    geom_text(aes(x = use_case,
                  y = label_position,
                  label = dec_to_perc(value, 0))) +
    scale_y_continuous(expand = expansion(add = c(0, 0.1)),
                       limits = c(0, 1.4),
                       oob = function(x, limits) x) +
    scale_fill_soa() +
    labs(subtitle = " ") +
    theme_soa() +
    # coord_flip() +
    theme(
      axis.text.y = element_blank(), 
      axis.line.x = element_line(
        colour = "#d6d6d6",
        size = 0.2
      ),
      
      # STRIP TEXT
      plot.subtitle = element_text(
        colour = "#707274",
        hjust = 0.01,
        vjust = 1,
        face = "bold",
        size = 13
      )
    )
}

# ** plotting the figure ----
png(file.path(visual_path, "10. Share of service users who have provided Aadhaar.png"),
    width = 154, 
    height = 80,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

(list_of_plots[[1]] + 
    plot_spacer() + 
    plot_layout(widths = c(7, 2.5))) / 
  # plot_spacer()  /
  ((list_of_plots[[2]] + 
      plot_spacer() +
      list_of_plots[[3]]) +
     plot_layout(nrow = 1, widths = c(2, 0.5, 7))) +
  plot_layout(heights = c(5, 5), ncol = 1) +
  plot_annotation(caption = caption_text,
                  # title = title_text,
                  # subtitle = subtitle_text,
                  theme = theme_soa())


dev.off()
graphics.off()
# ~~~~~~~~~


# Figure 11 | Use of Aadhaar by authentication feature ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
temp <- import(file.path(v_loc["figures_data"], "Figure 11. Use of different forms and features of Aadhaar.xlsx"))

# ** caption ----
caption_text <- paste0(
  "\n",
  source_core_survey,
  " (N = ",
  format(max(temp$overall), big.mark = ","),
  ")",
  "\n",
  "Note: Numbers in the text and the chart may not match due to rounding."
)

# ** plotting the figure ----
png(file.path(visual_path, "11. Use of different forms and features of Aadhaar.png"),
    width = 146.5, 
    height = 52.5,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  mutate(category = simple_cap(category),
         category = str_remove(category, "Aadhaar "),
         category = case_when(
           category == "Num" ~ "Aadhaar number\n(without card)",
           category == "Otp" ~ "OTP",
           category == "Virtual Num" ~ "Virtual\nAadhaar number",
           category == "Maadhaar" ~ "mAadhaar",
           category == "Qr Code" ~ "QR Code",
           TRUE ~ category
         ),
         category = fct_reorder(category, percent_yes, .desc = TRUE),
         type = ifelse(category == "OTP", "Traditional features", type)) %>%
  ggplot() +
  geom_col(aes(x = category, 
               y = percent_yes,
               fill = type),
           width = 0.6) +
  geom_text(aes(x = category,
                y = percent_yes + 0.04,
                label = dec_to_perc(percent_yes, 0))) +
  scale_fill_soa(reverse = TRUE, palette = "blue_purple") +
  labs(fill = "Legend",
       caption = caption_text) +
  scale_y_continuous(expand = expansion(add = c(0, 0.05))) +
  theme_soa() +
  theme(
    axis.text.y = element_blank(), 
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # LEGEND
    legend.position = c(0.85, 0.85)
  )

dev.off()
graphics.off()
# ~~~~~~~~~


# Figure 12 | Share of people who have been denied access to essential government services in the last three months ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created externaly
# ~~~~~~~~~


# Figure 13 | Benefits and Challenges of Aadhaar that are most important to peoplw ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** caption ----
caption_text <- paste0(
  source_core_survey,
  " (N =",
  format(nrow(random_df), big.mark = ","),
  ")\n",
  str_wrap(
    paste0(
      "Note: All respondents were asked to name up to 3 benefits and challenges that were important to them.",
      "There is no corollary challenge to the benefit of 'It gives me a personal identity/proof of address'. Numbers in the text and the chart may not match due to rounding."
    ),
    210
  ),
  "\n"
)

# ** importing dataset and changing the factor levels ----
temp <- import(file.path(v_loc["figures_data"], 
                         "Figure 13. Benefits and challenges of Aadhaar.xlsx")) %>%
  mutate(challenges = ifelse(is.na(challenges), "", challenges)) %>%
  mutate_at(vars(starts_with("perc_")), perc_to_dec) %>%
  mutate_at(vars(benefits, challenges), ~ str_wrap(., width = 40)) %>%
  mutate(benefits = ifelse(benefits == "Everyone accepts it as identity proof",
                           "Everyone accepts it as identity\nproof", benefits)) %>% 
  mutate_at(vars(benefits, challenges), ~ fct_reorder(., perc_benefit))
# top_n(5, perc_benefit)

# ** Creating challenges ----
challenges <- temp %>%
  mutate(perc_challenge = ifelse(challenges == "", NA, perc_challenge)) %>%
  ggplot() +
  geom_col(aes(
    x = challenges,
    y = perc_challenge,
  ),
  fill = soa_light[["orange"]],
  width = 0.6) +
  geom_text(aes(
    x = challenges,
    y = perc_challenge + 0.1,
    label = ifelse(challenges == "", NA, dec_to_perc(perc_challenge, 0))
  )) +
  coord_flip() +
  scale_y_continuous(expand = expansion(add = c(0, 0.3)),
                     limits = c(0, 0.8),
                     oob = function(x, limits) x) +
  theme_soa() +
  theme(
    axis.text.x = element_blank(),
    axis.line.y = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    axis.text.y = element_text(
      size = 4.5
    )
  )

# ** Creating benefits ----
benefits <- temp %>%
  ggplot() +
  geom_col(aes(
    x = benefits,
    y = perc_benefit,
  ),
  fill = soa_light[["main"]],
  width = 0.6) +
  geom_text(aes(
    x = benefits,
    y = ifelse(benefits != "None",
               perc_benefit + 0.1, 
               perc_benefit + 0.1),
    label = dec_to_perc(perc_benefit, 0)
  )) +
  coord_flip() +
  scale_y_continuous(expand = expansion(add = c(0, 0.3)),
                     limits = c(0, 0.8),
                     oob = function(x, limits) x) +
  theme_soa() +
  theme(
    axis.text.x = element_blank(), 
    axis.line.y = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    axis.text.y = element_text(
      size = 4.5
    )
  )

# ** Creating overlap ----
overlap <- temp %>%
  ggplot() +
  geom_point(aes(x = benefits,
                 y = 1,
                 size = perc_overlap/2),
             show.legend = FALSE,
             colour = soa_light[["purple"]],
             alpha = 0.8) +
  geom_text(aes(x = benefits,
                y = 1,
                label = ifelse(challenges == "", NA, dec_to_perc(perc_overlap, 0))),
            colour = "white",
            fontface = "bold",
            size = 3.5 * 0.3514598) +
  coord_flip(clip = "off") +
  scale_size_continuous(range = c(4, 10)) +
  scale_y_continuous(expand = expansion(add = c(1, 1))) +
  theme_soa() +
  theme(
    axis.text = element_blank(),
    axis.text.y = element_blank()
  )

# ** plotting the figure ----
png(file.path(visual_path, "13. Benefits and challenges of Aadhaar that are perceived as most important.png"),
    width = 154,
    height = 64,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

benefits +
  challenges +
  overlap +
  plot_layout(nrow = 1,
              widths = c(5, 5, 1)) +
  plot_annotation(theme = theme_soa(),
                  caption = caption_text)

dev.off()
graphics.off()

rm(caption_text, challenges, benefits, temp)
# ~~~~~~~~~~~~~~~~


# Figure 14 | Share of people who used Aadhaar to gain first-time access to a service ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], 
                         "Figure 14. Share of people who used Aadhaar to gain first time access.xlsx")) %>%
  mutate(
    service = ifelse(service == "Pensions", "Social pensions", service),
    service = fct_reorder(service, value)
  )

# ** caption ----
caption_text <- paste0(
  "\n",
  source_core_survey,
  " (N = ",
  format(max(temp$no_of_users), big.mark = ",", scientific = FALSE),
  ")",
  "\nNote: Numbers in the text and the chart may not match due to rounding."
)

# ** plotting the figure ----
png(file.path(visual_path, "14. Share of people who used Aadhaar to gain first time access to service.png"),
    width = 79.6, 
    height = 53,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  geom_col(
    aes(x = service, 
        y = value),
    fill = soa_light[["main"]],
    width = 0.5
  ) +
  geom_text(aes(
    x = service,
    y = value + 0.025,
    label = dec_to_perc(value, 0)
  )
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(fill = "Legend", 
       caption = caption_text) +
  coord_flip() +
  theme_soa() +
  theme(
    axis.text.x = element_blank(), 
    axis.line.y = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    plot.title.position = "plot"
  )

dev.off()
graphics.off()

rm(caption_text, temp)
# ~~~~~~~~~~~~~~~~


# Figure 15 | Satisfaction with Aadhaar ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Created externaly
# ~~~~~~~~~


# Figure 16 | Net satisfaction with Aadhaar ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** caption ----
caption_text <- paste0(
  source_cmie_survey,
  " (N = ",
  format(147868, big.mark = ",", scientific = FALSE),
  ")\n",
  str_wrap(
    paste(
      "Note: Net satisfaction is defined as % of respondents somewhat or very satisfied with Aadhaar minus % of respondents very or somewhat unsatisfied.",
      "Meghalaya has a negative net satisfaction, but rounds to zero (0). Numbers in the text and the chart may not match due to rounding."
      # collapse = " "
    ),
    95
  )
)

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], "Figure 16. Net satisfaction with Aadhaar.csv")) %>%
  mutate(psu_state = case_when(
    state == "Jammu and Kashmir" ~ "Jammu & Kashmir", 
    state == "Delhi" ~ "NCT of Delhi",
    TRUE ~ state
  ),
  satisfaction = case_when(
    satisfaction_aadhaar %in% c("Somewhat satisfied", "Very satisfied") ~ 1,
    satisfaction_aadhaar %in% c("Somewhat unsatisfied", "Very unsatisfied") ~ 2,
    TRUE ~ 0
  )) %>%
  filter(satisfaction > 0) %>%
  group_by(psu_state, satisfaction) %>%
  summarise(percent = sum(Percent, na.rm = TRUE)) %>%
  arrange(psu_state, satisfaction) %>%
  mutate(temp = percent - lead(percent)) %>%
  ungroup() %>%
  filter(!is.na(temp)) %>%
  mutate(temp = case_when(
    temp <= 0.75 ~ "0% to 75%",
    temp <= 0.9 ~ "75% to 90%",
    temp > 0.9 ~ "> 90%"
  ),
  trial = as_factor(temp)) %>%
  mutate(trial = fct_relevel(trial, 
                             "0% to 75%",
                             "75% to 90%",
                             "> 90%"),
         trial = fct_rev(trial)) %>%
  right_join(india_state_boundries, by = "psu_state")

# ** plotting the figure ----
png(file.path(visual_path, "16. Net satisfaction with Aadhaar.png"),
    width = 79.6, 
    height = 82,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  geom_sf(aes(fill = trial, 
              geometry = geometry),
          colour = "white",
          size = 0.2) +
  scale_fill_soa(na.value = "#d6d6d6",
                 palette = "main",
                 reverse = TRUE,
                 labels = c("> 90%",
                            "76 to 90%",
                            "0% to 75%",
                            "Not surveyed"
                 )
  ) +
  scale_x_continuous(expand = expansion(0.15)) +
  labs(fill = "Legend",
       caption = caption_text) +
  theme_soa() +
  theme(
    axis.text = element_blank(),
    
    # LEGEND
    legend.position = c(0.09, 0.20)
  )

dev.off()
graphics.off()
rm(caption_text, temp)
# ~~~~~~~~~


# Figure 17 | Share of people who answered correctly when asked whether providing Aadhaar is mandatory for specific services ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** caption ----
caption_text <- paste0(
  "\n",
  source_core_survey,
  " (N =",
  format(nrow(random_df), big.mark = ",", scientific = FALSE),
  ")\n",
  str_wrap(
    paste(
      "Note: We define mandatory according to our understanding of the 2018 Supreme Court ruling on Aadhaar: Aadhaar can be made mandatory", 
      "for targeted government welfare benefits and can no longer be made mandatory for private-sector service delivery. We understand that state-wise", 
      "implementation varies: some states have not implemented mandates for linking with Aadhaar for certain services, and/or they have made efforts", 
      "to reduce Aadhaar-related exclusion from welfare services. For simplicity,", 
      "this definition hinges on whether providing Aadhaar can be made mandatory in principle. Numbers in the text and the chart may not match due to rounding."
    ),
    100
  )
)

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], 
                         "Figure 17. Share of people who believe Aadhaar is mandatory by law.xlsx")) %>%
  mutate(category = case_when(
    category == "Sim Card" ~  "SIM card",
    category == "Pds Card" ~  "Ration card",
    category == "Mnrega Card" ~  "MNREGS job card",
    category == "School Enrolment" ~  "School enrolment",
    category == "Social Pension" ~  "Social pension",
    category == "Bank Account" ~  "Bank account",
    TRUE ~  category
  )) %>% 
  mutate(is_mandatory = case_when(
    category %in% c("Ration card", "MNREGS job card", "Social pension") ~ "Aadhaar can be legally mandatory",
    TRUE ~ "Aadhaar is not legally mandatory"
  ))

# ** plotting the figure ----
png(file.path(visual_path, "17. Share of people who mistakenly believe Aadhaar is mandatory by law.png"),
    width = 79.6, 
    height = 66,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  mutate(category = fct_reorder(category, value)) %>%
  ggplot() +
  geom_col(aes(x = category, 
               y = value,
               fill = is_mandatory),
           width = 0.6) +
  geom_text(aes(x = category, 
                y = value + 0.09,
                label = dec_to_perc(value, 0))) +
  coord_flip() +
  scale_fill_soa(palette = "blue_purple") +
  scale_x_discrete(expand = expansion(add = c(0, 1))) +
  scale_y_continuous(expand = expansion(c(0, 0.65))) +
  labs(caption = caption_text,
       fill = "Legend") +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_soa() +
  theme(
    axis.text.x = element_blank(), 
    axis.line.y = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    plot.title.position = "plot",
    
    # LEGEND
    legend.position = c(0.4, 0.95),
    legend.direction = "horizontal",
    legend.text = element_text(
      size = 4.5
    ),
    legend.title = element_text(
      size = 4.5
    )
  )

dev.off()
graphics.off()

rm(caption_text, temp)
# ~~~~~~~~~~~~~~~~

# Figure 018 | Reasons why people provide Aadhaar to access private-sector services ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], "Figure 18. Reasons why people provide Aadhaar to access private services.xlsx")) %>%
  filter(category != "Others (please specify)") %>% 
  mutate(category = ifelse(category == "The government made Aadhaar\r\nmandatory",
                           "Believe that the government made Aadhaar\r\nmandatory", 
                           category),
    category = str_wrap(category, 25),
       category = fct_reorder(category, value, .desc = TRUE),
       service = ifelse(service == "SIM Card", "SIM card", "Bank account"))

# ** caption ----
caption_text <- paste0(
  source_core_survey,
  " (For bank accounts, N = ",
  format(max(temp[temp$service == "Bank account", "overall"]), 
         big.mark = ",", scientific = FALSE),  
  "; for SIM cards, N = ",
  format(max(temp[temp$service != "Bank account", "overall"]),
         big.mark = ",", scientific = FALSE),  
  ")",
  "\nNote: Numbers in the text and the chart may not match due to rounding."
)

# ** plotting the figure ----
png(file.path(visual_path, "18. Reasons why people provide Aadhaar to access private-sector services.png"),
    width = 163.2, 
    height = 65,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  geom_col(aes(x = category, 
               y = value, 
               fill = service),
           width = 0.5,
           position = position_dodge(width = NULL)) +
  geom_text(aes(
    x = category,
    y = value + 0.02,
    group = service,
    label = dec_to_perc(value, 0)
  ),
  position = position_dodge(width = 0.55)) +
  scale_fill_soa(reverse = TRUE,
                 palette = "blue_purple") +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  labs(fill = "Legend",
       caption = caption_text) +
  theme_soa() +
  theme(
    axis.text.y = element_blank(), 
    axis.line.x = element_line(
      colour = "#d6d6d6",
      size = 0.2
    ),
    
    # LEGEND
    legend.position = c(0.85, 0.90)
  )

dev.off()
graphics.off()

rm(caption_text, temp)
# ~~~~~~~~~~~~~~~~

# Figure 019 | Share of children for whom providing Aadhaar was mandatory for school enrollment, after the Supreme Court judgement ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], 
                         "Figure 19. Share of children who provided Aadhaar for school enrolment.xlsx")) %>%
  mutate(percent_yes = value*100,
         percent_yes = case_when(
           percent_yes <= 50 ~ "0% to 50%",
           percent_yes <= 75 ~ "50% to 75%",
           percent_yes > 75 ~ "> 75%"
         ),
         percent_yes = fct_relevel(percent_yes,
                                   "0% to 50%",
                                   "50% to 75%",
                                   "> 75%"
         ),
         percent_yes = fct_rev(percent_yes)
  ) %>%
  right_join(india_state_boundries, by = c("zh_state" = "psu_state"))

# ** caption ----
caption_text <- paste0(
  source_core_survey,
  " (N = ",
  format(sum(temp$no_of_children, na.rm = TRUE),
         big.mark = ",", scientific = FALSE),
  " children in ",
  format(sum(temp$no_of_households, na.rm = TRUE),
         big.mark = ",", scientific = FALSE),
  " households)\n",
  str_wrap(
    paste(
      "Note: Respondents answered on behalf of all school-aged children", 
      "in their households. Numbers in the text and the chart may not match due to rounding."
    ),
    80
  )
)

# ** plotting the figure ----
png(file.path(visual_path, "19. Share of children who had to provide Aadhaar for school enrolment.png"),
    width = 68, 
    height = 70,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  geom_sf(aes(fill = percent_yes, 
              geometry = geometry),
          colour = "white",
          size = 0.2) +
  scale_fill_soa(na.value = "#d6d6d6",
                 palette = "orange",
                 reverse = TRUE,
                 drop = FALSE,
                 labels = c("> 75%",
                            "51 to 75%",
                            "0 to 50%",
                            "Not surveyed"
                 )
  ) +
  scale_x_continuous(expand = expansion(0.15)) +
  labs(fill = "Legend",
       caption = caption_text) +
  theme_soa() +
  theme(
    axis.text = element_blank(),
    
    # LEGEND
    legend.position = c(0.12, 0.25)
  )

dev.off()
graphics.off()
# ~~~~~~~~~

# Figure 020 | Share of children who have ever missed a meal under the governments Mid-Day Meal Scheme due to an Aadhaar-related reason ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ** importing data ----
temp <- import(file.path(v_loc["figures_data"], "Figure 20. Share of children who missed out on MDM.xlsx")) %>%
  mutate(percent_yes = value*100,
         percent_yes = case_when(
           percent_yes <= 5 ~ "0 to 5%",
           percent_yes <= 10 ~ "5 to 10%",
           percent_yes > 10 ~ "> 10%"
         ),
         percent_yes = fct_relevel(percent_yes,
                                   "0 to 5%",
                                   "5 to 10%",
                                   "> 10%"
         ),
         percent_yes = fct_rev(percent_yes)
  ) %>%
  right_join(india_state_boundries, by = c("zh_state" = "psu_state"))

# ** caption ----
caption_text <- paste0(
  source_core_survey,
  " (N = ",
  format(max(temp$no_of_children, na.rm = TRUE),
         big.mark = ",", scientific = FALSE),
  " children in ",
  format(max(temp$no_of_households, na.rm = TRUE),
         big.mark = ",", scientific = FALSE),
  " households)\n",
  str_wrap(
    paste(
      "Note: Respondents answered on behalf of all school-aged children", 
      "in their households. Numbers in the text and the chart may not match due to rounding."
    ),
    80
  )
)

# ** plotting the figure ----
png(file.path(visual_path, "20. Share of children who missed out on MDM.png"),
    width = 68, 
    height = 70,
    units = "mm",
    type = "windows",
    bg = "white",
    res = 600)

temp %>%
  ggplot() +
  geom_sf(aes(fill = percent_yes, 
              geometry = geometry),
          colour = "white",
          size = 0.2) +
  scale_fill_soa(na.value = "#d6d6d6",
                 palette = "orange",
                 reverse = TRUE,
                 drop = FALSE,
                 labels = c("> 10%",
                            "6% to 10%",
                            "0% to 5%",
                            "Not surveyed"
                 )
  ) +
  scale_x_continuous(expand = expansion(0.15)) +
  labs(fill = "Legend",
       caption = caption_text) +
  theme_soa() +
  theme(
    axis.text = element_blank(),
    
    # LEGEND
    legend.position = c(0.12, 0.25)
  )

dev.off()
graphics.off()
# ~~~~~~~~~


