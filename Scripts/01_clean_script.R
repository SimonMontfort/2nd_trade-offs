R.version
# platform       x86_64-w64-mingw32               
# arch           x86_64                           
# os             mingw32                          
# crt            ucrt                             
# system         x86_64, mingw32                  
# status                                          
# major          4                                
# minor          5.1                              
# year           2025                             
# month          06                               
# day            13                               
# svn rev        88306                            
# language       R                                
# version.string R version 4.5.1 (2025-06-13 ucrt)
# nickname       Great Square Root                

################################################################################
# packages and data loading
################################################################################

rm(list = ls())

library(readxl)
library(writexl)
library(tidyverse)
library(overlapping)
library(ggplot2)

setwd("/Users/simon/Documents/repo/2nd_trade-offs")
# setwd("F:/SWEET/EDGE/WP8 - Survey II/Auswertungen/input_data")
# 
gc <- readRDS("Data/raw_data_list_04082025_prios.rds")
gc <- gc[[1]]

dat <- read_csv("Data/EDGE_2025_September 9, 2025_10.44.csv") %>% as_tibble()
# dat <- dat %>% filter(ResponseId %in% gc$ResponseId)

################################################################################
# check data setup
################################################################################

check <- dat %>% 
  filter(tradeoff_con_treat == "treat") %>% 
  select(imp_to_avg_Swi_1, colnames(dat)[which(grepl("t_bio_emi", colnames(dat)) & grepl("First", colnames(dat)))]) %>% 
  rename_with(~ gsub(".*pos([0-9]+)_.*", "pos\\1", .x)) %>% 
  rename_with(~ gsub(".*neg([0-9]+)_.*", "neg\\1", .x)) %>% 
  mutate(treatment = 0)

check

check <- dat %>% 
  filter(tradeoff_con_treat == "treat") %>% 
  select(imp_to_avg_Swi_2, colnames(dat)[which(grepl("t_emi_land", colnames(dat)) & grepl("First", colnames(dat)))]) %>% 
  rename_with(~ gsub(".*pos([0-9]+)_.*", "pos\\1", .x)) %>% 
  rename_with(~ gsub(".*neg([0-9]+)_.*", "neg\\1", .x)) %>% 
  mutate(treatment = 1)

check

################################################################################
# experimental variables
################################################################################

# ----  prior and posterior beliefs  ---- #

# pior beliefs (before starting the experiment)
dat$prior_bioemi_2nd <- dat$imp_to_avg_Swi_1 
dat$prior_landemi_2nd <- dat$imp_to_avg_Swi_2 

dat$prior_bioemi <- dat$imp_to_you_1 
dat$prior_landemi <- dat$imp_to_you_2 

# posterior beliefs (after being exposed to the treatment)
dat <- dat %>% 
  # as the survrey includes blocks for treatmeent and control these need to be combined
  mutate(post_bioemi = ifelse(dat$tradeoff_con_treat=="treat", learning_bioemi_1, learning_control_1),
         post_landemi = ifelse(dat$tradeoff_con_treat=="treat", learning_emiland_1, learning_control_2))


# ---- treatment group and direction ---- #

# treatment group
table(dat$tradeoff_con_treat, useNA = "always")
dat$treatment_group <- ifelse(dat$tradeoff_con_treat=="treat", "treatment", "control")
dat$treatment_group <- factor(dat$treatment_group, levels = c("control", "treatment"))

# treatment direction
dat <- dat %>%
  mutate(treatment_positive_bioemi = case_when(
    e_tradeoff_biodiv <=0 & treatment_group == "control" ~ "positive_control",
    e_tradeoff_biodiv >0 & treatment_group == "control" ~ "negative_control",
    e_tradeoff_biodiv <=0 & treatment_group == "treatment" ~ "positive_treatment",
    e_tradeoff_biodiv >0 & treatment_group == "treatment" ~ "negative_treatment")
  ) %>% 
  mutate(treatment_positive_landemi = case_when(
    e_tradeoff_emiland <=0 & treatment_group == "control" ~ "positive_control",
    e_tradeoff_emiland >0 & treatment_group == "control" ~ "negative_control",
    e_tradeoff_emiland <=0 & treatment_group == "treatment" ~ "positive_treatment",
    e_tradeoff_emiland >0 & treatment_group == "treatment" ~ "negative_treatment")
  )

dat$treatment_positive_bioemi <- factor(dat$treatment_positive_bioemi, 
                                        levels = c("positive_control", "positive_treatment",
                                                   "negative_control", "negative_treatment"))
dat$treatment_positive_landemi <- factor(dat$treatment_positive_landemi, 
                                         levels = c("positive_control", "positive_treatment",
                                                    "negative_control", "negative_treatment"))

# combined treatment direction (land use vs. emissions + biodiversity vs emissions)
dat <- dat %>%
  mutate(
    treatment_positive_bioemi_and_landemi = case_when(
      treatment_positive_bioemi == "positive_control" & treatment_positive_landemi == "positive_control" ~ "positive_control",
      treatment_positive_bioemi == "negative_control" & treatment_positive_landemi == "negative_control" ~ "negative_control",
      treatment_positive_bioemi == "negative_control" & treatment_positive_landemi == "positive_control" ~ "mixed_control",
      treatment_positive_bioemi == "positive_control" & treatment_positive_landemi == "negative_control" ~ "mixed_control",
      
      treatment_positive_bioemi == "positive_treatment" & treatment_positive_landemi == "positive_treatment" ~ "positive_treatment",
      treatment_positive_bioemi == "negative_treatment" & treatment_positive_landemi == "negative_treatment" ~ "negative_treatment",
      treatment_positive_bioemi == "negative_treatment" & treatment_positive_landemi == "positive_treatment" ~ "mixed_treatment",
      treatment_positive_bioemi == "positive_treatment" & treatment_positive_landemi == "negative_treatment" ~ "mixed_treatment",

    ),
    treatment_positive_bioemi_and_landemi = factor(
      treatment_positive_bioemi_and_landemi,
      levels = c("positive_control", "mixed_control", "negative_control", "positive_treatment", "mixed_treatment", "negative_treatment")
    )
  )
table(dat$treatment_positive_bioemi_and_landemi)

# treatment intensity direction (land use vs. emissions + biodiversity vs emissions)
dat <- dat %>% 
  mutate(treatment_direction_intensity_bioemi = e_tradeoff_biodiv*(-1),
         treatment_direction_intensity_landemi = e_tradeoff_emiland*(-1))


# ----      belief confidence       ---- #
# helper function to reverse confidence scores
reverse_confidence <- function(x) {
  recode(x, "Not confident at all" = 1, "Not confident" = 2, "Neither nor" = 3, "Confident" = 4, "Very confident" = 5)
}

# reverse confidence scores as they are wrongly ordered
dat <- dat %>%
  mutate(
    confidence_biodiv_num = as.numeric(reverse_confidence(confidence_biodiv)),
    confidence_emiland_num = as.numeric(reverse_confidence(confidence_emiland)),
    confidence = confidence_biodiv_num + confidence_emiland_num
  )


# # ----        learning rate      ---- #
# 
# dat <- dat %>%
#   # compute the gap between prior belief and treatment
#   mutate(
#     gap_prior_treatement_bioemi = 0 - prior_bioemi_2nd,
#     gap_prior_treatement_landemi = 0 - prior_landemi_2nd,
#     # compute learning rates
#     learning_rate_bioemi = (post_bioemi - prior_bioemi) / gap_prior_treatement_bioemi,
#     learning_rate_landemi = (post_landemi - prior_landemi) / gap_prior_treatement_landemi,
#     # handle division by zero (when prior = treatment)
#     learning_rate_bioemi = ifelse(gap_prior_treatement_bioemi == 0, NA, learning_rate_bioemi),
#     learning_rate_landemi = ifelse(gap_prior_treatement_landemi == 0, NA, learning_rate_landemi)
#   ) %>% 
#   mutate(gap_prior_post_bioemi = post_bioemi - prior_bioemi, 
#          gap_prior_post_landemi = post_landemi - post_landemi)


# ----   dependent variables    ----#

dat$acceptance_alpinePV <- dat$Support_techs_1
dat$acceptance_wind <- dat$Support_techs_2
dat$acceptance_newnucs <- dat$Support_techs_3
dat$acceptance_prolongnucs <- dat$Support_techs_4

################################################################################
# (socio-demographic) covariates
################################################################################

# science trust
dat$trust_in_sci <- as.numeric(dat$H5_Trust_1) # higher dat = more trust (0-10)
# left-right political spectrum
dat$left_right <- as.numeric(dat$H2_1) # higher dat = more right

# gender binary
dat <- dat %>%
  mutate(
    gender_binary = ifelse(!Z3 %in% c("female", "male"), NA, Z3),
    gender_binary = factor(gender_binary, levels = c("female", "male"))
  )

# Check results
table(dat$gender_binary, exclude = NULL)

# education
dat <- dat %>%
  mutate(education_group = case_when(
    Z9 %in% c("No education completed", "Compulsory school") ~ "Primary education",
    Z9 %in% c("Professional apprenticeship or vocational school",
              "School leading to baccalaureate, Diploma for teaching in primary school or preprimary school, vocational baccalaureate") ~ "Secondary education",
    Z9 %in% c("Higher vocational training",
              "Higher technical school (3 years full-time or 4 years part-time)") ~ "Postsecondary vocational",
    Z9 %in% c("University, ETH, Universities of Applied Sciences") ~ "Tertiary education",
    Z9 %in% c("No answer") ~ NA_character_,
    is.na(Z9) ~ NA_character_
  ),
  education_group = factor(education_group, levels = c("Primary education", "Secondary education", "Postsecondary vocational", "Tertiary education")),
  education_numeric = as.numeric(education_group))

# Urban-rural
dat <- dat %>%
  mutate(urban_rural = Z13,
         urban_rural = factor(urban_rural, 
                              levels = c("City with more than 50,000 inhabitants", "City with less than 50,000 inhabitants", 
                                         "Agglomeration or suburb of a city", "Village, farm or house in the countryside")),
         urban_rural_numeric = as.numeric(urban_rural),
         urban_rural_binary = ifelse(urban_rural %in% c("City with more than 50,000 inhabitants", "City with less than 50,000 inhabitants"), "urban", "rural"))

# Income: higher value = higher income
dat <- dat %>%
  mutate(income = as.numeric(Z15))

# age
dat <- dat %>%
  mutate(brithyear = Z2, age =  Z2) %>% 
  mutate(
    across(
      .cols = c(age, brithyear),  
      .fns = ~ case_when(
        . == "-99" ~ NA_real_,
        TRUE ~ as.numeric(.)
      )
    )
  )

# income
dat <- dat %>%
  mutate(coping_on_income = ifelse(Z16 == "Yes", "Yes", "No"))

# Quick checks
unique(dat$edu_rec)
unique(dat$urbru_rec)
unique(dat$income)


################################################################################
# issue salience
################################################################################

dat <- dat %>% 
  mutate(climate_salience = ifelse(is.na(A1_climchange), "Yes", "No"))

################################################################################
# speeding & attention checks
################################################################################

# speeding
duration <- as.numeric(dat$`Duration (in seconds)`)
med_below40 <- summary(duration)[3]*.4
dat$speeder <- dat$`Duration (in seconds)` < med_below40

# attention ceck
table(is.na(dat$att_check_1_bioemi), is.na(dat$att_check_2_bioemi))
# TODO: clarify what the difference between the items att_check_1_bioemi and att_check_2_bioemi is
dat$att_check_1_bioemi

dat <- dat %>% 
  mutate(att_check_bioemi = case_when(is.na(att_check_1_bioemi) & is.na(att_check_2_bioemi) ~ NA,
                                     !is.na(att_check_1_bioemi) & is.na(att_check_2_bioemi) ~ att_check_1_bioemi,
                                     is.na(att_check_1_bioemi) & !is.na(att_check_2_bioemi) ~ att_check_2_bioemi)) %>% 
  mutate(attention_bioemy_yes = 
           (att_check_bioemi == "More people found CO2-emissions  more important than I tought." 
           & e_tradeoff_biodiv <= 0) | is.na(att_check_1_bioemi)) %>% 
  mutate(att_check_landemi = case_when(is.na(att_check_1_emiland) & is.na(att_check_2_emiland) ~ NA,
                                      !is.na(att_check_1_emiland) & is.na(att_check_2_emiland) ~ att_check_1_emiland,
                                      is.na(att_check_1_emiland) & !is.na(att_check_2_emiland) ~ att_check_2_emiland)) %>% 
  mutate(attention_landemi_yes = 
           (att_check_landemi == "More people found CO2-emissions  more important than I tought." 
           & e_tradeoff_emiland <= 1) | is.na(att_check_landemi)) %>% 
  mutate(attention_check_yes = attention_bioemy_yes & attention_landemi_yes)

table(dat$attention_check_yes, dat$treatment_positive_bioemi_and_landemi, useNA = "always")
table(dat$attention_landemi_yes, useNA = "always")

table(dat$attention_bioemy_yes, dat$attention_landemi_yes, useNA = "always")

identify_speeders <- function(dat, imp_to_avg_Swi_1, time_names = "t_bio_emi", variable_name = "speeder_treatment") {
  
  dat %>% 
    select(ResponseId, {{imp_to_avg_Swi_1}}, tradeoff_con_treat, 
           colnames(dat)[which(grepl(time_names, colnames(dat)) & grepl("Page", colnames(dat)))]) %>% 
    rename_with(~ gsub(".*pos([0-9]+)_.*", "pos\\1", .x)) %>% 
    rename_with(~ gsub(".*neg([0-9]+)_.*", "neg\\1", .x)) %>% 
    mutate(time_treatment = coalesce(pos4, pos3, pos2, pos1, pos0, neg1, neg2, neg3, neg4)) %>% 
    mutate(
      !!variable_name := ifelse(tradeoff_con_treat == "treat", 
                                time_treatment < median(time_treatment, na.rm = TRUE) * 0.4, 
                                FALSE)
    ) %>% 
    select(all_of(variable_name)) %>% pull
}

dat$speeder_treatment_bioemi <- identify_speeders(dat, imp_to_avg_Swi_1, time_names = "t_bio_emi", variable_name = "speeder_treatment_bioemi")
dat$speeder_treatment_landemi <- identify_speeders(dat, imp_to_avg_Swi_2, time_names = "t_emi_land", variable_name = "speeder_treatment_landemi")


# dat$e_tradeoff_biodiv <- as.numeric(dat$e_tradeoff_biodiv)
# dat$e_tradeoff_emiland <- as.numeric(dat$e_tradeoff_emiland) 
# 
# dat$attention_bioemi_yes <- dat$att_check_1_bioemi <=0 & dat$e_tradeoff_biodiv <= 0
# dat$attention_bioemi_yes <- ifelse(is.na(dat$attention_bioemi_yes), F, dat$attention_bioemi_yes)
# dat$attention_landemi_yes <- !is.na(dat$att_check_1_emiland) & dat$att_check_1_emiland == 1 & dat$e_tradeoff_emiland <= 0
# dat$attention_landemi_yes <- ifelse(is.na(dat$attention_landemi_yes), F, dat$attention_landemi_yes)
# 
# table(dat$attention_bioemi_yes, dat$attention_landemi_yes)

# med_below40_prior_benefit <- summary(as.numeric(dat$treat))[3]*.4
# vote <- vote[vote$`QID566_Page Submit` < med_below40_prior_benefit,]

################################################################################
# export clean data
################################################################################

# vector of variables
vars_of_interest <- c(
  "prior_bioemi", "prior_landemi", "prior_bioemi_2nd", "prior_landemi_2nd", 
  "post_bioemi", "post_landemi",
  "treatment_group", 
  "treatment_positive_bioemi", "treatment_positive_landemi",
  "treatment_direction_intensity_bioemi", "treatment_direction_intensity_landemi",
  "treatment_positive_bioemi_and_landemi",
  "confidence", "coping_on_income",
  # "gap_prior_treatement_bioemi", "gap_prior_treatement_landemi",
  # "gap_prior_post_bioemi", "gap_prior_post_landemi",
  # "learning_rate_bioemi", "learning_rate_landemi",
  
  "acceptance_alpinePV", "acceptance_wind", "acceptance_newnucs", "acceptance_prolongnucs",
  "trust_in_sci", "left_right", "gender_binary", "education_group", "education_numeric", "urban_rural", "urban_rural_numeric", "urban_rural_binary", "income", "age",
  "climate_salience",
  "speeder", "speeder_treatment_bioemi", "speeder_treatment_landemi", 
  "attention_bioemy_yes", "attention_landemi_yes", "attention_check_yes"
)

saveRDS(vars_of_interest, file = "data/vars_of_interest.RDS")

# NA count for each variable
na_counts <- sapply(dat[ , vars_of_interest], function(x) sum(is.na(x)))
na_counts/nrow(dat)

dat_clean <- dat %>% select(vars_of_interest)
levels(dat$treatment_positive_bioemi_and_landemi)
write.csv(dat_clean, "data/dat_clean.csv")
saveRDS(dat_clean, "data/dat_clean.RDS")
