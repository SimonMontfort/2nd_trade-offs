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
# packages and dat_cleana loading
################################################################################

rm(list = ls())

library(readxl)
library(writexl)
library(tidyverse)
library(overlapping)
library(ggplot2)
library(ggsci)
library(scales)
library(showtext)
library(ggpubr)
library(ggsci)
library(stringr)
library(stringi)
library(stargazer)
library(RColorBrewer)

setwd("/Users/simon/Documents/repo/2nd_trade-offs")

# dat_clean_clean <- read.csv("dat_cleana/dat_clean_clean.csv")
dat_clean <- readRDS("Data/dat_clean.RDS")

vars_of_interest <- readRDS("Data/vars_of_interest.RDS")

sure_treatment <- readxl::read_xlsx("Data/sure_treatment_distribution_data.xlsx")


################################################################################
# plot theme
################################################################################

# Load and register a modern font (e.g., Helvetica Neue)
showtext_auto()  # Automatically use showtext for fonts

# Check available fonts
# remotes::install_github("kjhealy/myriad")
# myriad::import_myriad(font_family = "Myriad Pro", silent = F)
theme_SM <- function(){
  theme_light() +   
    theme(panel.grid = element_blank(),
          panel.border = element_rect(colour = "grey50", fill=NA, linewidth=.5),
          strip.placement = "outside",
          # text = element_text(size = 12, family = "Myriad Pro"),
          axis.text.x = element_text(colour = "grey30", angle = 45, hjust = 1, vjust = 1),
          axis.text.y = element_text(colour = "grey30"),
          axis.ticks.length = unit(.2, "cm"),
          axis.ticks = element_line(colour = "grey50", linewidth=.5),
          strip.background = element_rect(fill = "white"),
          strip.text = element_text(colour = "black"),
          strip.clip = "off",
          legend.text = element_text(size = 7),
          legend.key.size = unit(.4, "cm"),
          legend.position = c(0.9,.05),
          legend.margin = margin(rep(2, 4)),
          legend.title = element_blank(),
          legend.justification = c(1, 0),
          legend.background = element_rect(fill="white", 
                                           size=.3, linetype=" of solid", 
                                           colour ="grey")
    )
}


################################################################################
# plot counts and NAs
################################################################################

dat_clean_desc <- dat_clean %>% 
  select(prior_bioemi, prior_landemi, prior_bioemi_2nd, prior_landemi_2nd, post_bioemi, post_landemi, complementarity_bioemi, complementarity_landemi,
         left_right, trust_in_sci, climate_salience, urban_rural_binary, gender_binary, coping_on_income, confidence, attention_check_yes,
         acceptance_alpinePV, acceptance_wind, acceptance_newnucs, acceptance_prolongnucs, treatment_group, treatment_positive_bioemi_and_landemi)

p_desc <- dat_clean_desc %>% 
  mutate(attention_check_yes = ifelse(attention_check_yes == T, 1, 0)) %>% 
  mutate_all(.funs = function(x){as.character(x)}) %>%
  pivot_longer(., everything(), names_to = "Question", values_to = "Response") %>% 
  mutate(Question = factor(Question, levels = unique(Question))) %>% 
  group_by(Question, Response) %>% 
  count(name = "freq") %>% 
  mutate(Response = stri_trans_totitle(Response)) %>% 
  mutate(
    Response = ifelse(Response == "Positive_control", "Control: Overestimated\nothers' CO₂ preference", Response), 
    Response = ifelse(Response == "Mixed_control", "Control: Mixed", Response),
    Response = ifelse(Response == "Negative_control", "Control: Underestimated\nothers' CO₂ preference", Response),
    Response = ifelse(Response == "Positive_treatment", "Treatment: Overestimated\nothers' CO₂ preference", Response),
    Response = ifelse(Response == "Mixed_treatment", "Treatment: Mixed", Response),
    Response = ifelse(Response == "Negative_treatment", "Treatment: Underestimated\nothers' CO₂ preference", Response),
    # Response = ifelse(Response == "TRUE", 1, 0)
  ) %>% 
  mutate(Response = factor(Response, levels = as.character(c(-10:10, NA,
                                                             "Control: Overestimated\nothers' CO₂ preference",
                                                             "Control: Mixed",
                                                             "Control: Underestimated\nothers' CO₂ preference",
                                                             "Treatment: Overestimated\nothers' CO₂ preference",
                                                             "Treatment: Mixed",
                                                             "Treatment: Underestimated\nothers' CO₂ preference",
                                                             "Treatment", "Control", "Male", "Female",
                                                             "Urban", "Rural", "Yes", "No"
                                                             )))) %>%
  ggplot(aes(x = Response, y = freq)) +
  geom_col(col = "black", fill = "lightblue", size = .5, width = .7) + labs(x = "", y = "") +
  facet_wrap(~Question, scales = "free_x", 
             labeller = labeller(Question = c(
               "trust_in_sci"       = "Trust in science",
               "gender_binary"  = "Gender",
               "left_right"         = "Left–right ideology",
               "education_numeric"  = "Education",
               "urban_rural_binary"= "Urban–rural",
               "coping_on_income"= "Coping on income",
               "confidence"         = "Confidence",
               "climate_salience" = "Salience: Climate Change",

               "attention_check_yes" = "Attention check (not failed)",
               "prior_bioemi"          = "Prior\nbiodiversity vs. emissions",
               "prior_landemi"         = "Prior\nland use vs. emissions",
               "prior_bioemi_2nd"      = "2nd order prior\nbiodiversity vs. emissions",
               "prior_landemi_2nd"     = "2nd order prior\nland use vs. emissions",
               "post_bioemi"           = "Posterior\nbiodiversity vs. emissions",
               "post_landemi"          = "Posterior\nland use vs. emissions",
               "acceptance_alpinePV"   = "Acceptance of\nalpine PV",
               "acceptance_wind"       = "Acceptance of\nwind power",
               "acceptance_newnucs"    = "Acceptance of\nnew nuclear plants",
               "acceptance_prolongnucs"= "Acceptance of\nprolonging nuclear plants",
               
               "treatment_group" = "Treatment",
               "treatment_positive_bioemi_and_landemi" = "Treatment: Directionality"
             ))
             ) +
  theme_SM() + 
  theme(axis.text.x = element_text(colour = "grey30", angle = 90, hjust = 1, vjust = .5))
p_desc
ggsave(p_desc, filename = "Plots/p_desc.pdf", height = 14, width = 10)

################################################################################
# summary statistics table: distirbution (eg. min max mean)
################################################################################


library(modelsummary)

dat_clean_desc_renamed <- dat_clean_desc %>% 
  as.data.frame() %>% 
  mutate(coping_on_income = ifelse(coping_on_income == "Yes", 1, 0),
         climate_salience = ifelse(climate_salience == "Yes", 1, 0),
         urban_rural_binary = ifelse(urban_rural_binary == "rural", 1, 0), # Combine control and treatment into three variables
         `Treatment directionality: Overestimated others' emission reduction preference` = ifelse(treatment_positive_bioemi_and_landemi %in% c("positive_control", "positive_treatment"), 1, 0),
         `Treatment directionality: Mixed` = ifelse(treatment_positive_bioemi_and_landemi %in% c("mixed_control", "mixed_treatment"), 1, 0),
         `Treatment directionality: Underestimated others' emission reduction preference` = ifelse(treatment_positive_bioemi_and_landemi %in% c("negative_control", "negative_treatment"), 1, 0)
  ) %>% 
  select(-treatment_positive_bioemi_and_landemi) %>% 
  mutate_all(as.numeric) %>%
  rename(
    "Trust in science" = trust_in_sci,
    "Gender (male yes)" = gender_binary,
    "Left–right ideology" = left_right,
    "Salience: Climate Change" = climate_salience,
    "Urban–rural" = urban_rural_binary,
    "Coping on income (yes)" = coping_on_income,
    "Confidence" = confidence,
    "Attention check (not failed)" = attention_check_yes,
    
    "Complementarity biodiversity conservation + emission reductions" = complementarity_bioemi, 
    "Complementarity landscape conservation + emission reductions" = complementarity_landemi,
    "Prior biodiversity vs. emissions" = prior_bioemi,
    "Prior land use vs. emissions" = prior_landemi,
    "2nd order prior biodiversity vs. emissions" = prior_bioemi_2nd,
    "2nd order prior land use vs. emissions" = prior_landemi_2nd,
    "Posterior biodiversity vs. emissions" = post_bioemi,
    "Posterior land use vs. emissions" = post_landemi,
    "Acceptance of alpine PV" = acceptance_alpinePV,
    "Acceptance of wind power" = acceptance_wind,
    "Acceptance of new nuclear plants" = acceptance_newnucs,
    "Acceptance of prolonging nuclear plants" = acceptance_prolongnucs,
    "Treatment" = treatment_group
  )


stargazer(dat_clean_desc_renamed %>% as.data.frame(),
          out.header = FALSE,
          no.space = TRUE,
          label = "tab:summary_stats",
          column.sep.width = "3pt",
          title = "Summary statistics",
          out = "Tables/summary_stats.tex")

################################################################################
# correlations
################################################################################

dat_clean_desc_renamed <- dat_clean_desc_renamed %>% as.data.frame() %>% mutate_all(as.numeric)

# Compute correlation matrix
cor_mat <- round(cor(dat_clean_desc_renamed, use = "pairwise.complete.obs"), 2)

# Convert to long format for ggplot
cor_long <- cor_mat %>%
  as.data.frame() %>%
  rownames_to_column(var = "Var1") %>%
  pivot_longer(-Var1, names_to = "Var2", values_to = "Correlation") %>%
  drop_na()


labs_desc <- c(
  "Trust in science",
  "Gender (male yes)",
  "Left–right ideology",
  "Salience: Climate Change",
  "Urban–rural",
  "Coping on income (yes)",
  "Confidence",

  "Attention check (not failed)",
  "Complementarity biodiversity conservation + emission reductions",
  "Complementarity landscape conservation + emission reductions",
  "Prior biodiversity vs. emissions",
  "Prior land use vs. emissions",
  "2nd order prior biodiversity vs. emissions",
  "2nd order prior land use vs. emissions",
  "Posterior biodiversity vs. emissions",
  "Posterior land use vs. emissions",
  
  "Acceptance of alpine PV",
  "Acceptance of wind power",
  "Acceptance of new nuclear plants",
  "Acceptance of prolonging nuclear plants",
  
  "Treatment",
  "Treatment directionality: Overestimated others' emission reduction preference",
  "Treatment directionality: Mixed",
  "Treatment directionality: Underestimated others' emission reduction preference"
)

# Convert Var1 and Var2 to factors in the clustered order
cor_long <- cor_long %>%
  mutate(
    Var1 = factor(Var1, levels = rev(labs_desc)), 
    Var2 = factor(Var2, levels = labs_desc)
  )

# Plot heatmap
p_correlation <- ggplot(cor_long, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile(color = "white") +
  geom_text(aes(label = Correlation), size = 2) +
  scale_fill_gradient2(
    low = "#2166AC",    # nice blue
    mid = "white",
    high = "#B2182B",   # nice red
    midpoint = 0,
    limits = c(-1, 1)
  ) +
  theme_minimal() +
  theme(
    legend.position = c(.9,-.3),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.text.y = element_text(hjust = 1)
  ) +
  labs(title = "", x = "", y = "")
p_correlation
ggsave(p_correlation, file = "Plots/p_correlation.pdf", width = 10, height = 10)

################################################################################
# treatment distribution comparison
################################################################################

sure_treatment <- sure_treatment %>% 
  select(treatment_bioemi_2nd_value = bio_emi_value, treatment_bioemi_2nd_n = count_bio_emi, 
         treatment_landemi_2nd_value = emi_lan_value, treatment_landemi_2nd_n = count_emi_lan)

pre_and_treat_dist <- bind_cols(sure_treatment, 
                                dat_clean %>% 
                                  select(prior_bioemi_2nd) %>% 
                                  group_by(prior_bioemi_2nd) %>% 
                                  count(name = "prior_bioemi_2nd_n") %>% 
                                  ungroup() %>% 
                                  filter(!is.na(prior_bioemi_2nd)),
                                dat_clean %>% 
                                  select(prior_landemi_2nd) %>% 
                                  group_by(prior_landemi_2nd) %>% 
                                  count(name = "prior_landemi_2nd_n") %>% 
                                  ungroup()  %>% 
                                  filter(!is.na(prior_landemi_2nd))
)

dat_clean


fig2a <- pre_and_treat_dist %>% 
  select(slider_value = treatment_bioemi_2nd_value, 
         treatment_bioemi_2nd_n, 
         treatment_landemi_2nd_n, 
         prior_bioemi_2nd_n, 
         prior_landemi_2nd_n) %>% 
  pivot_longer(
    c("treatment_bioemi_2nd_n", 
      "treatment_landemi_2nd_n", 
      "prior_bioemi_2nd_n", 
      "prior_landemi_2nd_n")
  ) %>% 
  mutate(
    treatment_group = ifelse(grepl("treatment", name), 
                             "True population preferences\n(treatment)", 
                             "Expectation about others' preferences\n(prior 2nd order belief)"),
    outcome = ifelse(grepl("bioemi", name), 
                     "Biodiversity conservation\nvs. emission reduction", 
                     "Land use\nvs. emission reduction")) %>% 
  mutate(
    truth = ifelse(outcome == "Land use\nvs. emission reduction", 0, 0),
    est_type = case_when(
      slider_value > truth & outcome == "Land use\nvs. emission reduction" ~ "Prefer\nemission\nreductions",
      slider_value < truth & outcome == "Land use\nvs. emission reduction" ~ "Prefer\nland\npreservation",
      slider_value == truth & outcome == "Land use\nvs. emission reduction" ~ "Both are\nequally valued",
      slider_value > truth & outcome != "Land use\nvs. emission reduction" ~ " Prefer\nemission\nreductions",
      slider_value < truth & outcome != "Land use\nvs. emission reduction" ~ "Prefer\nbiodiversity\nconservation",
      slider_value == truth & outcome != "Land use\nvs. emission reduction" ~ " Both are\nequally valued"
    ),
    est_type = factor(
      est_type,
      levels = c(
        "Prefer\nemission\nreductions",
        "Both are\nequally valued",
        "Prefer\nland\npreservation",
        " Prefer\nemission\nreductions",
        " Both are\nequally valued",
        "Prefer\nbiodiversity\nconservation"
      )
    )
  ) %>% 
  group_by(outcome, treatment_group, est_type) %>% 
  summarise(total = sum(value), .groups = "drop") %>% 
  group_by(outcome, treatment_group) %>% 
  mutate(total = total / sum(total)) %>% 
  ggplot(aes(x = est_type, y = total, fill = treatment_group)) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = .7), width = .6, col = "black", size = .2) +
  geom_text(aes(label = scales::percent(total, accuracy = 1)), 
            position = position_dodge(width = .7), 
            vjust = -0.3, size = 3.5) +  
  facet_wrap(~outcome, scales = "free_x") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  scale_fill_brewer(palette = "Pastel1") +
  labs(x = "", y = "Percentage", fill = "Group", title = "Expecations about others' beliefs and true population preferences") + 
  theme_SM() +
  theme(legend.position = c(.25,.75),
        legend.margin = margin(rep(2, 4)),
        legend.title = element_blank(),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill="white", 
                                         size=.3, linetype="solid", 
                                         colour ="grey"),
        axis.text.x = element_text(angle = 0, hjust = .5))
fig2a

fig2b <- dat_clean %>% 
  select(complementarity_bioemi, complementarity_landemi) %>% 
  mutate_all(.funs = function(x){case_when(x == 0 ~ "Neither nor",
                                           x > 0 ~ "Synergy",
                                           x < 0 ~ "Trade-off")}) %>% 
  pivot_longer(c("complementarity_bioemi", "complementarity_landemi"), names_to = "outcome", values_to = "name") %>% 
  filter(!is.na(name)) %>% 
  mutate(value = 1) %>% 
  group_by(outcome, name) %>% 
  summarise(value = sum(value), .groups = "drop") %>% 
  group_by(outcome) %>%
  mutate(value = value / sum(value)) %>% 
  mutate(
    outcome = ifelse(grepl("bioemi", outcome), 
                     "Biodiversity conservation\nvs. emission reduction", 
                     "Land use\nvs. emission reduction")) %>% 
  mutate(name = factor(name, levels = c("Synergy", "Neither nor", "Trade-off"))) %>% 
  ggplot(aes(x = name, y = value, )) +
  geom_bar(stat = "identity", 
           position = position_dodge(width = .7), width = .4, col = "black", size = .2, fill = "seashell3") +
  geom_text(aes(label = scales::percent(value, accuracy = 1)), 
            position = position_dodge(width = .7), 
            vjust = -0.3, size = 3.5) + 
  facet_wrap(~outcome, scales = "free_x") +
  scale_y_continuous(labels = scales::percent, expand = expansion(mult = c(0, 0.1))) +
  labs(x = "", y = "Percentage", fill = "Group", title = "Trade-offs and synergies in environmental goals") + 
  theme_SM() +
  theme(legend.position = c(.25,.85),
        legend.margin = margin(rep(2, 4)),
        legend.title = element_blank(),
        legend.justification = c(1, 0),
        legend.background = element_rect(fill="white", 
                                         size=.3, linetype="solid", 
                                         colour ="grey"), 
        axis.text.x = element_text(angle = 0, hjust = .5))
fig2b

fig2 <- ggarrange(fig2a, fig2b, labels = "auto", ncol = 1)
ggsave(fig2, file = "Plots/fig2.pdf", width = 10, height = 8)
