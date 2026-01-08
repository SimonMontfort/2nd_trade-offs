R.version
# platform       aarch64-apple-darwin20      
# arch           aarch64                     
# os             darwin20                    
# system         aarch64, darwin20           
# status                                     
# major          4                           
# minor          3.2                         
# year           2023                        
# month          10                          
# day            31                          
# svn rev        85441                       
# language       R                           
# version.string R version 4.3.2 (2023-10-31)
# nickname       Eye Holes                 

################################################################################
# packages and data loading
################################################################################

rm(list = ls())

library(readxl)
library(writexl)
library(tidyverse)
library(overlapping)
library(ggplot2)
library(showtext)
library(ggsci)
library(ggeffects)
library(texreg)
library(broom)
library(dplyr)
library(emmeans)
library(ggrepel)
library(patchwork)
library(ggpubr)
library(purrr)
library(forcats)

setwd("/Users/simon/Documents/repo/2nd_trade-offs")

# preprocessed dataset, see 01_clean_script.R
dat_clean <- readRDS("data/dat_clean.RDS")

# attention checks filtered
dat_clean_check <- dat_clean %>% filter(attention_check_yes)

# speeders checks filtered
dat_clean_speed <- dat_clean %>% filter(speeder_treatment_bioemi == F & speeder_treatment_bioemi == F)

################################################################################
# suff used throughout, e.g. plot theme
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
                                           size=.3, linetype="solid", 
                                           colour ="grey")
    )
}

# mapping variable names to labels
coef_labels <- c(
  "(Intercept)"        = "Intercept",
  "trust_in_sci"       = "Trust in science",
  "gender_binarymale"  = "Gender (male yes)",
  "left_right"         = "Left–right ideology",
  "education_numeric"  = "Education (numeric)",
  "urban_rural_numeric"= "Urban–rural scale",
  "coping_on_incomeYes"= "Coping on income (yes)",
  "confidence"         = "Confidence",
  "treatment_grouptreatment" = "Treatment (yes)",
  
  "gap_to_true_value_bioemi" = "\\shortstack{Gap to true value:\\\\Biodiversity vs. emissions}",
  "gap_to_true_value_landemi" = "\\shortstack{Gap to true value:\\\\Landscape vs. emissions}",
  
  "gap_to_true_value_bioemi:treatment_grouptreatment" = "\\shortstack{Gap to true value:\\\\Biodiversity vs. emissions\\\\$\\times$Treatment (yes)}",
  "treatment_grouptreatment:gap_to_true_value_landemi" = "\\shortstack{Gap to true value:\\\\Landscape vs. emissions\\\\$\\times$Treatment (yes)}",
  
  ##### Bio emi
  "treatment_positive_bioemi_and_landeminegative_control" = "\\shortstack{Control: others support\\\\emission reductions\\\\more than expected}",
  "treatment_positive_bioemi_and_landemipositive_control" = "\\shortstack{Control: others support\\\\emission reductions\\\\less than expected}",
  "treatment_positive_bioemi_and_landemimixed_control" = "Control: mixed",
  
  "treatment_positive_bioemi_and_landeminegative_treatment" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\less than expected}",
  "treatment_positive_bioemi_and_landemipositive_treatment" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\more than expected}",
  "treatment_positive_bioemi_and_landemimixed_treatment" = "Treatment: mixed",
  
  # trust in science interactions
  "treatment_positive_bioemi_and_landeminegative_control:trust_in_sci" = "\\shortstack{Control: others support\\\\emission reductions\\\\more than expected\\\\$\\times$Trust in science}",
  "treatment_positive_bioemi_and_landemipositive_control:trust_in_sci" = "\\shortstack{Control: others support\\\\emission reductions\\\\less than expected\\\\$\\times$Trust in science}",
  "treatment_positive_bioemi_and_landemimixed_control:trust_in_sci" = "\\shortstack{Control: mixed$\\times$Trust in science}",
  
  "treatment_positive_bioemi_and_landeminegative_treatment:trust_in_sci" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\less than expected\\\\$\\times$Trust in science}",
  "treatment_positive_bioemi_and_landemipositive_treatment:trust_in_sci" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\more than expected\\\\$\\times$Trust in science}",
  "treatment_positive_bioemi_and_landemimixed_treatment:trust_in_sci" = "\\shortstack{Treatment: mixed$\\times$Trust in science}",
  
  # left-right interactions
  "treatment_positive_bioemi_and_landeminegative_control:left_right" = "\\shortstack{Control: others support\\\\emission reductions\\\\more than expected\\\\$\\times$Left–right ideology}",
  "treatment_positive_bioemi_and_landemipositive_control:left_right" = "\\shortstack{Control: others support\\\\emission reductions\\\\less than expected\\\\$\\times$Left–right ideology}",
  "treatment_positive_bioemi_and_landemimixed_control:left_right" = "\\shortstack{Control: mixed$\\times$Left–right ideology}",
  
  "treatment_positive_bioemi_and_landeminegative_treatment:left_right" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\less than expected\\\\$\\times$Left–right ideology}",
  "treatment_positive_bioemi_and_landemipositive_treatment:left_right" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\more than expected\\\\$\\times$Left–right ideology}",
  "treatment_positive_bioemi_and_landemimixed_treatment:left_right" = "\\shortstack{Treatment: mixed$\\times$Left–right ideology}"
)

# get_coef_labels_list(models, coef_labels) get_coef_labels_list(models, coef_labels)

dv_labels <- c(
  prior_bioemi          = "\\rotatebox{90}{Prior biodiversity vs. emissions}",
  prior_landemi         = "\\rotatebox{90}{Prior landscape vs. emissions}",
  prior_bioemi_2nd      = "\\rotatebox{90}{2nd order prior biodiversity vs. emissions}",
  prior_landemi_2nd     = "\\rotatebox{90}{2nd order prior landscape vs. emissions}",
  post_bioemi           = "\\rotatebox{90}{Posterior biodiversity vs. emissions}",
  post_landemi          = "\\rotatebox{90}{Posterior landscape vs. emissions}",
  acceptance_alpinePV   = "\\rotatebox{90}{Acceptance of alpine PV}",
  acceptance_wind       = "\\rotatebox{90}{Acceptance of wind power}",
  acceptance_newnucs    = "\\rotatebox{90}{Acceptance of new nuclear plants}",
  acceptance_prolongnucs= "\\rotatebox{90}{Acceptance of prolonging nuclear plants}"
)


# Function to get DV labels for a list of models
get_dv_labels <- function(models, label_map) {
  sapply(models, function(model) {
    dv <- as.character(formula(model))[2]   # left-hand side
    if (dv %in% names(label_map)) {
      label_map[[dv]]
    } else {
      dv  # fallback: raw variable name
    }
  }, USE.NAMES = FALSE)
}

# Function to get EV (coefficient) labels for a list of models
get_coef_labels_list <- function(models, label_map) {
  coef_labels_list <- lapply(models, function(model) {
    coefs <- names(coef(model))
    out <- label_map[coefs]
    out[is.na(out)] <- coefs[is.na(out)]  # fallback
    unname(out)
  })
  unique(unlist(coef_labels_list))
}



################################################################################
# I. Explaining priors
################################################################################

# Example: regress prior beliefs on trust and left-right
lm_prior_bioemi <- lm(prior_bioemi ~ left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_bioemi)

lm_prior_landemi <- lm(prior_landemi ~ left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary  + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_landemi)

lm_prior_bioemi_2nd <- lm(prior_bioemi_2nd ~ left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary  + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_bioemi_2nd)

lm_prior_landemi_2nd <- lm(prior_landemi_2nd ~ left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary  + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_landemi_2nd)

models <- list(lm_prior_bioemi, lm_prior_landemi,
               lm_prior_bioemi_2nd, lm_prior_landemi_2nd)

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
 float.pos = "htb!",
  label   = "table:explaining_priors",
  caption = "Determinants of prior beliefs: ideology, trust in science, and covariates",
  file    = "Tables/explaining_priors.tex"
)

###########
# Fig 2
###########

# --- Collect model results into tidy format ---
mods <- list(
  "Biodiversity vs\nemissions\n(1st order)" = lm_prior_bioemi,
  "Landscape vs\nemissions\n(1st order)"     = lm_prior_landemi,
  "Biodiversity vs\nemissions\n(2nd order)" = lm_prior_bioemi_2nd,
  "Landscape vs\nemissions\n(2nd order)"     = lm_prior_landemi_2nd
)

# Collect results again
df_forest <- purrr::map_dfr(mods, ~tidy(.x, conf.int = TRUE), .id = "Model") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term,
                  "trust_in_sci" = "Trust in science",
                  "gender_binarymale" = "Male (ref: female)",
                  "left_right" = "Left–Right ideology",
                  "climate_salienceYes" = "Salience: Climate Change",
                  "education_numeric" = "Education",
                  "urban_rural_binaryurban" = "Urban–Rural",
                  "coping_on_incomeYes" = "Coping on income (yes)",
                  "confidence" = "Confidence"
    ),
    term = factor(term, levels = c(
      "Left–Right ideology",
      "Trust in science",
      "Urban–Rural",
      "Salience: Climate Change",
      "Male (ref: female)",
      "Education",
      "Coping on income (yes)",
      "Confidence"
    )),
    Model = factor(Model, 
                   levels = rev(c("Biodiversity vs\nemissions\n(1st order)", "Landscape vs\nemissions\n(1st order)",
                              "Biodiversity vs\nemissions\n(2nd order)", "Landscape vs\nemissions\n(2nd order)")))
  )

df_forest <- df_forest %>%
  mutate(
    label = case_when(
      # Left–Right ideology
      term == "Left–Right ideology" & Model == "Biodiversity vs\nemissions\n(1st order)" ~ 
        "Right-leaning respondents\nprioritize biodiversity over CO₂\nreductions.",
      term == "Left–Right ideology" & Model == "Landscape vs\nemissions\n(1st order)" ~ 
        "Right-leaning respondents\nprioritize landscape protection\nover CO₂ reductions.",
      term == "Left–Right ideology" & Model == "Biodiversity vs\nemissions\n(2nd order)" ~ 
        "",
      term == "Left–Right ideology" & Model == "Landscape vs\nemissions\n(2nd order)" ~ 
        "",
      
      # Trust in science
      term == "Trust in science" & Model %in% c("Biodiversity vs\nemissions\n(1st order)") ~ 
        "",
      term == "Trust in science" & Model %in% c("Biodiversity vs\nemissions\n(2nd order)", 
                                                "Landscape vs\nemissions\n(1st order)") ~ 
        "",
      term == "Trust in science" & Model == "Landscape vs\nemissions\n(2nd order)" ~ 
        "Those who trust in\nscience, expect others\nto prioritize landscape\nprotection",
      
      # Gender
      term == "Male (ref: female)" & Model %in% c("Biodiversity vs\nemissions\n(1st order)") ~
        "",
      term == "Male (ref: female)" & Model == "Biodiversity vs\nemissions\n(2nd order)" ~
        "Men expect others to\nprioritize biodiversity.",
      term == "Male (ref: female)" & Model == "Landscape vs\nemissions\n(2nd order)" ~
        "",
      
      # education_group
      term == "education_group" & Model %in% c("Biodiversity vs\nemissions\n(1st order)") ~
        "Higher education_group is linked\nto lower prioritization\nof emission reductions",
      term == "education_group" & Model %in% c("Landscape vs\nemissions\n(1st order)") ~
        "",
      term == "education_group" & Model %in% c(
                                         "Higher educated respondents expect\nothers to prioritize landscape protection") ~
        "Education has weak or mixed effect\non expectations about others'\nprioritization.",
      
      # Urban–Rural
      term == "Urban–Rural" & Model %in% c("Biodiversity vs\nemissions\n(1st order)", 
                                           "Landscape vs\nemissions\n(1st order)",
                                           "Biodiversity vs\nemissions\n(2nd order)") ~
        "",
      term == "Urban–Rural" & Model %in% c("Landscape vs\nemissions\n(2nd order)") ~
        "Rural residents think others\nprioritize landscape protection\nover emissions reduction",
      
      # Income
      term == "Income: Yes" & Model %in% c(
                                           "Biodiversity vs\nemissions\n(1st order)", 
                                           "Biodiversity vs\nemissions\n(2nd order)") ~
        "",
      term == "Coping on income (yes)" & Model %in% c("Landscape vs\nemissions\n(1st order)") ~
        "High-income respondents\n priorize landscape protection\nover emission reduction.",
      term == "Coping on income (yes)" & Model %in% c("Landscape vs\nemissions\n(2nd order)") ~
        "High-income respondents think\nothers priorize landscape pro-\ntection over emission reduction.",
      
      # Confidence
      term == "Confidence" & Model %in% c("Biodiversity vs\nemissions\n(1st order)", 
                                          "Landscape vs\nemissions\n(1st order)", 
                                          "Biodiversity vs\nemissions\n(2nd order)") ~
        "",
      term == "Confidence" & Model %in% c("Landscape vs\nemissions\n(2nd order)") ~
        "Respondents confident in their\nprior beliefs expect others to\nprioritize landscape protection",
      # Confidence
      term == "Salience: Climate Change" & Model %in% c("Biodiversity vs\nemissions\n(1st order)")~
        "Respondents who think climate\nchange is an important issue,\nprioritize emission reductions",
      term == "Salience: Climate Change" & Model %in% c("Landscape vs\nemissions\n(1st order)") ~
        "Respondents who think climate\nchange is an important issue,\nprioritize emission reductions",
      TRUE ~ NA_character_
    )
  )

# Create a data frame of labels positioned at the right
label_df <- df_forest %>%
  # filter(term)
  distinct(Model, term, label) %>%
  mutate(xpos = 0.23)  # adjust horizontal position

# Plot
fig4 <- ggplot(df_forest, aes(x = estimate, y = Model, color = Model)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text(data = label_df,
            aes(x = xpos, y = Model, label = label), nudge_y = 0,
            hjust = 0, size = 3.2, lineheight = 0.9) +
  scale_color_npg() +
  labs(x = "Coefficient estimate (95% CI)", y = "",
       title = "Explaining first- and second-order prior beliefs (before the experiment)") +
  facet_wrap(~term, scales = "free_y", ncol = 2) +
  theme_SM() +
  xlim(c(-0.75, 1)) +  # leave space for right-hand labels
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line()
  )
fig4
ggsave(fig4, file = "Plots/fig4.pdf", width = 11, height = 11)

################################################################################
# II.a) social influence: treatment-control group comparison
################################################################################

lm_influence_bioemi_main <- lm(post_bioemi ~ treatment_group, data = dat_clean %>% filter(attention_bioemi_yes))
lm_influence_landemi_main <- lm(post_landemi ~ treatment_group, data = dat_clean %>% filter(attention_landemi_yes))

models <- list(lm_influence_bioemi_main, lm_influence_landemi_main)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:social_influence_treatment_control",
  caption = "Social influence: biodiversity and land-use beliefs, treatment-control comparison",
  file    = "Tables/social_influence_treatment_control.tex"
)

###########
# Fig 4a
###########

# Main-effect models
mods <- list(
  "Biodiversity conservation\nvs. emission reduction" = lm_influence_bioemi_main,
  "Landscape protection\nvs. emission reduction" = lm_influence_landemi_main
)

df_forest <- purrr::map_dfr(mods, ~ {
  term <- emmeans(.x, ~ treatment_group) 
  tidy(term, conf.int = TRUE)
}, .id = "Model") %>%
  rename(term = treatment_group) %>%
  mutate(
    term_label = ifelse(term == "treatment", "Treatment group", "Control group"),
    Model = factor(Model, levels = rev(unique(Model)))
  )


# Forest plot
fig6a <- ggplot(df_forest, aes(x = term_label, y = estimate, group = Model, shape = term_label), col = "black") +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.6)) +
  facet_wrap(~Model, ncol = 2) +
  # ylim(-.7,1.4) +
  labs(x = "",
       y = "",
       title = "Social influence:\ntreatment-control group comparison") +
  scale_y_continuous(position = "right") + 
  theme_SM() +
  theme(legend.position = "none")

fig6a

# ################################################################################
# # II.b) social influence: (true value - prior 2nd order belief) x treatment group
# ################################################################################
# 
# lm_influence_bioemi_1 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
# lm_influence_bioemi_2 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
# lm_influence_bioemi_3 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
# lm_influence_bioemi_4 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
# lm_influence_bioemi_5 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
# lm_influence_bioemi_6 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
# lm_influence_bioemi_7 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
# lm_influence_bioemi_8 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)
# 
# models <- list(lm_influence_bioemi_1, lm_influence_bioemi_2, lm_influence_bioemi_3, lm_influence_bioemi_4,
#                lm_influence_bioemi_5, lm_influence_bioemi_6, lm_influence_bioemi_7, lm_influence_bioemi_8)
#   
# 
# texreg::texreg(
#   models,
#   custom.model.names = get_dv_labels(models, dv_labels),
#   custom.coef.names = get_coef_labels_list(models, coef_labels),  
#   custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
#   label   = "table:social_influence_bioemi_treatment_direction",
#   caption = "Social influence: biodiversity beliefs, treatment direction",
#   file    = "Tables/social_influence_bioemi_treatment_direction.tex"
# )
# 
# ## Stepwise build-up for Learning LandEmi ##
# lm_influence_landemi_1 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
# lm_influence_landemi_2 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
# lm_influence_landemi_3 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
# lm_influence_landemi_4 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
# lm_influence_landemi_5 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
# lm_influence_landemi_6 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
# lm_influence_landemi_7 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
# lm_influence_landemi_8 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)
# 
# models <- list(lm_influence_landemi_1, lm_influence_landemi_2, lm_influence_landemi_3, lm_influence_landemi_4,
#                lm_influence_landemi_5, lm_influence_landemi_6, lm_influence_landemi_7, lm_influence_landemi_8)
# 
# texreg::texreg(
#   models,
#   custom.model.names = get_dv_labels(models, dv_labels),
#   custom.coef.names = get_coef_labels_list(models, coef_labels),  
#   custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
#   label   = "table:social_influence_landemi_treatment_direction",
#   caption = "Social influence: land-use beliefs, treatment direction",
#   file    = "Tables/social_influence_landemi_treatment_direction.tex"
# )
# 
# 
# # Main-effect models
# mods <- list(
#   "Biodiversity conservation\nvs. emission reduction" = lm_influence_bioemi_8,
#   "Landscape\nvs. emission reduction" = lm_influence_landemi_8
# )
# 
# 
# # Function to extract both means and slopes
# extract_emm <- function(model, gap_var) {
#   form <- as.formula(paste("~ treatment_group |", gap_var))
#   
#   # Predicted means at a range of gap values
#   em_gap <- emmeans(model,
#                     form,
#                     at = setNames(list(seq(-4, 4, 1)), gap_var)) %>%
#     tidy(conf.int = TRUE) %>%
#     rename(gap_value = !!sym(gap_var)) %>%  # standardize column name
#     mutate(type = "Predicted means",
#            gap_var = gap_var)
#   
#   # Simple slopes of gap within treatment
#   tr_gap <- emtrends(model, ~ treatment_group, var = gap_var) %>%
#     tidy(conf.int = TRUE) %>%
#     mutate(type = "Simple slopes",
#            gap_var = gap_var)
#   
#   bind_rows(em_gap, tr_gap)
# }
# 
# ###########
# # Fig 2b
# ###########
# 
# # Both models
# mods <- list(
#   "Perception gap:\nBiodiversity conservation\nvs. emission reduction" = lm_influence_bioemi_8,
#   "Perception gap:\nLandscape\nvs. emission reduction" = lm_influence_landemi_8
# )
# 
# # Both gap variables
# gap_vars <- c("gap_to_true_value_bioemi", "gap_to_true_value_landemi")
# 
# # Apply: all gaps × all models
# df_forest <- purrr::map_dfr(mods, function(m) {
#   map_dfr(gap_vars, ~ extract_emm(m, .x))
# }, .id = "Model") %>% 
#   mutate(gap_var = ifelse(gap_var == "gap_to_true_value_bioemi",
#                           "Own belief:\nBiodiversity conservation\nvs. emission reduction",
#                           "Own belief:\nLandscape\nvs. emission reduction"))
# 
# # Check coverage
# table(df_forest$Model, df_forest$gap_var)
# 
# p_vals <- purrr::map_dfr(mods, tidy, .id = "Model") %>% 
#   filter(grepl(":", term)) %>% 
#   mutate(gap_var = ifelse(grepl("bioemi", term),
#                           "Own belief:\nBiodiversity conservation\nvs. emission reduction",
#                           "Own belief:\nLandscape\nvs. emission reduction")) %>%
#   group_by(Model, gap_var) %>%
#   summarise(p.value = min(p.value), .groups = "drop") %>%
#   mutate(label = paste0("P-value (interaction): ", signif(p.value, 3)))
# 
# 
# # Example: label above the top of blue treatment line
# label_arrow_df <- df_forest %>%
#   filter(type == "Predicted means", treatment_group == "treatment") %>%  
#   group_by(Model, gap_var) %>%
#   arrange(gap_value) %>% 
#   slice(4) %>% 
#   summarise(
#     x_arrow = gap_value,      # where arrow points on x-axis
#     y_arrow = estimate,       # point to highest value of blue line
#     .groups = "drop"
#   ) %>% 
#   left_join(p_vals, by = c("Model", "gap_var"))
# 
# # corner_labels <- data.frame(
# #   x = c(4, -4, -4, 4),     # push into corners of plot range
# #   y = c(3.2, 3.2, -2.3, -2.3),
# #   hjust = c(1, 0, 0, 1),           # align text nicely inside plot
# #   vjust = c(1, 1, 0, 0),
# #   label = c(
# #     "Respondent underestimated others' preference\nfor emission reductions,\nand self prefers emission reductions",
# #     "Respondent overestimated others' preference\nfor emission reductions,\nand self prefers emission reductions",
# #     "Respondent overestimated others' preference\nfor emission reductions,\nand self prefers biodiversity/land",
# #     "Respondent underestimated others' preference\nfor emission reductions,\nand self prefers biodiversity/land"
# #   )
# # )
# 
# 
# # Line plot with ribbons for CIs
# fig2b <- ggplot(df_forest %>% filter(type != "Simple slopes") , 
#                 aes(x = as.numeric(gap_value), 
#                     y = estimate,
#                     color = treatment_group,
#                     fill = treatment_group,
#                     group = treatment_group)) +
#   geom_line(size = 1) +
#   geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
#   geom_hline(yintercept = 0, lty = 2, col = "grey") +
#   geom_vline(xintercept = 0, lty = 2, col = "grey") +
#   # Curved arrow pointing to the line
#   # Label with rounded edges + arrow
#   geom_label_repel(data = label_arrow_df,
#                    aes(x = x_arrow, y = y_arrow, label = label),   # adjust x/y as needed
#                    inherit.aes = FALSE,
#                    label.r = unit(0.2, "lines"),       # rounded corners
#                    label.size = 0.3,                   # box border thickness
#                    arrow = arrow(length = unit(0.02, "npc")),  # arrow on segment
#                    segment.curvature = 0.3,            # curve the arrow
#                    segment.angle = 20,                 # tweak curvature
#                    segment.ncp = 3,                    # smoothness
#                    nudge_x = 1, 
#                    nudge_y = 1,
#                    segment.color = "black",
#                    size = 3) +
#   
#   # # Quadrant labels
#   # geom_text(data = corner_labels,
#   #           aes(x = x, y = y, label = label, hjust = hjust, vjust = vjust),
#   #           inherit.aes = FALSE,
#   #           size = 2, color = "black") +
#   scale_color_npg() +
#   scale_fill_npg() +
#   ylim(-2.5,3.5) +
#   facet_grid(gap_var~Model, scales = "free_y", switch = "y") +
#   labs(x = "Perception gap\n(True value of what others believe - expectation prior to the experiment)",
#        y = "",
#        color = "Experimental group",
#        fill = "Experimental group",
#        title = "Social influence:\nDepending on the perception gap") + 
#   theme_SM() +
#   theme(strip.text.y.left = element_text(angle = 0),
#         strip.placement = "outside",
#         legend.position = c(.7,.4),
#         legend.text = element_text(size = 7),
#         legend.key.size = unit(.4, "cm"),
#         legend.margin = margin(rep(2, 4)),
#         legend.title = element_blank(),
#         legend.justification = c(1, 0),
#         legend.background = element_rect(fill="white", 
#                                          size=.3, linetype="solid", 
#                                          colour ="grey"))
# 
# fig2 <- fig2a + fig2b + plot_layout(widths = c(1, 3))
# ggsave(fig2,  file = "Plots/fig2.pdf", width = 10, height = 8)

################################################################################
# II.b) social influence: treatment direction
################################################################################

# Biodiversity vs Emissions
lm_influence_bioemi_1 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_influence_bioemi_2 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_influence_bioemi_3 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_influence_bioemi_4 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_influence_bioemi_5 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_influence_bioemi_6 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_influence_bioemi_7 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_influence_bioemi_8 <- lm(post_bioemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)

models <- list(lm_influence_bioemi_1, lm_influence_bioemi_2, lm_influence_bioemi_3, lm_influence_bioemi_4,
               lm_influence_bioemi_5, lm_influence_bioemi_6, lm_influence_bioemi_7, lm_influence_bioemi_8)

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:social_influence_bioemi_treatment_direction",
  caption = "Social influence: biodiversity beliefs, treatment direction",
  file    = "Tables/social_influence_bioemi_treatment_direction.tex"
)

## Stepwise build-up for Learning LandEmi ##
lm_influence_landemi_1 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_influence_landemi_2 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_influence_landemi_3 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_influence_landemi_4 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_influence_landemi_5 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_influence_landemi_6 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_influence_landemi_7 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_influence_landemi_8 <- lm(post_landemi ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)

models <- list(lm_influence_landemi_1, lm_influence_landemi_2, lm_influence_landemi_3, lm_influence_landemi_4,
               lm_influence_landemi_5, lm_influence_landemi_6, lm_influence_landemi_7, lm_influence_landemi_8)

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:social_influence_landemi_treatment_direction",
  caption = "Social influence: land-use beliefs, treatment direction",
  file    = "Tables/social_influence_landemi_treatment_direction.tex"
)


# Main-effect models
mods <- list(
  "Biodiversity conservation\nvs. emission reduction" = lm_influence_bioemi_8,
  "Landscape protection\nvs. emission reduction" = lm_influence_landemi_8
)

# Extract coefficients for treatment effects only
df_forest <- purrr::map_dfr(mods, ~ {
  term <- emmeans(.x, ~ treatment_positive_bioemi_and_landemi)  # or whatever your treatment factor is
  tidy(term, conf.int = TRUE)
}, .id = "Model") %>%
  rename(term = treatment_positive_bioemi_and_landemi) %>% 
  mutate(
    term_label = case_when(
      term == "positive_control"   ~ "Control: Overestimated\nothers' CO₂ preference",
      term == "mixed_control"      ~ "Control: Mixed",
      term == "negative_control"   ~ "Control: Underestimated\nothers' CO₂ preference",
      term == "positive_treatment" ~ "Treatment: Overestimated\nothers' CO₂ preference",
      term == "mixed_treatment"    ~ "Treatment: Mixed",
      term == "negative_treatment" ~ "Treatment: Underestimated\nothers' CO₂ preference",
      TRUE ~ term
    ),
    term_label = factor(
      term_label,
      levels = c(
        "Control: Overestimated\nothers' CO₂ preference",
        "Treatment: Overestimated\nothers' CO₂ preference",
        "Control: Mixed",
        "Treatment: Mixed",
        "Control: Underestimated\nothers' CO₂ preference",
        "Treatment: Underestimated\nothers' CO₂ preference"
      )
    ),
    Model = factor(Model, levels = rev(unique(Model)))
  )


# Forest plot
fig6b <- ggplot(df_forest, aes(x = term_label, y = estimate, group = term_label, col = rev(term_label), shape = term_label)) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.6)) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  scale_shape_manual(values = rep(c(16,17), 3)) +
  facet_wrap(~Model) +
  scale_color_manual(values = c("blue", "blue", "grey","grey", "brown", "brown")) +
  labs(x = "2nd order trade-off belief treatment",
       y = "Own trade-off belief",
       title = "Social influence:\ntreatment direction") +
  # ylim(-.7,1.4) +
  theme_SM() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))
fig6b
fig6 <- ggarrange(fig6a, fig6b, labels = "auto", align = "hv", widths = c(1,1.5))
ggsave(fig6,  file = "Plots/fig6.pdf", width = 10, height = 5)

################################################################################
# III.a) acceptance: treatment-control group comparison
################################################################################

lm_acceptance_alpinePV_main <- lm(acceptance_alpinePV ~ treatment_group, data = dat_clean)
lm_acceptance_wind_main <- lm(acceptance_wind ~ treatment_group, data = dat_clean)
lm_acceptance_newnucs_main <- lm(acceptance_newnucs ~ treatment_group, data = dat_clean)
lm_acceptance_prolongnucs_main <- lm(acceptance_prolongnucs ~ treatment_group, data = dat_clean)

models <- list(lm_acceptance_alpinePV_main, lm_acceptance_wind_main,
               lm_acceptance_newnucs_main, lm_acceptance_prolongnucs_main)

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_treatment_control",
  caption = "Acceptance of renewable energy technologies: treatment-control comparison",
  file    = "Tables/acceptance_treatment_control.tex"
)

# Main-effect models
mods <- list(
  "Alpine PV" = lm_acceptance_alpinePV_main,
  "Wind" = lm_influence_landemi_main,
  "New nuclear" = lm_acceptance_newnucs_main,
  "Prolong nuclear" = lm_acceptance_prolongnucs_main
)

df_forest <- purrr::map_dfr(mods, ~ {
  term <- emmeans(.x, ~ treatment_group) 
  tidy(term, conf.int = TRUE)}, .id = "Model") %>%
  rename(term = treatment_group) %>%
  mutate(
    term_label = ifelse(term == "treatment", "Treatment group", "Control group"),
    Model = factor(Model, levels = unique(Model))
  )

# Forest plot
fig5a <- ggplot(df_forest, aes(x = term_label, y = estimate, group = Model, shape = term_label), col = "black") +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  # geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~Model) +
  # scale_color_npg() +
  ylim(-1.2,1.8) +
  labs(x = "",
       y = "Acceptance",
       title = "RE technology acceptance:\ntreatment-control group") +
  theme_SM() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

fig5a

################################################################################
# III.b) acceptance: positive/negative treatment
################################################################################

lm_alpinePV_1 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_alpinePV_2 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_alpinePV_3 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_alpinePV_4 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_alpinePV_5 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_alpinePV_6 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_alpinePV_7 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_alpinePV_8 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)

lm_wind_1 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_wind_2 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_wind_3 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_wind_4 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_wind_5 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_wind_6 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_wind_7 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_wind_8 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)

lm_newnucs_1 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_newnucs_2 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_newnucs_3 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_newnucs_4 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_newnucs_5 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_newnucs_6 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_newnucs_7 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_newnucs_8 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)

lm_prolongnucs_1 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_prolongnucs_2 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_prolongnucs_3 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_prolongnucs_4 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_prolongnucs_5 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_prolongnucs_6 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_prolongnucs_7 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_prolongnucs_8 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)

# Export tables 
models <- list(lm_alpinePV_1, lm_alpinePV_2, lm_alpinePV_3, lm_alpinePV_4,
               lm_alpinePV_5, lm_alpinePV_6, lm_alpinePV_7, lm_alpinePV_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_alpine_pv_treatment_direction",
  caption = "Acceptance of alpine PV energy technology: treatment direction",
  file    = "Tables/acceptance_alpine_pv_treatment_direction.tex"
)

models <- list(lm_wind_1, lm_wind_2, lm_wind_3, lm_wind_4,
               lm_wind_5, lm_wind_6, lm_wind_7, lm_wind_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_wind_treatment_direction",
  caption = "Acceptance of wind energy technology: treatment direction",
  file    = "Tables/acceptance_wind_treatment_direction.tex"
)

models <- list(lm_newnucs_1, lm_newnucs_2, lm_newnucs_3, lm_newnucs_4,
               lm_newnucs_5, lm_newnucs_6, lm_newnucs_7, lm_newnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_new_nucs_treatment_direction",
  caption = "Acceptance of new nuclear energy technology: treatment direction",
  file    = "Tables/acceptance_new_nucs_treatment_direction.tex"
)

models <- list(lm_prolongnucs_1, lm_prolongnucs_2, lm_prolongnucs_3, lm_prolongnucs_4,
               lm_prolongnucs_5, lm_prolongnucs_6, lm_prolongnucs_7, lm_prolongnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_prolong_nucs_treatment_direction",
  caption = "Acceptance of prolonging nuclear energy technology: treatment direction",
  file    = "Tables/acceptance_prolong_nucs_treatment_direction.tex"
)

# List of models
mods <- list(
  "Alpine PV" = lm_alpinePV_8,
  "Wind" = lm_wind_8,
  "New nuclear" = lm_newnucs_8,
  "Prolong nuclear" = lm_prolongnucs_8
)

# Extract coefficients for treatment effects only
df_forest <- purrr::map_dfr(mods, ~ {
  term <- emmeans(.x, ~ treatment_positive_bioemi_and_landemi)  # or whatever your treatment factor is
  tidy(term, conf.int = TRUE)
}, .id = "Model") %>%
  rename(term = treatment_positive_bioemi_and_landemi) %>% 
  mutate(
    term_label = case_when(
      term == "positive_control"   ~ "Control: Overestimated\nothers' CO₂ preference",
      term == "mixed_control"      ~ "Control: Mixed",
      term == "negative_control"   ~ "Control: Underestimated\nothers' CO₂ preference",
      term == "positive_treatment" ~ "Treatment: Overestimated\nothers' CO₂ preference",
      term == "mixed_treatment"    ~ "Treatment: Mixed",
      term == "negative_treatment" ~ "Treatment: Underestimated\nothers' CO₂ preference",
      TRUE ~ term
    ),
    term_label = factor(
      term_label,
      levels = c(
        "Control: Overestimated\nothers' CO₂ preference",
        "Treatment: Overestimated\nothers' CO₂ preference",
        "Control: Mixed",
        "Treatment: Mixed",
        "Control: Underestimated\nothers' CO₂ preference",
        "Treatment: Underestimated\nothers' CO₂ preference"
      )
    ),
    Model = factor(Model, levels = unique(Model))
  )


# Tidy results for plotting
fig5b <- df_forest %>% 
  ggplot(aes(x = term_label, y = estimate, col = rev(term_label), shape = term_label)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  scale_shape_manual(values = rep(c(16,17), 3)) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.2, position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c("blue", "blue", "grey","grey", "brown", "brown")) +
  ylim(-1.2,1.8) +
  labs(x = "", y = "Acceptance", color = "Outcome", 
       title = "RE technology acceptance:\ntreatment direction") +
  geom_hline(yintercept = 0, lty = 3) + 
  facet_wrap(~Model) + 
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
fig5b
fig5 <- ggarrange(fig5a, fig5b, labels = "auto", align = "hv", widths = c(1,2.5))
ggsave(fig5,  file = "Plots/fig5.pdf", width = 10, height = 7)

# --------------------------------------------------------------------------- #

################################################################################
# IV. Robustness checks: acceptance -- speeding, treatment direction
################################################################################

lm_alpinePV_1 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi, data = dat_clean_speed)
lm_alpinePV_2 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_speed)
lm_alpinePV_3 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_speed)
lm_alpinePV_4 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_speed)
lm_alpinePV_5 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_speed)
lm_alpinePV_6 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_speed)
lm_alpinePV_7 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_speed)
lm_alpinePV_8 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_speed)

lm_wind_1 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi, data = dat_clean_speed)
lm_wind_2 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_speed)
lm_wind_3 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_speed)
lm_wind_4 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_speed)
lm_wind_5 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_speed)
lm_wind_6 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_speed)
lm_wind_7 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_speed)
lm_wind_8 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_speed)

lm_newnucs_1 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean_speed)
lm_newnucs_2 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_speed)
lm_newnucs_3 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_speed)
lm_newnucs_4 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_speed)
lm_newnucs_5 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_speed)
lm_newnucs_6 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_speed)
lm_newnucs_7 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_speed)
lm_newnucs_8 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_speed)

lm_prolongnucs_1 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean_speed)
lm_prolongnucs_2 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_speed)
lm_prolongnucs_3 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_speed)
lm_prolongnucs_4 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_speed)
lm_prolongnucs_5 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_speed)
lm_prolongnucs_6 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_speed)
lm_prolongnucs_7 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_speed)
lm_prolongnucs_8 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_speed)

# Export tables 
models <- list(lm_alpinePV_1, lm_alpinePV_2, lm_alpinePV_3, lm_alpinePV_4,
               lm_alpinePV_5, lm_alpinePV_6, lm_alpinePV_7, lm_alpinePV_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_alpine_pv_speeders_excluded",
  caption = "Robustness checks for acceptance of alpine PV: treatment direction, speeders excluded",
  file    = "Tables/acceptance_alpine_pv_speeders_excluded.tex"
)

models <-   list(lm_wind_1, lm_wind_2, lm_wind_3, lm_wind_4,
                 lm_wind_5, lm_wind_6, lm_wind_7, lm_wind_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_wind_speeders_excluded",
  caption = "Robustness checks for acceptance of wind: treatment direction, speeders excluded",
  file    = "Tables/acceptance_wind_speeders_excluded.tex"
)

models <-   list(lm_newnucs_1, lm_newnucs_2, lm_newnucs_3, lm_newnucs_4,
                 lm_newnucs_5, lm_newnucs_6, lm_newnucs_7, lm_newnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_new_nucs_speeders_excluded",
  caption = "Robustness checks for acceptance of new nuclear: treatment direction, speeders excluded",
  file    = "Tables/acceptance_new_nucs_speeders_excluded.tex"
)

models <-   list(lm_prolongnucs_1, lm_prolongnucs_2, lm_prolongnucs_3, lm_prolongnucs_4,
                 lm_prolongnucs_5, lm_prolongnucs_6, lm_prolongnucs_7, lm_prolongnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_prolong_nucs_speeders_excluded",
  caption = "Robustness checks for acceptance of prolonging nuclear: treatment direction, speeders excluded",
  file    = "Tables/acceptance_prolong_nucs_speeders_excluded.tex"
)

################################################################################
# IV. Robustness checks: acceptance -- attention check, treatment direction
################################################################################

dat_clean_attention <- dat_clean %>% filter(attention_check_yes)

lm_alpinePV_1 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi, data = dat_clean_attention)
lm_alpinePV_2 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_attention)
lm_alpinePV_3 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_attention)
lm_alpinePV_4 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_attention)
lm_alpinePV_5 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_attention)
lm_alpinePV_6 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_attention)
lm_alpinePV_7 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_attention)
lm_alpinePV_8 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_attention)

lm_wind_1 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi, data = dat_clean_attention)
lm_wind_2 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_attention)
lm_wind_3 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_attention)
lm_wind_4 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_attention)
lm_wind_5 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_attention)
lm_wind_6 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_attention)
lm_wind_7 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_attention)
lm_wind_8 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_attention)

lm_newnucs_1 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean_attention)
lm_newnucs_2 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_attention)
lm_newnucs_3 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_attention)
lm_newnucs_4 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_attention)
lm_newnucs_5 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_attention)
lm_newnucs_6 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_attention)
lm_newnucs_7 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_attention)
lm_newnucs_8 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_attention)

lm_prolongnucs_1 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean_attention)
lm_prolongnucs_2 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean_attention)
lm_prolongnucs_3 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean_attention)
lm_prolongnucs_4 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean_attention)
lm_prolongnucs_5 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_attention)
lm_prolongnucs_6 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_attention)
lm_prolongnucs_7 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_attention)
lm_prolongnucs_8 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_attention)

# Export tables 
models <- list(lm_alpinePV_1, lm_alpinePV_2, lm_alpinePV_3, lm_alpinePV_4,
               lm_alpinePV_5, lm_alpinePV_6, lm_alpinePV_7, lm_alpinePV_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_alpine_pv_attention_check",
  caption = "Robustness checks for acceptance of alpine PV: treatment direction, only respondents who passed the attention check included",
  file    = "Tables/acceptance_alpine_pv_attention_check.tex"
)

models <-   list(lm_wind_1, lm_wind_2, lm_wind_3, lm_wind_4,
                 lm_wind_5, lm_wind_6, lm_wind_7, lm_wind_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_wind_attention_check",
  caption = "Robustness checks for acceptance of wind: treatment direction, only respondents who passed the attention check included",
  file    = "Tables/acceptance_wind_attention_check.tex"
)

models <-   list(lm_newnucs_1, lm_newnucs_2, lm_newnucs_3, lm_newnucs_4,
                 lm_newnucs_5, lm_newnucs_6, lm_newnucs_7, lm_newnucs_8)
texreg::texreg(
  models,
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_new_nucs_attention_check",
  caption = "Robustness checks for acceptance of new nuclear: treatment direction, only respondents who passed the attention check included",
  file    = "Tables/acceptance_new_nucs_attention_check.tex"
)

models <-   list(lm_prolongnucs_1, lm_prolongnucs_2, lm_prolongnucs_3, lm_prolongnucs_4,
                 lm_prolongnucs_5, lm_prolongnucs_6, lm_prolongnucs_7, lm_prolongnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_prolong_nucs_attention_check",
  caption = "Robustness checks for acceptance of prolonging nuclear: treatment direction, only respondents who passed the attention check included",
  file    = "Tables/acceptance_prolong_nucs_attention_check.tex"
)


################################################################################
# IV. Robustness checks: social influence -- attention check, gap to true value
################################################################################

lm_influence_bioemi_1 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
lm_influence_bioemi_2 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
lm_influence_bioemi_3 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
lm_influence_bioemi_4 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
lm_influence_bioemi_5 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
lm_influence_bioemi_6 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
lm_influence_bioemi_7 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
lm_influence_bioemi_8 <- lm(post_bioemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)

models <- list(lm_influence_bioemi_1, lm_influence_bioemi_2, lm_influence_bioemi_3, lm_influence_bioemi_4,
               lm_influence_bioemi_5, lm_influence_bioemi_6, lm_influence_bioemi_7, lm_influence_bioemi_8)


texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:social_influence_bioemi_gap_to_treatment",
  caption = "Robustness checks for social influence: biodiversity beliefs, gap to true value (true value - own prior belief)",
  file    = "Tables/social_influence_bioemi_gap_to_treatment.tex"
)

## Stepwise build-up for Learning LandEmi ##
lm_influence_landemi_1 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
lm_influence_landemi_2 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
lm_influence_landemi_3 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
lm_influence_landemi_4 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
lm_influence_landemi_5 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
lm_influence_landemi_6 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
lm_influence_landemi_7 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
lm_influence_landemi_8 <- lm(post_landemi ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)

models <- list(lm_influence_landemi_1, lm_influence_landemi_2, lm_influence_landemi_3, lm_influence_landemi_4,
               lm_influence_landemi_5, lm_influence_landemi_6, lm_influence_landemi_7, lm_influence_landemi_8)

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:social_influence_landemi_gap_to_treatment",
  caption = "Robustness checks for social influence: land-use beliefs, gap to true value (true value - own prior belief)",
  file    = "Tables/social_influence_landemi_gap_to_treatment.tex"
)


################################################################################
# IV. Robustness checks: acceptance -- gap to true value
################################################################################

lm_alpinePV_1 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
lm_alpinePV_2 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
lm_alpinePV_3 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
lm_alpinePV_4 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
lm_alpinePV_5 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
lm_alpinePV_6 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
lm_alpinePV_7 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
lm_alpinePV_8 <- lm(acceptance_alpinePV ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)

lm_wind_1 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
lm_wind_2 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
lm_wind_3 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
lm_wind_4 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
lm_wind_5 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
lm_wind_6 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
lm_wind_7 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
lm_wind_8 <- lm(acceptance_wind ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)

lm_newnucs_1 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
lm_newnucs_2 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
lm_newnucs_3 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
lm_newnucs_4 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
lm_newnucs_5 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
lm_newnucs_6 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
lm_newnucs_7 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
lm_newnucs_8 <- lm(acceptance_newnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)

lm_prolongnucs_1 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group, data = dat_clean_check)
lm_prolongnucs_2 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci, data = dat_clean_check)
lm_prolongnucs_3 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary, data = dat_clean_check)
lm_prolongnucs_4 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right, data = dat_clean_check)
lm_prolongnucs_5 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean_check)
lm_prolongnucs_6 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean_check)
lm_prolongnucs_7 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean_check)
lm_prolongnucs_8 <- lm(acceptance_prolongnucs ~ gap_to_true_value_bioemi*treatment_group + gap_to_true_value_landemi*treatment_group + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean_check)

# sjPlot::plot_model(lm_alpinePV_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")
# sjPlot::plot_model(lm_wind_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")
# sjPlot::plot_model(lm_newnucs_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")
# sjPlot::plot_model(lm_prolongnucs_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")

# Export tables 
models <- list(lm_alpinePV_1, lm_alpinePV_2, lm_alpinePV_3, lm_alpinePV_4,
               lm_alpinePV_5, lm_alpinePV_6, lm_alpinePV_7, lm_alpinePV_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_alpine_pv_gap_to_treatment",
  caption = "Robustness checks for acceptance of alpine PV energy technology: gap to the treatment (true value - prior belief)",
  file    = "Tables/acceptance_alpine_pv_gap_to_treatment.tex"
)

models <- list(lm_wind_1, lm_wind_2, lm_wind_3, lm_wind_4,
               lm_wind_5, lm_wind_6, lm_wind_7, lm_wind_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_wind_gap_to_treatment",
  caption = "Robustness checks for acceptance of wind energy technology: gap to the treatment (true value - prior belief)",
  file    = "Tables/acceptance_wind_gap_to_treatment.tex"
)

models <- list(lm_newnucs_1, lm_newnucs_2, lm_newnucs_3, lm_newnucs_4,
               lm_newnucs_5, lm_newnucs_6, lm_newnucs_7, lm_newnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_new_nucs_gap_to_treatment",
  caption = "Robustness checks for acceptance of nuclear energy technology: gap to the treatment (true value - prior belief)",
  file    = "Tables/acceptance_new_nucs_gap_to_treatment.tex"
)

models <- list(lm_prolongnucs_1, lm_prolongnucs_2, lm_prolongnucs_3, lm_prolongnucs_4,
               lm_prolongnucs_5, lm_prolongnucs_6, lm_prolongnucs_7, lm_prolongnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_prolong_nucs_gap_to_treatment",
  caption = "Robustness checks for acceptance of prolonging nuclear energy technology: gap to true value (true value - own prior belief)",
  file    = "Tables/acceptance_prolong_nucs_gap_to_treatment.tex"
)



################################################################################
# V. Interaction effects treatement direction: left-right and trust 
################################################################################

# --- Alpine PV ---
lm_alpinePV_int1 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi * trust_in_sci +
                         gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                       data = dat_clean)

lm_alpinePV_int2 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi * left_right +
                         trust_in_sci + gender_binary + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                       data = dat_clean)

# --- Wind ---
lm_wind_int1 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi * trust_in_sci +
                     gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                   data = dat_clean)

lm_wind_int2 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi * left_right +
                     trust_in_sci + gender_binary + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                   data = dat_clean)

# --- New nuclear ---
lm_newnucs_int1 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi * trust_in_sci +
                        gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                      data = dat_clean)

lm_newnucs_int2 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi * left_right +
                        trust_in_sci + gender_binary + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                      data = dat_clean)

# --- Prolonged nuclear ---
lm_prolongnucs_int1 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi * trust_in_sci +
                            gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                          data = dat_clean)

lm_prolongnucs_int2 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi * left_right +
                            trust_in_sci + gender_binary + education_numeric + urban_rural_numeric + coping_on_income + confidence,
                          data = dat_clean)

models <-   list(lm_alpinePV_int1,
                 lm_wind_int1,
                 lm_newnucs_int1,
                 lm_prolongnucs_int1)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_interactions_trust",
  caption = "Acceptance of renewable energy technologies with treatment interactions (trust)",
  file    = "Tables/acceptance_interactions_trust.tex"
  )


models <-   list(lm_alpinePV_int2,
                 lm_wind_int2,
                 lm_newnucs_int2,
                 lm_prolongnucs_int2)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
  label   = "table:acceptance_interactions_left_right",
  caption = "Acceptance of renewable energy technologies with treatment interactions (left-right ideology)",
  file    = "Tables/acceptance_interactions_left_right.tex"
)




################################################################################
# explaining acceptance
################################################################################

lm_acceptance_alpinePV_1 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi, data = dat_clean)
lm_acceptance_alpinePV_2 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right, data = dat_clean)
lm_acceptance_alpinePV_3 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci, data = dat_clean)
lm_acceptance_alpinePV_4 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience, data = dat_clean)
lm_acceptance_alpinePV_5 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary, data = dat_clean)
lm_acceptance_alpinePV_6 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary, data = dat_clean)
lm_acceptance_alpinePV_7 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income, data = dat_clean)
lm_acceptance_alpinePV_8 <- lm(acceptance_alpinePV ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income + confidence, data = dat_clean)

models <- list(lm_acceptance_alpinePV_1, lm_acceptance_alpinePV_2, lm_acceptance_alpinePV_3, 
               lm_acceptance_alpinePV_4, lm_acceptance_alpinePV_5, lm_acceptance_alpinePV_6,
               lm_acceptance_alpinePV_7, lm_acceptance_alpinePV_8)
texreg(models,
       custom.model.names = get_dv_labels(models, dv_labels),
       custom.coef.names = get_coef_labels_list(models, coef_labels),
       custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
       label   = "tab:explaining_acceptance_alpinePV",  float.pos = "htb!",
       caption = "Explaining acceptance: Alpine PV",
       file    = "Tables/explaining_acceptance_alpinePV.tex")

lm_acceptance_wind_1 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi, data = dat_clean)
lm_acceptance_wind_2 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right, data = dat_clean)
lm_acceptance_wind_3 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci, data = dat_clean)
lm_acceptance_wind_4 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience, data = dat_clean)
lm_acceptance_wind_5 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary, data = dat_clean)
lm_acceptance_wind_6 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary, data = dat_clean)
lm_acceptance_wind_7 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income, data = dat_clean)
lm_acceptance_wind_8 <- lm(acceptance_wind ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income + confidence, data = dat_clean)

models <- list(lm_acceptance_wind_1, lm_acceptance_wind_2, lm_acceptance_wind_3, 
               lm_acceptance_wind_4, lm_acceptance_wind_5, lm_acceptance_wind_6,
               lm_acceptance_wind_7, lm_acceptance_wind_8)
texreg(models,
       custom.model.names = get_dv_labels(models, dv_labels),
       custom.coef.names = get_coef_labels_list(models, coef_labels),
       custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
       label   = "tab:explaining_acceptance_wind", float.pos = "htb!",
       caption = "Explaining acceptance: wind",
       file    = "Tables/explaining_acceptance_wind.tex")

lm_acceptance_newnucs_1 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi, data = dat_clean)
lm_acceptance_newnucs_2 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right, data = dat_clean)
lm_acceptance_newnucs_3 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci, data = dat_clean)
lm_acceptance_newnucs_4 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience, data = dat_clean)
lm_acceptance_newnucs_5 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary, data = dat_clean)
lm_acceptance_newnucs_6 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary, data = dat_clean)
lm_acceptance_newnucs_7 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income, data = dat_clean)
lm_acceptance_newnucs_8 <- lm(acceptance_newnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income + confidence, data = dat_clean)

models <- list(lm_acceptance_newnucs_1, lm_acceptance_newnucs_2, lm_acceptance_newnucs_3, 
               lm_acceptance_newnucs_4, lm_acceptance_newnucs_5, lm_acceptance_newnucs_6,
               lm_acceptance_newnucs_7, lm_acceptance_newnucs_8)
texreg(models,
       custom.model.names = get_dv_labels(models, dv_labels),
       custom.coef.names = get_coef_labels_list(models, coef_labels),
       custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$", float.pos = "htb!",
       label   = "tab:explaining_acceptance_newnucs",
       caption = "Explaining acceptance: new nuclear",
       file    = "Tables/explaining_acceptance_newnucs.tex")

lm_acceptance_prolongnucs_1 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi, data = dat_clean)
lm_acceptance_prolongnucs_2 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right, data = dat_clean)
lm_acceptance_prolongnucs_3 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci, data = dat_clean)
lm_acceptance_prolongnucs_4 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience, data = dat_clean)
lm_acceptance_prolongnucs_5 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary, data = dat_clean)
lm_acceptance_prolongnucs_6 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary, data = dat_clean)
lm_acceptance_prolongnucs_7 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income, data = dat_clean)
lm_acceptance_prolongnucs_8 <- lm(acceptance_prolongnucs ~ prior_bioemi + prior_landemi + prior_bioemi_2nd + prior_landemi_2nd + complementarity_bioemi + complementarity_landemi + left_right + trust_in_sci + climate_salience + urban_rural_binary + gender_binary + coping_on_income + confidence, data = dat_clean)

models <- list(lm_acceptance_prolongnucs_1, lm_acceptance_prolongnucs_2, lm_acceptance_prolongnucs_3, 
               lm_acceptance_prolongnucs_4, lm_acceptance_prolongnucs_5, lm_acceptance_prolongnucs_6,
               lm_acceptance_prolongnucs_7, lm_acceptance_prolongnucs_8)
texreg(models,
       custom.model.names = get_dv_labels(models, dv_labels),
       custom.coef.names = get_coef_labels_list(models, coef_labels),
       custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",  float.pos = "htb!",
       label   = "tab:explaining_acceptance_prolongnucs",
       caption = "Explaining acceptance: prolonged nuclear",
       file    = "Tables/explaining_acceptance_prolongnucs.tex")


# Combine models into a named list
mods <- list(
  "Alpine PV" = lm_acceptance_alpinePV_8,
  "Wind" = lm_acceptance_wind_8,
  "Prolong nuclear" = lm_acceptance_prolongnucs_8,
  "New nuclear" = lm_acceptance_newnucs_8
)

terms_to_plot <- c("prior_bioemi", "prior_landemi",
                   "prior_bioemi_2nd", "prior_landemi_2nd",
                   "complementarity_bioemi", "complementarity_landemi")

df_forest <- purrr::map_dfr(mods, ~ {
  broom::tidy(.x, conf.int = TRUE)
}, .id = "Model") %>%
  filter(term %in% terms_to_plot) %>%
  mutate(
    term_label = recode(term,
                        "prior_bioemi"            = "1st-order prior: biodiversity vs emissions",
                        "prior_landemi"           = "1st-order prior: landscape vs emissions",
                        "prior_bioemi_2nd"        = "2nd-order prior: biodiversity vs emissions",
                        "prior_landemi_2nd"       = "2nd-order prior: landscape vs emissions",
                        "complementarity_bioemi"  = "Synergy: biodiversity vs emissions",
                        "complementarity_landemi" = "Synergy: landscape vs emissions"
    )
  )

# Set a fixed order of terms across all panels:
term_levels <- c(
  "Synergy: landscape vs emissions",
  "Synergy: biodiversity vs emissions",
  "2nd-order prior: landscape vs emissions",
  "2nd-order prior: biodiversity vs emissions",
  "1st-order prior: landscape vs emissions",
  "1st-order prior: biodiversity vs emissions"
)
df_forest <- df_forest %>%
  mutate(term_label = factor(term_label, levels = term_levels),
         Model = factor(Model, levels = names(mods)))

# Plot: horizontal forest with one facet per model
fig7 <- ggplot(df_forest, aes(x = estimate, y = term_label)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  facet_wrap(~ Model, ncol = 2, scales = "free_y") +
  geom_vline(xintercept = 0, linetype = 2, color = "grey50") +
  labs(x = "OLS point estimate (95% CI)", y = NULL, title = "") +
  theme_SM() +
  theme(strip.text = element_text(face = "bold"))
fig7
ggsave(fig7, filename = "Plots/fig7.pdf", width = 10, height = 7)

