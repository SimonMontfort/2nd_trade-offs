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

setwd("/Users/simon/Documents/repo/2nd_trade-offs")

# dat_clean <- read.csv("data/dat_clean.csv")
dat_clean <- readRDS("data/dat_clean.RDS")

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
  "treatment_positive_bioemi_and_landemiboth_negative" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\less than expected}",
  "treatment_positive_bioemi_and_landemiboth_positive" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\more than expected}",
  "treatment_positive_bioemi_and_landemimixed" = "Treatment: mixed",
  "treatment_positive_bioemi_and_landemiboth_negative:trust_in_sci" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\less than expected\\\\$\\times$Trust in science}",
  "treatment_positive_bioemi_and_landemiboth_positive:trust_in_sci" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\more than expected\\\\$\\times$Trust in science}",
  "treatment_positive_bioemi_and_landemimixed:trust_in_sci" = "\\shortstack{Treatment: mixed\\\\$\\times$Trust in science}",
  "treatment_positive_bioemi_and_landemiboth_negative:left_right" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\less than expected\\\\$\\times$Left–right ideology}",
  "treatment_positive_bioemi_and_landemiboth_positive:left_right" = "\\shortstack{Treatment: others support\\\\emission reductions\\\\more than expected\\\\$\\times$Left–right ideology}",
  "treatment_positive_bioemi_and_landemimixed:left_right" = "\\shortstack{Treatment: mixed\\\\$\\times$Left–right ideology}"
)

dv_labels <- c(
  prior_bioemi          = "\\rotatebox{90}{Prior biodiversity vs. emissions}",
  prior_landemi         = "\\rotatebox{90}{Prior land use vs. emissions}",
  prior_bioemi_2nd      = "\\rotatebox{90}{2nd order prior biodiversity vs. emissions}",
  prior_landemi_2nd     = "\\rotatebox{90}{2nd order prior land use vs. emissions}",
  post_bioemi           = "\\rotatebox{90}{Posterior biodiversity vs. emissions}",
  post_landemi          = "\\rotatebox{90}{Posterior land use vs. emissions}",
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
lm_prior_bioemi <- lm(prior_bioemi ~ trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_bioemi)

lm_prior_landemi <- lm(prior_landemi ~ trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_landemi)

lm_prior_bioemi_2nd <- lm(prior_bioemi_2nd ~ trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_bioemi_2nd)

lm_prior_landemi_2nd <- lm(prior_landemi_2nd ~ trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)
summary(lm_prior_landemi_2nd)

models <- list(lm_prior_bioemi, lm_prior_landemi,
               lm_prior_bioemi_2nd, lm_prior_landemi_2nd)

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",
  float.pos = "h",
  label   = "table:explaining_priors",
  caption = "Determinants of prior beliefs: ideology, trust in science, and covariates",
  file    = "Tables/explaining_priors.tex"
)


# --- Collect model results into tidy format ---
mods <- list(
  "Biodiversity vs\nemissions\n(1st order)" = lm_prior_bioemi,
  "Land use vs\nemissions\n(1st order)"     = lm_prior_landemi,
  "Biodiversity vs\nemissions\n(2nd order)" = lm_prior_bioemi_2nd,
  "Land use vs\nemissions\n(2nd order)"     = lm_prior_landemi_2nd
)

# Collect results again
df_forest <- purrr::map_dfr(mods, ~tidy(.x, conf.int = TRUE), .id = "Model") %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = recode(term,
                  "trust_in_sci" = "Trust in science",
                  "gender_binarymale" = "Male (ref: female)",
                  "left_right" = "Left–Right ideology",
                  "education_numeric" = "Education",
                  "urban_rural" = "Urban–Rural",
                  "coping_on_incomeYes" = "Income: Yes",
                  "confidence" = "Confidence"
    ),
    term = factor(term, levels = c(
      "Left–Right ideology",
      "Trust in science",
      "Male (ref: female)",
      "education_group",
      "Urban–Rural",
      "Income: No",
      "Income: Yes",
      "Confidence"
    )),
    Model = factor(Model, 
                   levels = rev(c("Biodiversity vs\nemissions\n(1st order)", "Land use vs\nemissions\n(1st order)",
                              "Biodiversity vs\nemissions\n(2nd order)", "Land use vs\nemissions\n(2nd order)")))
  )

df_forest <- df_forest %>%
  mutate(
    label = case_when(
      # Left–Right ideology
      term == "Left–Right ideology" & Model == "Biodiversity vs\nemissions\n(1st order)" ~ 
        "Right-leaning respondents\nprioritize biodiversity over CO₂\nreductions.",
      term == "Left–Right ideology" & Model == "Land use vs\nemissions\n(1st order)" ~ 
        "Right-leaning respondents\nprioritize land use over CO₂\nreductions.",
      term == "Left–Right ideology" & Model == "Biodiversity vs\nemissions\n(2nd order)" ~ 
        "",
      term == "Left–Right ideology" & Model == "Land use vs\nemissions\n(2nd order)" ~ 
        "Right-leaning respondents\nexpect others to prioritize\nCO₂ reductions over land use.",
      
      # Trust in science
      term == "Trust in science" & Model %in% c("Biodiversity vs\nemissions\n(1st order)", 
                                                "Land use vs\nemissions\n(1st order)") ~ 
        "Greater trust in science\nlinked to stronger prioritization\nof biodiversity/land use over CO₂.",
      term == "Trust in science" & Model == "Biodiversity vs\nemissions\n(2nd order)" ~ 
        "",
      term == "Trust in science" & Model == "Land use vs\nemissions\n(2nd order)" ~ 
        "Higher trust in science relates to\n higher expectations that others prioritize\nCO₂ reductions over land use.",
      
      # Gender
      term == "Male (ref: female)" & Model %in% c("Biodiversity vs\nemissions\n(1st order)") ~
        "",
      term == "Male (ref: female)" & Model == "Biodiversity vs\nemissions\n(2nd order)" ~
        "Men expect others to put less\nprioritize biodiversity.",
      term == "Male (ref: female)" & Model == "Land use vs\nemissions\n(2nd order)" ~
        "",
      
      # education_group
      term == "education_group" & Model %in% c("Biodiversity vs\nemissions\n(1st order)") ~
        "Higher education_group is linked\nto lower prioritization\nof emission reductions",
      term == "education_group" & Model %in% c("Land use vs\nemissions\n(1st order)") ~
        "",
      term == "education_group" & Model %in% c(
                                         "Higher educated respondents expect\nothers to prioritize land use") ~
        "education_group has weak or mixed effect\non expectations about others'\nprioritization.",
      
      # Urban–Rural
      term == "Urban–Rural" & Model %in% c("Biodiversity vs\nemissions\n(1st order)", 
                                           "Land use vs\nemissions\n(1st order)",
                                           "Biodiversity vs\nemissions\n(2nd order)") ~
        "",
      term == "Urban–Rural" & Model %in% c("Land use vs\nemissions\n(2nd order)") ~
        "Rural residents think others\nprioritize emissions over\nland use",
      
      # Income
      term == "Income: Yes" & Model %in% c("Land use vs\nemissions\n(1st order)",
                                           "Biodiversity vs\nemissions\n(1st order)", 
                                           "Biodiversity vs\nemissions\n(2nd order)") ~
        "",
      term == "Income: Yes" & Model %in% c("Land use vs\nemissions\n(2nd order)") ~
        "High-income respondents think\nothers priorize land use\nover emission reduction.",
      
      # Confidence
      term == "Confidence" & Model %in% c("Biodiversity vs\nemissions\n(1st order)", 
                                          "Land use vs\nemissions\n(1st order)", 
                                          "Biodiversity vs\nemissions\n(2nd order)") ~
        "",
      term == "Confidence" & Model %in% c("Land use vs\nemissions\n(2nd order)") ~
        "Respondents confident about their\nprior beliefs expect others to\nprioritize land use",
      
      TRUE ~ NA_character_
    )
  )

# Create a data frame of labels positioned at the right
label_df <- df_forest %>%
  distinct(Model, term, label) %>%
  mutate(xpos = 0.23)  # adjust horizontal position

# Plot
fig1 <- ggplot(df_forest, aes(x = estimate, y = Model, color = Model)) +
  geom_point(position = position_dodge(width = 0.6), size = 2.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.6), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_text(data = label_df,
            aes(x = xpos, y = Model, label = label), nudge_y = 0,
            hjust = 0, size = 3.2, lineheight = 0.9) +
  scale_color_npg() +
  labs(x = "Coefficient estimate (95% CI)", y = "",
       title = "Effects of ideology and covariates on first- and second-order priors") +
  facet_wrap(~term, scales = "free_y", ncol = 2) +
  theme_SM() +
  xlim(c(-0.6, 1)) +  # leave space for right-hand labels
  theme(
    legend.position = "bottom",
    axis.text.y = element_text(size = 10),
    axis.ticks.y = element_line()
  )
fig1
ggsave(fig1, file = "Plots/fig1.pdf", width = 11, height = 11)

################################################################################
# II.a) social influence: treatment-control group comparison
################################################################################

lm_influence_bioemi_main <- lm(post_bioemi ~ treatment_group, data = dat_clean)
lm_influence_landemi_main <- lm(post_landemi ~ treatment_group, data = dat_clean)

models <- list(lm_influence_bioemi_main, lm_influence_landemi_main)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:social_influence_treatment_control",
  caption = "Social influence: biodiversity and land-use beliefs, treatment-control comparison",
  file    = "Tables/social_influence_treatment_control.tex"
)

# Main-effect models
mods <- list(
  "Biodiversity conservation\nvs. emission reduction" = lm_influence_bioemi_main,
  "Land use\nvs. emission reduction" = lm_influence_landemi_main
)

# Extract coefficients for treatment effects only
df_forest <- purrr::map_dfr(mods, ~tidy(.x, conf.int = TRUE), .id = "Model") %>%
  filter(term %in% c(
    "treatment_grouptreatment"
  )) %>%
  mutate(
    term_label = case_when(term == "treatment_grouptreatment" ~ "Treatment group"),
    Model = factor(Model, levels = rev(unique(Model)))
  )

# Forest plot
fig2a <- ggplot(df_forest, aes(x = term_label, y = estimate, group = Model)) +
  geom_point(size = 3, position = position_dodge(width = 0.6), col = "black") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), col = "black", 
                width = 0.2, position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~Model, ncol = 2) +
  ylim(-.5,1) +
  labs(x = "2nd order trade-off belief treatment",
       y = "Own trade-off belief",
       title = "Social influence:\ntreatment-control group comparison") +
  theme_SM() +
  theme(legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1))

fig2a

################################################################################
# II.b) social influence: treatment direction
################################################################################

dat_clean <- dat_clean %>% filter(speed)
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
  
sjPlot::plot_model(lm_influence_bioemi_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
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

sjPlot::plot_model(lm_influence_landemi_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")

texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:social_influence_landemi_treatment_direction",
  caption = "Social influence: land-use beliefs, treatment direction",
  file    = "Tables/social_influence_landemi_treatment_direction.tex"
)


# Main-effect models
mods <- list(
  "Biodiversity conservation\nvs. emission reduction" = lm_influence_bioemi_8,
  "Land use\nvs. emission reduction" = lm_influence_landemi_8
)

# Extract coefficients for treatment effects only
df_forest <- purrr::map_dfr(mods, ~tidy(.x, conf.int = TRUE), .id = "Model") %>%
  # filter(term %in% c(
  #   "treatment_positive_bioemi_and_landemiboth_negative", 
  #   "treatment_positive_bioemi_and_landemimixed",
  #   "treatment_positive_bioemi_and_landemiboth_positive"
  # )) %>%
  filter(grepl("treatment_positive", term)) %>% 
  mutate(
    # term_label = case_when(
    #   term == "treatment_positive_bioemi_and_landemiboth_negative" ~ "Overestimated others'\nCO2 reduction preference",
    #   term == "treatment_positive_bioemi_and_landemimixed" ~ "Mixed",
    #   term == "treatment_positive_bioemi_and_landemiboth_positive" ~ "Underestimated others'\nCO2 reduction preference"),
    # term_label = factor(term_label, levels = c("Overestimated others'\nCO2 reduction preference", "Underestimated others'\nCO2 reduction preference", "Mixed")),
    term_label = term,
    Model = factor(Model, levels = rev(unique(Model)))
  )

# Forest plot
fig2b <- ggplot(df_forest, aes(x = term_label, y = estimate, group = term_label, col = rev(term_label))) +
  geom_point(size = 3, position = position_dodge(width = 0.6)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                 width = 0.2, position = position_dodge(width = 0.6)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  facet_wrap(~Model, ncol = 2) +
  # scale_color_manual(values = c("brown", "blue", "grey")) +
  labs(x = "2nd order trade-off belief treatment",
       y = "Own trade-off belief",
       title = "Social influence:\ntreatment direction") +
  # ylim(-.5,1) +
  theme_SM() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

fig2 <- ggarrange(fig2a, fig2b, labels = "auto", align = "hv", widths = c(1,1.5))
ggsave(fig2,  file = "Plots/fig2.pdf", width = 10, height = 5)



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
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
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

# Extract coefficients for treatment effects only
df_forest <- purrr::map_dfr(mods, ~tidy(.x, conf.int = TRUE), .id = "Model") %>%
  filter(term %in% c(
    "treatment_grouptreatment"
  )) %>%
  mutate(
    term_label = case_when(term == "treatment_grouptreatment" ~ "Treatment group"),
    Model = factor(Model, levels = rev(unique(Model)))
  )

# Forest plot
fig3a <- ggplot(df_forest, aes(x = term_label, y = estimate, group = Model, col = Model)) +
  geom_point(size = 3, position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), 
                width = 0.2, position = position_dodge(width = 0.7)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
  # facet_wrap(~Model, ncol = 4) +
  scale_color_npg() +
  ylim(-.9,.7) +
  labs(x = "2nd order trade-off belief treatment",
       y = "Acceptance",
       title = "RE technology acceptance:\ntreatment-control group") +
  facet_wrap(~., nrow = 1) + 
  theme_SM() +
  theme(
        axis.text.x = element_text(angle = 45, hjust = 1))

fig3a

################################################################################
# III.b) acceptance: positive/negative treatment
################################################################################

dat_clean <- dat_clean %>% filter(speeder_treatment_bioemi == F & speeder_treatment_bioemi == F)
# dat_clean <- dat_clean %>% filter(attention_check)
lm_alpinePV_1 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_alpinePV_2 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_alpinePV_3 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_alpinePV_4 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_alpinePV_5 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_alpinePV_6 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_alpinePV_7 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_alpinePV_8 <- lm(acceptance_alpinePV ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean)

lm_wind_1 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_wind_2 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_wind_3 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_wind_4 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_wind_5 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_wind_6 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_wind_7 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_wind_8 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence + age, data = dat_clean)

lm_newnucs_1 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_newnucs_2 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_newnucs_3 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_newnucs_4 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_newnucs_5 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_newnucs_6 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_newnucs_7 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_newnucs_8 <- lm(acceptance_newnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean %>% filter(attention_bioemy_yes, attention_landemi_yes))

lm_prolongnucs_1 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi, data = dat_clean)
lm_prolongnucs_2 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci, data = dat_clean)
lm_prolongnucs_3 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary, data = dat_clean)
lm_prolongnucs_4 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right, data = dat_clean)
lm_prolongnucs_5 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric, data = dat_clean)
lm_prolongnucs_6 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric, data = dat_clean)
lm_prolongnucs_7 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income, data = dat_clean)
lm_prolongnucs_8 <- lm(acceptance_prolongnucs ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence, data = dat_clean %>% filter(attention_bioemy_yes, attention_landemi_yes))

sjPlot::plot_model(lm_alpinePV_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")
sjPlot::plot_model(lm_wind_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")
sjPlot::plot_model(lm_newnucs_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")
sjPlot::plot_model(lm_prolongnucs_8, terms = c("treatment_positive_bioemi_and_landemi"), type = "emm")

# Export tables 
models <- list(lm_alpinePV_1, lm_alpinePV_2, lm_alpinePV_3, lm_alpinePV_4,
               lm_alpinePV_5, lm_alpinePV_6, lm_alpinePV_7, lm_alpinePV_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
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
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
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
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
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
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:acceptance_prolong_nucs_treatment_direction",
  caption = "Acceptance of prolonging nuclear energy technology: treatment direction",
  file    = "Tables/acceptance_prolong_nucs_treatment_direction.tex"
)


# List of models
models <- list(
  alpinePV = lm_alpinePV_8,
  wind = lm_wind_8,
  newnucs = lm_newnucs_8,
  prolongnucs = lm_prolongnucs_8
)

# Tidy results for plotting
fig3b <- lapply(names(models), function(outcome) {
  tidy(models[[outcome]]) %>%
    filter(grepl("treatment_positive", term)) %>%
    mutate(outcome = outcome)}) %>%
  bind_rows() %>%
  mutate(term = case_when(term == "treatment_positive_bioemi_and_landemiboth_negative" ~ "Overestimated others'\nCO2 reduction preference",
                          term == "treatment_positive_bioemi_and_landemimixed" ~ "Mixed",
                          term == "treatment_positive_bioemi_and_landemiboth_positive" ~ "Underestimated others'\nCO2 reduction preference"),
         term = factor(term, levels = c("Overestimated others'\nCO2 reduction preference", "Underestimated others'\nCO2 reduction preference", "Mixed")),
         outcome = case_when(outcome == "alpinePV" ~ "Alpine PV",
                             outcome == "wind" ~ "Wind",
                             outcome == "prolongnucs" ~ "Prolong nuclear",
                             outcome == "newnucs" ~ "New nuclear"),
         outcome = factor(outcome, levels = c("Alpine PV", "Wind", "Prolong nuclear", "New nuclear"))) %>% 
  ggplot(aes(x = term, y = estimate, color = outcome)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error, ymax = estimate + 1.96*std.error),
                width = 0.2, position = position_dodge(width = 0.5)) +
  # scale_color_manual(values = c("brown", "blue", "grey")) +
  scale_color_npg() +
  ylim(-.9,.7) +
  labs(x = "", y = "Renewable energy technology acceptance", color = "Outcome", 
       title = "RE technology acceptance:\ntreatment direction") +
  geom_hline(yintercept = 0, lty = 3) + 
  facet_wrap(~outcome, nrow = 1) + 
  theme_SM() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "none")
fig3b
fig3 <- ggarrange(fig3a, fig3b, labels = "auto", align = "hv", widths = c(1,2.5))
ggsave(fig3,  file = "Plots/fig3.pdf", width = 10, height = 5)

################################################################################
# IV. Robustness checks: speeding -- treatment direction
################################################################################

dat_clean_speed <- dat_clean %>% filter(speeder_treatment_bioemi == F & speeder_treatment_bioemi == F)

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
lm_wind_8 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi + trust_in_sci + gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence + age, data = dat_clean_speed)

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
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:acceptance_alpine_pv_speeders_excluded",
  caption = "Acceptance of alpine PV: treatment direction, speeders excluded",
  file    = "Tables/acceptance_alpine_pv_speeders_excluded.tex"
)

models <-   list(lm_wind_1, lm_wind_2, lm_wind_3, lm_wind_4,
                 lm_wind_5, lm_wind_6, lm_wind_7, lm_wind_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:acceptance_wind_speeders_excluded",
  caption = "Acceptance of wind: treatment direction, speeders excluded",
  file    = "Tables/acceptance_wind_speeders_excluded.tex"
)

models <-   list(lm_newnucs_1, lm_newnucs_2, lm_newnucs_3, lm_newnucs_4,
                 lm_newnucs_5, lm_newnucs_6, lm_newnucs_7, lm_newnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:acceptance_new_nucs_speeders_excluded",
  caption = "Acceptance of new nuclear: treatment direction, speeders excluded",
  file    = "Tables/acceptance_new_nucs_speeders_excluded.tex"
)

models <-   list(lm_prolongnucs_1, lm_prolongnucs_2, lm_prolongnucs_3, lm_prolongnucs_4,
                 lm_prolongnucs_5, lm_prolongnucs_6, lm_prolongnucs_7, lm_prolongnucs_8)
texreg::texreg(
  models,
  custom.model.names = get_dv_labels(models, dv_labels),
  custom.coef.names = get_coef_labels_list(models, coef_labels),  
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:acceptance_prolong_nucs_speeders_excluded",
  caption = "Acceptance of prolonging nuclear: treatment direction, speeders excluded",
  file    = "Tables/acceptance_prolong_nucs_speeders_excluded.tex"
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
                     gender_binary + left_right + education_numeric + urban_rural_numeric + coping_on_income + confidence + age,
                   data = dat_clean)

lm_wind_int2 <- lm(acceptance_wind ~ treatment_positive_bioemi_and_landemi * left_right +
                     trust_in_sci + gender_binary + education_numeric + urban_rural_numeric + coping_on_income + confidence + age,
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
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
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
  custom.note = "Standard errors in parentheses. $^{***}p<0.001$; $^{**}p<0.01$; $^{*}p<0.05$",   float.pos = "h",
  label   = "table:acceptance_interactions_left_right",
  caption = "Acceptance of renewable energy technologies with treatment interactions (left-right ideology)",
  file    = "Tables/acceptance_interactions_left_right.tex"
)

