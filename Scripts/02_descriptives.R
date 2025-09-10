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

setwd("/Users/simon/Documents/repo/2nd_trade-offs")

# dat_clean_clean <- read.csv("dat_cleana/dat_clean_clean.csv")
dat_clean_clean <- readRDS("dat_cleana/dat_clean_clean.RDS")

readRDS("dat_cleana/vars_of_interest.RDS")

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
                                           size=.3, linetype="solid", 
                                           colour ="grey")
    )
}


################################################################################
# plot counts and NAs
################################################################################

dat_clean_desc <- dat_clean 

p_desc <- dat_clean_desc %>% 
  mutate_all(.funs = function(x){as.character(as.numeric(x))}) %>% 
  pivot_longer(., everything(), names_to = "Question", values_to = "Response") %>% 
  mutate(Question = factor(Question, levels = unique(Question))) %>% 
  group_by(Question, Response) %>% 
  count(name = "freq") %>% 
  mutate(Response = factor(Response, levels = as.character(c(0:14, NA)))) %>% 
  ggplot(aes(x = Response, y = freq)) +
  geom_col() + labs(x = "", y = "") +
  facet_wrap(~Question, scales = "free_x", 
             # labeller = labeller(Question = c("driver" = "Driver",
             #                                  "educ" = "Education",
             #                                  "age" = "Age",
             #                                  "empl_sect" = "Employment Sector",
             #                                  "fin_cond" = "Financial Condition",
             #                                  "home_owner" = "Home Owner",
             #                                  "left_right" = "Left−Right",
             #                                  "prior_benefit_2" = "Perceived Effectiveness\n of Prior Benefits",
             #                                  "rate" = "Support \n(Rate Outcome)",
             #                                  "choice" = "Support \n(Choice Outcome)",
             #                                  "ratio_ev_to_muni_area" = "EV Charging Stations",
             #                                  "sal_env" = "Salience: \nEnvironment and Climate",
             #                                  "sal_glob" = "Salience: \nGlobalisation",
             #                                  "language" = "French",
             #                                  "Urban Area" = "Urban Area",
             #                                  "Intermediate Area" = "Intermediate Area",
             #                                  "Rural Area"  = "Rural Area",
             #                                  "Region: Geneva" = "Region: Geneva",
             #                                  "Region: Middle Land" = "Region: Middle Land",
             #                                  "Region: North East" = "Region: North East",
             #                                  "Region: Zurich" = "Region: Zurich",
             #                                  "Region: East" = "Region: East",
             #                                  "Region: Central" = "Region: Central",
             #                                  "Region: Ticino" = "Region: Ticino"))
             ) +
  theme_SM()
p_desc
ggsave(p_desc, filename = "Plots/p_desc.pdf", height = 16, width = 10)

################################################################################
# summary statistics table: distirbution (eg. min max mean)
################################################################################

labs_desc <- c("Support (Rate Outcome)", "Support (Choice Outcome)", "Perceived Effectiveness of Prior Benefits",
               "EV Charging Stations",  "Driver", "Home Owner", "Age", "Education", "French",
               "Employment Sector", "Financial Condition", "Left-Right", "Salience: Globalisation", "Salience: Environment and Climate",
               "Region: Geneva", "Region: Middle Land", "Region: North East", "Region: Zurich", "Region: East", "Region: Central", "Region: Ticino", "Urban Area", "Intermediate Area", "Rural Area")

library(stargazer)
stargazer(dat_clean_desc,
          out.header = F,
          no.space = TRUE, 
          label = "tab:summary_stats",
          column.sep.width = "3pt",
          font.size = "footnotesize",
          covariate.labels = labs_desc,
          out = "Tables/summary_stats.tex"
)

################################################################################
# correlation table
################################################################################

library(Hmisc)
correlation_matrix <- cor(dat_clean_desc %>% mutate_all(., as.numeric), use = "pairwise.complete.obs")
correlation_matrix <- round(correlation_matrix, 2)
correlation_matrix[upper.tri(correlation_matrix)] <- NA
diag(correlation_matrix) <- NA
colnames(correlation_matrix) <- rownames(correlation_matrix) <- labs_desc
correlation_matrix1 <- correlation_matrix[,1:12]
correlation_matrix2 <- correlation_matrix[,13:ncol(correlation_matrix)]

stargazer(correlation_matrix1, title="Correlation Matrix Part 1", 
          float.env = "sidewaystable", 
          type = "latex", 
          out.header = F,
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "1pt", # to reduce column width
          font.size = "footnotesize", # to make font size smaller
          label = "tab:correlation_pt1",
          out = "Tables/correlation_pt1.tex"
          # covariate.labels = labs_desc,
          # dep.var.labels = labs_desc
)
stargazer(correlation_matrix2, title="Correlation Matrix  Part 2", 
          float.env = "sidewaystable", 
          type = "latex", 
          out.header = F,
          no.space = TRUE, # to remove the spaces after each line of coefficients
          column.sep.width = "1pt", # to reduce column width
          font.size = "footnotesize", # to make font size smaller
          label = "tab:correlation_pt2",
          out = "Tables/correlation_pt2.tex"
)

