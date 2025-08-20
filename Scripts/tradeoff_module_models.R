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

####################################################Loading packages ##################################################

library(readxl)
library(writexl)
library(tidyverse)
library(overlapping)
library(ggplot2)

setwd("F:/SWEET/EDGE/WP8 - Survey II/Auswertungen/input_data")


####################################################################################### Load the data from the RDS file
values1 <- readRDS("raw_data_list_04082025_prios.rds")
values <- values1[[1]]

## make excerpt - all values entered, potential speeders included

values_ex1 <- values[,c(156,157,162,163,168,169,174,179,220,225,230, 271, 276,281,290,291,296:299, 
                        304:309,349:388,512,520:529)]

##rename support techs

supp_tech_qs1 <- values_ex1 %>% select(starts_with("Support_techs_"))
colnames(supp_tech_qs1) <- c("Support_techs_alpinePV", "Support_techs_wind", "Support_techs_prolongnuclear", "Support_techs_newnuclear", 
                             "Support_techs_2nd_openspacePV", "Support_techs_2nd_wind", "Support_techs_2nd_prolongnuclear", "Support_techs_2nd_newnuclear", 
                             "Support_techs_2nd_rooftopPV", "Support_techs_2nd_alpinePV")

##merge back with values_ex1
values_ex1_wo <- values_ex1 %>% select(-starts_with("Support_techs_"))
values_ex2 <- bind_cols(values_ex1_wo, supp_tech_qs1)


########################################Descriptives calculations#############################################


##calculate learning difference

values_ex2$treat_own_learn_diff_bioemi <- values_ex2$imp_to_you_1 - values_ex2$learning_bioemi_1
values_ex2$treat_own_learn_diff_emiland <- values_ex2$imp_to_you_2 - values_ex2$learning_emiland_1
values_ex2$control_own_learn_diff_bioemi <- values_ex2$imp_to_you_1 - values_ex2$learning_control_1
values_ex2$control_own_learn_diff_emiland <- values_ex2$imp_to_you_2 - values_ex2$learning_control_2

values_ex2$own_learn_diff_bioemi <- ifelse(is.na(values_ex2$treat_own_learn_diff_bioemi), values_ex2$control_own_learn_diff_bioemi, values_ex2$treat_own_learn_diff_bioemi )
values_ex2$own_learn_diff_emiland <- ifelse(is.na(values_ex2$treat_own_learn_diff_emiland), values_ex2$control_own_learn_diff_emiland, values_ex2$treat_own_learn_diff_emiland)

values_ex2$inverse_treat_own_learn_diff_bioemi <- values_ex2$learning_bioemi_1 -values_ex2$imp_to_you_1
values_ex2$inverse_treat_own_learn_diff_emiland <- values_ex2$learning_emiland_1 -values_ex2$imp_to_you_2
values_ex2$inverse_control_own_learn_diff_bioemi <- values_ex2$learning_control_1 -values_ex2$imp_to_you_1
values_ex2$inverse_control_own_learn_diff_emiland <- values_ex2$learning_control_2 -values_ex2$imp_to_you_2

values_ex2$inverse_own_learn_diff_bioemi <- ifelse(is.na(values_ex2$inverse_treat_own_learn_diff_bioemi), values_ex2$inverse_control_own_learn_diff_bioemi, values_ex2$inverse_treat_own_learn_diff_bioemi )
values_ex2$inverse_own_learn_diff_emiland <- ifelse(is.na(values_ex2$inverse_treat_own_learn_diff_emiland), values_ex2$inverse_control_own_learn_diff_emiland, values_ex2$inverse_treat_own_learn_diff_emiland)


values_ex2$treat_diff_tradeoff_biodiv_updated <- (0-values_ex2$learning_bioemi_1)
values_ex2$treat_diff_tradeoff_emiland_updated <- (1-values_ex2$learning_emiland_1)
values_ex2$control_diff_tradeoff_biodiv_updated <- (0-values_ex2$learning_control_1)
values_ex2$control_diff_tradeoff_emiland_updated <- (1-values_ex2$learning_control_2)

values_ex2$diff_tradeoff_biodiv_updated <- ifelse(is.na(values_ex2$treat_diff_tradeoff_biodiv_updated), values_ex2$control_diff_tradeoff_biodiv_updated, values_ex2$treat_diff_tradeoff_biodiv_updated)
values_ex2$diff_tradeoff_emiland_updated <- ifelse(is.na(values_ex2$treat_diff_tradeoff_emiland_updated), values_ex2$control_diff_tradeoff_emiland_updated, values_ex2$treat_diff_tradeoff_emiland_updated)

##clearer naming
values_ex2$diff_tradeoff_biodiv_orig <- values_ex2$diff_tradeoff_biodiv
values_ex2$diff_tradeoff_emiland_orig <- values_ex2$diff_tradeoff_emiland

###make same col of post-treatment evaluations
values_ex2$bioemi_updated <- coalesce(values_ex2$learning_bioemi_1,values_ex2$learning_control_1)
values_ex2$emiland_updated <- coalesce(values_ex2$learning_emiland_1,values_ex2$learning_control_2)


##########################create learning steps (positive = steps in the right direction) ###############

#######for biodiversity

values_ex2$sign_diff_tradeoff_biodiv_orig <- ifelse(values_ex2$diff_tradeoff_biodiv_orig>0, "pos", ifelse(values_ex2$diff_tradeoff_biodiv_orig<0, "neg", "zero"))
values_ex2$sign_diff_tradeoff_biodiv_updated <- ifelse(values_ex2$diff_tradeoff_biodiv_updated>0, "pos", ifelse(values_ex2$diff_tradeoff_biodiv_updated<0, "neg", "zero"))

values_ex2$sign_diff_tradeoff_biodiv_compared <- ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig == "pos" & values_ex2$sign_diff_tradeoff_biodiv_updated =="pos", "pos_to_pos", 
                                                        ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig == "neg" & values_ex2$sign_diff_tradeoff_biodiv_updated =="neg", "neg_to_neg", 
                                                               ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig=="pos" & values_ex2$sign_diff_tradeoff_biodiv_updated =="neg", "pos_to_neg",
                                                                      ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig=="neg" & values_ex2$sign_diff_tradeoff_biodiv_updated =="pos", "neg_to_pos", 
                                                                             ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig=="zero" & values_ex2$sign_diff_tradeoff_biodiv_updated =="pos", "zero_to_pos", 
                                                                                    ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig=="pos" & values_ex2$sign_diff_tradeoff_biodiv_updated =="zero", "pos_to_zero",
                                                                                           ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig=="zero" & values_ex2$sign_diff_tradeoff_biodiv_updated =="neg", "zero_to_neg", 
                                                                                                  ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig=="neg" & values_ex2$sign_diff_tradeoff_biodiv_updated =="zero", "neg_to_zero",
                                                                                                         ifelse(values_ex2$sign_diff_tradeoff_biodiv_orig=="zero" & values_ex2$sign_diff_tradeoff_biodiv_updated =="zero", "zero_to_zero", NA
                                                                                                         )))))))))

###get same-sign values and make positive numbers as learning (=better)

values_ex2$diff_tradeoff_biodiv_orig <- as.numeric(values_ex2$diff_tradeoff_biodiv_orig)

values_ex2$same_sign_diff_tradeoff_biodiv <- ifelse(values_ex2$sign_diff_tradeoff_biodiv_compared=="pos_to_pos"| values_ex2$sign_diff_tradeoff_biodiv_compared=="neg_to_neg", 
                                                    values_ex2$diff_tradeoff_biodiv_orig-values_ex2$diff_tradeoff_biodiv_updated, NA)

values_ex2$same_sign_diff_tradeoff_biodiv_pos_better <- ifelse(values_ex2$sign_diff_tradeoff_biodiv_compared=="neg_to_neg", -values_ex2$same_sign_diff_tradeoff_biodiv,values_ex2$same_sign_diff_tradeoff_biodiv)

###get different-sign values and make positive numbers as learning (=better)

values_ex2$different_sign_diff_tradeoff_biodiv <- ifelse(values_ex2$sign_diff_tradeoff_biodiv_compared=="neg_to_pos"| values_ex2$sign_diff_tradeoff_biodiv_compared=="pos_to_neg", 
                                                         abs(values_ex2$diff_tradeoff_biodiv_orig)-abs(values_ex2$diff_tradeoff_biodiv_updated), NA)

values_ex2$different_sign_diff_tradeoff_biodiv_pos_better <- values_ex2$different_sign_diff_tradeoff_biodiv

###get zero-to-... values 

values_ex2$zero_to_zero_diff_tradeoff_biodiv <- ifelse(values_ex2$sign_diff_tradeoff_biodiv_compared=="zero_to_zero", 0, NA)

values_ex2$neg_to_zero_or_zero_to_neg_diff_tradeoff_biodiv_pos_better <- ifelse(values_ex2$sign_diff_tradeoff_biodiv_compared=="neg_to_zero"| values_ex2$sign_diff_tradeoff_biodiv_compared=="zero_to_neg", abs(values_ex2$diff_tradeoff_biodiv_orig) - abs(values_ex2$diff_tradeoff_biodiv_updated), NA)

values_ex2$pos_to_zero_or_zero_to_pos_diff_tradeoff_biodiv_pos_better <- ifelse(values_ex2$sign_diff_tradeoff_biodiv_compared=="pos_to_zero"| values_ex2$sign_diff_tradeoff_biodiv_compared=="zero_to_pos", values_ex2$diff_tradeoff_biodiv_orig - values_ex2$diff_tradeoff_biodiv_updated, NA)

####bring it together


values_ex2$diff_tradeoff_biodiv_pos_better<- coalesce(values_ex2$same_sign_diff_tradeoff_biodiv_pos_better,
                                                      values_ex2$different_sign_diff_tradeoff_biodiv_pos_better,
                                                      values_ex2$zero_to_zero_diff_tradeoff_biodiv,
                                                      values_ex2$neg_to_zero_or_zero_to_neg_diff_tradeoff_biodiv_pos_better,
                                                      values_ex2$pos_to_zero_or_zero_to_pos_diff_tradeoff_biodiv_pos_better) 


ex_learning_biodiv1 <- bind_cols("updated_minus_orig_diff"= -values_ex2$own_learn_diff_bioemi,
                                 "change_dir"= values_ex2$sign_diff_tradeoff_biodiv_compared, "change_closer_pos_yes"=values_ex2$diff_tradeoff_biodiv_pos_better)                                                    

table_ex_learning_biodiv_1 <- as.data.frame(table(ex_learning_biodiv1))
table_ex_learning_biodiv_2 <- table_ex_learning_biodiv_1 %>% filter(Freq>=1)

mean_learning_biodiv <- mean(ex_learning_biodiv1$change_closer_pos_yes, na.rm=T)
##0.15 steps (all)

learning_biodiv_change1 <- as.data.frame(table(ex_learning_biodiv1$change_closer_pos_yes))
colnames(learning_biodiv_change1) <- c("biodiv_learning_steps", "freq")


learning_biodiv_change1_pos <- learning_biodiv_change1[c(6:9),]
learning_biodiv_change1_pos2 <- sum(learning_biodiv_change1_pos$freq)
##521

learning_biodiv_change1_neg <- learning_biodiv_change1[c(1:4),]
learning_biodiv_change1_neg2 <- sum(learning_biodiv_change1_neg$freq)
##385

###outputs here
table_ex_learning_biodiv_2 ##updated minus orig diff: to be interpreted as positive number = more Co2 reduction weight, negative = more biodiversity weight given in the update than in the original
mean_learning_biodiv ##the mean person learnt a 0.15 step in the right direction. 
learning_biodiv_change1_pos2 ##number of people that had at least 1 step of learning in the correct direction
learning_biodiv_change1_neg2 ##number of people that had at least 1 step of learning in the wrong direction



###### for emiland

values_ex2$sign_diff_tradeoff_emiland_orig <- ifelse(values_ex2$diff_tradeoff_emiland_orig>0, "pos", ifelse(values_ex2$diff_tradeoff_emiland_orig<0, "neg", "zero"))
values_ex2$sign_diff_tradeoff_emiland_updated <- ifelse(values_ex2$diff_tradeoff_emiland_updated>0, "pos", ifelse(values_ex2$diff_tradeoff_emiland_updated<0, "neg", "zero"))

values_ex2$sign_diff_tradeoff_emiland_compared <- ifelse(values_ex2$sign_diff_tradeoff_emiland_orig == "pos" & values_ex2$sign_diff_tradeoff_emiland_updated =="pos", "pos_to_pos", 
                                                         ifelse(values_ex2$sign_diff_tradeoff_emiland_orig == "neg" & values_ex2$sign_diff_tradeoff_emiland_updated =="neg", "neg_to_neg", 
                                                                ifelse(values_ex2$sign_diff_tradeoff_emiland_orig=="pos" & values_ex2$sign_diff_tradeoff_emiland_updated =="neg", "pos_to_neg",
                                                                       ifelse(values_ex2$sign_diff_tradeoff_emiland_orig=="neg" & values_ex2$sign_diff_tradeoff_emiland_updated =="pos", "neg_to_pos", 
                                                                              ifelse(values_ex2$sign_diff_tradeoff_emiland_orig=="zero" & values_ex2$sign_diff_tradeoff_emiland_updated =="pos", "zero_to_pos", 
                                                                                     ifelse(values_ex2$sign_diff_tradeoff_emiland_orig=="pos" & values_ex2$sign_diff_tradeoff_emiland_updated =="zero", "pos_to_zero",
                                                                                            ifelse(values_ex2$sign_diff_tradeoff_emiland_orig=="zero" & values_ex2$sign_diff_tradeoff_emiland_updated =="neg", "zero_to_neg", 
                                                                                                   ifelse(values_ex2$sign_diff_tradeoff_emiland_orig=="neg" & values_ex2$sign_diff_tradeoff_emiland_updated =="zero", "neg_to_zero",
                                                                                                          ifelse(values_ex2$sign_diff_tradeoff_emiland_orig=="zero" & values_ex2$sign_diff_tradeoff_emiland_updated =="zero", "zero_to_zero", NA
                                                                                                          )))))))))

###get same-sign values and make positive numbers as learning (=better)

values_ex2$diff_tradeoff_emiland_orig <- as.numeric(values_ex2$diff_tradeoff_emiland_orig)

values_ex2$same_sign_diff_tradeoff_emiland <- ifelse(values_ex2$sign_diff_tradeoff_emiland_compared=="pos_to_pos"| values_ex2$sign_diff_tradeoff_emiland_compared=="neg_to_neg", 
                                                     values_ex2$diff_tradeoff_emiland_orig-values_ex2$diff_tradeoff_emiland_updated, NA)

values_ex2$same_sign_diff_tradeoff_emiland_pos_better <- ifelse(values_ex2$sign_diff_tradeoff_emiland_compared=="neg_to_neg", -values_ex2$same_sign_diff_tradeoff_emiland,values_ex2$same_sign_diff_tradeoff_emiland)

###get different-sign values and make positive numbers as learning (=better)

values_ex2$different_sign_diff_tradeoff_emiland <- ifelse(values_ex2$sign_diff_tradeoff_emiland_compared=="neg_to_pos"| values_ex2$sign_diff_tradeoff_emiland_compared=="pos_to_neg", 
                                                          abs(values_ex2$diff_tradeoff_emiland_orig)-abs(values_ex2$diff_tradeoff_emiland_updated), NA)

values_ex2$different_sign_diff_tradeoff_emiland_pos_better <- values_ex2$different_sign_diff_tradeoff_emiland

###get zero-to-... values 

values_ex2$zero_to_zero_diff_tradeoff_emiland <- ifelse(values_ex2$sign_diff_tradeoff_emiland_compared=="zero_to_zero", 0, NA)

values_ex2$neg_to_zero_or_zero_to_neg_diff_tradeoff_emiland_pos_better <- ifelse(values_ex2$sign_diff_tradeoff_emiland_compared=="neg_to_zero"| values_ex2$sign_diff_tradeoff_emiland_compared=="zero_to_neg", abs(values_ex2$diff_tradeoff_emiland_orig) - abs(values_ex2$diff_tradeoff_emiland_updated), NA)

values_ex2$pos_to_zero_or_zero_to_pos_diff_tradeoff_emiland_pos_better <- ifelse(values_ex2$sign_diff_tradeoff_emiland_compared=="pos_to_zero"| values_ex2$sign_diff_tradeoff_emiland_compared=="zero_to_pos", values_ex2$diff_tradeoff_emiland_orig - values_ex2$diff_tradeoff_emiland_updated, NA)

####bring it together


values_ex2$diff_tradeoff_emiland_pos_better<- coalesce(values_ex2$same_sign_diff_tradeoff_emiland_pos_better,
                                                       values_ex2$different_sign_diff_tradeoff_emiland_pos_better,
                                                       values_ex2$zero_to_zero_diff_tradeoff_emiland,
                                                       values_ex2$neg_to_zero_or_zero_to_neg_diff_tradeoff_emiland_pos_better,
                                                       values_ex2$pos_to_zero_or_zero_to_pos_diff_tradeoff_emiland_pos_better) 


ex_learning_emiland1 <- bind_cols("updated_minus_orig_diff"= -values_ex2$own_learn_diff_emiland,
                                  "change_dir"= values_ex2$sign_diff_tradeoff_emiland_compared, "change_closer_pos_yes"=values_ex2$diff_tradeoff_emiland_pos_better)                                                    

table_ex_learning_emiland_1 <- as.data.frame(table(ex_learning_emiland1))
table_ex_learning_emiland_2 <- table_ex_learning_emiland_1 %>% filter(Freq>=1)

mean_learning_emiland <- mean(ex_learning_emiland1$change_closer_pos_yes, na.rm=T)
##0.18 steps (all)

learning_emiland_change1 <- as.data.frame(table(ex_learning_emiland1$change_closer_pos_yes))
colnames(learning_emiland_change1) <- c("emiland_learning_steps", "freq")


learning_emiland_change1_pos <- learning_emiland_change1[c(7:11),]
learning_emiland_change1_pos2 <- sum(learning_emiland_change1_pos$freq)
##574 (all)

learning_emiland_change1_neg <- learning_emiland_change1[c(1:5),]
learning_emiland_change1_neg2 <- sum(learning_emiland_change1_neg$freq)
##388 (all)

###outputs here
table_ex_learning_emiland_2 ##updated minus orig diff: to be interpreted as positive number = more Co2 reduction weight, negative = more landscape protection weight given in the update than in the original
mean_learning_emiland ##the mean person learnt a 0.15 step in the right direction. 
learning_emiland_change1_pos2 ##number of people that had at least 1 step of learning in the correct direction
learning_emiland_change1_neg2 ##number of people that had at least 1 step of learning in the wrong direction

################################################### clean sociodemographics ############################################### 

##################################education

values_ex2 <- values_ex2 %>% mutate(edu_rec=recode(Z9, 
                                                   "1"="1",
                                                   "3"="2",
                                                   "5"="3",
                                                   "8"="4",
                                                   "9"="5",
                                                   "11"="6",
                                                   "12"="7",
                                                   "13"=NA_character_
))

values_ex2$edu_rec <- as.numeric(values_ex2$edu_rec)

####higher value = more highly educated;
unique(values_ex2$Z13)

values_ex2 <- values_ex2 %>% mutate(urbru_rec=recode(Z13, 
                                                     "1"="1",
                                                     "4"="2",
                                                     "5"="3",
                                                     "6"="4"
))

values_ex2$urbru_rec <- as.numeric(values_ex2$urbru_rec) ##höhere Werte = ruraler

values_ex2$income<- as.numeric(values_ex2$Z15) ##höhere Werte = höhere Einkommen


##################################gender

# view the out of qualtrics vars: 
table(values_ex2$Z3, exclude = NULL)

table(values_ex2$Z3, values_ex2$Z3_7_TEXT, exclude = NULL)

# Convert Z3_7_TEXT to lowercase 
values_ex2$Z3_open <- tolower(trimws(values_ex2$Z3_7_TEXT))


values_ex2 <-
  values_ex2 |>
  mutate(
    gender_temp = case_when(
      Z3 %in% c("7", "-99") ~ "other",
      .default = as.character(Z3)) )

table(values_ex2$gender_temp)

table(values_ex2$gender_temp, values_ex2$Z3_7_TEXT)


values_ex2 <-
  values_ex2 |>
  mutate(
    gender1 = case_when( gender_temp == "other"  & Z3_7_TEXT == "-99" ~ "",
                         gender_temp == "other"  & Z3_7_TEXT %in% c(
                           "Muss ich jetzt alle Daten die unter den Datenschutz fallen offen legen?", 
                           "Keine Antwort")   ~ "",
                         .default = as.character(gender_temp)))



values_ex2 <-
  values_ex2 |>
  mutate(
    gender_binary = case_when(
      gender1 == "other"  ~ NA, 
      gender1 == ""  ~ NA, 
      .default = gender1
    )
  ) 

values_ex2$gender_binary <- as.factor(values_ex2$gender_binary)

table(values_ex2$gender_binary , exclude = NULL) ##4 is female, 5 is male


values_ex2 <-
  values_ex2 |>
  mutate(
    gender1 = case_when(
      gender1 == ""  ~ NA, 
      .default = gender1
    )
  ) 
table(values_ex2$gender1,  exclude = NULL)

values_ex2_test_temp <-  values_ex2 |> filter (gender1 == "other" | is.na(gender1))
table(values_ex2_test_temp$Z3_7_TEXT, values_ex2_test_temp$gender1)


values_ex2$gender1 <- as.factor(values_ex2$gender1)
table(values_ex2$gender1,  exclude = NULL)

gender_binary <- as.numeric(values_ex2$gender_binary)


##############################age

class(values_ex2$Z2) # Year of Birth
unique(values_ex2$Z2)
table(values_ex2$Z2,  exclude = NULL)

values_ex2 <- values_ex2 %>%
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

values_ex2$birthyear <- 2025-as.numeric(values_ex2$age)


#age_rec categories (as in EDGE 1)
values_ex2$age_reccat <- NA
values_ex2$age_reccat[values_ex2$age <=30] <- "18-30 years"
values_ex2$age_reccat[values_ex2$age <=40&values_ex2$age>=31] <- "31-40 years"
values_ex2$age_reccat[values_ex2$age <=50&values_ex2$age>=41] <- "41-50 years"
values_ex2$age_reccat[values_ex2$age <=60&values_ex2$age>=51] <- "51-60 years"
values_ex2$age_reccat[values_ex2$age <=70&values_ex2$age>=61] <- "61-70 years"
values_ex2$age_reccat[values_ex2$age <=80&values_ex2$age>=71] <- "71-80 years"
values_ex2$age_reccat[values_ex2$age>=81] <- "> 80 years"
values_ex2$age_reccat <- ordered(values_ex2$age_reccat, levels=c("18-30 years","31-40 years","41-50 years","51-60 years","61-70 years",
                                                                 "71-80 years","> 80 years"))

table(values_ex2$age_reccat)

# label the age groups

values_ex2$age_reccat <- factor(values_ex2$age_reccat,
                                levels = c("18-30 years", "31-40 years",
                                           "41-50 years", "51-60 years",
                                           "61-70 years", "71-80 years", "> 80 years"),
                                labels = c("18-30 years", "31-40 years",
                                           "41-50 years", "51-60 years",
                                           "61-70 years", "71-80 years", "> 80 years"))
values_ex2$age_reccat <- factor(values_ex2$age_reccat, ordered=T)

table(values_ex2$age_reccat, exclude = NULL)

values_ex2$age_reccat_num <- as.numeric(values_ex2$age_reccat) #higher = older age category

#########################################left_right

str(values_ex2$H2_1)

values_ex2$left_right <- as.numeric(values_ex2$H2_1)###Higher values = more right

###################################trust in science

str(values_ex2$H5_Trust_1)
unique(values_ex2$H5_Trust_1)

values_ex2$trust_in_sci <- as.numeric(values_ex2$H5_Trust_1)###Higher values = more trust (0-10)

str(values_ex2$H5_Trust_2nd_1)
unique(values_ex2$H5_Trust_2nd_1)

values_ex2$trust_in_sci_2nd_pp <- as.numeric(values_ex2$H5_Trust_2nd_1)###Higher values = more trust, percentage points 0-100

#############################tradeoff_con_treat

values_ex2$tradeoff_con_treat_treat1 <- ifelse(values_ex2$tradeoff_con_treat=="treat", 1,
                                               ifelse(values_ex2$tradeoff_con_treat=="con", 0,NA))
values_ex2$tradeoff_con_treat_treat1<- as.numeric(values_ex2$tradeoff_con_treat_treat1)


###########################confidence
##biodiv
values_ex2 <- values_ex2 %>% mutate(confidence_biodiv_rec=recode(confidence_biodiv, 
                                                   "1"="5",
                                                   "2"="4",
                                                   "3"="3",
                                                   "4"="2",
                                                   "5"="1",
                                                   .missing=NA_character_))

values_ex2$confidence_biodiv_rec <- as.numeric(values_ex2$confidence_biodiv_rec)

##emiland
values_ex2 <- values_ex2 %>% mutate(confidence_emiland_rec=recode(confidence_emiland, 
                                                                 "1"="5",
                                                                 "2"="4",
                                                                 "3"="3",
                                                                 "4"="2",
                                                                 "5"="1",
                                                                 .missing=NA_character_))

values_ex2$confidence_emiland_rec <- as.numeric(values_ex2$confidence_emiland_rec)


##############################overview over model components##############################################################

#####################################DVs:

#pre-treat
imp_to_you_1 ##bioemi: positive: Co2-reduction is more important
imp_to_you_2 ##emiland: positive: Co2-reduction is more important

#post-treat
bioemi_updated ##post-treatment (=updated) bioemi: positive: Co2-reduction is more important
emiland_updated ##post-treatment (=updated) emiland: positive: Co2-reduction is more important

#difference
inverse_own_learn_diff_bioemi ###updated minus original, positive values indicate that Co2-reduction is valued more strongly in the update than before
inverse_own_learn_diff_emiland ##updated minus original, positive values indicate that Co2-reduction is valued more strongly in the update than before

##learning steps
diff_tradeoff_biodiv_pos_better
diff_tradeoff_emiland_pos_better

###indep vars
confidence_biodiv_rec #higher values = more confident
confidence_emiland_rec # higher values = more confident
left_right #lower values = left
tradeoff_con_treat_treat1 #1=treatment
trust_in_sci #higher values 0-10 more trust
trust_in_sci_2nd_pp #higher values 0-100 more trust
age_reccat #higher category = older
age_rec #age in years
gender_binary #higher=male
urbru_rec #higher values = more rural
income #higher values = richer
edu_rec #higher value = more highly educated


##################################multiple linear regressions###################################################################

####################### prepare combinations

IVnames1 <- c("left_right",
              "tradeoff_con_treat_treat1",
              "trust_in_sci",
              "trust_in_sci_2nd_pp",
              "age_reccat_num",
              "age",
              "gender_binary",
              "urbru_rec",
              "income",
              "edu_rec")

list_combi_IV_names <- list()

for(i in 1:10){
  
  IVnames1_combined <- combn(IVnames1,i)
  
  IVnames1_combined_t <- t(IVnames1_combined)
  
  IVnames1_combined_t_df <- as.data.frame(IVnames1_combined_t)
  
  list_combi_IV_names[[i]] <- IVnames1_combined_t_df
  
}

IVs_1 <- bind_rows(list_combi_IV_names)

IVs_2 <- IVs_1 %>%
  filter(apply(IVs_1, 1, function(row) "tradeoff_con_treat_treat1" %in% row))

IVs_3 <- IVs_2 %>% 
  filter(!apply(IVs_2, 1, function(row) all(c("age", "age_reccat_num") %in% row)))

IVs_4 <- IVs_3 %>% 
  filter(!apply(IVs_3, 1, function(row) all(c("trust_in_sci", "trust_in_sci_2nd_pp") %in% row)))

#export: 
#write.table(IVs_4, "F:/SWEET/EDGE/WP8 - Survey II/Auswertungen/input_data/ivs_1.csv", sep=",")

IVs_5 <- read.csv("ivs_1.csv")

###############################automate modelling

##for bioemi

#for first DV

DV_bioemi_orig_list <- list()

for (i in 1:144){
  
  y="imp_to_you_1"
  x <- noquote(IVs_5[i,2]) #!!!!!! BEWARE!!!! CORRECT ENTRY!!!!
  
  auto_formula <- as.formula(paste(y,paste(x), sep="~"))
  
  model_auto <- lm(auto_formula,
                   data=values_ex2)
  
  model_auto_summary1 <- summary(model_auto)
  
  coefs1 <- as.data.frame(model_auto_summary1$coefficients)
  coefs2 <- rownames_to_column(coefs1)
  colnames(coefs2)[1] <- "varname"
  
  rsq<- as.data.frame(model_auto_summary1$r.squared)
  colnames(rsq) <- "r_squared"
  
  adjrsq <- as.data.frame(model_auto_summary1$adj.r.squared)
  colnames(adjrsq) <- "adjusted_r_squared"
  
  
  model_auto_summary_pro_Modell <- cbind("model_no"=i, "DV"="bioemi_orig", coefs2, rsq, adjrsq, "IV_components"=IVs_5[i,2], "tradeoff"="bioemi")
  
  DV_bioemi_orig_list[[i]] <- model_auto_summary_pro_Modell
}

DV_bioemi_orig_reg_df <- bind_rows(DV_bioemi_orig_list)

#for second DV

DV_bioemi_upd_list <- list()

for (i in 1:144){
  
  y="bioemi_updated"
  x <- noquote(IVs_5[i,2]) #!!!!!! BEWARE!!!! CORRECT ENTRY!!!!
  
  auto_formula <- as.formula(paste(y,paste(x), sep="~"))
  
  model_auto <- lm(auto_formula,
                   data=values_ex2)
  
  model_auto_summary1 <- summary(model_auto)
  
  coefs1 <- as.data.frame(model_auto_summary1$coefficients)
  coefs2 <- rownames_to_column(coefs1)
  colnames(coefs2)[1] <- "varname"
  
  rsq<- as.data.frame(model_auto_summary1$r.squared)
  colnames(rsq) <- "r_squared"
  
  adjrsq <- as.data.frame(model_auto_summary1$adj.r.squared)
  colnames(adjrsq) <- "adjusted_r_squared"
  
  
  model_auto_summary_pro_Modell <- cbind("model_no"=i, "DV"="bioemi_upd", coefs2, rsq, adjrsq, "IV_components"=IVs_5[i,2], "tradeoff"="bioemi")
  
  DV_bioemi_upd_list[[i]] <- model_auto_summary_pro_Modell
}

DV_bioemi_upd_reg_df <- bind_rows(DV_bioemi_upd_list)

#for third DV

DV_bioemi_diff_list <- list()

for (i in 1:144){
  
  y="inverse_own_learn_diff_bioemi"
  x <- noquote(IVs_5[i,2]) #!!!!!! BEWARE!!!! CORRECT ENTRY!!!!
  
  auto_formula <- as.formula(paste(y,paste(x), sep="~"))
  
  model_auto <- lm(auto_formula,
                   data=values_ex2)
  
  model_auto_summary1 <- summary(model_auto)
  
  coefs1 <- as.data.frame(model_auto_summary1$coefficients)
  coefs2 <- rownames_to_column(coefs1)
  colnames(coefs2)[1] <- "varname"
  
  rsq<- as.data.frame(model_auto_summary1$r.squared)
  colnames(rsq) <- "r_squared"
  
  adjrsq <- as.data.frame(model_auto_summary1$adj.r.squared)
  colnames(adjrsq) <- "adjusted_r_squared"
  
  
  model_auto_summary_pro_Modell <- cbind("model_no"=i, "DV"="bioemi_diff", coefs2, rsq, adjrsq, "IV_components"=IVs_5[i,2], "tradeoff"="bioemi")
  
  DV_bioemi_diff_list[[i]] <- model_auto_summary_pro_Modell
}

DV_bioemi_diff_reg_df <- bind_rows(DV_bioemi_diff_list)


###for emiland

#for first DV

DV_emiland_orig_list <- list()

for (i in 1:144){
  
  y="imp_to_you_2"
  x <- noquote(IVs_5[i,2]) #!!!!!! BEWARE!!!! CORRECT ENTRY!!!!
  
  auto_formula <- as.formula(paste(y,paste(x), sep="~"))
  
  model_auto <- lm(auto_formula,
                   data=values_ex2)
  
  model_auto_summary1 <- summary(model_auto)
  
  coefs1 <- as.data.frame(model_auto_summary1$coefficients)
  coefs2 <- rownames_to_column(coefs1)
  colnames(coefs2)[1] <- "varname"
  
  rsq<- as.data.frame(model_auto_summary1$r.squared)
  colnames(rsq) <- "r_squared"
  
  adjrsq <- as.data.frame(model_auto_summary1$adj.r.squared)
  colnames(adjrsq) <- "adjusted_r_squared"
  
  
  model_auto_summary_pro_Modell <- cbind("model_no"=i, "DV"="emiland_orig", coefs2, rsq, adjrsq, "IV_components"=IVs_5[i,2], "tradeoff"="emiland")
  
  DV_emiland_orig_list[[i]] <- model_auto_summary_pro_Modell
}

DV_emiland_orig_reg_df <- bind_rows(DV_emiland_orig_list)

#for second DV

DV_emiland_upd_list <- list()

for (i in 1:144){
  
  y="emiland_updated"
  x <- noquote(IVs_5[i,2]) #!!!!!! BEWARE!!!! CORRECT ENTRY!!!!
  
  auto_formula <- as.formula(paste(y,paste(x), sep="~"))
  
  model_auto <- lm(auto_formula,
                   data=values_ex2)
  
  model_auto_summary1 <- summary(model_auto)
  
  coefs1 <- as.data.frame(model_auto_summary1$coefficients)
  coefs2 <- rownames_to_column(coefs1)
  colnames(coefs2)[1] <- "varname"
  
  rsq<- as.data.frame(model_auto_summary1$r.squared)
  colnames(rsq) <- "r_squared"
  
  adjrsq <- as.data.frame(model_auto_summary1$adj.r.squared)
  colnames(adjrsq) <- "adjusted_r_squared"
  
  
  model_auto_summary_pro_Modell <- cbind("model_no"=i, "DV"="emiland_upd", coefs2, rsq, adjrsq, "IV_components"=IVs_5[i,2], "tradeoff"="emiland")
  
  DV_emiland_upd_list[[i]] <- model_auto_summary_pro_Modell
}

DV_emiland_upd_reg_df <- bind_rows(DV_emiland_upd_list)

#for third DV

DV_emiland_diff_list <- list()

for (i in 1:144){
  
  y="inverse_own_learn_diff_emiland"
  x <- noquote(IVs_5[7,2]) #!!!!!! BEWARE!!!! CORRECT ENTRY!!!!
  
  auto_formula <- as.formula(paste(y,paste(x), sep="~"))
  
  model_auto <- lm(auto_formula,
                   data=values_ex2)
  
  model_auto_summary1 <- summary(model_auto)
  
  coefs1 <- as.data.frame(model_auto_summary1$coefficients)
  coefs2 <- rownames_to_column(coefs1)
  colnames(coefs2)[1] <- "varname"
  
  rsq<- as.data.frame(model_auto_summary1$r.squared)
  colnames(rsq) <- "r_squared"
  
  adjrsq <- as.data.frame(model_auto_summary1$adj.r.squared)
  colnames(adjrsq) <- "adjusted_r_squared"
  
  
  model_auto_summary_pro_Modell <- cbind("model_no"=i, "DV"="emiland_diff", coefs2, rsq, adjrsq, "IV_components"=IVs_5[i,2], "tradeoff"="emiland")
  
  DV_emiland_diff_list[[i]] <- model_auto_summary_pro_Modell
}

DV_emiland_diff_reg_df <- bind_rows(DV_emiland_diff_list)

###########################################bring modelsummaries together

summaries1 <- bind_rows(
  DV_bioemi_orig_reg_df,
  DV_bioemi_upd_reg_df,
  DV_bioemi_diff_reg_df,
  DV_emiland_orig_reg_df,
  DV_emiland_upd_reg_df,
  DV_emiland_diff_reg_df)

summaries2 <- summaries1 %>% left_join(IVs_5, by=c("IV_components"="Combined_cleaned"))

#write_xlsx(summaries2,"F:/SWEET/EDGE/WP8 - Survey II/Auswertungen/input_data/lm_model_summaries_v1.xlsx" )


##############################################explaining prior beliefs###################################################

#biodiv
prior_beliefs_biodiv1 <- lm(imp_to_you_1 ~ confidence_biodiv_rec + left_right+trust_in_sci, data=values_ex2)
summary(prior_beliefs_biodiv1)

#int: left-right neg: the more right you are, the lower the Co2-reduction emphasis (the greater the biodiversity emphasis)

#emiland
prior_beliefs_emiland1 <- lm(imp_to_you_2 ~ confidence_emiland_rec + left_right+trust_in_sci, data=values_ex2)
summary(prior_beliefs_emiland1)

#int: left-right neg: the more right you are, the lower the Co2-reduction emphasis (the greater the land protection emphasis)


#######################################Learning

####DV updated-orig beliefs

#biodiv
learning_biodiv1 <- lm(inverse_own_learn_diff_bioemi ~ tradeoff_con_treat_treat1+ confidence_biodiv_rec + left_right+trust_in_sci, data=values_ex2)
summary(learning_biodiv1)

#int: confidence_biodiv_rec neg: the more confident you are, the lower the difference, or the higher respondents value biodiversity in the updated preferences 
#int: trust in sci: the higher the trust in science, the larger the difference, or the stronger respondents value Co2-reduction in the updated preferences

#emiland
learning_emiland1 <- lm(inverse_own_learn_diff_emiland ~ tradeoff_con_treat_treat1+ confidence_emiland_rec + left_right+trust_in_sci, data=values_ex2)
summary(learning_emiland1)

#int: left_right neg: the more right-wing you are, the lower the difference, or the higher respondents value landscape protection in the updated preferences 
#int: trust in sci: the higher the trust in science, the larger the difference, or the stronger respondents value Co2-reduction in the updated preferences


####DV learning steps

#biodiv
learning_biodiv2 <- lm(diff_tradeoff_biodiv_pos_better ~ tradeoff_con_treat_treat1+ confidence_biodiv_rec + left_right+trust_in_sci, data=values_ex2)
summary(learning_biodiv2)

#int: confidence_biodiv_rec neg: the more confident you are, the lower the difference, or the higher respondents value biodiversity in the updated preferences 
#int: trust in sci: the higher the trust in science, the larger the difference, or the stronger respondents value Co2-reduction in the updated preferences

#emiland
learning_emiland2 <- lm(diff_tradeoff_emiland_pos_better ~ tradeoff_con_treat_treat1+ confidence_emiland_rec + left_right+trust_in_sci, data=values_ex2)
summary(learning_emiland2)

#int: left_right neg: the more right-wing you are, the lower the difference, or the higher respondents value landscape protection in the updated preferences 
#int: trust in sci: the higher the trust in science, the larger the difference, or the stronger respondents value Co2-reduction in the updated preferences


##################################Belief updating

######biodiv
belief_updating_biodiv1 <- lm(inverse_own_learn_diff_bioemi ~ tradeoff_con_treat_treat1, data=values_ex2)
summary(belief_updating_biodiv1)

#int: nothing

belief_updating_biodiv2 <- lm(inverse_own_learn_diff_bioemi ~ tradeoff_con_treat_treat1*confidence_biodiv_rec, data=values_ex2)
summary(belief_updating_biodiv2)

#int: nothing

belief_updating_biodiv3 <- lm(inverse_own_learn_diff_bioemi ~ tradeoff_con_treat_treat1*left_right, data=values_ex2)
summary(belief_updating_biodiv3)

#int: nothing

belief_updating_biodiv4 <- lm(inverse_own_learn_diff_bioemi ~ tradeoff_con_treat_treat1*trust_in_sci, data=values_ex2)
summary(belief_updating_biodiv4)

#int: nothing

belief_updating_biodiv5 <- lm(inverse_own_learn_diff_bioemi ~ tradeoff_con_treat_treat1*trust_in_sci + tradeoff_con_treat_treat1*left_right + tradeoff_con_treat_treat1*trust_in_sci, data=values_ex2)
summary(belief_updating_biodiv5)

#int: nothing

######emiland
belief_updating_emiland1 <- lm(inverse_own_learn_diff_emiland ~ tradeoff_con_treat_treat1, data=values_ex2)
summary(belief_updating_emiland1)

#int: nothing

belief_updating_emiland2 <- lm(inverse_own_learn_diff_emiland ~ tradeoff_con_treat_treat1*confidence_emiland_rec, data=values_ex2)
summary(belief_updating_emiland2)

#int: nothing

belief_updating_emiland3 <- lm(inverse_own_learn_diff_emiland ~ tradeoff_con_treat_treat1*left_right, data=values_ex2)
summary(belief_updating_emiland3)

#int: nothing

belief_updating_emiland4 <- lm(inverse_own_learn_diff_emiland ~ tradeoff_con_treat_treat1*trust_in_sci, data=values_ex2)
summary(belief_updating_emiland4)

#int: nothing

belief_updating_emiland5 <- lm(inverse_own_learn_diff_emiland ~ tradeoff_con_treat_treat1*trust_in_sci + tradeoff_con_treat_treat1*left_right + tradeoff_con_treat_treat1*trust_in_sci, data=values_ex2)
summary(belief_updating_emiland5)

#int: nothing
