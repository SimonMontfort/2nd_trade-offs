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

###################################################### variable labels and interpretation################################# 

#imp_to_you_1 = own_tradeoff_biodiv = 
#QiD: 370; positive values =Co2-reduction is more important than higher biodiv

#imp_to_you_2 = own_tradeoff_emiland = 
#QiD: 370; positive values =Co2-reduction is more important than higher landscape protection

##complementary_1
#positive values indicate that the respondent sees biodiv and emission reduction as complementary goals

##complementary_2
#positive values indicate that the respondent sees lower land-use and emission reduction as complementary goals

#imp_to_avg_Swi_1 = e_tradeoff_biodiv = 
#QiD:374; positive values =Co2-reduction is more important than higher biodiv for the majority of Swiss residents

#imp_to_avg_Swi_2 = e_tradeoff_emiland = 
#QiD:374; positive values =Co2-reduction is more important than higher landscape protection for the majority of Swiss residents

#att_check_1_bioemi = value=2 is correct for those treat that have had exposure to bio_emi_pos4 to 1
#value=1 is correct for those treat that have had exposure to bio_emi_negpos0.

#att_check_2_bioemi = value=1 is correct

##learning_bioemi = treat group own updating of opinion on bio vs. Co2. Higher values indicate
#that the respondent holds Co2-emission reductions to be more important than biodiv

#att_check_1_emiland = value=1 is correct (Co2)
#att_check_2_emiland = value=2 is correct (land-use)

##learning_emiland = treat group own updating of opinion on land-use vs. Co2. Higher values indicate
#that the respondent holds Co2-emission reductions to be more important than land-use protection

##learning_control_1 = biodiv/Co2 trade-off, positive values indicate that
#the respondent holds Co2-emission reductions to be more important than biodiv

##learning_control_2 = landscape protection/Co2 trade-off, positive values indicate that
#the respondent holds Co2-emission reductions to be more important than landscape protection

##diff_tradeoff_biodiv_orig = difference in steps between most-selected category and the original self-selected category in the tradeoff
#negative values indicate that the respondent has a trade-off position that values Co2-emission reduction more strongly
#than the most-selected category

#diff_tradeoff_biodiv_updated = difference in steps between most-selected category and self-selected updated/learnt! category
#Values indicate the respondent's updated trade-off position vis-à-vis the most selected category.
#Positive values show that the updated response values biodiversity more strongly than the most selected category
#Negative values show that the updated response values Co2 more strongly than the most selected category.

##diff_tradeoff_emiland_orig = difference in steps between most-selected category and self-selected category in the tradeoff
#a 1 indicates that the respondent values Co2 slightly less than the most selected category in the trade-off
#negative values indicate that the respondent has a trade-off position that values Co2-emission reduction more strongly
#than the most-selected category
#positive values indicate that the respondent has a trade-off position that values Co2-emission reduction less strongly
#than the most-selected category

#diff_tradeoff_emiland_updated = difference in steps between most-selected category and self-selected updated/learnt! category
#Values indicate the respondent's updated trade-off position vis-à-vis the most selected category.
#A value of 1 indicates that land-use protection is valued more strongly vis-à-vis the most selected category.
#Positive values show that the updated response values landscape protection more strongly than the most selected category
#Negative values show that the updated response values Co2 more strongly than the most selected category.


##diff_tradeoff_biodiv_others = difference in steps between most-selected category and the self-selected estimate of others in the tradeoff
#negative values indicate that the respondent views others to value Co2-emission reduction more strongly
#than the most-selected category

##diff_tradeoff_emiland_others = difference in steps between most-selected category and self-selected estimate of others in the tradeoff
#a 1 indicates that the respondent thinks others value Co2 slightly less than the most selected category in the trade-off
#negative values indicate that the respondent views others to value Co2-emission reduction more strongly
#than the most-selected category
#positive values indicate that the respondent views others to value Co2-emission reduction less strongly
#than the most-selected category

##own_learn_diff_bioemi = difference of first with updated opinion on self regarding bioemi-tradeoff.
#Positive values indicate that biodiversity is valued more strongly in the update than before
#Negative values indicate that Co2 is valued more strongly in the update than before 

##own_learn_diff_emiland = difference of first with updated opinion on self regarding emiland-tradeoff.
#Positive values indicate that landscape protection is valued more strongly in the update than before
#Negative values indicate that Co2 is valued more strongly in the update than before

##inverse_own_learn_diff_bioemi = difference of updated minus original opinion on self regarding bioemi-tradeoff.
#Positive values indicate that Co2 reduction is valued more strongly in the update than before
#Negative values indicate that biodiversity is valued more strongly in the update than before 

##inverse_own_learn_diff_emiland = difference of updated opinion minus original opinion on self regarding emiland-tradeoff.
#Positive values indicate that Co2 reduction is valued more strongly in the update than before
#Negative values indicate that landscape protection is valued more strongly in the update than before


#######################################################Evaluate Learning: Plots ############################################

####check whether the differences between self-first with most common-cat and self-updated with most common cat. are becoming smaller

###for all controls and treatment groups!

values_ex2 <- values_ex2 %>% filter(tradeoff_con_treat=="treat")

#diff_tradeoff_biodiv_orig vs. diff_tradeoff_biodiv_updated

values_ex2$diff_tradeoff_biodiv_orig <- as.numeric(values_ex2$diff_tradeoff_biodiv_orig)
values_ex2$diff_tradeoff_biodiv_updated <- as.numeric(values_ex2$diff_tradeoff_biodiv_updated)

summary_diff_tradeoff_biodiv_orig <- as.data.frame(table(values_ex2$diff_tradeoff_biodiv_orig))
colnames(summary_diff_tradeoff_biodiv_orig) <- c("bioemi_scale", "n")
summary_diff_tradeoff_biodiv_orig$sum <-sum(summary_diff_tradeoff_biodiv_orig$n, na.rm=T)
summary_diff_tradeoff_biodiv_orig$percent_orig <- summary_diff_tradeoff_biodiv_orig$n/summary_diff_tradeoff_biodiv_orig$sum

summary_diff_tradeoff_biodiv_updated <- as.data.frame(table(values_ex2$diff_tradeoff_biodiv_updated))
colnames(summary_diff_tradeoff_biodiv_updated) <- c("bioemi_scale", "n")
summary_diff_tradeoff_biodiv_updated$sum <-sum(summary_diff_tradeoff_biodiv_updated$n, na.rm=T)
summary_diff_tradeoff_biodiv_updated$percent_upd <- summary_diff_tradeoff_biodiv_updated$n/summary_diff_tradeoff_biodiv_updated$sum

summary_diff_tradeoff_biodiv2 <- left_join(summary_diff_tradeoff_biodiv_orig, summary_diff_tradeoff_biodiv_updated, by=c("bioemi_scale"))
summary_diff_tradeoff_biodiv3 <- summary_diff_tradeoff_biodiv2[,c(1,4,7)]
summary_diff_tradeoff_biodiv4 <- summary_diff_tradeoff_biodiv3 %>% pivot_longer(c(2:3), names_to="names", values_to="percent")

p <- ggplot(summary_diff_tradeoff_biodiv4, aes(x = bioemi_scale, y = percent, fill=names)) +
  geom_bar(stat = "identity", position="dodge", alpha = 1) +  # half-transparent bars
  labs(
    x = "Difference of own preference to the most selected preference",     # Custom x-axis label
    y = "% of respondents",  # Custom y-axis label
    title = "CO2 (neg.) vs. Biodiversity (pos)."
  ) +
  theme_minimal()+
  scale_fill_brewer(name = 'Level of Approval', palette = 'YlGnBu', 
                    labels = c( "Pre", "Post")) +
  theme(axis.title.x=element_text(margin=margin(t=15)))+
  theme(legend.position = "bottom")

  
##interpretation of X-Axis: It shows the difference in most frequently chosen category - self original, vs. most frequently chosen category-self updated. 
#The negative numbers indicate that in updated self, CO2 is more highly valued
#Positive numbers indicate that in self updated, landscape protection is more highly valued

p_summary_diff_tradeoff_biodiv_all
p_summary_diff_tradeoff_biodiv_control
p_summary_diff_tradeoff_biodiv_treat 
<- p
## here, if the updated shows a negative % (y-axis), it means that relatively more people in the update have selected this category
##a negative turn towards the center (-1,0,1) is expected, as more people have smaller differences to the most selected category due to learning

#diff_tradeoff_emiland_orig vs. diff_tradeoff_emiland_updated

values_ex2$diff_tradeoff_emiland_orig <- as.numeric(values_ex2$diff_tradeoff_emiland_orig)
values_ex2$diff_tradeoff_emiland_updated <- as.numeric(values_ex2$diff_tradeoff_emiland_updated)

summary_diff_tradeoff_emiland_orig <- as.data.frame(table(values_ex2$diff_tradeoff_emiland_orig))
colnames(summary_diff_tradeoff_emiland_orig) <- c("emiland_scale", "n")
summary_diff_tradeoff_emiland_orig$sum <-sum(summary_diff_tradeoff_emiland_orig$n, na.rm=T)
summary_diff_tradeoff_emiland_orig$percent_orig <- summary_diff_tradeoff_emiland_orig$n/summary_diff_tradeoff_emiland_orig$sum

summary_diff_tradeoff_emiland_updated <- as.data.frame(table(values_ex2$diff_tradeoff_emiland_updated))
colnames(summary_diff_tradeoff_emiland_updated) <- c("emiland_scale", "n")
summary_diff_tradeoff_emiland_updated$sum <-sum(summary_diff_tradeoff_emiland_updated$n, na.rm=T)
summary_diff_tradeoff_emiland_updated$percent_upd <- summary_diff_tradeoff_emiland_updated$n/summary_diff_tradeoff_emiland_updated$sum

summary_diff_tradeoff_emiland2 <- left_join(summary_diff_tradeoff_emiland_orig, summary_diff_tradeoff_emiland_updated, by=c("emiland_scale"))

summary_diff_tradeoff_emiland3 <- summary_diff_tradeoff_emiland2[,c(1,4,7)]
summary_diff_tradeoff_emiland4 <- summary_diff_tradeoff_emiland3 %>% pivot_longer(c(2:3), names_to="names", values_to="percent")

p <- ggplot(summary_diff_tradeoff_emiland4, aes(x = emiland_scale, y = percent, fill=names)) +
  geom_bar(stat = "identity", position="dodge", alpha = 1) +  # half-transparent bars
  labs(
    x = "Difference of own preference to the most selected preference",     # Custom x-axis label
    y = "% of respondents",  # Custom y-axis label
    title = "CO2 (neg.) vs. Landscape protection (pos)."
  ) +
  theme_minimal()+
  scale_fill_brewer(name = 'Level of Approval', palette = 'YlGnBu', 
                    labels = c( "Pre", "Post")) +
  theme(axis.title.x=element_text(margin=margin(t=15)))+
  theme(legend.position = "bottom")


##interpretation of X-Axis: It shows the difference in most frequently chosen category minus self original, vs. most frequently chosen category minus self updated. 
#The negative numbers indicate that in updated self, CO2 is more highly valued
#Positive numbers indicate that in self updated, biodiversity is more highly valued

p_summary_diff_tradeoff_emiland_all 
p_summary_diff_tradeoff_emiland_control
p_summary_diff_tradeoff_emiland_treat 
<- p
## here, if the updated shows a negative %, it means that relatively more people in the update have selected this category
##a negative turn towards the center (-1,0,1) is expected, as more people have smaller differences to the most selected category due to learning

#####put the graphs together

#for biodiv

summary_treat_biodiv1$group <- "treat"
summary_all_biodiv1$group <- "all"
summary_control_biodiv1$group <- "control"
summary_biodiv1 <- bind_rows(summary_treat_biodiv1, summary_all_biodiv1, summary_control_biodiv1)

summary_biodiv1$names_conc <- paste0(summary_biodiv1$names,"_", summary_biodiv1$group)
summary_biodiv2 <- summary_biodiv1[,c(1,3,5)]
##for emiland

summary_treat_emiland1$group <- "treat"
summary_all_emiland1$group <- "all"
summary_control_emiland1$group <- "control"
summary_emiland1 <- bind_rows(summary_treat_emiland1, summary_all_emiland1, summary_control_emiland1)

summary_emiland1$names_conc <- paste0(summary_emiland1$names,"_", summary_emiland1$group)
summary_emiland2 <- summary_emiland1[,c(1,3,5)]


#####graph both of them

summary_biodiv2 <- summary_biodiv1 %>% filter(names_conc!="percent_orig_all" & names_conc!="percent_upd_all" )

p <- ggplot(summary_biodiv2, aes(x = bioemi_scale, y = percent, fill=names_conc)) +
  geom_bar(stat = "identity", position="dodge", alpha = 1) +  # half-transparent bars
  labs(
    x = "Difference of own preference to the most selected preference",     # Custom x-axis label
    y = "share of respondents",  # Custom y-axis label
    title = "CO2 reduction (neg.) vs. Biodiversity (pos.)"
  ) +
  theme_minimal()+
  scale_fill_brewer(name = 'Level of Approval', palette = 'RdYlGn', 
                    labels=c("Original-Control", "Original-Treatment", "Updated-Control", "Updated-Treatment")) +
  theme(axis.title.x=element_text(margin=margin(t=15)))+
  theme(legend.position = "bottom")

graph_biodiv <- p
graph_biodiv

##interpretation of X-Axis: It shows the difference in most frequently chosen category minus self original, vs. most frequently chosen category minus self updated. 
#The negative numbers indicate that in updated self, CO2 is more highly valued
#Positive numbers indicate that in self updated, biodiversity is more highly valued


#####graph both of them
summary_emiland2 <- summary_emiland1 %>% filter(names_conc!="percent_orig_all" & names_conc!="percent_upd_all" )


p <- ggplot(summary_emiland2, aes(x = emiland_scale, y = percent, fill=names_conc)) +
  geom_bar(stat = "identity", position="dodge", alpha = 1) +  # half-transparent bars
  labs(
    x = "Difference of own preference to the most selected preference",     # Custom x-axis label
    y = "share of respondents",  # Custom y-axis label
    title = "CO2 reduction (neg.) vs. Landscape protection (pos.)"
  ) +
  theme_minimal()+
  scale_fill_brewer(name = 'Level of Approval', palette = 'RdYlGn', 
                    labels=c("Original-Control", "Original-Treatment", "Updated-Control", "Updated-Treatment")) +
  theme(axis.title.x=element_text(margin=margin(t=15)))+
  theme(legend.position = "bottom")

graph_emiland <- p
graph_emiland



ggsave("EDGE2_learning_diff_biodiv_across_groups.pdf", graph_biodiv,
       
       width = 10,
       
       height = 6,
       
       dpi = 600)

ggsave("EDGE2_learning_diff_emiland_across_groups.pdf", graph_emiland,
       
       width = 10,
       
       height = 6,
       
       dpi = 600)

####################################################### Evaluate change of direction: Plots #############################

########################################for biodiversity

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



##################################### for emiland

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


##### for regressions - get sociodemographics for data 

###education

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


# GENDER ### 
#Make Gender Variable #

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

# TidyUpHere

# Get a list of objects in the current environment
objects_temp <- ls()

# Remove objects whose names include "_temp"
to_remove_temp <- objects_temp[grepl("_temp", objects_temp)]
rm(list = to_remove_temp, envir = .GlobalEnv)


#### AGE #####
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

####left_right

str(values_ex2$H2_1)

values_ex2$left_right <- as.numeric(values_ex2$H2_1)###Higher values = more right

###trust in science

str(values_ex2$H5_Trust_1)
unique(values_ex2$H5_Trust_1)

values_ex2$trust_in_sci <- as.numeric(values_ex2$H5_Trust_1)###Higher values = more trust (0-10)

str(values_ex2$H5_Trust_2nd_1)
unique(values_ex2$H5_Trust_2nd_1)

values_ex2$trust_in_sci_2nd_pp <- as.numeric(values_ex2$H5_Trust_2nd_1)###Higher values = more trust, percentage points 0-100

####tradeoff_con_treat

values_ex2$tradeoff_con_treat_treat1 <- ifelse(values_ex2$tradeoff_con_treat=="treat", 1,
                                               ifelse(values_ex2$tradeoff_con_treat=="con", 0,NA))
values_ex2$tradeoff_con_treat_treat1<- as.numeric(values_ex2$tradeoff_con_treat_treat1)

################ all sociodemographics completed

##DVs:

#pre-treat
imp_to_you_1 ##bioemi: positive: Co2-reduction is more important
imp_to_you_2 ##emiland: positive: Co2-reduction is more important

#post-treat
bioemi_updated ##post-treatment (=updated) bioemi: positive: Co2-reduction is more important
emiland_updated ##post-treatment (=updated) emiland: positive: Co2-reduction is more important

#difference
inverse_own_learn_diff_bioemi ###updated minus original, positive values indicate that Co2-reduction is valued more strongly in the update than before
inverse_own_learn_diff_emiland ##updated minus original, positive values indicate that Co2-reduction is valued more strongly in the update than before

###indep vars
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



#### prepare combinations

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
write.table(IVs_4, "F:/SWEET/EDGE/WP8 - Survey II/Auswertungen/input_data/ivs_1.csv", sep=",")

IVs_5 <- read.csv("ivs_1.csv")

###automate modelling

##for bioemi

##for first DV

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

##for second DV

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

##for first DV

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

##for second DV

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

###bring modelsummaries together

summaries1 <- bind_rows(
  DV_bioemi_orig_reg_df,
  DV_bioemi_upd_reg_df,
  DV_bioemi_diff_reg_df,
  DV_emiland_orig_reg_df,
  DV_emiland_upd_reg_df,
  DV_emiland_diff_reg_df)

summaries2 <- summaries1 %>% left_join(IVs_5, by=c("IV_components"="Combined_cleaned"))

write_xlsx(summaries2,"F:/SWEET/EDGE/WP8 - Survey II/Auswertungen/input_data/lm_model_summaries_v1.xlsx" )
