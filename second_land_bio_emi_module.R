library(readxl)
library(writexl)
library(tidyverse)
library(overlapping)
library(ggplot2)

setwd("Y:/SWEET/EDGE/WP8 - Survey II/Auswertungen/input_data")


## Load the data from the RDS file
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

##labels and interpretation 

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

###############################

###get change direction and number of people that changed direction for biodiversity tradeoff

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



###get change direction and number of people that changed direction for emiland tradeoff

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
