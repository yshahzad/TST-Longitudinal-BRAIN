#Brain Canada Longitudinal Demographic Comparisons
#Author: Yavuz Shahzad

#Age Comparison by TST Group <- ANOVA
age_demographic.aov <- aov(BrainCanada$Age_at_Intervention ~ MEP_factor)
  #age_demographic.aov, p-value is 0.203
summary(age_demographic.aov)


demographic_chars_continous <- function(response_var, explanatory_var){
  treatment_groups <- list("LI", "MI", "HI")
  
  for (x in 1:3) {
    print(paste("Mean ± sd when MEP Group is", treatment_groups[x], "is", 
                round(mean(response_var[explanatory_var == x]), digits = 2), "±",
                round(sd(response_var[explanatory_var == x]), digits = 2)))
    print("-----------------------------------------------------------------")
    print("")}
}

demographic_chars_proportions <- function(response_var, explanatory_var, factor_levels){
  table = table(response_var, as.factor(explanatory_var))
  print(chisq.test(table))
  summary(as.factor(response_var))
  
  treatment_groups <- list("LI", "MI", "HI")
  
  for (x in 1:3) {
    
    print(paste("Characteristic distribution in", treatment_groups[x], "is", 
                summary(as.factor(response_var[explanatory_var == x]))))
    }
}

baseline_values <- function(response_var, explanatory_var){
  anova <- aov(response_var ~ as.factor(explanatory_var))
  
  treatment_groups <- list("LI", "MI", "HI")
  
  for (x in 1:3) {
    print(paste("Mean ± sd when MEP Group is", treatment_groups[x], "is", 
                round(mean(response_var[explanatory_var == x]), digits = 2), "±",
                round(sd(response_var[explanatory_var == x]), digits = 2)))
    print("-----------------------------------------------------------------")
    print("")
  }
  
  print(summary(anova))
  anova_p_val <- summary(anova)[[1]][["Pr(>F)"]][1]
  
  if (anova_p_val < 0.05) {
    print(TukeyHSD(anova, conf.level = 0.95))}
  
}

#Baseline values needs to be run for


mean(BrainCanada$Age_at_Intervention[BrainCanada$MEP_Group == 3])
sd(BrainCanada$Age_at_Intervention[BrainCanada$MEP_Group == 3])
  #Age at Intervention LI:71.25 (3.40), MI:60.00 (10.68), HI:	65.82 (8.83)


#Time since Stroke at Intervention
time_since_stroke.aov <- aov(BrainCanada$T1_CVA_Difference ~ as.factor(BrainCanada$MEP_Group))
  #time_since_stroke.aov, p-value is 0.377

mean(BrainCanada$T1_CVA_Difference[BrainCanada$MEP_Group == 3]) / 365
sd(BrainCanada$T1_CVA_Difference[BrainCanada$MEP_Group == 3]) / 365
  #Comment: S97 and S98 have unusually long times since stroke, which drags
  # up the standard deviation of the time since stroke.

#Handedness
handedness_factor <- factor(BrainCanada$Handedness, levels = c(1, 2))
handedness_table = table(BrainCanada$Handedness, BrainCanada$MEP_Group)
chisq.test(handedness_table)

#Side  of Stroke
side_stroke_factor <- factor(BrainCanada$Stroke_Location, levels = c(1, 2))
stroke_location_table = table(BrainCanada$Stroke_Location, BrainCanada$MEP_Group)
chisq.test(stroke_location_table)
summary(side_stroke_factor)

#Sex
sex_factor <- factor(BrainCanada$Gender, levels = c(1, 2))
sex_table = table(BrainCanada$Gender, BrainCanada$MEP_Group)
chisq.test(sex_table)


#Type of Stroke
type_stroke_factor <- factor(BrainCanada$Type_of_Stroke, levels = c(1, 2, 3), labels = c('I', 'H', 'O'))
type_stroke_table = table(type_stroke_factor, BrainCanada$MEP_Group)
chisq.test(type_stroke_table)
summary(type_stroke_factor)
type_stroke_table


#Between-group Comparisons at Baseline
#FMA at Baseline
baseline_FMA.aov <- aov(BrainCanada$FMA_T1 ~ as.factor(BrainCanada$MEP_Group))
  #baseline_FMA.aov, p = 0.0101
mean(BrainCanada$FMA_T1[BrainCanada$MEP_Group == 3])
sd(BrainCanada$FMA_T1[BrainCanada$MEP_Group == 3])
#Since there are differences, we perform the Tukey HSD Test as a post-hoc.
TukeyHSD(baseline_FMA.aov, conf.level = 0.95)


#Box and Blocks Test
baseline_BBT.aov <- aov(BBT_T1_A ~ as.factor(MEP_Group), data = BrainCanada)
mean(BrainCanada$BBT_T1_A[BrainCanada$MEP_Group == 3])
sd(BrainCanada$BBT_T1_A[BrainCanada$MEP_Group == 3])
TukeyHSD(baseline_BBT.aov, conf.level = 0.95)

#Grip Strength
baseline_GS.aov <- aov(GS_T1_A ~ as.factor(MEP_Group), data = BrainCanada)
summary(baseline_GS.aov)
mean(BrainCanada$GS_T1_A[BrainCanada$MEP_Group == 3])
sd(BrainCanada$GS_T1_A[BrainCanada$MEP_Group == 3])
TukeyHSD(baseline_GS.aov, conf.level = 0.95)

#Quantiative MAL
baseline_Quanti_MAL.aov <- aov(MAL_Quant_T1~ as.factor(MEP_Group), data = BrainCanada)
summary(baseline_Quanti_MAL.aov)
mean(BrainCanada$MAL_Quant_T1[BrainCanada$MEP_Group == 3])
sd(BrainCanada$MAL_Quant_T1[BrainCanada$MEP_Group == 3])
TukeyHSD(baseline_Quanti_MAL.aov, conf.level = 0.95)

pairwise.t.test(BrainCanada$MAL_Quant_T1, as.factor(BrainCanada$MEP_Group), 
                p.adjust.method="bonferroni")


baseline_values <- function(response_var, explanatory_var){
  anova <- aov(response_var ~ as.factor(explanatory_var))
  
  treatment_groups <- list("LI", "MI", "HI")
  
  for (x in 1:3) {
    print(paste("Mean ± sd when MEP Group is", treatment_groups[x], "is", 
                round(mean(response_var[explanatory_var == x]), digits = 2), "±",
                round(sd(response_var[explanatory_var == x]), digits = 2)))
    print("-----------------------------------------------------------------")
    print("")
  }
  
  print(summary(anova))
  anova_p_val <- summary(anova)[[1]][["Pr(>F)"]][1]
  
  if (anova_p_val < 0.05) {
    print(TukeyHSD(anova, conf.level = 0.95))}
  
}


baseline_values(BrainCanada$MAL_Quant_T1, BrainCanada$MEP_Group)
#Qualitative MAL
baseline_Quali_MAL.aov <- aov(MAL_Quali_T1 ~ as.factor(MEP_Group), data = BrainCanada)
summary(baseline_Quali_MAL.aov)
mean(BrainCanada$MAL_Quali_T1[BrainCanada$MEP_Group == 3])
sd(BrainCanada$MAL_Quali_T1[BrainCanada$MEP_Group == 3])
TukeyHSD(baseline_Quali_MAL.aov, conf.level = 0.95)
