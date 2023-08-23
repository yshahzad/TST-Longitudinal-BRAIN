library(effsize)
install.packages("sjstats")
library(sjstats)
library(lsr)

#"""Hypothesis testing main variables: FMA, BBT, GS, and MAL"""

#Function that performs battery of normality tests on each evaluation
normality_tests <- function(dataframe){
  evaluations <- list("T1", "T2", "T3")
  
  for (eval in evaluations){
    print(paste("Normality for ", eval, ":", sep = ""))
    print(shapiro.test(dataframe[[eval]]))
    qqnorm(dataframe[[eval]])
    qqline(dataframe[[eval]])}
    
}

#Function that bootstrap a CI of differences
bootstrap_CI <- function(difference, n) {
  
  B <- 100000 #One-hundred thousand re-samples
  bootMean <- vector()
  
  for (i in 1:B){
    bootSample <- sample(difference, size = n,
                         replace = TRUE)
    
    #Assigns mean values of each resample to the vector bootMean
    bootMean[i] <- mean(bootSample)
    cat("Bootstrap sample:", i ,"\n")
  }
  
  mean(bootMean)
  
  #Lends a confidence interval, since this is an approximate sample distribution.
  CI <- quantile(bootMean, probs = c(0.025, 0.975))
  
  return(CI)
}

#Fugl-Meyer Assessment

FMA.df <- data.frame(T1 = BrainCanada$FMA_T1,
                     T2 = BrainCanada$FMA_T2,
                     T3 = BrainCanada$FMA_T3)

#FMA - values are not normal for all sessions
normality_tests(FMA.df)

wilcox.test(BrainCanada$FMA_T3, BrainCanada$FMA_T1, paired = TRUE)
cohen.d(BrainCanada$FMA_T3, BrainCanada$FMA_T1)
boxplot(FMA.df, xlab = "Session", ylab = "FMA Score")

#FMA Difference Confidence Interval: Bootstrapping Methods
FMA_T3_T2_Diff <- BrainCanada$FMA_T3 - BrainCanada$FMA_T2
bootstrap_CI(FMA_T3_T2_Diff, 25)


#BBT - likely not normally distributed
BBT.df <- data.frame(T1 = BrainCanada$BBT_T1_A,
                     T2 = BrainCanada$BBT_T2_A,
                     T3 = BrainCanada$BBT_T3_A)

normality_tests(BBT.df)

wilcox.test(BrainCanada$BBT_T3_A, BrainCanada$BBT_T1_A, paired = TRUE)
cohen.d(BrainCanada$BBT_T3_A, BrainCanada$BBT_T1_A)
boxplot(BBT.df, xlab = "Session", ylab = "# of Blocks")

#Regular confidence interval for BBT Difference between T3 and T1:
t.test(BBT_T3_T1_Diff, mu = 0)
  #95 percent confidence interval:
  #-0.005758511  4.350586097

#Bootstrapped CI for BBT Difference between T3 and T1:
  #2.5%      97.5% 
  #0.03448276 4.17241379 
BBT_T3_T2_Diff <- BrainCanada$BBT_T3_A - BrainCanada$BBT_T2_A
bootstrap_CI(BBT_T3_T2_Diff, 25)


#Grip Strength
GS.df <- data.frame(T1 = BrainCanada$BBT_T1_A,
                    T2 = BrainCanada$BBT_T2_A,
                    T3 = BrainCanada$BBT_T3_A)
normality_tests(GS.df)

wilcox.test(BrainCanada$GS_T3_A, BrainCanada$GS_T1_A, paired = TRUE, correct = TRUE)
cohen.d(BrainCanada$GS_T3_A, BrainCanada$GS_T1_A)
boxplot(GS.df, xlab = "Session", ylab = "Grip Strength (kg)")

#Bootstrapping the Confidence Interval
GS_T3_T2_Diff <- BrainCanada$GS_T3_A - BrainCanada$GS_T2_A
bootstrap_CI(GS_T3_T2_Diff, 25)


#MAL Quantitative (AOU)
MAL_quant.df <- data.frame(T1 = BrainCanada$MAL_Quant_T1,
                           T2 = BrainCanada$MAL_Quant_T2,
                           T3 = BrainCanada$MAL_Quant_T3)

normality_tests(MAL_quant.df)
wilcox.test(BrainCanada$MAL_Quant_T3, BrainCanada$MAL_Quant_T1, paired = TRUE)
cohen.d(BrainCanada$MAL_Quant_T3, BrainCanada$MAL_Quant_T1)
boxplot(MAL_quant.df, xlab = "Session", ylab = "Quantitative MAL Score")

MAL_Quant_T3_T2_Diff <- BrainCanada$MAL_Quant_T3 - BrainCanada$MAL_Quant_T2
bootstrap_CI(MAL_Quant_T3_T2_Diff, 25)


#MAL Qualitative (QOU)
MAL_quali.df <- data.frame(T1 = BrainCanada$MAL_Quali_T1,
                           T2 = BrainCanada$MAL_Quali_T2,
                           T3 = BrainCanada$MAL_Quali_T3)

normality_tests(MAL_quali.df)
wilcox.test(BrainCanada$MAL_Quali_T3, BrainCanada$MAL_Quali_T1, paired = TRUE)

boxplot(MAL_quali.df, xlab = "Session", ylab = "Qualitative MAL Score")
cohen.d(BrainCanada$MAL_Quali_T3, BrainCanada$MAL_Quali_T2)
MAL_Quant_T3_T2_Diff <- BrainCanada$MAL_Quant_T3 - BrainCanada$MAL_Quant_T2
bootstrap_CI(MAL_Quant_T3_T2_Diff, 25)

tDCS_Group <- factor(BrainCanada$TDSC_Group, levels = c(1,2))
TST_Group <- factor(BrainCanada$MEP_Group, levels = c(1,2,3))
institution_anova <- aov(BrainCanada$T1_CVA_Difference~TST_Group)


#Hypothesis Testing ROM values
wilcox.test(BrainCanada$ROM_Wrist_Affected_Active_T2, 
              BrainCanada$ROM_Wrist_Affected_Active_T1, paired = TRUE)

Shoulder_AROM_diff <- BrainCanada$ROM_Shoulder_Affected_Active_T3 - BrainCanada$ROM_Shoulder_Affected_Active_T1
#ANOVA Analysis
wilcox.test(BrainCanada$ROM_Shoulder_Affected_Active_T3, BrainCanada$ROM_Shoulder_Affected_Active_T1, paired = TRUE)

MEP_factor <- factor(BrainCanada$MEP_Group,
                     levels = c(1, 2, 3))
my_anova_mep <- aov(BrainCanada$MAL_Quant_Difference~MEP_factor)
summary(my_anova_mep)

fma_aov <- aov(FMA_Difference ~ MEP_Group + FMA_T1, data = BrainCanada)
etaSquared(fma_aov, type = 3)
summary(fma_aov)

bbt_aov <- aov(BBT_A_Difference ~ MEP_Group + BBT_T1_A, data = BrainCanada)
etaSquared(bbt_aov, type = 3)
summary(bbt_aov)

gs_aov <- aov(GS_A_Difference ~ MEP_Group + GS_T1_A, data = BrainCanada)
etaSquared(gs_aov, type = 3)
summary(gs_aov)
#Demographics Tests

mal_quant_aov <-  aov(MAL_Quant_Difference ~ MEP_Group + MAL_Quant_T1, data = BrainCanada)
etaSquared(mal_quant_aov, type = 3)
summary(mal_quant_aov)

mal_quali_aov <-  aov(MAL_Quali_Difference ~ MEP_Group + MAL_Quali_T1, data = BrainCanada)
etaSquared(mal_quali_aov, type = 3)
summary(mal_quali_aov)


