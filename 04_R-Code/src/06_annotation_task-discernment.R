####  00. Setup  ####

library(dplyr) #data manipulation, summarize
library(car)# regression diagnostics
library(tidyverse) # pivot longer
library(caret) #F1 Scores & Confusion matrix
library(lme4) #lmer()
library(lmerTest) #p values for lmer()
library("report") # genereate reports from R objects
library(ggResidpanel) #visualize residual plots
library(mltools)
library(psycho) # pyschometrics

#### 01.Load data ####

rm(list = ls())
setwd("/Users/ann/Documents/University/SS23/MA_Media-Literacy-by-Design/Repository")

df_d <-read.csv("./data/processed/survey_detection_long")
mat_d <-read.csv("./data/raw/mat_detection.csv")

gold1 <- read.csv("./data/temp/gold_1.csv")
gold2 <- read.csv("./data/temp/gold_2.csv")
gold3 <- read.csv("./data/temp/gold_3.csv")
gold4 <- read.csv("./data/temp/gold_4.csv")
gold5 <- read.csv("./data/temp/gold_5.csv")
gold6 <- read.csv("./data/temp/gold_6.csv")


#### 02.Quick comparison between participants ####

#Look at the word-level annotations of the gold standard and two participants 
view(gold3[, c(1, 2, grep("cd1a9208", colnames(gold3)), grep("7888fc", colnames(gold3)))])


#### 03. Calculate dprime & join tables ####

# Remove NAs 
df_d <- df_d[complete.cases(df_d$Annotations), ]

#Calculate mcc and dprime based on the confusion matrix values, filter NA rows
#df_d$mcc <- mcc(TP = df_d$TP, FP = df_d$FP, TN = df_d$TN, FN = df_d$FN)
dprime <- dprime(n_hit = df_d$TP, n_fa = df_d$FP, n_miss =  df_d$FN, n_cr =  df_d$TN)
df_d <- cbind(df_d, dprime)

# Join with material values
df_d <- left_join(df_d, mat_d, by = "s_id")
df_d <- rename(df_d, matPol = polDummy)

# Fix BiasScore values
df_d$biasScore <- gsub(",", ".", df_d$biasScore)


#### 04. Correlations ####

cor.test(df_d$bias,as.numeric(df_d$biasScore), method ="pearson")
cor(df_d$bias, as.numeric(df_d$biasScore))
cor(df_d$bias, df_d$dprime)
cor(df_d$bias, df_d$Annotations)
cor(df_d$bias, df_d$TP)


#### 05. Exploratory F1 Plots ####

##### 5.1 Per Treatment Group #####
ggplot(df_d, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(co), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Heuristic") +
  ggtitle("Perceived Bias vs Actual Bias by Heuristic") +
  theme_bw()


##### 5.2 Per Congruence #####
ggplot(df_d, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(co), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = congruence, color = congruence), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Heuristic") +
  ggtitle("Perceived Bias vs Actual Bias by Heuristic")

##### 5.3 Per Pol Category: 1 = liberal, 2= moderate, 3=conservative #####
ggplot(df_d, aes(x = bias, y = F1)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(co), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = pol_dummy, color = pol_dummy), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "F1", color = "Heuristic") +
  ggtitle("Perceived Bias vs Actual Bias by Heuristic")



#### 06.  Exploratory D Prime Plots ####

##### 6.1 One regression line per treatment #####
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(color = heuristic), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Heuristic") +
  ggtitle("Perceived Bias vs Actual Bias by Heuristic")

##### 6.2 One regression line per pol category (pol_dummy: 1 = liberal, 2 = moderate, 3 = conservative) #####
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(color = heuristic), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = pol_dummy, color = pol_dummy), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Heuristic") +
  ggtitle("Perceived Bias vs Actual Bias by Heuristic")


##### 6.3 One regression line per congruence #####
ggplot(df_d, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.1, size = 2) +
  #geom_jitter(aes(color = heuristic), alpha = 0.5, size = 3) +
  geom_smooth(aes(group = congruence, color = congruence), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Heuristic") +
  ggtitle("Perceived Bias vs Actual Bias by Heuristic")

##### 6.4 Filter per Statements #####
df_d_subset <- filter(df_d, s_id == 6)
ggplot(df_d_subset, aes(x = bias, y = dprime)) +
  geom_jitter(alpha = 0.2, size = 2) +
  geom_smooth(aes(group = heuristic, color = heuristic), method = lm, se = FALSE) +
  labs(x = "Perceived Bias", y = "DPrime", color = "Heuristic") +
  ggtitle("Perceived Bias vs Actual Bias by Heuristic")


#### 07. Treatment Summary Plots ####

##### 7.1 F1 score per heuristic #####
ggplot(df_d, aes(x=heuristic, y=F1, fill=heuristic)) +
  geom_boxplot(notch=TRUE) + 
  #geom_point(position=position_jitter(width=0.2, height=0), color="black", size=1) +
  stat_summary(fun=mean, geom="point", shape=18, size=3, color="darkgrey") +
  scale_fill_manual(values = c("#F7DC6F", "#F5B041", "#E67E22", "grey", "#82E0AA", "#AF7AC5", "#85C1E9"))+
  labs(title="Average Bias Perception per Heuristic", x="Treatment Group", y="F1 Score")  # set custom x-axis labels if needed

##### 7.2 D Prime plot per heuristic #####
ggplot(df_d, aes(x=heuristic, y=dprime, fill=heuristic)) +
  geom_boxplot(notch=TRUE) + 
  #geom_point(position=position_jitter(width=0.2, height=0), color="black", size=1) +
  #stat_summary(fun=mean, geom="point", shape=18, size=3, color="darkgrey") +
  scale_fill_manual(values = c("#F7DC6F", "#F5B041", "#E67E22", "grey", "#82E0AA", "#AF7AC5", "#85C1E9"))+
  labs(x="Treatment Group", y="D-Prime")  # set custom x-axis labels if needed


#### 08. Report average bias perception, f1 and d-prime  ####

##### 8.1 Statment Percpetion ####
# Aggregate the data by s_id
df_summary <- aggregate(df_d[, c("bias", "sent", "trust", "share")], 
                        by = list(df_d$s_id, df_d$key), 
                        FUN = function(x) c(mean = round(mean(x),2), sd = round(sd(x),2)))

print(df_summary)

key_order <- c("cmea", "rmea", "lmea", "chia", "rhia", "lhia")

# Convert the 'key' column to a factor with the custom order
df_d$key <- factor(df_d$key, levels = key_order)

# Plot with the custom order
ggplot(df_d, aes(x = biasScore, y = bias, group = biasScore)) +
  geom_boxplot() +
  xlab("biasScore") +
  ylab("bias") +
  ggtitle("Boxplot of Bias by Bias Score") +
  facet_wrap(~key, nrow = 1, scales = "free_x") +   
  theme_bw()



##### 8.2 Average d-prime scores #####
print(aggregate(df_d$dprime, by = list(df_d$participantId), FUN = mean))
print(aggregate(df_d$dprime, by = list(df_d$key), FUN = function(x) c(mean = mean(x), sd = sd(x))))
print(aggregate(df_d$dprime, by = list(df_d$heuristic, df_d$s_id), FUN = function(x) c(mean = mean(x), sd = sd(x))))

# Aggregate the dprime data by heuristic and s_id
df_summary <- aggregate(df_d$dprime, by = list(df_d$heuristic, df_d$s_id), FUN = function(x) c(mean = mean(x), sd = sd(x)))
# Reshape the data from long to wide format using the pivot_wider function from tidyr
df_wide <- pivot_wider(df_summary, names_from = "Group.2", values_from = "x", names_prefix = "dprime_")
print(df_wide)


##### 8.3 Average f1 scores #####
print(aggregate(df_d$F1, by = list(df_d$heuristic), FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))))
print(aggregate(df_d$precision, by = list(df_d$key), FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))))
print(aggregate(df_d$recall, by = list(df_d$key), FUN = function(x) c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE))))


#### 09. Annotations ####

##### 9.1 Report Annotations over Bias ####

# Colored edition with TP, FP, Sum #

df_anno <- df_d %>% 
  pivot_longer(c(Annotations, TP, FP), names_to = "anno_cat", values_to = "anno_val") %>%
  mutate(anno_cat = factor(anno_cat, levels = c("Total", "TP", "FP")))

ggplot(df_anno, aes(factor(bias), anno_val, fill = anno_cat)) +
  geom_boxplot(position = position_dodge(width = 0.85)) +
  scale_fill_manual(values = c("Annotations" = "grey", "TP" = "lightblue", "FP" = "red")) +
  labs(
    x = "Perceived Amount of Bias",
    y = "Number of Annotations") +
  scale_x_discrete(labels = c("Strongly\ndisagree", "Disagree", "Somewhat\ndisagree", "Somewhat\nagree", "Agree", "Strongly\nagree"))


##### 9.2 Annotations per statement ####

df_anno_sum <- df_d %>% 
  group_by(s_id, key) %>% 
  summarise(
    avg_bias = round(mean(bias),3),
    sd_bias = round(sd(bias),3),
    avg_annotations = round(mean(Annotations),3),
    sd_annotations = round(sd(Annotations),3),
    avg_TP = round(mean(TP),3),
    sd_TP = round(sd(TP),3)
  )

view(df_anno_sum)


#### 10.0 Create Contrast ####

df_d$treatment.f = as.factor(df_d$heuristic)
contrasts(df_d$treatment.f) <- contr.treatment(7, base = 4) #make "CONTROL" the fourth category the baseline

df_d$pol.m = as.factor(df_d$matPol)
contrasts(df_d$pol.m) <- contr.treatment(3, base = 2) 

df_d$pol.p = as.factor(df_d$pol_dummy)
contrasts(df_d$pol.p) <- contr.treatment(3, base = 2) 

df_d$congruence = as.factor(df_d$congruence)
contrasts(df_d$congruence) <- contr.treatment(2, base = 1) #base = not congruent 

df_d$gender <- dplyr::recode(df_d$gender, "Female" = 0, "Male" = 1, "I prefer not to say" = 2, "Other" = 2)
df_d$gender.g = as.factor(df_d$gender)
contrasts(df_d$gender.g) <- contr.treatment(3, base = 1) #base = female, 1 = male, 2 = other 

df_d$biasScore = as.numeric(df_d$biasScore)


#### 11.0 Main Model Detection: D-Prime ####

##### 11.1 D-PRIME LMM, REML ####

# Main Model
lmm_d <- lmer(dprime ~ bias + pol.p + congruence + pol.p * congruence+ treatment.f + gender.g + age + edu_score + (1|participantId), data = df_d)
summary(lmm_d)


#identifying which fixed or random effects have the most impact on the model fit
drop1(lmm_d, test = "Chisq")


##### 11.2 Likelihood ratio ####
#Use ML to compare models and REML for reporting, as ML may underestimate the variance of random effects

lmm_d_null_ML <- lmer(dprime ~ (1|participantId), data = df_d, REML = FALSE)
lmm_d_main_ML <- lmer(dprime ~ bias + pol.p + congruence + treatment.f +gender.g + age + edu_score + (1|participantId), data = df_d, REML = FALSE)
lmm_d_int_ML <- lmer(dprime ~ bias + pol.p*congruence + pol.p + congruence + treatment.f + gender.g + age + edu_score + (1|participantId), data = df_d, REML = FALSE)

anova(lmm_d_main_ML, lmm_d_int_ML)

##### 11.3 Assumptions ####
performance::check_model(lmm_d_main, panel = TRUE, check = "all") 
ggResidpanel::resid_panel(lmm_d_main, smoother = TRUE, qqbands = TRUE, type = "pearson")

#model_check <- performance::check_model(lmm_d_main, panel = FALSE) 
#plot(model_check, panel = FALSE, filename = c("fitted_res.png", "res_vs_pred.png", "leverage.png", "cooks_dist.png", "scale_loc.png"))


#### 12.0 Main Model Detection: F1 ####

##### 12.1 F1 LMM, REML ####

lmm_F1 <- lmer(F1 ~ bias + pol.p*congruence + pol.p + pol.p*congruence + congruence + treatment.f + gender.g + age + edu_score + (1|participantId), data = df_d)
summary(lmm_F1)

#identifying which fixed or random effects have the most impact on the model fit
drop1(lmm_F1_main, test = "Chisq")

##### 12.2 Likelihood ratio test ####
#Use ML to compare models and REML for reporting, as ML may underestimate the variance of random effects

lmm_F1_null_ML <- lmer(F1 ~ (1|participantId), data = df_d, REML = FALSE)

lmm_F1_main_ML <- lmer(F1 ~ bias + pol.p + congruence + treatment.f + gender + age + edu_score + (1|participantId), data = df_d, REML = FALSE)
lmm_F1_int_ML <- lmer(F1 ~ bias + pol.p*congruence + pol.p + congruence + treatment.f + gender + age + edu_score + (1|participantId), data = df_d, REML = FALSE)

anova(lmm_F1_main_ML, lmm_F1_int_ML)


##### 12.3 Assumptions ####
performance::check_model(lmm_F1_main) 
ggResidpanel::resid_panel(lmm_F1_main, smoother = TRUE, qqbands = TRUE, type = "pearson")



