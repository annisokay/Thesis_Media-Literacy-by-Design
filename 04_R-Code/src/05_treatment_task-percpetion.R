####  00. Setup  ####

library(tidyverse) # pivot longer
library(dplyr) #data manipulation, summarize
library(caret) #F1 Scores & Confusion matrix
library(psych) #kmo
library(ltm)
library(lme4) #lmer()
library(car)# regression diagnostics
library(lmerTest) #p values for lmer()
library(ordinal) #clmm()
library("report") # genereate reports from R objects
library(ggResidpanel) #visualize residual plots
library(mltools)
library(psycho) pyschometrics
library(viridis)  # Required for scale_fill_viridis


#### 01.Load data ####
rm(list = ls())
setwd("/Users/ann/Documents/University/SS23/MA_Media-Literacy-by-Design/Repository")

mat_p <-read.csv("./data/raw/mat_perception.csv")
df_p <-read.csv("./data/processed/survey_perception_long")

### 02. Join material with survey results###
df_p <- left_join(df_p, mat_p, by = "s_id")
df_p <- rename(df_p, matPol = polDummy)

#Fix comma in biasScore
df_p$biasScore <- gsub(",", ".", df_p$biasScore)


#### 02. Item Correlation ####

# Select the relevant columns
df_corr <- df_p[, c("bias", "sent", "share", "trust")]

# Compute the correlation matrix with significance values
corr <- corr.test(df_corr,  method ="spearman")

# Print the correlation matrix with significance values
print(corr$r)
print(corr$p)

corr_list <- list()

# Split the data frame by s_id and apply the cor() function to each subset
corr_list <- by(df_p, df_p$s_id, function(subset) {
  # Select the relevant columns
  subset_corr <- subset[, c("bias", "sent", "share", "trust")]
  
  # Compute the correlation matrix and store it in the list
  s_id <- as.character(unique(subset$s_id))
  if (!(s_id %in% names(corr_list))) {
    corr_list[[s_id]] <- cor(subset_corr,  method ="spearman")
  }
})

# Print the correlation matrices with significance values for all statements
print(corr_list)



#### 03. Validity Check: Statement Perception ####

# Plot bias over bias score
key_order <- c("clot", "llot", "rlot", "cmet", "lmet", "rmet", "chit","lhit", "rhit")

# Convert the 'key' column to a factor with the custom order
df_p$key <- factor(df_p$key, levels = key_order)

# Define the custom colors
custom_colors <- c("grey", "grey", "grey","yellow", "yellow", "yellow", "orange", "orange", "orange")

# Plot with the custom order and colors
ggplot(df_p, aes(x = biasScore, y = bias, group = biasScore, fill = factor(biasScore))) +
  geom_boxplot() +
  xlab("biasScore") +
  ylab("bias") +
  ggtitle("Boxplot of Bias by Bias Score") +
  facet_wrap(~key, nrow = 1, scales = "free_x") +   
  theme_bw() +
  scale_fill_manual(values = custom_colors) 


#### 04. Average Bias Percepetion ####

##### 4.1. Aggregate bias by s_id across all treatments #####
  
# Aggregate the data by s_id
item_summary <- aggregate(df_p[, c("bias", "sent", "trust", "share")], 
                        by = list(df_p$s_id, df_p$key), 
                        FUN = function(x) c(mean = round(mean(x),2), sd = round(sd(x),2)))
view(item_summary)



##### 4.2. Aggregate bias by s_id for each treatment #####
  
bias <- aggregate(df_p[, c("bias")], 
                           by = list(df_p$s_id, df_p$key, df_p$heuristic), 
                           FUN = function(x) c(mean = round(mean(x),2), sd = round(sd(x),2)))

colnames(bias)[0:4] <- c("s_id", "key", "Treatment", "MEAN", "SD")
bias_wide <- pivot_wider(bias, names_from= Treatment, values_from=c(ends_with("mean"), ends_with("SD")))
view(bias_wide)


#### 05. Create Contrasts ####

df_p$treatment.f = as.factor(df_p$heuristic)
contrasts(df_p$treatment.f) <- contr.treatment(7, base = 4) #make "CONTROL" the fourth category the baseline

df_p$pol.m = as.factor(df_p$matPol)
contrasts(df_p$pol.m) <- contr.treatment(3, base = 2) 

df_p$pol.p = as.factor(df_p$pol_dummy)
contrasts(df_p$pol.p) <- contr.treatment(3, base = 2) 

df_p$congruence = as.factor(df_p$congruence)
contrasts(df_p$congruence) <- contr.treatment(2, base = 1) #base = not congruent 

df_p$biasScore = as.numeric(df_p$biasScore)

df_p$gender <- dplyr::recode(df_p$gender, "Female" = 0, "Male" = 1, "I prefer not to say" = 2, "Other" = 2)
df_p$gender.g = as.factor(df_p$gender)


#### 06. Main Model Bias Perception ####

lmm_bias <- lmer(bias ~ biasScore + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p)
lmm_bias_ml <- lmer(bias ~ biasScore + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)

conf_intervals <- confint(lmm_bias, level = 0.95)

#identifying which fixed or random effects have the most impact on the model fit
drop1(lmm_bias_ml, test = "LRT")

#### 07. Main Model Sharing Intention ####

lmm_share_p <- lmer(share ~ bias + sent + trust + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p)
lmm_share_ml <- lmer(share ~ bias + sent + trust + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)

report(lmm_share_p)
drop1(lmm_share_ml, test = "LRT")


#### 08. Main Model Trust #####

lmm_trust <- lmer(trust ~ bias + sent + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence  + gender.g + age + (1|participantId), data = df_p)
lmm_trust_ml <- lmer(trust ~ bias + sent + + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)

summary(lmm_trust)
drop1(lmm_trust_ml, test = "LRT")

#### 09. Main Model Emotionality #####

lmm_sent <- lmer(sent ~ bias + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p)
lmm_sent_ml <- lmer(sent ~ bias + sentMag + treatment.f + musk1 + musk_opinion + edu_score + pol.p + congruence + gender.g + age + (1|participantId), data = df_p, REML = FALSE)

summary(lmm_sent)

(drop1(lmm_sent, test = "LRT"))

#Assumptions
ggResidpanel::resid_panel(lmm_sent, smoother = TRUE, qqbands = TRUE, type = "pearson")



#### 10. Plot average perceived bias per treatment and bias level #####

# Prepare data
parallel <- data.frame(treatment = df_p$heuristic, 
                            statement = df_p$key, 
                            perceived_bias = df_p$bias)

parallel_summary<- aggregate(perceived_bias ~ treatment + statement, data = parallel, mean)

parallel$treatment <- factor(parallel$treatment, levels = c("CONTROL", "BIAS_BAR", "BIAS_GAUGE", "BIAS_HIGH", "POLITICAL", "SENTIMENT", "TRUST"))

# Gather data for each bias level
summary_low <-  parallel_summary[parallel_summary$statement %in% c("clot", "llot", "rlot"), ]
summary_med <-  parallel_summary[parallel_summary$statement %in% c("cmet", "lmet", "rmet"), ]
summary_hi <-  parallel_summary[parallel_summary$statement %in% c("lhit", "rhit", "chit"), ]


# Set colors
statement_colors <- c("clot" = "seagreen3",
                      "cmet" = "seagreen3",
                      "chit" = "seagreen3",
                      "llot" = "steelblue3",
                      "lmet" ="steelblue3",
                      "lhit" = "steelblue3",
                      "rlot" = "indianred3",
                      "rmet" = "indianred3",
                      "rhit" = "indianred3")

#### 10.1 Plot average perceived bias per treatment: low bias #####

ggplot(summary_low, aes(x = treatment, y = perceived_bias, group = statement, color = statement)) +
  geom_path() +
  geom_point(shape = 23) +
  scale_color_manual(values = statement_colors) +
  ylim(-3, 3) +
  labs(x = "Treatment",
       y = "Average Perceived Bias",
       color = "Statement")  +
  scale_y_continuous(
    breaks = seq(-3, 3, 0.5),
    minor_breaks = seq(-3, 3, 0.1),
    limits = c(-3, 3),
    expand = c(0, 0)
  )


#### 10.2 Plot average perceived bias per treatment: medium bias #####

ggplot(summary_med, aes(x = treatment, y = perceived_bias, group = statement, color = statement)) +
  geom_path() +
  geom_point(shape = 23) +
  scale_color_manual(values = statement_colors) +
  ylim(-3, 3) +
  labs(x = "Treatment",
       y = "Average Perceived Bias",
       color = "Statement")  +
  scale_y_continuous(
    breaks = seq(-3, 3, 0.5),
    minor_breaks = seq(-3, 3, 0.1),
    limits = c(-3, 3),
    expand = c(0, 0)
  )

#### 10.2 Plot average perceived bias per treatment: high bias #####

ggplot(summary_hi, aes(x = treatment, y = perceived_bias, group = statement, color = statement)) +
  geom_path() +
  geom_point(shape = 23) +
  scale_color_manual(values = statement_colors) +
  ylim(-3, 3) +
  labs(x = "Treatment",
       y = "Average Perceived Bias",
       color = "Statement")  +
  scale_y_continuous(
    breaks = seq(-3, 3, 0.5),
    minor_breaks = seq(-3, 3, 0.1),
    limits = c(-3, 3),
    expand = c(0, 0)
  )

