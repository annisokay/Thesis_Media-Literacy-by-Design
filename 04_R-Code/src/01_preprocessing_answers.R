####  00. Setup  ####
#install.packages('tidyverse')
#install.packages('dplyr')
#install.packages('data.table')

library(tidyverse) # Data Science
library(dplyr) #data manipulation, summarize
library(data.table) #join & modify tables

rm(list = ls())
setwd("/Users/ann/Documents/University/SS23/MA_Media-Literacy-by-Design/Repository")

#### 01.Load data ####

#Results
sessions <-read.csv("./data/raw/surveyinfo.csv")
progress <-read.csv("./data/raw/progress.csv")
participants <- read.csv("./data/raw/demo.csv")
task_1 <- read.csv("./data/raw/taskone.csv")
task_2 <- read.csv("./data/raw/tasktwo.csv")


#### 02. Transform Tables ####

# One column per statement 
task1_items <- pivot_wider(task_1, id_cols = participantId, names_from = visualizationIndex, values_from = questionAnswers)
task2_items <- pivot_wider(task_2 , id_cols = participantId, names_from = snippetIndex, values_from = questionAnswers)
task2_anno <- pivot_wider(task_2, id_cols = participantId, names_from = snippetIndex, values_from = highlightedWordIndices)

timestamps <- pivot_wider(progress, id_cols = participantId, names_from = progress, values_from = createdAt)
timestamps$duration <- difftime(timestamps$END_11, timestamps$START_01, units = "mins")
duration <- data.frame(participantId = timestamps$participantId, duration = timestamps$duration)

# Create unique column names 
setnames(task1_items, old = c(2:11), new = c('perception1','perception2','perception3','perception4','attentioncheck',
                                             'perception5','perception6','perception7','perception8','perception9'))

setnames(task2_items, old = c(2:7), new = c('detection1','detection2','detection3','detection4','detection5',
                                             'detection6'))

setnames(task2_anno, old = c(2:7), new = c('annotation1','annotation2','annotation3','annotation4','annotation5',
                                            'annotation6'))

# Rename id column of sessions dataset so the participant ID can be used as join key####
names(sessions)[names(sessions) == 'id'] <- 'participantId'

#### 03. Merge Tables ####

# Merge 
dta <- list(sessions, participants, task1_items, task2_items, task2_anno, duration) %>% reduce(inner_join, by='participantId')
dta <- na.omit(dta) #I have no clue why 926ed46c-011a-4106-827f-54e843a300d1 is still in, even they they have NAs  

# Subset: Retain only relevant rows
dta <- subset(dta, select = -c(source, sessionId, profileId, createdAt.x, id, createdAt.y))

#### 04. Filtering Unusable Data#####

#### 04.1 Usable for Research ####
#241 participants

df <- dta[dta$dataUsableForResearch=="True",] # 7 participants out, 234 remaining

#### 04.2 Attention Check ####
df <- df[df$attentioncheck==df$attentioncheck[1],] # 8 participants out, 226 remaining (uff)

#### 04. Extract Items #####

#### 04.01 Split up the item lists #####

split_answers <- function(cell_value) {
  
  # Remove quotes and other weird characters from items 
  cell_value <- gsub("\\{", "", cell_value)
  cell_value <- gsub("\\}", "", cell_value)
  cell_value <- gsub("\"", "", cell_value)
  
  # Split the string on the comma
  cell_value <- strsplit(cell_value, ",")[[1]]
  return(cell_value)
}

# Splits the answers at each comma 
item_extraction <- function(input,i) {
  return(unlist(input)[i])
}

#### 04.02 Recode Items #####

recode_bias <- function(input) {
  recode(input, "Strongly disagree" = -3,"Disagree"= -2,"Somewhat disagree" = -1,"Somewhat agree" =1,"Agree" = 2,"Strongly agree" = 3)
}
recode_sent <- function(input) {
  recode(input, "Not at all emotional" = 1, "Low emotionality" = 2, "Slightly emotional" = 3, "Moderately emotional" = 4, "Emotional"= 5, "Very emotional" = 6,"Extremely emotional" = 7)
}
recode_truth <- function(input) {
  recode(input, "Not at all truthful" = 1, "Low truthfulness"= 2, "Slightly truthful" = 3, "Moderately truthful" = 4, "Truthful" = 5, "Very truthful" = 6,"Extremely truthful" = 7)
}
recode_bel <- function(input) {
  recode(input, "Not at all believable"= 1, "Low believability"= 2, "Slightly believable"= 3, "Moderately believable" = 4, "Believable"= 5, "Very believable" = 6, "Extremely believable" = 7)
}
recode_cred <- function(input) {
  recode(input, "Not at all credible"= 1, "Low credibility"= 2, "Slightly credible" = 3, "Moderately credible" = 4, "Credible" = 5, "Very credible" = 6,"Extremely credible" = 7)
}
recode_share <- function(input) {
  recode(input, "Very unlikely"= -3, "Unlikely"= -2,"Somewhat unlikely"= -1, "Neither unlikely nor likely" = 0 ,"Somewhat likely" = 1, "Likely" = 2, "Very likely" = 3)
}

#### 04.03 Extract all items using recode & split functions #####

extract_bias <- function(statement) {
  bias_tmp <- lapply(statement, split_answers)
  df$bias_tmp <-  mapply(item_extraction, bias_tmp,i=1)
  df$bias_tmp <- recode_bias(df$bias_tmp)
  return(df$bias_tmp)
}

extract_sent <- function(statement) {
  sent_tmp <- lapply(statement, split_answers)
  df$sent_tmp <-  mapply(item_extraction, sent_tmp,i=2)
  df$sent_tmp <- recode_sent(df$sent_tmp)
  return(df$sent_tmp)
}

extract_bel <- function(statement) {
  bel_tmp <- lapply(statement, split_answers)
  df$bel_tmp <-  mapply(item_extraction, bel_tmp,i=3)
  df$bel_tmp <- recode_bel(df$bel_tmp)
  return(df$bel_tmp)
}

extract_truth <- function(statement) {
  truth_tmp <- lapply(statement, split_answers)
  df$truth_tmp <-  mapply(item_extraction, truth_tmp,i=4)
  df$truth_tmp <- recode_truth(df$truth_tmp)
  return(df$truth_tmp)
}

extract_cred <- function(statement) {
  cred_tmp <- lapply(statement, split_answers)
  df$cred_tmp <-  mapply(item_extraction, cred_tmp,i=5)
  df$cred_tmp <- recode_cred(df$cred_tmp)
  return(df$cred_tmp)
}

extract_share <- function(statement) {
  share_tmp <- lapply(statement, split_answers)
  df$share_tmp <-  mapply(item_extraction, share_tmp,i=6)
  df$share_tmp <- recode_share(df$share_tmp)
  return(df$share_tmp)
}

#### 04.03 Create subsets for the answers and #####

# Create a subset that contains all the statements
df_statements <- df %>% select(perception1, perception2, perception3, perception4, perception5,
                               perception6, perception7, perception8, perception9, detection1, detection2,
                               detection3, detection4, detection5, detection6)

view(df_statements)
# Extract all items from the all of the statements in the subset
df_bias <- df_statements %>% mutate_all(extract_bias)
df_sent <- df_statements %>% mutate_all(extract_sent)
df_bel <- df_statements %>% mutate_all(extract_bel)
df_truth <- df_statements %>% mutate_all(extract_truth)
df_cred <- df_statements %>% mutate_all(extract_cred)
df_share <- df_statements %>% mutate_all(extract_share)

# Rename the columns to "bias" + the original column name
df_bias <- df_bias %>% rename_all(~ paste0("bias_", .))
df_sent <- df_sent %>% rename_all(~ paste0("sent_", .))
df_bel <- df_bel %>% rename_all(~ paste0("bel_", .))
df_truth <- df_truth %>% rename_all(~ paste0("truth_", .))
df_cred <- df_cred %>% rename_all(~ paste0("cred_", .))
df_share <- df_share %>% rename_all(~ paste0("share_", .))

df <- bind_cols(df, df_bias, df_sent, df_bel, df_truth, df_cred, df_share)

# Output the merged results table 
write.csv(df,file="./data/temp/survey_part1_res.csv", row.names=FALSE)

