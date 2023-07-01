####  00. Setup  ####
#install.packages('dplyr')
#install.packages("reshape2")
#install.packages("data.table")
#install.packages("gsubfn")
#install.packages("stopwords")
#install.packages("irr")

library(tidyverse) # Data Science
library(jsonlite) #conver JSON to R
library(dplyr) #data manipulation, summarize
library(data.table) #join & modify tables
library(reshape2) #restructure and aggregate with melt & dcast 
library(caret) #F1 Scores & Confusion matrix
library(stopwords)
library(irr)

rm(list =ls())
setwd("/Users/ann/Documents/University/SS23/MA_Media-Literacy-by-Design/Repository")

#### 1.0 Load data ####


# Results
df_p <-read.csv("./data/temp/survey_part1_res.csv")

# Particpants that were identified as cheaters manually, e.g. not annotating at all, just clicking on the first word
cheat <-read.csv("./data/temp/anno_cheated.csv")
df_p <- df_p[!df_p$participantId %in% cheat[,1], ]


# Read Gold standard
gold <- fromJSON("./data/raw/articles_ref.json", flatten = TRUE) 

#### 2.0 Transform Gold Standard ####

# Flatten it into something understandable:[[2 = select Conten]][[ID e.g rmea]][[select content]][[select sentence]]
# 1 = rmea, 2 = cmea, 3 = lmea, 4 = rhia, 5 = lhi, 6 = chia
# Access item of nested list gold_f[[X]][[X]][[X]]

gold_f <- list()

for (k in 1:length(gold[[1]])){
  json_inner <- data.frame()
  for (i in 1:length(gold[[2]][[k]][[2]])){
    json_iteration <- data.frame(gold[[2]][[k]][[2]][i])
    json_inner <- rbind(json_inner, json_iteration)
  }
  gold_f <- c(gold_f, list(json_inner))
}
rm(json_inner, json_iteration, gold, i ,k)

#### 3.0 Add Annotation Results #### 


### 3.1 Create an annotation list per participant ###
anno_unlist <- function(statement, gold_index) { 
  for (i in 1:nrow(df_p)) {
    
    #Get participantID, create new participant column and set all annotations to false
    column_char <- as.character(df_p$participantId[i])
    gold_f[[gold_index]][column_char] <- rep("FALSE", nrow(gold_f[[gold_index]]))
    
    #Unpack the annotation list
    anno_lst <- as.numeric(unlist(regmatches(statement[i], gregexpr("[[:digit:]]+",statement[i]))))
    
    #Insert true values for participant annotations, if the participant did not annotate at all replace with NA
    if(length(anno_lst) != 0  ) {
      for (k in 1:length(anno_lst)) {
        gold_f[[gold_index]][[column_char]][anno_lst[k]+1] <- "TRUE"
      }
    } else {gold_f[[gold_index]][column_char] <- rep(NA, nrow(gold_f[[gold_index]]))}
  }
  return(gold_f[[gold_index]])
}

gold_f[[1]] <- anno_unlist(df_p$annotation1, 1)
gold_f[[2]] <- anno_unlist(df_p$annotation2, 2)
gold_f[[3]] <- anno_unlist(df_p$annotation3, 3)
gold_f[[4]] <- anno_unlist(df_p$annotation4, 4)
gold_f[[5]] <- anno_unlist(df_p$annotation5, 5)
gold_f[[6]] <- anno_unlist(df_p$annotation6, 6)


### 3.2 Replace zero annotations with false only for people who did not see any bias ###

## Return the bias perception for each participant with 0 annotations
p_no_bias <- function(pID, col) {
  row_index <- which(pID == df_p$participantId)
  p_bias <- df_p[row_index, col]
  
  if (p_bias < 1) {
    return(TRUE)
  } else { return(FALSE)}
}

### Get participabt IDs for all participants with 0 annotations per statement
na_cols_1 <- colSums(is.na(gold_f[[1]])) > 0
cols_with_na_1 <- colnames(gold_f[[1]])[na_cols_1]

na_cols_2 <- colSums(is.na(gold_f[[2]])) > 0
cols_with_na_2 <- colnames(gold_f[[2]])[na_cols_2]

na_cols_3 <- colSums(is.na(gold_f[[3]])) > 0
cols_with_na_3 <- colnames(gold_f[[3]])[na_cols_3]
#view(gold_f[[3]][, na_cols_3])

na_cols_4 <- colSums(is.na(gold_f[[4]])) > 0
cols_with_na_4 <- colnames(gold_f[[4]])[na_cols_4]

na_cols_5 <- colSums(is.na(gold_f[[5]])) > 0
cols_with_na_5 <- colnames(gold_f[[5]])[na_cols_5]

na_cols_6 <- colSums(is.na(gold_f[[6]])) > 0
cols_with_na_6 <- colnames(gold_f[[6]])[na_cols_6]


### Create a list of participants that really did not see any bias
cols_unbiased_1 <- cols_with_na_1[sapply(cols_with_na_1, p_no_bias, "bias_detection1")]
cols_unbiased_2 <- cols_with_na_2[sapply(cols_with_na_2, p_no_bias, "bias_detection2")]
cols_unbiased_3 <- cols_with_na_3[sapply(cols_with_na_3, p_no_bias, "bias_detection3")]
cols_unbiased_4 <- cols_with_na_4[sapply(cols_with_na_4, p_no_bias, "bias_detection4")]
cols_unbiased_5 <- cols_with_na_5[sapply(cols_with_na_5, p_no_bias, "bias_detection5")]
cols_unbiased_6 <- cols_with_na_6[sapply(cols_with_na_6, p_no_bias, "bias_detection6")]

## Replace annotations of all participants who did not see any bias with FALSE
replace_unbiased <- function(pID, sID) {
  for (id in pID) {
    matching_col <- grep(id, colnames(sID), value = TRUE)
    sID[, matching_col] <- "FALSE"
  } 
  return(sID)
}

### Apply the replace function to all statements
gold_f[[1]] <- replace_unbiased(cols_unbiased_1, gold_f[[1]])
gold_f[[2]] <- replace_unbiased(cols_unbiased_2, gold_f[[2]])
gold_f[[3]] <- replace_unbiased(cols_unbiased_3, gold_f[[3]])
gold_f[[4]] <- replace_unbiased(cols_unbiased_4, gold_f[[4]])
gold_f[[5]] <- replace_unbiased(cols_unbiased_5, gold_f[[5]])
gold_f[[6]] <- replace_unbiased(cols_unbiased_6, gold_f[[6]])



#### 4.0 Handle Stopwords #### 

# Get the english stopword list
stopwords <- stopwords("en")
# Remove words that can still contain bias 
to_remove <- c("only", "over", "do", "again", "not", "by", "more")
stopwords <- setdiff(stopwords, to_remove)
view(stopwords)
#Add make as stopword because of the majority problem with "made a risky bet"
stopwords <- c(stopwords, "made", "going")

# takes a word, makes it lower case and checks if it's contained in the stopword dictionary
is_stopw <- function(word) {
  word <- tolower(gsub('"', "", word))
  if (word %in% stopwords){
    word <- NA
  } 
  return(word) 
}

# Create a gold standard with word and NAs for stopword
gold_f[[1]]$exp <- lapply(gold_f[[1]]$text, is_stopw)
gold_f[[2]]$exp <- lapply(gold_f[[2]]$text, is_stopw)
gold_f[[3]]$exp <- lapply(gold_f[[3]]$text, is_stopw)
gold_f[[4]]$exp <- lapply(gold_f[[4]]$text, is_stopw)
gold_f[[5]]$exp <- lapply(gold_f[[5]]$text, is_stopw)
gold_f[[6]]$exp <- lapply(gold_f[[6]]$text, is_stopw)

#Experimental: Remove all the rows that contain stopwords
gold_f[[1]] <- gold_f[[1]][!is.na(gold_f[[1]]$exp), ]
gold_f[[2]] <- gold_f[[2]][!is.na(gold_f[[2]]$exp), ]
gold_f[[3]] <- gold_f[[3]][!is.na(gold_f[[3]]$exp), ]
gold_f[[4]] <- gold_f[[4]][!is.na(gold_f[[4]]$exp), ]
gold_f[[5]] <- gold_f[[5]][!is.na(gold_f[[5]]$exp), ]
gold_f[[6]] <- gold_f[[6]][!is.na(gold_f[[6]]$exp), ]

#Remove the exp stopwords column
gold_f[[1]] <- gold_f[[1]][, -ncol(gold_f[[1]])]
gold_f[[2]] <- gold_f[[2]][, -ncol(gold_f[[2]])]
gold_f[[3]] <- gold_f[[3]][, -ncol(gold_f[[3]])]
gold_f[[4]] <- gold_f[[4]][, -ncol(gold_f[[4]])]
gold_f[[5]] <- gold_f[[5]][, -ncol(gold_f[[5]])]
gold_f[[6]] <- gold_f[[6]][, -ncol(gold_f[[6]])]


#### 5.0 Annotator Agreement #### 

# 5.1 Majority vote on TRUE / FALSE labels 

calc_maj <- function(row) {
  # exclude NA values from the input row vector
  row_no_na <- row[!is.na(row)]
  na_count <- sum(is.na(row))
  t_count <- sum(row_no_na == TRUE)
  f_count <- sum(row_no_na == FALSE)
  if (t_count > f_count) {
    maj <- TRUE
    maj_percent <- t_count / length(row_no_na) * 100
  } else {
    maj <- FALSE
    maj_percent <- f_count / length(row_no_na) * 100
  }
  c(maj, maj_percent, t_count, f_count, na_count)
}

# Apply the calculation to each row of the data frame using lapply
results_list <- apply(gold_f[[2]][, -c(1:2)], 1,calc_maj)
results_df <- data.frame(majority = results_list[1,],
                         majority_percentage = results_list[2,],
                         true_votes = results_list[3,],
                         false_votes = results_list[4,],
                         na_count = results_list[5,])

st_1 <- cbind(gold_f[[1]][, 1:2], results_df)
st_2 <- cbind(gold_f[[2]][, 1:2], results_df)
st_3 <- cbind(gold_f[[3]][, 1:2], results_df)
st_4 <- cbind(gold_f[[4]][, 1:2], results_df)
st_5 <- cbind(gold_f[[5]][, 1:2], results_df)
st_6 <- cbind(gold_f[[6]][, 1:2], results_df)

# 5.2 IRR per Article

#Krippendorf alpha 0.4 (Expert - Krieger Study) / 
#Spinde: α = .101. Annotators on Prolific reach an α = .144
#Krippendorff's Alpha is based on the concept of ratio calculated between observed disagreement(Pa) & disagreement expected by chance(Pe)

kripp.alpha(t(gold_f[[6]][, -c(1:2)]))


#### 6.0 Annotation Accuracy ####

#create confusion matrix and calculate metrics related to confusion matrix

# Calculate the F1 score per participants

calc_metrics <- function(participant, gold) { 
  if (!is.na(participant[1])) {
    levels <- c("TRUE", "FALSE")
    gold <- factor(gold, levels = levels)
    participant <- factor(participant, levels = levels)
    
    # Compute confusion matrix
    cm <- confusionMatrix(data = participant, reference = gold, mode = "everything", positive="TRUE")

    # Calculate metrics
    acc <- round(unname(cm$overall['Accuracy']), digits = 3)
    prec <- round(unname(cm$byClass['Precision']), digits = 3)
    recall <- round(unname(cm$byClass['Recall']), digits = 3)
    f1 <-  round(unname(cm$byClass['F1']), digits = 3)
    pos <- sum(participant == "TRUE")
    tp <- unname(cm$table[1,1])
    fp <- unname(cm$table[1,2])
    fn <- unname(cm$table[2,1])
    tn <- unname(cm$table[2,2])
    
    
    # Return results
    result <- list(ACC = acc,
                   Precision = prec,
                   Recall = recall,
                   F1 = f1,
                   Positives = pos,
                   TP = tp,
                   FP = fp,
                   FN = fn,
                   TN = tn)
  } else {
    result <- list(ACC = NA,
                   Precision = NA,
                   Recall = NA,
                   F1 = NA,
                   Positives = NA,
                   TP = NA,
                   FP = NA,
                   FN = NA,
                   TN = NA)
  }
  
  return(result)
}

### 6.1 Calculate scores for each statement ###
metrics_1 <- lapply(gold_f[[1]][,3:216], calc_metrics , gold = gold_f[[1]][,2])
scores1_l <- do.call(rbind, metrics_1)
scores1_df <- rownames_to_column(as.data.frame(scores1_l), var = "participantId")
colnames(scores1_df) <- c("participantId", "ACC_1", "Precision_1", "Recall_1", "F1_1", "Annotations_1", "TP_1", "FP_1", "FN_1", "TN_1")
scores1_df[, -1] <- lapply(scores1_df[, -1], as.numeric)

metrics_2 <- lapply(gold_f[[2]][,3:216], calc_metrics , gold = gold_f[[2]][,2])
scores2_l <- do.call(rbind, metrics_2)
scores2_df <- rownames_to_column(as.data.frame(scores2_l), var = "participantId")
colnames(scores2_df) <- c("participantId", "ACC_2", "Precision_2", "Recall_2", "F1_2", "Annotations_2", "TP_2", "FP_2", "FN_2", "TN_2")
scores2_df[, -1] <- lapply(scores2_df[, -1], as.numeric)

metrics_3 <- lapply(gold_f[[3]][,3:216], calc_metrics , gold = gold_f[[3]][,2])
scores3_l <- do.call(rbind, metrics_3)
scores3_df <- rownames_to_column(as.data.frame(scores3_l), var = "participantId")
colnames(scores3_df) <- c("participantId", "ACC_3", "Precision_3", "Recall_3", "F1_3", "Annotations_3", "TP_3", "FP_3", "FN_3", "TN_3")
scores3_df[, -1] <- lapply(scores3_df[, -1], as.numeric)

metrics_4 <- lapply(gold_f[[4]][,3:216], calc_metrics , gold = gold_f[[4]][,2])
scores4_l <- do.call(rbind, metrics_4)
scores4_df <- rownames_to_column(as.data.frame(scores4_l), var = "participantId")
colnames(scores4_df) <- c("participantId", "ACC_4", "Precision_4", "Recall_4", "F1_4", "Annotations_4", "TP_4", "FP_4", "FN_4", "TN_4")
scores4_df[, -1] <- lapply(scores4_df[, -1], as.numeric)

metrics_5 <- lapply(gold_f[[5]][,3:216], calc_metrics , gold = gold_f[[5]][,2])
scores5_l <- do.call(rbind, metrics_5)
scores5_df <- rownames_to_column(as.data.frame(scores5_l), var = "participantId")
colnames(scores5_df) <- c("participantId", "ACC_5", "Precision_5", "Recall_5", "F1_5", "Annotations_5", "TP_5", "FP_5", "FN_5", "TN_5")
scores5_df[, -1] <- lapply(scores5_df[, -1], as.numeric)

metrics_6 <- lapply(gold_f[[6]][,3:216], calc_metrics , gold = gold_f[[6]][,2])
scores6_l <- do.call(rbind, metrics_6)
scores6_df <- rownames_to_column(as.data.frame(scores6_l), var = "participantId")
colnames(scores6_df) <- c("participantId", "ACC_6", "Precision_6", "Recall_6", "F1_6", "Annotations_6", "TP_6", "FP_6", "FN_6", "TN_6")
scores6_df[, -1] <- lapply(scores6_df[, -1], as.numeric)

dsc <- list(scores1_df, scores2_df, scores3_df, scores4_df, scores5_df, scores6_df) %>% reduce(inner_join, by='participantId')

# Output the merged results table 
write.csv(dsc,file="./data/temp/survey_part2_res2.csv", row.names=FALSE)

write.csv(gold_f[[1]],file="./data/temp/gold_1.csv", row.names=FALSE)
write.csv(gold_f[[2]],file="./data/temp/gold_2.csv", row.names=FALSE)
write.csv(gold_f[[3]],file="./data/temp/gold_3.csv", row.names=FALSE)
write.csv(gold_f[[4]],file="./data/temp/gold_4.csv", row.names=FALSE)
write.csv(gold_f[[5]],file="./data/temp/gold_5.csv", row.names=FALSE)
write.csv(gold_f[[6]],file="./data/temp/gold_6.csv", row.names=FALSE)
