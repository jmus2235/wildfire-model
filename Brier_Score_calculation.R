# install.packages("DescTools")
# install.packages("ModelMetrics")

# Load the DescTools package
library(DescTools)
library(ModelMetrics)
library (dplyr)
library(pROC)

# set working directory
wd <- "~/R_Scripts/NEON-Climate-variable-validation/data/validation" 
setwd(wd)

# Create an empty data frame
validation_stats <- data.frame(filename=character(),
                           accuracy=numeric(),
                           precision=numeric(),
                           recall=numeric(),
                           specificity=numeric(),
                           F1_Score=numeric(),
                           brier_score=numeric(),
                           auc_score=numeric(),
                           stringsAsFactors=FALSE)

# Save all file names to  variable
filelist<- list.files(pattern = ".csv") #make file list with all .csv files in directory

# Loop through validation files
for(i in filelist){

  # Read csv file
df <- read.csv(i, stringsAsFactors = FALSE)

# Define predicted probabilities and observed outcomes
dfsubset <- subset(df, select=c(label, classification))
predicted_probabilities <- dfsubset$classification
observed_outcomes <- dfsubset$label

# Using DescTools
brier_score_desc <- as.data.frame(BrierScore(observed_outcomes, predicted_probabilities))

# Using Manual Calculation
goulden_score_manual <- sqrt(mean((predicted_probabilities - observed_outcomes)^2))

# Log-loss using ModelMetrics
logloss_modelmetric <- logLoss(observed_outcomes, predicted_probabilities)

# AUC calculation
roc_object <- roc( observed_outcomes, predicted_probabilities)
# auc1 <- as.data.frame(auc(roc_object))
auc1 <- (auc(roc_object))
auc1 <- as.numeric(auc1)
auc2 <- as.data.frame(auc1)

# Create confusion matrix
true_positive <- dfsubset %>% 
  filter(label == 1) %>% 
  filter(classification > 0.5)

true_negative <- dfsubset %>% 
  filter(label == 0) %>% 
  filter(classification < 0.5)

false_positive <- dfsubset %>% 
  filter(label == 0) %>% 
  filter(classification > 0.5)

false_negative <- dfsubset %>% 
  filter(label == 1) %>% 
  filter(classification < 0.5)

# Calculate stats
accuracy <- (count(true_positive) + count(true_negative)) / (count(true_positive) + count(true_negative) + count(false_positive) + count(false_negative))

precision <- count(true_positive) / (count(true_positive) + count(false_positive))

recall <-  count(true_positive) / (count(true_positive) + count(false_negative))

specificity <- count(true_negative) / (count(true_negative) + count(false_positive))

F1_Score <- 2*(precision*recall)/(precision + recall)

# Print statistics
print(brier_score_desc)
print(goulden_score_manual)
print(logloss_modelmetric)
accuracy
precision
recall
specificity
F1_Score

# Save current csv file name to data frame
filename <- as.data.frame(i)

# Merge data frames with stats
df_merge <- cbind(filename, accuracy, precision, recall, specificity, F1_Score, brier_score_desc, auc2)

# Rename columns
colnames(df_merge)[1] = "filename"
colnames(df_merge)[2] = "accuracy"
colnames(df_merge)[3] = "precision"
colnames(df_merge)[4] = "recall"
colnames(df_merge)[5] = "specificity"
colnames(df_merge)[6] = "F1_Score"
colnames(df_merge)[7] = "brier_score"
colnames(df_merge)[8] = "auc_score"

# Append dataframes from different years together into one output dataframe
validation_stats <- bind_rows(validation_stats, df_merge)

}

write.csv(validation_stats, file=("~/R_Scripts/NEON-Climate-variable-validation/data_out/validation_accuracy/model_validation_accuracy_stats.csv"))