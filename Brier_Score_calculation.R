# install.packages("DescTools")

# Load the DescTools package
library(DescTools)
library(ModelMetrics)
library (dplyr)

# set working directory
wd <- "~/R_Scripts/NEON-Climate-variable-validation/data" 
setwd(wd)

# Read csv file
df <- read.csv("Validated_Fire_No_Fire_Sample_Points_2021_seed_1.csv", stringsAsFactors = FALSE)

# Define your predicted probabilities and observed outcomes
dfsubset <- subset(df, select=c(label, classification))
# dfsubset <- dfsubset %>% 
#   filter(classification > 0.7)
predicted_probabilities <- dfsubset$classification
observed_outcomes <- dfsubset$label

# Using DescTools
brier_score_desc <- BrierScore(observed_outcomes, predicted_probabilities)

# Using Manual Calculation
goulden_score_manual <- sqrt(mean((predicted_probabilities - observed_outcomes)^2))

# Log-loss using ModelMetrics
logloss_modelmetric <- logLoss(observed_outcomes, predicted_probabilities)

# Print both Brier Scores
print(brier_score_desc)
print(goulden_score_manual)
print(logloss_modelmetric)

