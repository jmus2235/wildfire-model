# install.packages("DescTools")

# Load the DescTools package
library(DescTools)

# set working directory
wd <- "~/R_Scripts/NEON-Climate-variable-validation/data" 
setwd(wd)

# Read csv file
df <- read.csv("Validated_Fire_No_Fire_Sample_Points_2019_seed_2.csv", stringsAsFactors = FALSE)

# Define your predicted probabilities and observed outcomes
predicted_probabilities <- df$classification
observed_outcomes <- df$label

# Using DescTools
brier_score_desc <- BrierScore(observed_outcomes, predicted_probabilities)

# Using Manual Calculation
brier_score_manual <- mean((predicted_probabilities - observed_outcomes)^2)

# Print both Brier Scores
print(brier_score_desc)
print(brier_score_manual)
