# jonashaslbeck@gmail.com; May 5, 2022

# ------------------------------------------------------------------
# ------------- Load data ------------------------------------------
# ------------------------------------------------------------------

data <- read.table("DataFromAuthors/Bringmann2016/Data95.csv", header=TRUE, sep=";")

# ------------------------------------------------------------------
# ------------- Process --------------------------------------------
# ------------------------------------------------------------------

head(data)

# Replace 999 with NA
data[data==9999] <- NA

# Remove lagged variables
data <- data[, -c(3, 5, 7, 9, 11, 13, 14)]

# Rename to English from paper
colnames(data) <- c("subj_id", "Angry", "Depressed", "Dysphoric", "Anxious", "Relaxed", "Happy", "Neuroticism")

head(data)

# ------------------------------------------------------------------
# ------------- Save --------------------------------------------
# ------------------------------------------------------------------

saveRDS(data, file = "DataClean/Bringmann2016/data_Bringmann2016.RDS")
