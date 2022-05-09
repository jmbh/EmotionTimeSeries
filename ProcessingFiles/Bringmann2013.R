# jonashaslbeck@gmail.com; May 5, 2022

# ------------------------------------------------------------------
# ------------- Load data ------------------------------------------
# ------------------------------------------------------------------

data <- read.table("DataFromAuthors/Bringmann2013/pone.0060188.s004.txt", 
                   header=TRUE, sep=",")


# ------------------------------------------------------------------
# ------------- Process --------------------------------------------
# ------------------------------------------------------------------

colnames(data) <- c("subj_id", "dayno", "beepno", "informat", "period", "Excited", "PlstEvent", "Worried", "Anxious", "Sad", "Relaxed", "Neurotic")

# Take out between-measurements
out <- ddply(data, .(subj_id), function(x) x$Neurotic[1] )
colnames(out)[2] <- "Neurotic"

# Delete Neurotic from within-data
data2 <- data[, -12]
head(data2)

# ------------------------------------------------------------------
# ------------- Save --------------------------------------------
# ------------------------------------------------------------------

# Within
saveRDS(data2, file ="DataClean/Bringmann2013/data_Bringmann2013.RDS")

# Between
saveRDS(out, file ="DataClean/Bringmann2013/data_Bringmann2013_between.RDS")