# jonashaslbeck@gmail.com; May 5, 2022

# ------------------------------------------------------------------
# ------------- Load data ------------------------------------------
# ------------------------------------------------------------------

data <- read_dta(file = "DataFromAuthors/Vrijen2018/dataprep_detrending/rawdata.dta") 



# ------------------------------------------------------------------
# ------------- Process --------------------------------------------
# ------------------------------------------------------------------

# ----- Within Data -----

data <- as.data.frame(data)

# Remove cases in which time of day indicator is missing
kill_id <- rowSums(data[, 8:10])!=0
table(kill_id)
data_new <- data[kill_id, ]

# Create a beepvar variable
beepvar <- unlist(apply(data[, 8:10], 1, function(x) which(x==1)))

data_new2 <- cbind(data_new[, 18], beepvar, data_new[, c(12, 1:7)])
data_newbp <- data_new[,"happybias"]

colnames(data_new2) <- c("subj_id", "beepvar", "dayvar", 
                         "Feel_Inter", "Joy", "Sad", "Irritated", 
                         "Worried", "Pleas_Exp", "Unpleas_Exp")

data <- data_new2

# ----- Between Data -----
databet <- cbind(data_new2,data_newbp)
out <- ddply(databet, .(subj_id), function(x) x$data_newbp[1] )
colnames(out)[2] <- "happybias"
head(out)


# ------------------------------------------------------------------
# ------------- Save --------------------------------------------
# ------------------------------------------------------------------

# Within
saveRDS(data, file = "DataClean/Vrijen2018/data_Vrijen2018.RDS")

# Between
saveRDS(out, file = "DataClean/Vrijen2018/data_Vrijen2018_between.RDS")


