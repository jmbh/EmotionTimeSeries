# jonashaslbeck@gmail.com; May 5, 2022

# ------------------------------------------------------------------
# ------------- Load data ------------------------------------------
# ------------------------------------------------------------------

dat <- read.table("DataFromAuthors/Rowland2020/data_affect.csv", sep=",", header=TRUE, na.strings="")

# ------------------------------------------------------------------
# ------------- Process --------------------------------------------
# ------------------------------------------------------------------

# Var names from paper
outnames <- c("happy", "excited", "relaxed", "satisfied", "angry","anxious", "depressed", "sad")

# Sort data & add item names
dat <- dat[order(dat$subjno, dat$group, dat$dayno, dat$beep),]
names(dat)[pmatch(c("emo1_m", "emo2_m", "emo3_m", "emo4_m", "emo5_m", "emo6_m", "emo7_m", "emo8_m"),
                  names(dat))] <- outnames
data <- dat
colnames(data)[1] <- "subj_id"

head(data)


# ------------------------------------------------------------------
# ------------- Save --------------------------------------------
# ------------------------------------------------------------------

saveRDS(data, file = "DataClean/Rowland2020/data_Rowland2020.RDS")


