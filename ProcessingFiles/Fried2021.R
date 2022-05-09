# jonashaslbeck@gmail.com; May 5, 2022

# ------------------------------------------------------------------
# ------------- Load data ------------------------------------------
# ------------------------------------------------------------------

load("DataFromAuthors/Fried2021/4.Data/clean_ema.RData") # within-person measurements
load("DataFromAuthors/Fried2021/4.Data/clean_prepost.RData") # between-person measurements

# ------------------------------------------------------------------
# ------------- Process --------------------------------------------
# ------------------------------------------------------------------

# ----- Within Data -----

data <- rawdata[, -c(2:5, 24)] # delete unnecessary columns

# Turn id into integer
data$ID <- as.character(data$ID)
u_id <- unique(data$ID)
for(i in 1:length(u_id)) data$ID[data$ID == u_id[i]] <- i
table(data$ID)

# Turn Day into Integer
data$Day <- as.character(data$Day)
u_day <- unique(data$Day)
for(i in 1:length(u_day)) data$Day[data$Day == u_day[i]] <- i
table(data$Day)

# Add Variable Names

colnames(data) <- c("subj_id", "Relax", "Irritable", "Worry", "Nervous", "Future", "Anhedonia", 
                    "Tired", "Hunger",  "Alone", "Angry", "Social_Offline", "Social_Online", 
                    "Music", "Procrast", "Outdoors", "C19_Occupied", 
                    "C19_Worry", "Home", 
                    "dayvar", "beepvar")
head(data)

data$dayvar <- as.numeric(data$dayvar)
data$beepvar <- as.numeric(data$beepvar)

# ----- Between Data -----
# Oisin added 30 March 2022 - add between person information

df <- rawdata_full
DASS <- data.frame("id"= df[,1], "DASSpre"= rowSums(df[,c(14:34)]))

# drop people who are not in the original dataset
u_id2 <- as.character(DASS[,1])
DASS1 <- DASS[(u_id2 %in% u_id),]
DASS1

# check for people who are in the time series dataset but not the betweenperson
u_id[!u_id %in% u_id2]
all(as.character(DASS1[,1]) == u_id[-45])

# add new 
DASS_pre <- c(DASS1[1:44, 2], NA, DASS1[45:nrow(DASS1),2])

id <- as.numeric(unique(data$subj_id))
databetween <- cbind(id, DASS_pre)


# ------------------------------------------------------------------
# ------------- Save --------------------------------------------
# ------------------------------------------------------------------

# within
saveRDS(data, file = "DataClean/Fried2021/data_Fried2021.RDS")

# between
saveRDS(databetween, file = "DataClean/Fried2021/data_Fried2021_between.RDS")


