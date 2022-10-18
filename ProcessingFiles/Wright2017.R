# jonashaslbeck@gmail.com; Oct 18, 2022

# ------------------------------------------------------------------
# ------------- Load data ------------------------------------------
# ------------------------------------------------------------------

# Fisher Reanalysis
data <- read.table("DataFromAuthors/Wright2017/data/iads_sample3_ild.csv", 
                   header=TRUE, sep=";")

datab <- read.table("DataFromAuthors/Wright2017/data/ffm3.csv", 
                    header=TRUE, sep=";")


# ------------------------------------------------------------------
# ------------- Process --------------------------------------------
# ------------------------------------------------------------------


# -------- Within-person data --------

#### From here: code from Wendt 2020
dat <- data

dummy2 <- data.frame(ddply( dat, .(UsrID), function(x) length(x$UsrID) ))

which.na <- which(dat$tmstmp1 == 2)
dat1 <- dat[-which.na,]
dat1 <- unique(dat1)
dat1$na.sum <- rowSums(is.na(dat1))

which.na.sum <- which(dat1$na.sum > 4)
dat1 <- dat1[-which.na.sum ,]

dat1 <- dat1[
  with(dat1, order(UsrID, Date, tmstmp1)),
]


trial <- 0
id <- 0
i <- 1
date <- 0
#  if (dat1$date[i]==id) { 

for (i in 1:nrow(dat1)){
  if (dat1$UsrID[i]==id) {
    trial <- trial+1
  } else {    
    trial <- 0
  }
  id <- dat1$UsrID[i]
  dat1$trial[i] <- trial+1
}

date <- 1160
id <- 80000
trial <- 1

for (i in 1:nrow(dat1)){
  
  if (dat1$UsrID[i]==id) { 
    
    if (dat1$Date[i]==date) {trial <- trial+1} else {trial <- trial+2}
    
    dat1$trial[i] <- trial
    date <- dat1$Date[i]
    
  } else 
    
  {
    trial <- 1
    date <- dat1$Date[i]
    id <- dat1$UsrID[i]
  }
  
}

min.occ = 20 #minimum of occasions per subject (=/= subsequent occasions)
max.occ = 600 #maximum of occasions per subject (here = 30)


# Daten ordnen nach vp_id und tbday
dat1 <- dat1[with(dat1, order(UsrID, trial)),]

table <- table(dat1$UsrID) # Occasions pro Subject anzeigen
which1 <- names(table[table < min.occ]) # Subjects mit weniger als x occasions ausschlie?en
which2 <- names(table[table > max.occ]) # Subjects mit mehr als x occasions ausschlie?en
dat2 <- dat1[ ! dat1$UsrID %in% which1, ] # anwenden
dat2 <- dat2[ ! dat2$UsrID %in% which2, ] # anwenden

dat <- dat2

colnames(dat) <- c("tmstmp1", "UsrID", "PTNUM", "Dyad", "Date", "Afraid", "Ashamed",
                   "Distressed", "Guilty", "Hostile", "Irritable", "Jittery", 
                   "Nervous", "Scared", "Upset", "Frightened", "Shaky", "Angry",
                   "Scornful", "Disgusted", "Loathing", "Sad", "Blue", "Downhearted",
                   "Alone", "Lonely", "Active", "Alert", "Attentive", "Determined",
                   "Enthusiastic", "Excited", "Inspired", "Interested", "Proud",
                   "Strong","na.sum" , "trial")
dat <- dat[c(38,1:36)]

daily_trial <- 0
Date <- 0
i <- 1

for (i in 1:nrow(dat)){
  if (dat$Date[i]==Date) {
    daily_trial <- daily_trial+1
  } else {
    daily_trial <- 0
  }
  Date <- dat$Date[i]
  dat$daily_trial[i] <- daily_trial+1
}


day <- 1
Date <- 0
i <- 1
UsrID <- 0

for (i in 1:nrow(dat)){
  
  if (dat$UsrID[i]==UsrID) {
  } else {
    day <- 0
  }
  
  if (dat$Date[i]==Date) {
  } else {
    day <- day+1
  }
  Date <- dat$Date[i]
  UsrID <- dat$UsrID[i]
  dat$day[i] <- day
}

dat <- dat[c(39,38,1:37)]
colnames(dat) <- c("day", "beep", "trial", "tmstmp1", "id", "PTNUM", "Dyad","Date",
                   "Afraid","Ashamed", "Distressed",
                   "Guilty" ,"Hostile","Irritable","Jittery","Nervous","Scared",
                   "Upset","Frightened","Shaky","Angry","Scornful", "Disgusted","Loathing",
                   "Sad","Blue",
                   "Downhearted", "Alone", "Lonely", "Active", "Alert","Attentive" ,
                   "Determined", "Enthusiastic", "Excited","Inspired","Interested",
                   "Proud", "Strong")


#### Until here: code from Wendt 2020

dim(dat)
head(dat)

tb <- table(dat$id)
tb
range(tb)
length(tb)

data <- dat

colnames(data)[5] <- "subj_id"


# Transform 3:7 scale into 1:5 scale
table(unlist(data[, 9:39]))
data_15 <- data
data_15[, 9:39] <- data[, 9:39] - 2
table(unlist(data_15[, 9:39]))


# -------- Between-person data --------
# Code: Oisin 

datab <- datab[,1:2]
length(unique(datab[,1]))
length(unique(data$subj_id))

u_id <- unique(data$subj_id)
u_id2 <- unique(datab[,1])

# first, cut from the between data those participants who are not in the original data
u_id3 <- u_id2[(u_id2 %in% u_id)]

# drop rows which aren't in the esm data
datab1 <- datab[(u_id2 %in% u_id),]

# add NAs for people who are in the ESM data
u_id[(u_id %in% u_id3)]

new <- matrix(NA, nrow = length(u_id), ncol = 2)
new[,1] <- u_id
for(i in 1:length(u_id)){
  if(u_id[i] %in% u_id3){
    new[i,2] <- datab1[which(u_id3 == u_id[i]),2]
  } else{
    new[i,2] <- NA
  }
}

colnames(new) = c("subj_id","neuroticism")

# ------------------------------------------------------------------
# ------------- Save --------------------------------------------
# ------------------------------------------------------------------

# Within
saveRDS(data_15, file = "DataClean/Wendt2020/data_Wendt2020.RDS")
# Between
saveRDS(new, file = "DataClean/Wendt2020/data_Wendt2020_between.RDS")




