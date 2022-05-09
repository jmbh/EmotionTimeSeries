### ### ### ### ### ### ### ### ### ### ### ### ###
### INDICATORS OF AFFECT DYNAMICS   ###
###
###  REPRODUCIBLE CODE

### submitted to the European Journal of 
### Personality's special issue:
###      New approaches towards conceptualizing and 
###      assessing personality

# For this code to run, you need the packages installed
# and five .csv-files readily available at the OSF page

# Link: https://osf.io/6ghcx/

### Data preparation: Sample 3

setwd("D:/sera/my google drive/R Daten/OSF iads")



## Packages
library(mgcv)# version 1.8-15
library(quantmod)# version 0.4-0
library(schoolmath)
library(paran)
library(MplusAutomation)
library(mlVAR)
library(summarytools)
library(psych)
library(plyr)
library(dplyr)
library(data.table)
library(graphicalVAR)
library(foreign)
library(qgraph)
library(MplusAutomation)
library(ggplot2)
library(sjPlot)
library(naniar)
library(relativeVariability) # not available on CRAN, see https://ppw.kuleuven.be/okp/software/relative_variability/
library(Hmisc)


dat <- read.table("iads_sample3_ild.csv", sep = ";", header = TRUE)

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

#############################
#############################
####
####    Datenaufbereitung
####
#############################


# Daten ordnen nach vp_id und tbday
dat1 <- dat1[with(dat1, order(UsrID, trial)),]
## Datenbereinigung (Subjects)


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
#view(dfSummary(dat))





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
colnames(dat) <- c("day", "beep", "trial", "tmstmp1", "id", "PTNUM", "Dyad","Date","Afraid","Ashamed", "Distressed",
                   "Guilty" ,"Hostile","Irritable","Jittery","Nervous","Scared",
                   "Upset","Frightened","Shaky","Angry","Scornful", "Disgusted","Loathing","Sad","Blue",
                   "Downhearted", "Alone", "Lonely", "Active", "Alert","Attentive" ,
                   "Determined", "Enthusiastic", "Excited","Inspired","Interested",
                   "Proud", "Strong")


mean(ddply( dat, .(id), function(x) length(unique(x$day)) )$V1) # mean of 20.50 unique days
min(ddply( dat, .(id), function(x) length(unique(x$day)) )$V1) # minimum of 7 unique days
max(ddply( dat, .(id), function(x) length(unique(x$day)) )$V1) # maximum of 33 unique days

mean(ddply( dat, .(id), function(x) mean(x$beep) )$V1)          # mean of 4.12 beeps per day
min(ddply( dat, .(id), function(x) mean(x$beep) )$V1)          # mean of 1.43 beeps per day
max(ddply( dat, .(id), function(x) mean(x$beep) )$V1)          # mean of 8.36 beeps per day
length(table(dat$id))                                           # N = 225





write.table(dat, "sample3_ema.csv", col.names = T, sep = ";", row.names = F)

dat2 <- read.table("sample3_ema.csv", sep = ";", header = TRUE)
dat <- dat2

resp.day.mean <- ddply( dat2, .(id), function(x) length(unique(x$day)) )
use2 <- resp.day.mean$id[resp.day.mean$V1 > 20]
# at least 20 days
dat3 <- dat2[dat2$id %in% use2, ]

resp.beep.mean <- ddply( dat3, .(id), function(x) mean(x$beep))
use3 <- resp.beep.mean$id[resp.beep.mean$V1 > 2]
# at least 3 beeps per day on average
dat4 <- dat3[dat3$id %in% use3, ]

mean(ddply( dat4, .(id), function(x) length(x$trial) )$V1)       # mean of 125 trials
mean(ddply( dat4, .(id), function(x) length(unique(x$day)) )$V1) # mean of 22.81 unique days
mean(ddply( dat4, .(id), function(x) mean(x$beep) )$V1)          # mean of 3.68 beeps per day
length(table(dat4$id))  


# 
test <- data.frame(matrix(, nrow = 0, ncol = 712))
names(test)<-c(colnames(dat4))
retest <- data.frame(matrix(, nrow = 0, ncol = 712))
names(retest)<-c(colnames(dat4))


# even/uneven
i=1
for (i in 1:20552){
  #  resp.day.mean <- ddply( dat4, .(id[id == dat4[i,"id"]]), function(x) is.even(x$day) )[2]
  if (is.even(dat4$day[i]) == TRUE) {
    test <- rbind(test, dat4[i,])
  } else {
    retest <- rbind(retest, dat4[i,])
  }
}


consec.occ.test <- ddply( test, .(id), function(x) length(which(diff(c(x$trial, na.rm = T)) == 1)) )
which.not.consec.test <- which(consec.occ.test$V1 < 20)
which.id.test <- consec.occ.test[which.not.consec.test,]$id

consec.occ.retest <- ddply( retest, .(id), function(x) length(which(diff(c(x$trial, na.rm = T)) == 1)) )
which.not.consec.retest <- which(consec.occ.retest$V1 < 20)
which.id.retest <- consec.occ.retest[which.not.consec.retest,]$id

which.id.not.consec <- c(which.id.test, which.id.retest)

retest <- retest[retest$id %nin% which.id.not.consec, ]
test <- test[test$id %nin% which.id.not.consec, ]

# consec days full
consec.occ.dat <- ddply( dat, .(id), function(x) length(which(diff(c(x$trial, na.rm = T)) == 1)) )
which.not.consec.dat <- which(consec.occ.dat$V1 < 20)
which.id.dat <- consec.occ.dat[which.not.consec.dat,]$id
dat <- dat[dat$id %nin% which.id.dat, ]




t1_days <- mean(ddply( test, .(id), function(x) length(unique(x$day)) )$V1) # mean of 20.50 unique days
t2_days <-mean(ddply( retest, .(id), function(x) length(unique(x$day)) )$V1) # mean of 20.50 unique days



t1_beep <- mean(ddply( test, .(id), function(x) mean(x$beep) )$V1)          # mean of 4.12 beeps per day
t2_beep <- mean(ddply( retest, .(id), function(x) mean(x$beep) )$V1)          # mean of 4.12 beeps per day

(t1_days+t2_days)/2
(t1_beep+t2_beep)/2
(t1_days*t1_beep+t2_days*t2_beep)/2

(t1_days*t1_beep)
(t2_days*t2_beep)

length(table(retest$id)) 

# first half vs. second half
# i=1
# for (i in 1:17047){
#   resp.day.mean <- ddply( dat4, .(id[id == dat4[i,"id"]]), function(x) length(unique(x$day))/2 )[2]
#   if (dat4$day[i]< resp.day.mean) {
#     test <- rbind(test, dat4[i,])
#   } else {
#     retest <- rbind(retest, dat4[i,])
#   }
# }

dummy <- data.frame(ddply( dat, .(id), function(x) length(x$id) ))
nrow(dummy)/nrow(dummy2)

write.table(retest, "paul_retest_lachs.csv", col.names = T, sep = ";", row.names = F)
write.table(test, "paul_test_lachs.csv", col.names = T, sep = ";", row.names = F)
write.table(dat, "paul_dat.csv", col.names = T, sep = ";", row.names = F)

## START HERE
retest <- read.table("paul_retest_lachs.csv", sep=";", header = TRUE)
test <- read.table("paul_test_lachs.csv", sep=";", header = TRUE)
dat <- read.table("paul_dat.csv", sep=";", header = TRUE)

subset.pca <- function (x) {
  x <- subset(x, select = c(  "id","mean.pos", "mean.neg", "mean.hos", 
                              "sd.pos","sd.neg","sd.hos",  
                              "mssd.pos","mssd.neg","mssd.hos",
                              "rl.sd.pos" ,"rl.sd.neg","rl.sd.hos" ,
                              "rl.mssd.pos","rl.mssd.neg","rl.mssd.hos", 
                              "alpha.pos","alpha.neg","alpha.hos",
                              "temp.ar.pos","temp.ar.neg","temp.ar.hos" ,
                              "temp.cl.pos.neg", "temp.cl.pos.hos", 
                              "temp.cl.neg.hos","temp.cl.neg.pos",
                              "temp.cl.hos.pos","temp.cl.hos.neg",     
                              "cont.pcor.neg.pos", "cont.pcor.neg.hos","cont.pcor.hos.pos"))
  colnames(x) <- c("id","P.Mean","N.Mean","H.Mean", 
                   "P.SD","N.SD","H.SD",        
                   "P.MSSD","N.MSSD","H.MSSD", 
                   "P.rSD","N.rSD","H.rSD",
                   "P.rMSSD","N.rMSSD","H.rMSSD", 
                   "P.ALPHA","N.ALPHA","H.ALPHA", 
                   "pP","nN","hH",
                   "pN","pH",
                   "nH","nP",
                   "hP","hN",
                   "P_N","N_H","P_H")
  return(x)
}


## Multi-level EFA

paul_all_1515mefa <- mplusObject(
  TITLE = "ML EFA;",  
  DATA = "SWMATRIX = paul_all_svm.txt;",
  ANALYSIS = "TYPE= TWOLEVEL EFA 1 5 1 5;
  ROTATION= GEOMIN;",
  VARIABLE = "CLUSTER = id;
  USEVAR = Afraid - Strong;
  CATEGORICAL = Afraid - Strong;",
  SAVEDATA = 'FILE = "paul_all_1515mefa.dat";
  #              SWMATRIX = paul_all_svm.txt;',
  usevariables = c("id","Afraid","Ashamed","Distressed","Guilty",
                   "Hostile","Irritable","Jittery","Nervous",    
                   "Scared","Upset","Frightened","Shaky",
                   "Angry","Scornful","Disgusted","Loathing",
                   "Sad","Blue","Downhearted","Alone","Lonely",     
                   "Active","Alert","Attentive","Determined","Enthusiastic",
                   "Excited","Inspired","Interested","Proud","Strong"),
  rdata = dat)
#fit <- mplusModeler(paul_all_1515mefa, "paul_all_1515mefa.dat", 
#                    modelout = "paul_all_1515mefa.inp", check = TRUE, run = 1)


#### lets go ####


### SKALENBILDUNG ###

items.pos <- c("Alert", "Attentive", "Determined", "Inspired",  "Active", "Enthusiastic", "Excited", 
               "Interested", "Proud", "Strong") 
items.neg <- c("Afraid", "Ashamed", "Nervous","Distressed" ,"Guilty", "Jittery", "Scared", "Frightened", 
               "Shaky", "Sad", "Blue", "Downhearted", "Alone", "Lonely") 
items.hos <- c("Hostile", "Upset", "Irritable", "Angry", "Scornful", "Disgusted", "Loathing") 
d.pos <- dat[,items.pos]
d.neg <- dat[,items.neg]
d.hos <- dat[,items.hos]

dat$day.mean.pos <- apply(d.pos[,items.pos],1,mean,na.rm = TRUE)
dat$day.mean.neg <- apply(d.neg[,items.neg],1,mean,na.rm = TRUE)
dat$day.mean.hos <- apply(d.hos[,items.hos],1,mean,na.rm = TRUE)


dat$pos <- dat$day.mean.pos
dat$hos <- dat$day.mean.hos
dat$neg <- dat$day.mean.neg

dat$day.mean.pos <- scales::rescale(dat$day.mean.pos, to = c(0,10), from = c(3,7))
dat$day.mean.neg <- scales::rescale(dat$day.mean.neg, to = c(0,10), from = c(3,7))
dat$day.mean.hos <- scales::rescale(dat$day.mean.hos, to = c(0,10), from = c(3,7))





person.mean.neg <- ddply( dat, .(id), function(x) mean(x$day.mean.neg, na.rm = TRUE) )
person.mean.hos <- ddply( dat, .(id), function(x) mean(x$day.mean.hos, na.rm = TRUE) )
person.mean.pos <- ddply( dat, .(id), function(x) mean(x$day.mean.pos, na.rm = TRUE) )

dat$day.mean.pos_lag1 =Lag(as.numeric(dat$day.mean.pos,1))
dat$day.mean.neg_lag1 =Lag(as.numeric(dat$day.mean.neg,1))
dat$day.mean.hos_lag1 =Lag(as.numeric(dat$day.mean.hos,1))


person.sd.neg <- ddply( dat, .(id), function(x) sd(x$day.mean.neg, na.rm = TRUE) )
person.sd.hos <- ddply( dat, .(id), function(x) sd(x$day.mean.hos, na.rm = TRUE) )
person.sd.pos <- ddply( dat, .(id), function(x) sd(x$day.mean.pos, na.rm = TRUE) )



# person-specific scale relative mssd
person.rl.mssd.neg <- ddply( dat, .(id), function(x) relativeMSSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.mssd.hos <- ddply( dat, .(id), function(x) relativeMSSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.mssd.pos <- ddply( dat, .(id), function(x) relativeMSSD(x$day.mean.pos, MIN = 0, MAX = 10) )

person.rl.sd.neg <- ddply( dat, .(id), function(x) relativeSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.sd.hos <- ddply( dat, .(id), function(x) relativeSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.sd.pos <- ddply( dat, .(id), function(x) relativeSD(x$day.mean.pos, MIN = 0, MAX = 10) )


# person-specific scale cronbachs alpha
avgcor <- function(y){mean(abs(cor(y)[lower.tri(y)]),na.rm = TRUE)}
person.alpha.pos <- ddply( dat, .(id), function(x) avgcor(x[,c("Alert", "Active", "Attentive", "Determined", "Inspired", 
                                                               "Enthusiastic", "Excited","Interested",
                                                               "Proud", "Strong")]))
person.alpha.hos <- ddply( dat, .(id), function(x) avgcor(x[,c("Hostile", "Upset", "Irritable", "Angry", "Scornful", 
                                                               "Disgusted", "Loathing")]))
person.alpha.neg <- ddply( dat, .(id), function(x) avgcor(x[,c("Nervous", "Ashamed", "Afraid", "Distressed","Guilty", 
                                                               "Jittery", "Scared", "Frightened", 
                                                               "Shaky", "Blue", "Downhearted", "Alone",
                                                               "Lonely")]))



# DSEM
dsem_3 <- mplusObject(
  TITLE = "DSEM3 mlVAR Model;", 
  VARIABLE = "
  CLUSTER = id;
  USEVAR = neg pos hos id trial;
  MISSING = ALL(-999);
  TINTERVAL = trial(1);
  LAGGED = pos(1) hos(1) neg(1);",
  ANALYSIS = "TYPE = TWOLEVEL RANDOM;
  ESTIMATOR = BAYES;
  PROC = 8;
  BITER = (100);  
  BSEED = 526;
  THIN = 10;
  CHAINS = 8;",
  MODEL = "  %WITHIN%

  AR_NN | neg ON neg&1;
  R_NP | pos ON neg&1;
  R_NH   | hos ON neg&1;
  R_PN | neg ON pos&1;
  AR_PP | pos ON pos&1;
  R_PH | hos ON pos&1;
  R_HN | neg ON hos&1;
  R_HP | pos ON hos&1;
  AR_HH | hos ON hos&1;
  M_N | neg;
  M_P | pos;
  M_H | hos;
  Fac1 BY neg@1 pos@1;
  Fac2 BY neg@1 hos@1;
  Fac3 BY pos@1 hos@1;
  C_NP | Fac1;
  C_NH | Fac2;
  C_PH | Fac3;
   GDUMMY@1;
  GDUMMY BY Fac1@0;
  GDUMMY BY Fac2@0;
  GDUMMY BY Fac3@0;

  %BETWEEN%
  neg  -  hos  WITH  neg  -  hos;
  AR_NN WITH AR_NN;
  R_NP WITH R_NP;
  R_NH   WITH R_NH  ;
  R_PN WITH R_PN;
  AR_PP WITH AR_PP;
  R_PH WITH R_PH;
  R_HN WITH R_HN;
  R_HP WITH R_HP;
  AR_HH WITH AR_HH;
  M_N WITH M_N;
  M_P WITH M_P;
  M_H WITH M_H;
  C_NP WITH C_NP;
  C_NH WITH C_NH;
  C_PH WITH C_PH;
  AR_NN WITH R_NP;
  AR_NN WITH R_NH  ;
  AR_NN WITH R_PN;
  AR_NN WITH AR_PP;
  AR_NN WITH R_PH;
  AR_NN WITH R_HN;
  AR_NN WITH R_HP;
  AR_NN WITH AR_HH;
  AR_NN WITH M_N;
  AR_NN WITH M_P;
  AR_NN WITH M_H;
  AR_NN WITH C_NP;
  AR_NN WITH C_NH;
  AR_NN WITH C_PH;
  R_NP WITH R_NH  ;
  R_NP WITH R_PN;
  R_NP WITH AR_PP;
  R_NP WITH R_PH;
  R_NP WITH R_HN;
  R_NP WITH R_HP;
  R_NP WITH AR_HH;
  R_NP WITH M_N;
  R_NP WITH M_P;
  R_NP WITH M_H;
  R_NP WITH C_NP;
  R_NP WITH C_NH;
  R_NP WITH C_PH;
  R_NH   WITH R_PN;
  R_NH   WITH AR_PP;
  R_NH   WITH R_PH;
  R_NH   WITH R_HN;
  R_NH   WITH R_HP;
  R_NH   WITH AR_HH;
  R_NH   WITH M_N;
  R_NH   WITH M_P;
  R_NH   WITH M_H;
  R_NH   WITH C_NP;
  R_NH   WITH C_NH;
  R_NH   WITH C_PH;
  R_PN WITH AR_PP;
  R_PN WITH R_PH;
  R_PN WITH R_HN;
  R_PN WITH R_HP;
  R_PN WITH AR_HH;
  R_PN WITH M_N;
  R_PN WITH M_P;
  R_PN WITH M_H;
  R_PN WITH C_NP;
  R_PN WITH C_NH;
  R_PN WITH C_PH;
  AR_PP WITH R_PH;
  AR_PP WITH R_HN;
  AR_PP WITH R_HP;
  AR_PP WITH AR_HH;
  AR_PP WITH M_N;
  AR_PP WITH M_P;
  AR_PP WITH M_H;
  AR_PP WITH C_NP;
  AR_PP WITH C_NH;
  AR_PP WITH C_PH;
  R_PH WITH R_HN;
  R_PH WITH R_HP;
  R_PH WITH AR_HH;
  R_PH WITH M_N;
  R_PH WITH M_P;
  R_PH WITH M_H;
  R_PH WITH C_NP;
  R_PH WITH C_NH;
  R_PH WITH C_PH;
  R_HN WITH R_HP;
  R_HN WITH AR_HH;
  R_HN WITH M_N;
  R_HN WITH M_P;
  R_HN WITH M_H;
  R_HN WITH C_NP;
  R_HN WITH C_NH;
  R_HN WITH C_PH;
  R_HP WITH AR_HH;
  R_HP WITH M_N;
  R_HP WITH M_P;
  R_HP WITH M_H;
  R_HP WITH C_NP;
  R_HP WITH C_NH;
  R_HP WITH C_PH;
  AR_HH WITH M_N;
  AR_HH WITH M_P;
  AR_HH WITH M_H;
  AR_HH WITH C_NP;
  AR_HH WITH C_NH;
  AR_HH WITH C_PH;
  M_N WITH M_P;
  M_N WITH M_H;
  M_N WITH C_NP;
  M_N WITH C_NH;
  M_N WITH C_PH;
  M_P WITH M_H;
  M_P WITH C_NP;
  M_P WITH C_NH;
  M_P WITH C_PH;
  M_H WITH C_NP;
  M_H WITH C_NH;
  M_H WITH C_PH;
  C_NP WITH C_NH;
  C_NP WITH C_PH;
  C_NH WITH C_PH;",
  OUTPUT = "TECH1 TECH4 (CLUSTER) TECH8 STDYX FSCOMPARISON;",
  PLOT = "TYPE = PLOT3;
  FACTORS = ALL;",
  usevariables = c("id", "pos", "hos", "neg", "trial"),
  SAVEDATA = 'FILE = "dsem_3.dat";',
  rdata = dat)
#dsem_3 <- mplusModeler(dsem_3, 
#                       "dsem_3.dat", 
#                       modelout = "dsem_3.inp", 
#                       check = TRUE, run = 1)


# Read Factor Scores
# fs <- readModels("dsem_2.out", what = "savedata")$savedata
# 
# 
# list.par <- c("ID","AR_PP.Median", "R_HP.Median", "R_NP.Median", "R_PH.Median", "AR_HH.Median",  "R_NH.Median",
#               "R_PN.Median",  "R_HN.Median",  "AR_NN.Median",   "C_PH.Mean", "C_NP.Mean",  "C_NH.Mean")
# fs_sort <- fs[c(list.par)] # adjust for new estimation
# colnames(fs_sort) <- c("id", "pos.ar", "hos.pos.cr", "neg.pos.cr", "pos.hos.cr", "hos.ar", "neg.hos.cr",
#                             "pos.neg.cr", "hos.neg.cr", "neg.ar", "cont.pos.hos",
#                             "cont.pos.neg", "cont.neg.hos")
# length(table(fs_sort$id))
# fs_sort <- fs_sort[ ! fs_sort$id %in% which1, ]  
# fs_sort <- fs_sort[ ! fs_sort$id %in% which2, ] 
# length(table(fs_sort$id))
# 
# # person-specific factor scores
# person$temp.ar.pos <- ddply( fs_sort, .(id), function(x) mean(x$pos.ar, na.rm = TRUE) )$V1
# person$temp.ar.hos <- ddply( fs_sort, .(id), function(x) mean(x$hos.ar, na.rm = TRUE) )$V1
# person$temp.ar.neg <- ddply( fs_sort, .(id), function(x) mean(x$neg.ar, na.rm = TRUE) )$V1
# person$temp.cl.pos.hos <- ddply( fs_sort, .(id), function(x) mean(x$pos.hos.cr, na.rm = TRUE) )$V1
# person$temp.cl.pos.neg <- ddply( fs_sort, .(id), function(x) mean(x$pos.neg.cr, na.rm = TRUE) )$V1
# person$temp.cl.hos.pos <- ddply( fs_sort, .(id), function(x) mean(x$hos.pos.cr, na.rm = TRUE) )$V1
# person$temp.cl.hos.neg <- ddply( fs_sort, .(id), function(x) mean(x$hos.neg.cr, na.rm = TRUE) )$V1
# person$temp.cl.neg.pos <- ddply( fs_sort, .(id), function(x) mean(x$neg.pos.cr, na.rm = TRUE) )$V1
# person$temp.cl.neg.hos <- ddply( fs_sort, .(id), function(x) mean(x$neg.hos.cr, na.rm = TRUE) )$V1
# person$cont.pcor.hos.pos <- ddply( fs_sort, .(id), function(x) mean(x$cont.pos.hos, na.rm = TRUE) )$V1
# person$cont.pcor.neg.pos <- ddply( fs_sort, .(id), function(x) mean(x$cont.pos.neg, na.rm = TRUE) )$V1
# person$cont.pcor.neg.hos <- ddply( fs_sort, .(id), function(x) mean(x$cont.neg.hos, na.rm = TRUE) )$V1



# Two Step Frequentist mlVAR
list <- c("neg", "pos", "hos")

mlVAR.fit3 <- mlVAR(dat, vars = list, idvar = "id", lags = 1, beepvar = "trial", 
                    temporal = "correlated", contemporaneous = "correlated", 
                    scale = T, scaleWithin = F)

# fixed effects
p1 <- plot(mlVAR.fit3, type = "temporal", nonsig = "show", fade = FALSE, edge.labels = TRUE)
p3 <- plot(mlVAR.fit3, type = "contemporaneous", nonsig = "show", fade = FALSE, edge.labels = TRUE)
# random effects
p2 <- plot(mlVAR.fit3, type = "temporal", SD = T, nonsig = "show", fade = FALSE, edge.labels = TRUE, posCol = "black")
p4 <- plot(mlVAR.fit3, type = "contemporaneous", SD = T, nonsig = "show", edge.labels = TRUE, posCol = "black")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(p1)
plot(p2)
plot(p3)
plot(p4)
graphics::layout(1) # 12,6

#### lvl 2 dataset

person <- Reduce(function(x, y) merge(x, y, by="id"), 
                 list(person.mean.pos, person.mean.neg, person.mean.hos, 
                      
                      person.rl.sd.pos, person.rl.sd.neg, person.rl.sd.hos, 
                      
                      person.rl.mssd.pos, person.rl.mssd.neg, person.rl.mssd.hos, 
                      
                      person.sd.pos, person.sd.neg, person.sd.hos, 
                      
                      person.alpha.neg, person.alpha.pos, person.alpha.hos
                      
                 ))

colnames(person) <- c("id",
                      "mean.pos", "mean.neg", "mean.hos", 
                      
                      "rl.sd.pos", "rl.sd.neg", "rl.sd.hos", 
                      
                      "rl.mssd.pos", "rl.mssd.neg", "rl.mssd.hos", 
                      
                      
                      "sd.pos", "sd.neg", "sd.hos", 
                      
                      
                      "alpha.neg", "alpha.pos", "alpha.hos"
)


# compute within-person Mean of Squared Successive Differences
person$mssd.neg <- mssd(dat$day.mean.neg, dat$id, na.rm = TRUE)
person$mssd.pos <- mssd(dat$day.mean.pos, dat$id, na.rm = TRUE)
person$mssd.hos <- mssd(dat$day.mean.hos, dat$id, na.rm = TRUE)

i = 1
for(i in 1:222) {
  tryCatch({
    cont.netw <- mlVAR.fit3$results$Theta$pcor$subject[[i]]
    temp.netw <- as.data.frame(mlVAR.fit3$results$Beta$subject[[i]])
    person$temp.ar.neg[i] <- temp.netw$neg.1[1] # neg.ar
    person$temp.cl.pos.neg[i] <- temp.netw$pos.1[1] # pos.neg
    person$temp.cl.hos.neg[i] <- temp.netw$hos.1[1] # hos.neg
    person$temp.cl.neg.pos[i] <- temp.netw$neg.1[2] # neg.pos
    person$temp.ar.pos[i] <- temp.netw$pos.1[2] # pos.ar
    person$temp.cl.hos.pos[i] <- temp.netw$hos.1[2] # hos.pos
    person$temp.cl.neg.hos[i] <- temp.netw$neg.1[3] # neg.hos
    person$temp.cl.pos.hos[i] <- temp.netw$pos.1[3] # pos.hos
    person$temp.ar.hos[i] <- temp.netw$hos.1[3] # hos.ar
    person$cont.pcor.neg.pos[i] <- cont.netw[1,2] # neg pos
    person$cont.pcor.neg.hos[i] <- cont.netw[1,3] # neg hos
    person$cont.pcor.hos.pos[i] <- cont.netw[2,3] # hos pos
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    print(i)
  })
}

median.imp=function (x){
  x<-as.numeric(as.character(x))
  x[is.na(x)]=median(x, na.rm=TRUE)
  x
}
id <- person$id
person <- data.frame(apply(person[,2:ncol(person)],2,median.imp))
person$id <- id

write.table(person, "soed_minrev_sample3.csv", col.names = T, sep = ";", row.names = F)


person <- read.table("soed_minrev_sample3.csv", sep=";", header = TRUE)

dat <- subset.pca(person)

write.table(dat, "soed_minrev_sample3_sub.csv", col.names = T, sep = ";", row.names = F)


### RETEST
dat.retest <- read.table("paul_retest_lachs.csv", sep=";", header = TRUE)
#dat.retest$id <- factor(dat.retest$id)
#dat.retest$id <- droplevels(dat.retest$id, exclude = if(anyNA(levels(dat.retest$id))) NULL else NA) 


items.pos <- c("Alert", "Attentive", "Determined", "Inspired",  "Active", "Enthusiastic", "Excited", 
               "Interested", "Proud", "Strong") 
items.neg <- c("Afraid", "Ashamed", "Nervous","Distressed" ,"Guilty", "Jittery", "Scared", "Frightened", 
               "Shaky", "Sad", "Blue", "Downhearted", "Alone", "Lonely") 
items.hos <- c("Hostile", "Upset", "Irritable", "Angry", "Scornful", "Disgusted", "Loathing") 
d.pos <- dat.retest[,items.pos]
d.neg <- dat.retest[,items.neg]
d.hos <- dat.retest[,items.hos]

dat.retest$day.mean.pos <- apply(d.pos[,items.pos],1,mean,na.rm = TRUE)
dat.retest$day.mean.neg <- apply(d.neg[,items.neg],1,mean,na.rm = TRUE)
dat.retest$day.mean.hos <- apply(d.hos[,items.hos],1,mean,na.rm = TRUE)

dat.retest$pos <- dat.retest$day.mean.pos
dat.retest$hos <- dat.retest$day.mean.hos
dat.retest$neg <- dat.retest$day.mean.neg


dat.retest$day.mean.pos <- scales::rescale(dat.retest$day.mean.pos, to = c(0,10), from = c(3,7))
dat.retest$day.mean.neg <- scales::rescale(dat.retest$day.mean.neg, to = c(0,10), from = c(3,7))
dat.retest$day.mean.hos <- scales::rescale(dat.retest$day.mean.hos, to = c(0,10), from = c(3,7))







dat.retest$day.mean.pos_lag1 <- shift(dat.retest$day.mean.pos, n=1L, fill=NA, type="lag", give.names=FALSE)
dat.retest$day.mean.neg_lag1 <- shift(dat.retest$day.mean.neg, n=1L, fill=NA, type="lag", give.names=FALSE)
dat.retest$day.mean.hos_lag1 <- shift(dat.retest$day.mean.hos, n=1L, fill=NA, type="lag", give.names=FALSE)

person.mean.neg <- ddply( dat.retest, .(id), function(x) mean(x$day.mean.neg, na.rm = TRUE) )
person.mean.hos <- ddply( dat.retest, .(id), function(x) mean(x$day.mean.hos, na.rm = TRUE) )
person.mean.pos <- ddply( dat.retest, .(id), function(x) mean(x$day.mean.pos, na.rm = TRUE) )

person.sd.neg <- ddply( dat.retest, .(id), function(x) sd(x$day.mean.neg, na.rm = TRUE) )
person.sd.hos <- ddply( dat.retest, .(id), function(x) sd(x$day.mean.hos, na.rm = TRUE) )
person.sd.pos <- ddply( dat.retest, .(id), function(x) sd(x$day.mean.pos, na.rm = TRUE) )



# person-specific scale relative mssd
person.rl.mssd.neg <- ddply( dat.retest, .(id), function(x) relativeMSSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.mssd.hos <- ddply( dat.retest, .(id), function(x) relativeMSSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.mssd.pos <- ddply( dat.retest, .(id), function(x) relativeMSSD(x$day.mean.pos, MIN = 0, MAX = 10) )

person.rl.sd.neg <- ddply( dat.retest, .(id), function(x) relativeSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.sd.hos <- ddply( dat.retest, .(id), function(x) relativeSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.sd.pos <- ddply( dat.retest, .(id), function(x) relativeSD(x$day.mean.pos, MIN = 0, MAX = 10) )



# person-specific scale cronbachs alpha
avgcor <- function(y){mean(abs(cor(y)[lower.tri(y)]),na.rm = TRUE)}
person.alpha.pos <- ddply( dat.retest, .(id), function(x) avgcor(x[,c("Alert", "Active", "Attentive", "Determined", "Inspired", 
                                                                      "Enthusiastic", "Excited","Interested",
                                                                      "Proud", "Strong")]))
person.alpha.hos <- ddply( dat.retest, .(id), function(x) avgcor(x[,c("Hostile", "Upset", "Irritable", "Angry", "Scornful", 
                                                                      "Disgusted", "Loathing")]))
person.alpha.neg <- ddply( dat.retest, .(id), function(x) avgcor(x[,c("Nervous", "Ashamed", "Afraid", "Distressed","Guilty", 
                                                                      "Jittery", "Scared", "Frightened", 
                                                                      "Shaky", "Blue", "Downhearted", "Alone",
                                                                      "Lonely")]))



# Two Step Frequentist mlVAR
list <- c("neg", "pos", "hos")
mlVAR.fit3 <- mlVAR(dat.retest, vars = list, idvar = "id", lags = 1, beepvar = "trial",
                    temporal = "correlated", contemporaneous = "correlated", 
                    scale = T, scaleWithin = F)

length(table(dat.retest$id))


# fixed effects
p1 <- plot(mlVAR.fit3, type = "temporal", nonsig = "show", fade = FALSE, edge.labels = TRUE)
p3 <- plot(mlVAR.fit3, type = "contemporaneous", nonsig = "show", fade = FALSE, edge.labels = TRUE)
# random effects
p2 <- plot(mlVAR.fit3, type = "temporal", SD = T, nonsig = "show", fade = FALSE, edge.labels = TRUE, posCol = "black")
p4 <- plot(mlVAR.fit3, type = "contemporaneous", SD = T, nonsig = "show", edge.labels = TRUE, posCol = "black")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(p1)
plot(p2)
plot(p3)
plot(p4)
graphics::layout(1) # 12,6

#### lvl 2 dataset

person <- Reduce(function(x, y) merge(x, y, by="id"), 
                 list(person.mean.pos, person.mean.neg, person.mean.hos, 
                      
                      person.rl.sd.pos, person.rl.sd.neg, person.rl.sd.hos, 
                      
                      person.rl.mssd.pos, person.rl.mssd.neg, person.rl.mssd.hos, 
                      
                      person.sd.pos, person.sd.neg, person.sd.hos, 
                      
                      person.alpha.neg, person.alpha.pos, person.alpha.hos
                 ))

colnames(person) <- c("id",
                      "mean.pos", "mean.neg", "mean.hos", 
                      
                      "rl.sd.pos", "rl.sd.neg", "rl.sd.hos", 
                      
                      "rl.mssd.pos", "rl.mssd.neg", "rl.mssd.hos", 
                      
                      "sd.pos", "sd.neg", "sd.hos", 
                      
                      "alpha.neg", "alpha.pos", "alpha.hos"
                      
)


# compute within-person Mean of Squared Successive Differences
person$mssd.neg <- mssd(dat.retest$day.mean.neg, dat.retest$id, na.rm = TRUE)
person$mssd.pos <- mssd(dat.retest$day.mean.pos, dat.retest$id, na.rm = TRUE)
person$mssd.hos <- mssd(dat.retest$day.mean.hos, dat.retest$id, na.rm = TRUE)



i = 1
for(i in 1:143) {
  tryCatch({
    cont.netw <- mlVAR.fit3$results$Theta$pcor$subject[[i]]
    temp.netw <- as.data.frame(mlVAR.fit3$results$Beta$subject[[i]])
    person$temp.ar.neg[i] <- temp.netw$neg.1[1] # neg.ar
    person$temp.cl.pos.neg[i] <- temp.netw$pos.1[1] # pos.neg
    person$temp.cl.hos.neg[i] <- temp.netw$hos.1[1] # hos.neg
    person$temp.cl.neg.pos[i] <- temp.netw$neg.1[2] # neg.pos
    person$temp.ar.pos[i] <- temp.netw$pos.1[2] # pos.ar
    person$temp.cl.hos.pos[i] <- temp.netw$hos.1[2] # hos.pos
    person$temp.cl.neg.hos[i] <- temp.netw$neg.1[3] # neg.hos
    person$temp.cl.pos.hos[i] <- temp.netw$pos.1[3] # pos.hos
    person$temp.ar.hos[i] <- temp.netw$hos.1[3] # hos.ar
    person$cont.pcor.neg.pos[i] <- cont.netw[1,2] # neg pos
    person$cont.pcor.neg.hos[i] <- cont.netw[1,3] # neg hos
    person$cont.pcor.hos.pos[i] <- cont.netw[2,3] # hos pos
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    print(i)
  })
}

median.imp=function (x){
  x<-as.numeric(as.character(x))
  x[is.na(x)]=median(x, na.rm=TRUE)
  x
}
id <- person$id
person <- data.frame(apply(person[,2:ncol(person)],2,median.imp))
person$id <- id


dat.retest <- subset.pca(person)

write.table(dat.retest, "soed_minrev_sample3_sub_retest.csv", col.names = T, sep = ";", row.names = F)


### TEST
dat.test <- read.table("paul_test_lachs.csv", sep=";", header = TRUE)
#which <- which(dat.test$id == "81100")
#dat.test <- dat.test[-which,]

items.pos <- c("Alert", "Attentive", "Determined", "Inspired",  "Active", "Enthusiastic", "Excited", 
               "Interested", "Proud", "Strong") 
items.neg <- c("Afraid", "Ashamed", "Nervous","Distressed" ,"Guilty", "Jittery", "Scared", "Frightened", 
               "Shaky", "Sad", "Blue", "Downhearted", "Alone", "Lonely") 
items.hos <- c("Hostile", "Upset", "Irritable", "Angry", "Scornful", "Disgusted", "Loathing") 
d.pos <- dat.test[,items.pos]
d.neg <- dat.test[,items.neg]
d.hos <- dat.test[,items.hos]

dat.test$day.mean.pos <- apply(d.pos[,items.pos],1,mean,na.rm = TRUE)
dat.test$day.mean.neg <- apply(d.neg[,items.neg],1,mean,na.rm = TRUE)
dat.test$day.mean.hos <- apply(d.hos[,items.hos],1,mean,na.rm = TRUE)

dat.test$pos <- dat.test$day.mean.pos
dat.test$hos <- dat.test$day.mean.hos
dat.test$neg <- dat.test$day.mean.neg

dat.test$day.mean.pos <- scales::rescale(dat.test$day.mean.pos, to = c(0,10), from = c(3,7))
dat.test$day.mean.neg <- scales::rescale(dat.test$day.mean.neg, to = c(0,10), from = c(3,7))
dat.test$day.mean.hos <- scales::rescale(dat.test$day.mean.hos, to = c(0,10), from = c(3,7))








dat.test$day.mean.pos_lag1 <- shift(dat.test$day.mean.pos, n=1L, fill=NA, type="lag", give.names=FALSE)
dat.test$day.mean.neg_lag1 <- shift(dat.test$day.mean.neg, n=1L, fill=NA, type="lag", give.names=FALSE)
dat.test$day.mean.hos_lag1 <- shift(dat.test$day.mean.hos, n=1L, fill=NA, type="lag", give.names=FALSE)

person.mean.neg <- ddply( dat.test, .(id), function(x) mean(x$day.mean.neg, na.rm = TRUE) )
person.mean.hos <- ddply( dat.test, .(id), function(x) mean(x$day.mean.hos, na.rm = TRUE) )
person.mean.pos <- ddply( dat.test, .(id), function(x) mean(x$day.mean.pos, na.rm = TRUE) )

person.sd.neg <- ddply( dat.test, .(id), function(x) sd(x$day.mean.neg, na.rm = TRUE) )
person.sd.hos <- ddply( dat.test, .(id), function(x) sd(x$day.mean.hos, na.rm = TRUE) )
person.sd.pos <- ddply( dat.test, .(id), function(x) sd(x$day.mean.pos, na.rm = TRUE) )



# person-specific scale relative mssd
person.rl.mssd.neg <- ddply( dat.test, .(id), function(x) relativeMSSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.mssd.hos <- ddply( dat.test, .(id), function(x) relativeMSSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.mssd.pos <- ddply( dat.test, .(id), function(x) relativeMSSD(x$day.mean.pos, MIN = 0, MAX = 10) )

person.rl.sd.neg <- ddply( dat.test, .(id), function(x) relativeSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.sd.hos <- ddply( dat.test, .(id), function(x) relativeSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.sd.pos <- ddply( dat.test, .(id), function(x) relativeSD(x$day.mean.pos, MIN = 0, MAX = 10) )



# person-specific scale cronbachs alpha
avgcor <- function(y){mean(abs(cor(y)[lower.tri(y)]),na.rm = TRUE)}
person.alpha.pos <- ddply( dat.test, .(id), function(x) avgcor(x[,c("Alert", "Active", "Attentive", "Determined", "Inspired", 
                                                                    "Enthusiastic", "Excited","Interested",
                                                                    "Proud", "Strong")]))
person.alpha.hos <- ddply( dat.test, .(id), function(x) avgcor(x[,c("Hostile", "Upset", "Irritable", "Angry", "Scornful", 
                                                                    "Disgusted", "Loathing")]))
person.alpha.neg <- ddply( dat.test, .(id), function(x) avgcor(x[,c("Nervous", "Ashamed", "Afraid", "Distressed","Guilty", 
                                                                    "Jittery", "Scared", "Frightened", 
                                                                    "Shaky", "Blue", "Downhearted", "Alone",
                                                                    "Lonely")]))

# Two Step Frequentist mlVAR
list <- c("neg", "pos", "hos")
mlVAR.fit3 <- mlVAR(dat.test, vars = list, idvar = "id", lags = 1, beepvar = "trial",
                    temporal = "correlated", contemporaneous = "correlated", 
                    scale = T, scaleWithin = F)

# fixed effects
p1 <- plot(mlVAR.fit3, type = "temporal", nonsig = "show", fade = FALSE, edge.labels = TRUE)
p3 <- plot(mlVAR.fit3, type = "contemporaneous", nonsig = "show", fade = FALSE, edge.labels = TRUE)
# random effects
p2 <- plot(mlVAR.fit3, type = "temporal", SD = T, nonsig = "show", fade = FALSE, edge.labels = TRUE, posCol = "black")
p4 <- plot(mlVAR.fit3, type = "contemporaneous", SD = T, nonsig = "show", edge.labels = TRUE, posCol = "black")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(p1)
plot(p2)
plot(p3)
plot(p4)
graphics::layout(1) # 12,6

# DSEM
dsem_test <- mplusObject(
  TITLE = "DSEM3test mlVAR Model;", 
  VARIABLE = "
  CLUSTER = id;
  USEVAR = pos hos neg;
  MISSING = ALL(-999);
  TINTERVAL = trial(1);
  LAGGED = pos(1) hos(1) neg(1);",
  ANALYSIS = "TYPE = TWOLEVEL RANDOM;
  ESTIMATOR = BAYES;
  PROC = 8;
  BITER = (5000);  
  BSEED = 526;
  THIN = 10;
  CHAINS = 8;",
  MODEL = "%WITHIN%
  
  par1 | pos ON pos&1;
  par2 | hos ON pos&1;
  par3 | neg ON pos&1;
  par4 | pos ON hos&1;
  par5 | hos ON hos&1;
  par6 | neg ON hos&1;
  par7 | pos ON neg&1;
  par8 | hos ON neg&1;
  par9 | neg ON neg&1;
  NCov1 BY hos@1 pos@-1 ;
  NCov2 BY neg@1 pos@-1 ;
  NCov3 BY neg@1 hos@1 ;
  par10 | NCov1;
  par11 | NCov2;
  par12 | NCov3;
  GDUMMY@1;
  GDUMMY BY NCov1@0;
  GDUMMY BY NCov2@0;
  GDUMMY BY NCov3@0;
  
  %BETWEEN%
  pos  -  neg  WITH  pos  -  neg;
  par1 - par12 WITH par1 - par12",
  OUTPUT = "TECH1 TECH8 STDYX;",
  PLOT = "TYPE = PLOT3;
  FACTORS = ALL;",
  usevariables = c("id", "pos", "hos", "neg", "trial"),
  SAVEDATA = 'FILE = "dsem_3retest.dat";',
  rdata = dat.test)
#dsem_3retest_mod <- mplusModeler(dsem_3retest, 
#                               "dsem_3retest.dat", 
#                               modelout = "dsem_3retest.inp", 
#                               check = TRUE, run = 1)


#### lvl 2 dataset

person <- Reduce(function(x, y) merge(x, y, by="id"), 
                 list(person.mean.pos, person.mean.neg, person.mean.hos, 
                      
                      person.rl.sd.pos, person.rl.sd.neg, person.rl.sd.hos, 
                      
                      person.rl.mssd.pos, person.rl.mssd.neg, person.rl.mssd.hos, 
                      
                      person.sd.pos, person.sd.neg, person.sd.hos, 
                      
                      person.alpha.neg, person.alpha.pos, person.alpha.hos
                 ))

colnames(person) <- c("id",
                      "mean.pos", "mean.neg", "mean.hos", 
                      
                      "rl.sd.pos", "rl.sd.neg", "rl.sd.hos", 
                      
                      "rl.mssd.pos", "rl.mssd.neg", "rl.mssd.hos", 
                      
                      "sd.pos", "sd.neg", "sd.hos", 
                      
                      "alpha.neg", "alpha.pos", "alpha.hos"
                      
)


# compute within-person Mean of Squared Successive Differences
person$mssd.neg <- mssd(dat.test$day.mean.neg, dat.test$id, na.rm = TRUE)
person$mssd.pos <- mssd(dat.test$day.mean.pos, dat.test$id, na.rm = TRUE)
person$mssd.hos <- mssd(dat.test$day.mean.hos, dat.test$id, na.rm = TRUE)



i = 1
for(i in 1:143) {
  tryCatch({
    cont.netw <- mlVAR.fit3$results$Theta$pcor$subject[[i]]
    temp.netw <- as.data.frame(mlVAR.fit3$results$Beta$subject[[i]])
    person$temp.ar.neg[i] <- temp.netw$neg.1[1] # neg.ar
    person$temp.cl.pos.neg[i] <- temp.netw$pos.1[1] # pos.neg 
    person$temp.cl.hos.neg[i] <- temp.netw$hos.1[1] # hos.neg 
    person$temp.cl.neg.pos[i] <- temp.netw$neg.1[2] # neg.pos
    person$temp.ar.pos[i] <- temp.netw$pos.1[2] # pos.ar
    person$temp.cl.hos.pos[i] <- temp.netw$hos.1[2] # hos.pos
    person$temp.cl.neg.hos[i] <- temp.netw$neg.1[3] # neg.hos
    person$temp.cl.pos.hos[i] <- temp.netw$pos.1[3] # pos.hos
    person$temp.ar.hos[i] <- temp.netw$hos.1[3] # hos.ar
    person$cont.pcor.neg.pos[i] <- cont.netw[1,2] # neg pos
    person$cont.pcor.neg.hos[i] <- cont.netw[1,3] # neg hos
    person$cont.pcor.hos.pos[i] <- cont.netw[2,3] # hos pos
  }
  , error=function(e){cat("ERROR :",conditionMessage(e), "\n")
    print(i)
  })
}

median.imp=function (x){
  x<-as.numeric(as.character(x))
  x[is.na(x)]=median(x, na.rm=TRUE)
  x
}
id <- person$id
person <- data.frame(apply(person[,2:ncol(person)],2,median.imp))
person$id <- id

subset.pca <- function (x) {
  x <- subset(x, select = c(  "id","mean.pos", "mean.neg", "mean.hos", 
                              "sd.pos","sd.neg","sd.hos",  
                              "mssd.pos","mssd.neg","mssd.hos",
                              "rl.sd.pos" ,"rl.sd.neg","rl.sd.hos" ,
                              "rl.mssd.pos","rl.mssd.neg","rl.mssd.hos", 
                              "alpha.pos","alpha.neg","alpha.hos",
                              "temp.ar.pos","temp.ar.neg","temp.ar.hos" ,
                              "temp.cl.pos.neg", "temp.cl.pos.hos", 
                              "temp.cl.neg.hos","temp.cl.neg.pos",
                              "temp.cl.hos.pos","temp.cl.hos.neg",     
                              "cont.pcor.neg.pos", "cont.pcor.neg.hos","cont.pcor.hos.pos"))
  colnames(x) <- c("id","P.Mean","N.Mean","H.Mean", 
                   "P.SD","N.SD","H.SD",        
                   "P.MSSD","N.MSSD","H.MSSD", 
                   "P.rSD","N.rSD","H.rSD",
                   "P.rMSSD","N.rMSSD","H.rMSSD", 
                   "P.ALPHA","N.ALPHA","H.ALPHA", 
                   "pP","nN","hH",
                   "pN","pH",
                   "nH","nP",
                   "hP","hN",
                   "P_N","N_H","P_H")
  return(x)
}

dat <- subset.pca(person)

write.table(dat, "soed_minrev_sample3_sub_test.csv", col.names = T, sep = ";", row.names = F)

rm(list = ls())
