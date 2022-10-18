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

### Data preparation: Sample 1

# set your working directory to the folder
# that contains the .csv-files
setwd("D:/sera/my google drive/R Daten/OSF iads")

# load packages (install packages if needed)
library(Hmisc)
library(MplusAutomation)
library(mlVAR)
library(psych)
library(plyr)
library(dplyr)
library(data.table)
library(graphicalVAR)
library(foreign)
library(summarytools)
library(mice)
library(qgraph)
library(relativeVariability) # not available on CRAN, see https://ppw.kuleuven.be/okp/software/relative_variability/
library(mgcv)# version 1.8-15
library(quantmod)# version 0.4-0

## Load data
dat.x <- read.spss("D:/sera/my google drive/R Daten/OSF iads/tbuch_days_items.sav", to.data.frame=TRUE) #minrev

dat.y <- read.spss("D:/sera/my google drive/R Daten/OSF iads/tbuch_days_scales.sav", to.data.frame=TRUE) #minrev

dat.y <- dat.y[,c("day_id", "tbday_nr")]

dat.z <- read.spss("D:/sera/my google drive/R Daten/OSF iads/tbuch_scales.sav", to.data.frame=TRUE) #minrev


dummy2 <- data.frame(ddply( dat.x, .(vp_id2), function(x) length(x$day_id) ))
hist(dummy2)

dat1 <- merge(dat.x, dat.y, by = "day_id", all = F)


psych::describe(dat1$tbday_nr)

dat1$day <- dat1$tbday_nr #minrev
dat1$id <- dat1$vp_id2 #minrev

# identify improper cases
dat1$day[dat1$day < 1] <- NA #identify improper cases (entry < 1)
table2 <- table(dat1$id)
dat1$combine <- paste(dat1$id, dat1$day) # combine variable to identify duplicate cases
dat3 <- dat1[!duplicated(dat1$combine), ] # apply
dat1 <- dat1[with(dat1, order(id, day)),]

# inclusion criterion
table <- table(dat1$id) 
min.occ = 20 # minimum 20 cases
max.occ = 30 # identify improper cases
which1 <- names(table[table < min.occ]) # identify cases to be excluded 
which2 <- names(table[table > max.occ]) 
dat2 <- dat1[ ! dat1$id %in% which1, ]  
dat2 <- dat2[ ! dat2$id %in% which2, ] 

## minrev
dat <- subset(dat2, select = c("vp_id2","emo_1", "emo_2", "emo_3", "emo_4", "emo_5", "emo_6", "emo_7"
                               , "emo_8", "emo_9", "emo_10", "emo_11", "emo_12", "emo_13", "emo_14"
                               , "emo_15", "emo_16", "emo_17", "emo_18", "emo_19", "emo_20", "emo_21"
                               , "emo_22", "emo_23", "emo_24", "emo_25", "emo_26", "emo_27", "emo_28"
                               , "emo_29", "emo_30", "emo_31", "emo_32", "emo_33", "emo_34", "emo_35"
                               , "emo_36", "emo_37", "emo_38", "day"))
colnames(dat) <- c("id", "atease", "calm", "relaxed", "drowsy", "sluggish", "tired", "content",
                   "pleased", "happy", "alert", "aroused", "hyperactivated", "miserable", "troubled",
                   "unhappy", "sleepy", "quiet", "still", "afraid", "ashamed", "distressed", "guilty",
                   "hostile", "irritable", "jittery", "nervous", "scared", "upset", "active", "alone",
                   "attentive", "determined", "enthusiastic", "excited", "inspired", "interested", 
                   "proud", "strong", "day")


# drop factor levels
dat$id <- droplevels(dat$id, exclude = if(anyNA(levels(dat$id))) NULL else NA) # drop factor level
table <- table(dat$id)
length(table) # number of individuals

mean(ddply( dat, .(id), function(x) length(x$id) )$V1) # 24.91
max(ddply( dat, .(id), function(x) length(x$id) )$V1) # 28
min(ddply( dat, .(id), function(x) length(x$id) )$V1) # 20

# multi level EFA
sample1_mefa <- mplusObject(
  TITLE = "ML EFA;",
  #  DATA = "SWMATRIX = sample1_mefa.txt;",
  ANALYSIS = "TYPE= TWOLEVEL EFA 5 6 5 6;
  ROTATION= GEOMIN;",
  VARIABLE = "CLUSTER = id;
  USEVAR = atease - strong;
  CATEGORICAL = atease - strong;",
  SAVEDATA = 'FILE = "sample1_mefa.dat";
  SWMATRIX = sample1_mefa.txt;',
  usevariables = c("id", "atease", "calm", "relaxed", "drowsy", "sluggish", "tired", "content",
                   "pleased", "happy", "alert", "aroused", "hyperactivated", "miserable", "troubled",
                   "unhappy", "sleepy", "quiet", "still", "afraid", "ashamed", "distressed", "guilty",
                   "hostile", "irritable", "jittery", "nervous", "scared", "upset", "active", "alone",
                   "attentive", "determined", "enthusiastic", "excited", "inspired", "interested", 
                   "proud", "strong"),
  rdata = dat)
#fit <- mplusModeler(sample1_mefa, "sample1_mefa.dat",
#                    modelout = "sample1_mefa.inp", check = TRUE, run = 1)

# drop factor levels
dat$id <- droplevels(dat$id, exclude = if(anyNA(levels(dat$id))) NULL else NA) # drop factor level
table <- table(dat$id)
length(table) # number of individuals




consec.occ.dat <- ddply( dat, .(id), function(x) length(which(diff(c(x$day, na.rm = T)) == 1)) )
which.not.consec.dat <- which(consec.occ.dat$V1 < 20)
which.not.consec.dat
which.id.dat <- consec.occ.dat[which.not.consec.dat,]$id
which.id.dat
dat <- dat[dat$id %nin% which.id.dat, ]

dat <- dat[dat$id!="ulgu13bi",]
dat <- dat[dat$id!="urma27ha",]


# drop factor levels
dat$id <- droplevels(dat$id, exclude = if(anyNA(levels(dat$id))) NULL else NA) # drop factor level
table <- table(dat$id)
length(table) # number of individuals


# sample descriptives
dummy <- data.frame(ddply( dat, .(id), function(x) length(x$id) ))
dat.z$id <- dat.z$vp_id2 
dat_z <- merge(dummy, dat.z, by = "id", all.x = T)

# age
psych::describe(dat_z$vp_female)
psych::describe(dat_z$vp_age)

mean(ddply( dat, .(id), function(x) length(x$id) )$V1) # 24.92
max(ddply( dat, .(id), function(x) length(x$id) )$V1) # 29
min(ddply( dat, .(id), function(x) length(x$id) )$V1) # 20
nrow(dat)

nrow(dummy)/nrow(dummy2)


### emotion scales based on ML-EFA
items.pos <- c("content", "pleased", "happy", "aroused", "hyperactivated",  
               "active", "attentive","determined", "enthusiastic", "excited",
               "inspired", "interested","proud", "strong") 
items.neg <- c("miserable", "troubled", "unhappy", "afraid", 
               "ashamed", "distressed", "guilty", 
               "scared", "alone") 
items.hos <- c("hostile", "irritable", "upset") 
d.pos <- dat[,items.pos]
d.neg <- dat[,items.neg]
d.hos <- dat[,items.hos]

dat$day.mean.pos <- apply(d.pos[,items.pos],1,mean,na.rm = TRUE)
dat$day.mean.neg <- apply(d.neg[,items.neg],1,mean,na.rm = TRUE)
dat$day.mean.hos <- apply(d.hos[,items.hos],1,mean,na.rm = TRUE)


dat$neg <- dat$day.mean.neg
dat$pos <- dat$day.mean.pos
dat$hos <- dat$day.mean.hos

dat$day.mean.pos <- scales::rescale(dat$day.mean.pos, to = c(0,10), from = c(1,5))
dat$day.mean.neg <- scales::rescale(dat$day.mean.neg, to = c(0,10), from = c(1,5))
dat$day.mean.hos <- scales::rescale(dat$day.mean.hos, to = c(0,10), from = c(1,5))


# person-specific scale mean
person.mean.neg <- ddply( dat, .(id), function(x) mean(x$day.mean.neg, na.rm = TRUE) )
person.mean.hos <- ddply( dat, .(id), function(x) mean(x$day.mean.hos, na.rm = TRUE) )
person.mean.pos <- ddply( dat, .(id), function(x) mean(x$day.mean.pos, na.rm = TRUE) )

dat$day.mean.pos_lag1 =Lag(as.numeric(dat$day.mean.pos,1))
dat$day.mean.neg_lag1 =Lag(as.numeric(dat$day.mean.neg,1))
dat$day.mean.hos_lag1 =Lag(as.numeric(dat$day.mean.hos,1))

# person-specific scale sd
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
person.alpha.pos <- ddply( dat, .(id), function(x) avgcor(x[,c(items.pos)]))
person.alpha.hos <- ddply( dat, .(id), function(x) avgcor(x[,c(items.hos)]))
person.alpha.neg <- ddply( dat, .(id), function(x) avgcor(x[,c(items.neg)]))



# create dataset including person-specific statistics
person <- Reduce(function(x, y) merge(x, y, by="id"), 
                 list( 
                   person.mean.pos, person.mean.neg, person.mean.hos,
                   person.rl.sd.pos, person.rl.sd.neg, person.rl.sd.hos, 
                   person.rl.mssd.pos, person.rl.mssd.neg, person.rl.mssd.hos,
                   person.sd.pos, person.sd.neg, person.sd.hos, 
                   person.alpha.pos, person.alpha.neg, person.alpha.hos))

# column names
colnames(person) <- c("id",
                      "mean.pos", "mean.neg", "mean.hos", 
                      "rl.sd.pos", "rl.sd.neg", "rl.sd.hos", 
                      "rl.mssd.pos", "rl.mssd.neg", "rl.mssd.hos", 
                      "sd.pos", "sd.neg", "sd.hos", 
                      "alpha.pos", "alpha.neg", "alpha.hos")

# mean square succesive difference

person$mssd.pos <- mssd(dat$day.mean.pos, dat$id, na.rm = TRUE)[,1]
person$mssd.neg <- mssd(dat$day.mean.neg, dat$id, na.rm = TRUE)[,1]
person$mssd.hos <- mssd(dat$day.mean.hos, dat$id, na.rm = TRUE)[,1]


# Two Step Frequentist mlVAR
list <- c("neg", "pos", "hos")
mlVAR.fit3 <- mlVAR(dat, vars = list, idvar = "id", lags = 1, beepvar = "day",
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

i = 1
for(i in 1:870) {
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


id <- person[,1]
person_2 <- data.frame(apply(person[,2:ncol(person)],2,median.imp))

person_bind <- cbind(id, person_2)


write.table(person_bind, "soed_minrev_sample1.csv", col.names = T, sep = ";", row.names = F)

dat2 <- read.table("soed_minrev_sample1.csv", sep = ";", header = TRUE)
colnames(dat2)
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

dat <- subset.pca(dat2)

write.table(dat, "soed_minrev_sample1_sub.csv", col.names = T, sep = ";", row.names = F)

rm(list = ls())

