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

### Data preparation: Sample 2


# set your working directory to the folder
# that contains the .csv-files
setwd("D:/sera/my google drive/R Daten/OSF iads")

# load packages (install packages if needed)
library(Hmisc)
library(scales)
library(MplusAutomation)
library(mlVAR)
library(psych)
library(plyr)
library(dplyr)
library(schoolmath)
library(data.table)
library(graphicalVAR)
library(foreign)
library(summarytools)
library(mice)
library(qgraph)
library(relativeVariability) # not available on CRAN, see https://ppw.kuleuven.be/okp/software/relative_variability/
library(mgcv)# version 1.8-15
library(quantmod)# version 0.4-0


dat1 <- read.table("iads_sample2_ild.csv", sep = ";", header = TRUE)

#

# Daten ordnen nach vp_id und tbday
dat1 <- dat1[with(dat1, order(id, day)),]

# identify improper cases
dat1$day[dat1$day < 1] <- NA #identify improper cases (entry < 1)
table2 <- table(dat1$id)
dat1$combine <- paste(dat1$id, dat1$day) # combine variable to identify duplicate cases
dat1 <- dat1[!duplicated(dat1$combine), ] # apply
dat1 <- dat1[with(dat1, order(id, day)),]

# inclusion criterion
table <- table(dat1$id) 
min.occ = 40 # minimum 30 cases
max.occ = 101 # identify improper cases
which1 <- names(table[table < min.occ]) # identify cases to be excluded 
which2 <- names(table[table > max.occ]) 
dat1 <- dat1[ ! dat1$id %in% which1, ]  
dat1 <- dat1[ ! dat1$id %in% which2, ] 

table <- table(dat1$id)
length(table) # number of individuals 99

mean(ddply( dat1, .(id), function(x) length(x$id) )$V1) # 86.76
max(ddply( dat1, .(id), function(x) length(x$id) )$V1) # 101
min(ddply( dat1, .(id), function(x) length(x$id) )$V1) # 47
days_s2 <- ddply( dat1, .(id), function(x) length(x$id) )$V1
table(days_s2)

# 
test_net <- dat1
retest_net <- dat1

i <- 1
x <- 0
test_net$day_dummy <- NULL

for (i in 1:nrow(test_net)){
  if (is.even(test_net$day[i]) == TRUE) {
    test_net$day_dummy[i] <- test_net$day[i] + x
  } else {
    test_net$day_dummy[i] <- test_net$day[i] + 1 + x
  }
  x <- x+1
}
# 

i <- 1
x <- 0
retest_net$day_dummy <- NULL

for (i in 1:nrow(retest_net)){
  if (is.even(retest_net$day[i]) == FALSE) {
    retest_net$day_dummy[i] <- retest_net$day[i] + x
  } else {
    retest_net$day_dummy[i] <- retest_net$day[i] + 1 + x
  }
  x <- x+1
}
# 

consec.occ.test <- ddply( test_net, .(id), function(x) length(which(diff(c(x$day, na.rm = T)) == 1)) )
which.not.consec.test <- which(consec.occ.test$V1 < 20)
which.id.test <- consec.occ.test[which.not.consec.test,]$id


consec.occ.retest <- ddply( retest_net, .(id), function(x) length(which(diff(c(x$day, na.rm = T)) == 1)) )
which.not.consec.retest <- which(consec.occ.retest$V1 < 20)
which.id.retest <- consec.occ.retest[which.not.consec.retest,]$id

which.id.not.consec <- c(which.id.test, which.id.retest)

retest_net <- retest_net[retest_net$id %nin% which.id.not.consec, ]
test_net <- test_net[test_net$id %nin% which.id.not.consec, ]

#
# 
test_simple <- data.frame(matrix(, nrow = 0, ncol = 712))
names(test_simple)<-c(colnames(dat1))
retest_simple <- data.frame(matrix(, nrow = 0, ncol = 712))
names(retest_simple)<-c(colnames(dat1))

i=1
for (i in 1:nrow(dat1)){
  if (is.even(dat1$day[i]) == TRUE) {
    test_simple <- rbind(test_simple, dat1[i,])
  } else {
    retest_simple <- rbind(retest_simple, dat1[i,])
  }
}
# 

(mean(ddply( test_simple, .(id), function(x) length(x$id) )$V1)+mean(ddply( retest_simple, .(id), function(x) length(x$id) )$V1))/2
#


dat_z <- read.table("iads_sample2_dmgrph.csv", sep = ";", header = TRUE)

dat_z$id <- dat_z$ParticipantID
dummy <- data.frame(ddply( dat1, .(id), function(x) length(x$id) ))
dummy2 <- data.frame(ddply( dat_z, .(id), function(x) length(x$id) ))
nrow(dummy)/nrow(dummy2)
dat_z <- merge(dummy, dat_z, by = "id", all.x = T)

age <- ddply( dat_z, .(id), function(x) mean(x$Age, na.rm = T) )$V1
psych::describe(age)
gender <- ddply( dat_z, .(id), function(x) mean(x$Gender, na.rm = T) )$V1
psych::describe(gender)

table <- table(test_net$id)
length(table) # number of individuals 99



write.table(retest_net, "aid_retest_net.csv", col.names = T, sep = ";", row.names = F)
write.table(test_net, "aid_test_net.csv", col.names = T, sep = ";", row.names = F)

write.table(retest_simple, "aid_retest_simple.csv", col.names = T, sep = ";", row.names = F)
write.table(test_simple, "aid_test_simple.csv", col.names = T, sep = ";", row.names = F)


# multi level EFA
sample2_mefa <- mplusObject(
  TITLE = "ML EFA;",
  #  DATA = "SWMATRIX = sample2_mefa.txt;",
  ANALYSIS = "TYPE= TWOLEVEL EFA 1 3 1 3;
  ROTATION= GEOMIN;",
  VARIABLE = "CLUSTER = id;
  USEVAR = alert - upset;
  CATEGORICAL = alert - upset;",
  SAVEDATA = 'FILE = "sample2_mefa.dat";
  SWMATRIX = sample2_mefa.txt;',
  usevariables = c("id","alert","active","attentive","determined","inspired",
                   "nervous","ashamed","afraid","hostile","upset"),
  rdata = dat1)
#fit <- mplusModeler(sample2_mefa, "sample2_mefa.dat",
#                    modelout = "sample2_mefa.inp", check = TRUE, run = 1)

consec.occ.dat <- ddply( dat1, .(id), function(x) length(which(diff(c(x$day, na.rm = T)) == 1)) )
which.not.consec.dat <- which(consec.occ.dat$V1 < 20)
which.id.dat <- consec.occ.dat[which.not.consec.dat,]$id
dat1 <- dat1[dat1$id %nin% which.id.dat, ]

dat <- dat1

#dat[is.nan(dat)] <- NA

table <- table(dat1$id)
length(table) # number of individuals 99

### emotion scales based on ML-EFA
items.pos <- c("alert", "attentive", "determined", "inspired",  "active") 
items.neg <- c("afraid", "ashamed", "nervous") 
items.hos <- c("hostile", "upset") 
d.pos <- dat[,items.pos]
d.neg <- dat[,items.neg]
d.hos <- dat[,items.hos]
dat$day.mean.pos <- apply(d.pos[,items.pos],1,mean,na.rm = TRUE)
dat$day.mean.neg <- apply(d.neg[,items.neg],1,mean,na.rm = TRUE)
dat$day.mean.hos <- apply(d.hos[,items.hos],1,mean,na.rm = TRUE)

dat$neg <- dat$day.mean.neg
dat$pos <- dat$day.mean.pos
dat$hos <- dat$day.mean.hos


dat$day.mean.pos <- scales::rescale(dat$day.mean.pos, to = c(0,10), from = c(0,4))
dat$day.mean.neg <- scales::rescale(dat$day.mean.neg, to = c(0,10), from = c(0,4))
dat$day.mean.hos <- scales::rescale(dat$day.mean.hos, to = c(0,10), from = c(0,4))




dat$pos_lag1 =Lag(as.numeric(dat$day.mean.pos,1))
dat$neg_lag1 =Lag(as.numeric(dat$day.mean.neg,1))
dat$hos_lag1 =Lag(as.numeric(dat$day.mean.hos,1))




# test -retest network structures
network.list <- c("day", "id", "pos", "pos_lag1", "neg", "neg_lag1", "hos", "hos_lag1")
dat.net.test <- as.data.frame(dat[,network.list])
dat.net.retest <- as.data.frame(dat[,network.list])


dat.net.test$neg_lag1[which(is.even(dat.net.test$day))] <- NA
dat.net.retest$neg_lag1[which(is.odd(dat.net.retest$day))] <- NA
dat.net.test$pos_lag1[which(is.even(dat.net.test$day))] <- NA
dat.net.retest$pos_lag1[which(is.odd(dat.net.retest$day))] <- NA
dat.net.test$hos_lag1[which(is.even(dat.net.test$day))] <- NA
dat.net.retest$hos_lag1[which(is.odd(dat.net.retest$day))] <- NA

#view(dfSummary(dat.net.test))

# person-specific scale mean
person.mean.neg <- ddply( dat, .(id), function(x) mean(x$day.mean.neg, na.rm = TRUE) )
person.mean.hos <- ddply( dat, .(id), function(x) mean(x$day.mean.hos, na.rm = TRUE) )
person.mean.pos <- ddply( dat, .(id), function(x) mean(x$day.mean.pos, na.rm = TRUE) )


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
person.alpha.pos <- ddply( dat, .(id), function(x) avgcor(x[,c("alert", "active", "attentive", "determined", "inspired")]))
person.alpha.hos <- ddply( dat, .(id), function(x) avgcor(x[,c("hostile", "upset")]))
person.alpha.neg <- ddply( dat, .(id), function(x) avgcor(x[,c("afraid", "ashamed", "nervous")]))


# create dataset including person-specific statistics
person <- Reduce(function(x, y) merge(x, y, by="id"), 
                 list( 
                   person.mean.pos, person.mean.neg, person.mean.hos,
                   
                   person.rl.sd.pos, person.rl.sd.neg, person.rl.sd.hos, 
                   
                   person.rl.mssd.pos, person.rl.mssd.neg, person.rl.mssd.hos, 
                   
                   person.sd.pos, person.sd.neg, person.sd.hos, 
                   
                   person.alpha.neg, person.alpha.pos, person.alpha.hos
                 ))



# column names
colnames(person) <- c("id",
                      "mean.pos", "mean.neg", "mean.hos", 
                      
                      "rl.sd.pos", "rl.sd.neg", "rl.sd.hos", 
                      
                      "rl.mssd.pos", "rl.mssd.neg", "rl.mssd.hos", 
                      
                      "sd.pos", "sd.neg", "sd.hos", 
                      
                      "alpha.neg", "alpha.pos", "alpha.hos")

# average correlation
person$alpha  <- apply(person[,c("alpha.neg", "alpha.pos", "alpha.hos")],1,mean,na.rm = TRUE)


person$mssd.paf <- mssd(dat$day.mean.pos, dat$id, na.rm = TRUE)[,1]
person$mssd.neg <- mssd(dat$day.mean.neg, dat$id, na.rm = TRUE)[,1]
person$mssd.hos <- mssd(dat$day.mean.hos, dat$id, na.rm = TRUE)[,1]

dat$day.mean.pos_lag1 =Lag(as.numeric(dat$day.mean.pos,1))
dat$day.mean.neg_lag1 =Lag(as.numeric(dat$day.mean.neg,1))
dat$day.mean.hos_lag1 =Lag(as.numeric(dat$day.mean.hos,1))


# Two Step Frequentist mlVAR
list <- c("neg", "pos", "hos")
mlVAR.fit1 <- mlVAR(dat, vars = list, idvar = "id", lags = 1, beepvar = "day",
                    temporal = "correlated", contemporaneous = "correlated", 
                    scale = T, scaleWithin = F)

# fixed effects
p1 <- plot(mlVAR.fit1, type = "temporal", nonsig = "show", fade = FALSE, edge.labels = TRUE)
p3 <- plot(mlVAR.fit1, type = "contemporaneous", nonsig = "show", fade = FALSE, edge.labels = TRUE)
# random effects
p2 <- plot(mlVAR.fit1, type = "temporal", SD = T, nonsig = "show", fade = FALSE, edge.labels = TRUE, posCol = "black")
p4 <- plot(mlVAR.fit1, type = "contemporaneous", SD = T, nonsig = "show", edge.labels = TRUE, posCol = "black")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(p1)
plot(p2)
plot(p3)
plot(p4)
graphics::layout(1) # 12,6



i = 1
for(i in 1:100) {
  tryCatch({
    cont.netw <- mlVAR.fit1$results$Theta$pcor$subject[[i]]
    temp.netw <- as.data.frame(mlVAR.fit1$results$Beta$subject[[i]])
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






#### TEST DATA

test1_simple <- read.table("aid_test_simple.csv", sep = ";", header = TRUE) 
test1_net <- read.table("aid_test_net.csv", sep = ";", header = TRUE) 

# identify improper cases
test1_simple$day[test1_simple$day < 1] <- NA #identify improper cases (entry < 1)
table2 <- table(test1_simple$id)
test1_simple$combine <- paste(test1_simple$id, test1_simple$day) # combine variable to identify duplicate cases
test3_simple <- test1_simple[!duplicated(test1_simple$combine), ] # apply
test1_simple <- test3_simple[with(test3_simple, order(id, day)),]

# inclusion criterion
table <- table(test1_simple$id) 
min.occ = 20 # minimum 20 cases
max.occ = 101 # identify improper cases
which1 <- names(table[table < min.occ]) # identify cases to be excluded 
which2 <- names(table[table > max.occ]) 
test2_simple <- test1_simple[ ! test1_simple$id %in% which1, ]  
test2_simple <- test2_simple[ ! test2_simple$id %in% which2, ] 

# identify improper cases
test1_net$day[test1_net$day < 1] <- NA #identify improper cases (entry < 1)
table2 <- table(test1_net$id)
test1_net$combine <- paste(test1_net$id, test1_net$day) # combine variable to identify duplicate cases
test3_net <- test1_net[!duplicated(test1_net$combine), ] # apply
test1_net <- test3_net[with(test3_net, order(id, day)),]

# inclusion criterion
table <- table(test1_net$id) 
min.occ = 20 # minimum 20 cases
max.occ = 101 # identify improper cases
which1 <- names(table[table < min.occ]) # identify cases to be excluded 
which2 <- names(table[table > max.occ]) 
test2_net <- test1_net[ ! test1_net$id %in% which1, ]  
test2_net <- test2_net[ ! test2_net$id %in% which2, ] 

test_net <- subset(test2_net, select = c("id","day" , "alert", "attentive", "determined", "inspired", "active", 
                                         "afraid", "ashamed", "nervous", "hostile", "upset", "day_dummy"))

table <- table(test_net$id)
length(table) # number of individuals

test_simple <- subset(test2_simple, select = c("id","day" , "alert", "attentive", "determined", "inspired", "active", 
                                               "afraid", "ashamed", "nervous", "hostile", "upset"))

table <- table(test_simple$id)
length(table) # number of individuals


### emotion scales based on ML-EFA
items.pos <- c("alert", "attentive", "determined", "inspired",  "active") 
items.neg <- c("afraid", "ashamed", "nervous") 
items.hos <- c("hostile", "upset") 
test_net.pos <- test_net[,items.pos]
test_net.neg <- test_net[,items.neg]
test_net.hos <- test_net[,items.hos]
test_simple.pos <- test_simple[,items.pos]
test_simple.neg <- test_simple[,items.neg]
test_simple.hos <- test_simple[,items.hos]
test_simple$day.mean.pos <- apply(test_simple.pos[,items.pos],1,mean,na.rm = TRUE)
test_simple$day.mean.neg <- apply(test_simple.neg[,items.neg],1,mean,na.rm = TRUE)
test_simple$day.mean.hos <- apply(test_simple.hos[,items.hos],1,mean,na.rm = TRUE)
test_net$day.mean.pos <- apply(test_net.pos[,items.pos],1,mean,na.rm = TRUE)
test_net$day.mean.neg <- apply(test_net.neg[,items.neg],1,mean,na.rm = TRUE)
test_net$day.mean.hos <- apply(test_net.hos[,items.hos],1,mean,na.rm = TRUE)

test_net$neg <- test_net$day.mean.neg
test_net$pos <- test_net$day.mean.pos
test_net$hos <- test_net$day.mean.hos

test_simple$day.mean.pos <- scales::rescale(test_simple$day.mean.pos, to = c(0,10), from = c(0,4))
test_simple$day.mean.neg <- scales::rescale(test_simple$day.mean.neg, to = c(0,10), from = c(0,4))
test_simple$day.mean.hos <- scales::rescale(test_simple$day.mean.hos, to = c(0,10), from = c(0,4))

test_net$day.mean.pos <- scales::rescale(test_net$day.mean.pos, to = c(0,10), from = c(0,4))
test_net$day.mean.neg <- scales::rescale(test_net$day.mean.neg, to = c(0,10), from = c(0,4))
test_net$day.mean.hos <- scales::rescale(test_net$day.mean.hos, to = c(0,10), from = c(0,4))

test_simple$neg <- test_simple$day.mean.neg
test_simple$pos <- test_simple$day.mean.pos
test_simple$hos <- test_simple$day.mean.hos



# person-specific scale mean
person.mean.neg <- ddply( test_simple, .(id), function(x) mean(x$day.mean.neg, na.rm = TRUE) )
person.mean.hos <- ddply( test_simple, .(id), function(x) mean(x$day.mean.hos, na.rm = TRUE) )
person.mean.pos <- ddply( test_simple, .(id), function(x) mean(x$day.mean.pos, na.rm = TRUE) )

# person-specific scale sd
person.sd.neg <- ddply( test_simple, .(id), function(x) sd(x$day.mean.neg, na.rm = TRUE) )
person.sd.hos <- ddply( test_simple, .(id), function(x) sd(x$day.mean.hos, na.rm = TRUE) )
person.sd.pos <- ddply( test_simple, .(id), function(x) sd(x$day.mean.pos, na.rm = TRUE) )


# person-specific scale relative mssd 
person.rl.mssd.neg <- ddply( test_simple, .(id), function(x) relativeMSSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.mssd.hos <- ddply( test_simple, .(id), function(x) relativeMSSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.mssd.pos <- ddply( test_simple, .(id), function(x) relativeMSSD(x$day.mean.pos, MIN = 0, MAX = 10) )

person.rl.sd.neg <- ddply( test_simple, .(id), function(x) relativeSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.sd.hos <- ddply( test_simple, .(id), function(x) relativeSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.sd.pos <- ddply( test_simple, .(id), function(x) relativeSD(x$day.mean.pos, MIN = 0, MAX = 10) )

# person-specific scale cronbachs alpha
avgcor <- function(y){mean(abs(cor(y)[lower.tri(y)]),na.rm = TRUE)}
person.alpha.pos <- ddply( test_simple, .(id), function(x) avgcor(x[,c("alert", "active", "attentive", "determined", "inspired")]))
person.alpha.hos <- ddply( test_simple, .(id), function(x) avgcor(x[,c("hostile", "upset")]))
person.alpha.neg <- ddply( test_simple, .(id), function(x) avgcor(x[,c("afraid", "ashamed", "nervous")]))

# create dataset including person-specific statistics
person.test <- Reduce(function(x, y) merge(x, y, by="id"), 
                      list( 
                        person.mean.pos, person.mean.neg, person.mean.hos,
                        
                        
                        person.rl.sd.pos, person.rl.sd.neg, person.rl.sd.hos, 
                        
                        person.rl.mssd.pos, person.rl.mssd.neg, person.rl.mssd.hos, 
                        
                        person.sd.pos, person.sd.neg, person.sd.hos, 
                        
                        person.alpha.neg, person.alpha.pos, person.alpha.hos
                      ))

# column names
colnames(person.test) <- c("id",
                           "mean.pos", "mean.neg", "mean.hos", 
                           
                           "rl.sd.pos", "rl.sd.neg", "rl.sd.hos", 
                           
                           "rl.mssd.pos", "rl.mssd.neg", "rl.mssd.hos", 
                           
                           "sd.pos", "sd.neg", "sd.hos", 
                           
                           "alpha.neg", "alpha.pos", "alpha.hos")


# mean square succesive difference

person.test$mssd.paf <- mssd(test_simple$day.mean.pos, test_simple$id, na.rm = TRUE)[,1]
person.test$mssd.neg <- mssd(test_simple$day.mean.neg, test_simple$id, na.rm = TRUE)[,1]
person.test$mssd.hos <- mssd(test_simple$day.mean.hos, test_simple$id, na.rm = TRUE)[,1]


# Two Step Frequentist mlVAR
list <- c("neg", "pos", "hos")
mlVAR.fit2 <- mlVAR(test_net, vars = list, idvar = "id", lags = 1, beepvar = "day_dummy",
                    temporal = "correlated", contemporaneous = "correlated", 
                    scale = T, scaleWithin = F)

# fixed effects
p1 <- plot(mlVAR.fit2, type = "temporal", nonsig = "show", fade = FALSE, edge.labels = TRUE)
p3 <- plot(mlVAR.fit2, type = "contemporaneous", nonsig = "show", fade = FALSE, edge.labels = TRUE)
# random effects
p2 <- plot(mlVAR.fit2, type = "temporal", SD = T, nonsig = "show", fade = FALSE, edge.labels = TRUE, posCol = "black")
p4 <- plot(mlVAR.fit2, type = "contemporaneous", SD = T, nonsig = "show", edge.labels = TRUE, posCol = "black")

layout(matrix(c(1,2,3,4), 2, 2, byrow = TRUE))
plot(p1)
plot(p2)
plot(p3)
plot(p4)
graphics::layout(1) # 12,6

i = 1
for(i in 1:100) {
  tryCatch({
    cont.netw <- mlVAR.fit2$results$Theta$pcor$subject[[i]]
    temp.netw <- as.data.frame(mlVAR.fit2$results$Beta$subject[[i]])
    person.test$temp.ar.neg[i] <- temp.netw$neg.1[1] # neg.ar
    person.test$temp.cl.pos.neg[i] <- temp.netw$pos.1[1] # pos.neg
    person.test$temp.cl.hos.neg[i] <- temp.netw$hos.1[1] # hos.neg
    person.test$temp.cl.neg.pos[i] <- temp.netw$neg.1[2] # neg.pos
    person.test$temp.ar.pos[i] <- temp.netw$pos.1[2] # pos.ar
    person.test$temp.cl.hos.pos[i] <- temp.netw$hos.1[2] # hos.pos
    person.test$temp.cl.neg.hos[i] <- temp.netw$neg.1[3] # neg.hos
    person.test$temp.cl.pos.hos[i] <- temp.netw$pos.1[3] # pos.hos
    person.test$temp.ar.hos[i] <- temp.netw$hos.1[3] # hos.ar
    person.test$cont.pcor.neg.pos[i] <- cont.netw[1,2] # neg pos
    person.test$cont.pcor.neg.hos[i] <- cont.netw[1,3] # neg hos
    person.test$cont.pcor.hos.pos[i] <- cont.netw[2,3] # hos pos
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
id <- person.test$id
person.test <- data.frame(apply(person.test[,2:ncol(person.test)],2,median.imp))
person.test$id <- id
colnames(person.test)

#### TEST DATA

retest1_simple <- read.table("aid_retest_simple.csv", sep = ";", header = TRUE) 
retest1_net <- read.table("aid_retest_net.csv", sep = ";", header = TRUE) 

# identify improper cases
retest1_simple$day[retest1_simple$day < 1] <- NA #identify improper cases (entry < 1)
table2 <- table(retest1_simple$id)
retest1_simple$combine <- paste(retest1_simple$id, retest1_simple$day) # combine variable to identify duplicate cases
retest3_simple <- retest1_simple[!duplicated(retest1_simple$combine), ] # apply
retest1_simple <- retest3_simple[with(retest3_simple, order(id, day)),]

# inclusion criterion
table <- table(retest1_simple$id) 
min.occ = 20 # minimum 20 cases
max.occ = 101 # identify improper cases
which1 <- names(table[table < min.occ]) # identify cases to be excluded 
which2 <- names(table[table > max.occ]) 
retest2_simple <- retest1_simple[ ! retest1_simple$id %in% which1, ]  
retest2_simple <- retest2_simple[ ! retest2_simple$id %in% which2, ] 

# identify improper cases
retest1_net$day[retest1_net$day < 1] <- NA #identify improper cases (entry < 1)
table2 <- table(retest1_net$id)
retest1_net$combine <- paste(retest1_net$id, retest1_net$day) # combine variable to identify duplicate cases
retest3_net <- retest1_net[!duplicated(retest1_net$combine), ] # apply
retest1_net <- retest3_net[with(retest3_net, order(id, day)),]

# inclusion criterion
table <- table(retest1_net$id) 
min.occ = 20 # minimum 20 cases
max.occ = 101 # identify improper cases
which1 <- names(table[table < min.occ]) # identify cases to be excluded 
which2 <- names(table[table > max.occ]) 
retest2_net <- retest1_net[ ! retest1_net$id %in% which1, ]  
retest2_net <- retest2_net[ ! retest2_net$id %in% which2, ] 

retest_net <- subset(retest2_net, select = c("id","day" , "alert", "attentive", "determined", "inspired", "active", 
                                             "afraid", "ashamed", "nervous", "hostile", "upset", "day_dummy"))

table <- table(retest_net$id)
length(table) # number of individuals

retest_simple <- subset(retest2_simple, select = c("id","day" , "alert", "attentive", "determined", "inspired", "active", 
                                                   "afraid", "ashamed", "nervous", "hostile", "upset"))

table <- table(retest_simple$id)
length(table) # number of individuals


### emotion scales based on ML-EFA
items.pos <- c("alert", "attentive", "determined", "inspired",  "active") 
items.neg <- c("afraid", "ashamed", "nervous") 
items.hos <- c("hostile", "upset") 
retest_net.pos <- retest_net[,items.pos]
retest_net.neg <- retest_net[,items.neg]
retest_net.hos <- retest_net[,items.hos]
retest_simple.pos <- retest_simple[,items.pos]
retest_simple.neg <- retest_simple[,items.neg]
retest_simple.hos <- retest_simple[,items.hos]
retest_simple$day.mean.pos <- apply(retest_simple.pos[,items.pos],1,mean,na.rm = TRUE)
retest_simple$day.mean.neg <- apply(retest_simple.neg[,items.neg],1,mean,na.rm = TRUE)
retest_simple$day.mean.hos <- apply(retest_simple.hos[,items.hos],1,mean,na.rm = TRUE)
retest_net$day.mean.pos <- apply(retest_net.pos[,items.pos],1,mean,na.rm = TRUE)
retest_net$day.mean.neg <- apply(retest_net.neg[,items.neg],1,mean,na.rm = TRUE)
retest_net$day.mean.hos <- apply(retest_net.hos[,items.hos],1,mean,na.rm = TRUE)

retest_net$neg <- retest_net$day.mean.neg
retest_net$pos <- retest_net$day.mean.pos
retest_net$hos <- retest_net$day.mean.hos


retest_simple$day.mean.pos <- scales::rescale(retest_simple$day.mean.pos, to = c(0,10), from = c(0,4))
retest_simple$day.mean.neg <- scales::rescale(retest_simple$day.mean.neg, to = c(0,10), from = c(0,4))
retest_simple$day.mean.hos <- scales::rescale(retest_simple$day.mean.hos, to = c(0,10), from = c(0,4))

retest_net$day.mean.pos <- scales::rescale(retest_net$day.mean.pos, to = c(0,10), from = c(0,4))
retest_net$day.mean.neg <- scales::rescale(retest_net$day.mean.neg, to = c(0,10), from = c(0,4))
retest_net$day.mean.hos <- scales::rescale(retest_net$day.mean.hos, to = c(0,10), from = c(0,4))

retest_simple$neg <- retest_simple$day.mean.neg
retest_simple$pos <- retest_simple$day.mean.pos
retest_simple$hos <- retest_simple$day.mean.hos



# person-specific scale mean
person.mean.neg <- ddply( retest_simple, .(id), function(x) mean(x$day.mean.neg, na.rm = TRUE) )
person.mean.hos <- ddply( retest_simple, .(id), function(x) mean(x$day.mean.hos, na.rm = TRUE) )
person.mean.pos <- ddply( retest_simple, .(id), function(x) mean(x$day.mean.pos, na.rm = TRUE) )

# person-specific scale sd
person.sd.neg <- ddply( retest_simple, .(id), function(x) sd(x$day.mean.neg, na.rm = TRUE) )
person.sd.hos <- ddply( retest_simple, .(id), function(x) sd(x$day.mean.hos, na.rm = TRUE) )
person.sd.pos <- ddply( retest_simple, .(id), function(x) sd(x$day.mean.pos, na.rm = TRUE) )



# person-specific scale relative mssd 
person.rl.mssd.neg <- ddply( retest_simple, .(id), function(x) relativeMSSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.mssd.hos <- ddply( retest_simple, .(id), function(x) relativeMSSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.mssd.pos <- ddply( retest_simple, .(id), function(x) relativeMSSD(x$day.mean.pos, MIN = 0, MAX = 10) )

person.rl.sd.neg <- ddply( retest_simple, .(id), function(x) relativeSD(x$day.mean.neg, MIN = 0, MAX = 10) )
person.rl.sd.hos <- ddply( retest_simple, .(id), function(x) relativeSD(x$day.mean.hos, MIN = 0, MAX = 10) )
person.rl.sd.pos <- ddply( retest_simple, .(id), function(x) relativeSD(x$day.mean.pos, MIN = 0, MAX = 10) )

# person-specific scale cronbachs alpha
avgcor <- function(y){mean(abs(cor(y)[lower.tri(y)]),na.rm = TRUE)}
person.alpha.pos <- ddply( retest_simple, .(id), function(x) avgcor(x[,c("alert", "active", "attentive", "determined", "inspired")]))
person.alpha.hos <- ddply( retest_simple, .(id), function(x) avgcor(x[,c("hostile", "upset")]))
person.alpha.neg <- ddply( retest_simple, .(id), function(x) avgcor(x[,c("afraid", "ashamed", "nervous")]))

# create dataset including person-specific statistics
person.retest <- Reduce(function(x, y) merge(x, y, by="id"), 
                        list( 
                          person.mean.pos, person.mean.neg, person.mean.hos,
                          
                          
                          person.rl.sd.pos, person.rl.sd.neg, person.rl.sd.hos, 
                          
                          person.rl.mssd.pos, person.rl.mssd.neg, person.rl.mssd.hos, 
                          
                          person.sd.pos, person.sd.neg, person.sd.hos, 
                          
                          person.alpha.neg, person.alpha.pos, person.alpha.hos
                        ))

# column names
colnames(person.retest) <- c("id",
                             "mean.pos", "mean.neg", "mean.hos", 
                             
                             "rl.sd.pos", "rl.sd.neg", "rl.sd.hos", 
                             
                             "rl.mssd.pos", "rl.mssd.neg", "rl.mssd.hos", 
                             
                             "sd.pos", "sd.neg", "sd.hos", 
                             
                             "alpha.neg", "alpha.pos", "alpha.hos")


# mean square succesive difference

person.retest$mssd.paf <- mssd(retest_simple$day.mean.pos, retest_simple$id, na.rm = TRUE)[,1]
person.retest$mssd.neg <- mssd(retest_simple$day.mean.neg, retest_simple$id, na.rm = TRUE)[,1]
person.retest$mssd.hos <- mssd(retest_simple$day.mean.hos, retest_simple$id, na.rm = TRUE)[,1]



# Two Step Frequentist mlVAR
list <- c("neg", "pos", "hos")
mlVAR.fit3 <- mlVAR(retest_net, vars = list, idvar = "id", lags = 1, beepvar = "day_dummy",
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
for(i in 1:100) {
  tryCatch({
    cont.netw <- mlVAR.fit3$results$Theta$pcor$subject[[i]]
    temp.netw <- as.data.frame(mlVAR.fit3$results$Beta$subject[[i]])
    person.retest$temp.ar.neg[i] <- temp.netw$neg.1[1] # neg.ar
    person.retest$temp.cl.pos.neg[i] <- temp.netw$pos.1[1] # pos.neg
    person.retest$temp.cl.hos.neg[i] <- temp.netw$hos.1[1] # hos.neg
    person.retest$temp.cl.neg.pos[i] <- temp.netw$neg.1[2] # neg.pos
    person.retest$temp.ar.pos[i] <- temp.netw$pos.1[2] # pos.ar
    person.retest$temp.cl.hos.pos[i] <- temp.netw$hos.1[2] # hos.pos
    person.retest$temp.cl.neg.hos[i] <- temp.netw$neg.1[3] # neg.hos
    person.retest$temp.cl.pos.hos[i] <- temp.netw$pos.1[3] # pos.hos
    person.retest$temp.ar.hos[i] <- temp.netw$hos.1[3] # hos.ar
    person.retest$cont.pcor.neg.pos[i] <- cont.netw[1,2] # neg pos
    person.retest$cont.pcor.neg.hos[i] <- cont.netw[1,3] # neg hos
    person.retest$cont.pcor.hos.pos[i] <- cont.netw[2,3] # hos pos
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
id <- person.retest$id
person.retest <- data.frame(apply(person.retest[,2:ncol(person.retest)],2,median.imp))
person.retest$id <- id

colnames(person.retest)
#####


#write.table(dat_crit, file = "dat_crit.csv", col.names = T,
#            sep = ";", row.names = F)
# 
write.table(person, "soed_minrev_sample2.csv", col.names = T, sep = ";", row.names = F)
write.table(person.test, "soed_minrev_sample2_test.csv", col.names = T, sep = ";", row.names = F)
write.table(person.retest, "soed_minrev_sample2_retest.csv", col.names = T, sep = ";", row.names = F)

colnames(person.retest)


dat2 <- read.table("soed_minrev_sample2.csv", sep = ";", header = TRUE)
dat2_test <- read.table("soed_minrev_sample2_test.csv", sep = ";", header = TRUE)
dat2_retest <- read.table("soed_minrev_sample2_retest.csv", sep = ";", header = TRUE)

subset.pca <- function (x) {
  x <- subset(x, select = c(  "id","mean.pos", "mean.neg", "mean.hos", 
                              "sd.pos","sd.neg","sd.hos",  
                              "mssd.paf","mssd.neg","mssd.hos",
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
dat_test <- subset.pca(dat2_test)
dat_retest <- subset.pca(dat2_retest)

write.table(dat, "soed_minrev_sample2_sub.csv", col.names = T, sep = ";", row.names = F)
write.table(dat_test, "soed_minrev_sample2_sub_test.csv", col.names = T, sep = ";", row.names = F)
write.table(dat_retest, "soed_minrev_sample2_sub_retest.csv", col.names = T, sep = ";", row.names = F)

rm(list = ls())

