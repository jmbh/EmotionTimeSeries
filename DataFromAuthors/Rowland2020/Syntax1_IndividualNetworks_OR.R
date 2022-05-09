install.packages("qgraph")
install.packages("graphicalVAR")

library("qgraph")
library("graphicalVAR")

getwd()

###include path
getwd()
setwd("")

### outcome names
outnames <- c("happy", "excited", "relaxed", "satisfied", "angry","anxious", "depressed", "sad")
dat <- read.table("data_affect.csv", sep=",", header=TRUE, na.strings="")
##sort data
dat <- dat[order(dat$subjno, dat$group, dat$dayno, dat$beep),]
names(dat)[pmatch(c("emo1_m", "emo2_m", "emo3_m", "emo4_m", "emo5_m", "emo6_m", "emo7_m", "emo8_m"),
                  names(dat))] <- outnames

## modeling temporal networks
myData_ind_dat<- mlGraphicalVAR(dat, vars = c("happy", "excited", "relaxed", "satisfied", "angry", "anxious", "depressed", "sad"), dayvar = "dayno", idvar = "subjno",lags = 1, beepvar = "beep", subjectNetworks = TRUE, centerWithin = TRUE, gamma = 0, lambda_kappa = 0, lambda_beta = 0)
ID = myData_ind_dat$ids

myData_ind_dat$fixedResults$N

## extracting individuals' temporal network density
t_oad=c()
for (i in 1:125){
  t_oad[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 1:8]))}

t_pad=c()
for (i in 1:125){
  t_pad[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 1:4]))}

t_nad=c()
for (i in 1:125){
  t_nad[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 5:8]))}



## extracting individuals' contemporaneous network density
c_oad=c()
for (i in 1:125){
  c_oad[i] <- ((sum(abs(myData_ind_dat$subjectPCC[[i]][1:8, 1:8]))))/2/28}

c_pad=c()
for (i in 1:125){
  c_pad[i] <- ((sum(abs(myData_ind_dat$subjectPCC[[i]][1:4, 1:4]))))/2/6}

c_nad=c()
for (i in 1:125){
  c_nad[i] <- ((sum(abs(myData_ind_dat$subjectPCC[[i]][5:8, 5:8]))))/2/6}

c_pa_nad=c()
for (i in 1:125){
  c_pa_nad[i] <- ((mean(abs(myData_ind_dat$subjectPCC[[i]][1:4, 5:8]))))}

options(max.print = 99999999)


##extracting temporal single-affect network densities
t_happy=c()
for (i in 1:125){
  t_happy[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 1:1]))}

t_excited=c()
for (i in 1:125){
  t_excited[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 2:2]))}

t_relaxed=c()
for (i in 1:125){
  t_relaxed[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 3:3]))}

t_satisfied=c()
for (i in 1:125){
  t_satisfied[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 4:4]))}

t_angry=c()
for (i in 1:125){
  t_angry[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 5:5]))}

t_anxious=c()
for (i in 1:125){
  t_anxious[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 6:6]))}

t_depressed=c()
for (i in 1:125){
  t_depressed[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 7:7]))}

t_sad=c()
for (i in 1:125){
  t_sad[i] <- mean(abs(myData_ind_dat$subjectPDC[[i]][1:8, 8:8]))}


## combining all individual network densities
all_matrix <- cbind(ID, t_oad, t_pad, t_nad, c_oad, c_pad, c_nad, c_pa_nad, t_happy, t_excited, t_relaxed, t_satisfied, t_angry, t_anxious, t_depressed, t_sad)
write.csv(all_matrix, "all_matrix.csv")



## population networks: Figure 1
pdf("laggednetworkOR.pdf")
qgraph(myData_ind_dat$fixedPDC, layout = "circle", edge.labels = TRUE, theme = "colorblind", fade = T)
dev.off()

pdf("residualnetworkOR.pdf")
qgraph(myData_ind_dat$fixedPCC, layout = "circle", edge.labels = T,theme = "colorblind")
dev.off()

pdf("betweennetworkOR.pdf")
qgraph(myData_ind_dat$betweenNet, layout = "circle", theme = "gray")
dev.off()