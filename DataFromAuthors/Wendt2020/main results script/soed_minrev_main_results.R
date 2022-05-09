### ### ### ### ### ### ### ### ### ### ### ### ###
### INDICATORS OF AFFECT DYNAMICS: STRUCTURE, 
### RELIABILITY, AND PERSONALITY CORRELATES
###
###  REPRODUCIBLE DATA ANALYSIS

### submitted to the European Journal of 
### Personality's special issue:
###      New approaches towards conceptualizing and 
###      assessing personality

# For this code to run, you need the packages installed
# and .csv-files readily available at the OSF page

# Link: https://osf.io/6ghcx/

# set your working directory to the folder
# containing the .csv-files

#setwd()

# load package (install packages if needed)
library(Hmisc)
library(mgcv)# version 1.8-15
library(quantmod)# version 0.4-0
library(ggplot2)
library(gridExtra)
library(psychmeta)
library(metafor)
library(car)
library(psych)
library(qgraph)
library(DescTools)
library(summarytools)
library(weights)
library(semPlot)
library(reshape2)
library(plotly)
library(sjPlot)
library(CTT)
library(numform)
library(plotly)
source('D:/sera/my google drive/R Daten/corstar_function.R')
options(max.print=1000000)

setwd("D:/sera/my google drive/R Daten/OSF iads")


# load data
sample1 <- read.table("soed_minrev_sample1_sub.csv", sep = ";", header = TRUE)
sample2 <- read.table("soed_minrev_sample2_sub.csv", sep = ";", header = TRUE)
sample3 <- read.table("soed_minrev_sample3_sub.csv", sep = ";", header = TRUE)

# (first half of the assessment period in Sample 2)
test1    <- read.table("soed_minrev_sample2_sub_test.csv", sep = ";", header = TRUE)
# (second half of the assessment period in Sample 2)
retest1  <- read.table("soed_minrev_sample2_sub_retest.csv", sep = ";", header = TRUE)

# (first half of the assessment period in Sample 3)
test2    <- read.table("soed_minrev_sample3_sub_test.csv", sep = ";", header = TRUE)
# (second half of the assessment period in Sample 3)
retest2  <- read.table("soed_minrev_sample3_sub_retest.csv", sep = ";", header = TRUE)

# load five-factor personality data at baseline
ffm1 <- read.table("ffm1.csv", sep = ";", header = TRUE)
ffm2 <- read.table("ffm2.csv", sep = ";", header = TRUE)
ffm3 <- read.table("ffm3.csv", sep = ";", header = TRUE)

### DESCRIPTIVE STATISTICS

desc1 <- psych::describe(sample1[,c(2:7,11:13,8:10,14:24,26,25,27:29,31,30)], quant=c(.25,.75), skew = T, IQR = T)
desc2 <- psych::describe(sample2[,c(2:7,11:13,8:10,14:24,26,25,27:29,31,30)], quant=c(.25,.75), skew = T, IQR = T)
desc3 <- psych::describe(sample3[,c(2:7,11:13,8:10,14:24,26,25,27:29,31,30)], quant=c(.25,.75), skew = T, IQR = T)

descr1 <- psych::describe(retest1[,c(2:7,11:13,8:10,14:24,26,25,27:29,31,30)], quant=c(.25,.75), skew = T, IQR = T)
desct1 <- psych::describe(test1[,c(2:7,11:13,8:10,14:24,26,25,27:29,31,30)], quant=c(.25,.75), skew = T, IQR = T)

# save id
id1 <- sample1$id
id2 <- sample2$id
id3 <- sample3$id

# delete id, z-transform pca indicators
sample1.pca <- data.frame(scale(sample1[,c(2:ncol(sample1))]))
sample2.pca <- data.frame(scale(sample2[,c(2:ncol(sample2))]))
sample3.pca <- data.frame(scale(sample3[,c(2:ncol(sample3))]))

# create spearman matrix
sample1.spearman.cor <- cor(sample1.pca[,1:30], method = "spearman") 
sample2.spearman.cor <- cor(sample2.pca[,1:30], method = "spearman")
sample3.spearman.cor <- cor(sample3.pca[,1:30], method = "spearman") 

#### 1) PARALLEL ANALYSIS ####

#parallel analysis in Sample 1
# parallel1 <- fa.parallel(sample1.spearman.cor,
#                          n.iter = 5000,
#                          fa = 'pc',
#                          plot = FALSE,
#                          use = "listwise",
#                          SMC = TRUE,
#                          quant = .95,
#                          n.obs= nrow(sample1)) # 9 comp.
# #
# # # Sample 2
# parallel2 <- fa.parallel(sample2.spearman.cor,
#                          n.iter = 5000,
#                          fa = 'pc',
#                          plot = FALSE,
#                          use = "listwise",
#                          SMC = TRUE,
#                          quant = .95,
#                          n.obs= nrow(sample2)) # 9 comp.
# #
# # # Sample 3
# parallel3 <- fa.parallel(sample3.spearman.cor,
#                          n.iter = 5000,
#                          fa = 'pc',
#                          plot = FALSE,
#                          use = "listwise",
#                          SMC = TRUE,
#                          quant = .95,
#                          n.obs= nrow(sample3)) # 7 comp.


#### 2) SCREE PLOT ####

# Sample 1
VSS.scree(sample1.spearman.cor, main = "scree plot")

# Sample 2
VSS.scree(sample2.spearman.cor, main = "scree plot")

# Sample 3
VSS.scree(sample3.spearman.cor, main = "scree plot")

# no elbow



### SEMPATHS



manifests <- c("nP","P.Mean","P.SD",  "P.MSSD","P.rSD",    "P.rMSSD",         
               
               "H.Mean", "H.SD", "H.MSSD",
               "N.Mean", "N.SD", "N.MSSD",
               
               "N.rSD", "N.rMSSD", 
               "H.rSD", "H.rMSSD",  
               
               
               "P_H", "P_N","N_H", 
               "H.ALPHA","N.ALPHA","P.ALPHA",
               
               "nH","nN" ,"pH", 
               "hN",  "hH","pN", 
               
               "hP", "pP")

ora <- "#FAD7A0"
red <- "#FADBD8"
blu <-"#D6EAF8"
tra <- "transparent"

color <- c(tra, blu, blu, blu, blu, blu, 
           red, red, red, 
           ora, ora, ora, 
           ora, ora,         
           red, red, 
           
           tra, tra, tra, 
           red, ora,   blu,            
           tra, tra,tra, 
           
           
           tra, tra, tra,
           tra, tra
)


rotate = "varimax"
nf <- 7
cut <- .60
min <- .60
sizeMan1 <- 3.5+.8
sizeMan2 <- 3.4+.8
sizeLat1 <- 5.7
sizeLat2 <- 5.5
label.cex = 3
edge.label.size = 2
ed.pos <- .15

labels1 <- c("xNP","PA M","PA SD","PA MSSD", "PA SDc", "PA MSSDc", 
             
             "HA M", "HA SD", "HA MSSD",
             "NA M", "NA SD", "NA MSSD",
             
             
             
             "NA SDc", "NA MSSDc",
             "HA SDc","HA MSSDc", 
             "rPN", "rPH", "rNH",  
             "HA a","NA a", "PA A",
             
             "xNH", "xNN","xPH",  
             "xHN", "xHH","xPN", 
             
             
             
             "xHP","xPP" ,
             
             "C1","C2",  "C5","C7", "C3", "C4", "C6")

latents1 <- c( "RC1",  "RC2","RC5","RC7", "RC3", "RC4", "RC6")
#latents1 <- c("TC1","TC2" , "TC3",  "TC4", "TC5")
pca1_fig <- principal(sample1.spearman.cor, nfactors = nf, rotate = rotate)

path1 <- semPaths(pca1_fig, "standardized", style = "lisrel", 
                  layout ="circle", cut = cut,borders = TRUE, 
                  minimum = min, nCharNodes = 0,
                  edge.labels = TRUE, edge.label.cex = edge.label.size, 
                  edge.color = "black",label.cex = label.cex, groups = "latents", 
                  reorder = TRUE, nodeLabels = labels1, color = color, 
                  latents = latents1, manifests = manifests,
                  shapeLat = "circle",
                  sizeMan = sizeMan1,
                  sizeMan2 = sizeMan2,
                  sizeLat = sizeLat1,
                  sizeLat2 = sizeLat2, edge.label.position = ed.pos)



pca2_fig <- principal(sample2.spearman.cor, nfactors = nf, rotate = rotate)


labels2 <- c("xNP","PA M","PA SD","PA MSSD", "PA SDc", "PA MSSDc", 
             
             "HA M", "HA SD", "HA MSSD",
             "NA M", "NA SD", "NA MSSD",
             
             
             
             "NA SDc", "NA MSSDc",
             "HA SDc","HA MSSDc", 
             "rPN", "rPH", "rNH",  
             "HA a","NA a", "PA A",
             
             
             "xNH", "xNN","xPH",  
             "xHN", "xHH","xPN", 
             
             
             
             "xHP","xPP" ,
             
             "C5", "C6","C1","C2", "C7",  "C4","C3")

latents2 <- c("RC5","RC6","RC1","RC2", "RC7",   "RC4",  "RC3" )
#latents2 <- c("TC1","TC2" , "TC3",  "TC4", "TC5")
path2 <- semPaths(pca2_fig, "standardized", style = "lisrel", 
                  layout ="circle", cut = cut,borders = TRUE, 
                  minimum = min, nCharNodes = 0,label.cex = label.cex,
                  edge.labels = TRUE, edge.label.cex = edge.label.size, 
                  edge.color = "black", groups = "latents", 
                  reorder = TRUE, nodeLabels = labels2, color = color, 
                  latents = latents2, manifests = manifests,
                  shapeLat = "circle",
                  sizeMan = sizeMan1,
                  sizeMan2 = sizeMan2,
                  sizeLat = sizeLat1,
                  sizeLat2 = sizeLat2, edge.label.position = ed.pos)


pca3_fig <- principal(sample3.spearman.cor, nfactors = nf, rotate = rotate)

labels3 <- c("xNP","PA M","PA SD","PA MSSD", "PA SDc", "PA MSSDc", 
             
             "HA M", "HA SD", "HA MSSD",
             "NA M", "NA SD", "NA MSSD",
             
             
             "NA SDc", "NA MSSDc",
             "HA SDc","HA MSSDc", 
             "rPN", "rPH", "rNH",  
             "HA a", "NA a", "PA A",
             
             
             "xNH", "xNN","xPH",  
             "xHN", "xHH","xPN", 
             
             
             
             "xHP","xPP" ,
             "C3", "C5" , "C1", "C2", "C6", "C4", "C7")

latents3 <- c("RC3","RC5" , "RC1",  "RC2", "RC6", "RC4",  "RC7")
#latents3 <- c("TC1","TC2" , "TC3",  "TC4", "TC5")
path3 <- semPaths(pca3_fig, "standardized", style = "lisrel", 
                  layout ="circle", cut = cut,borders = TRUE, 
                  minimum = min,  nCharNodes = 0,label.cex = label.cex,
                  edge.labels = TRUE, edge.label.cex = edge.label.size, 
                  edge.color = "black", groups = "latents", 
                  reorder = TRUE, nodeLabels = labels3, color = color, 
                  latents = latents3, manifests = manifests,
                  shapeLat = "circle",
                  sizeMan = sizeMan1,
                  sizeMan2 = sizeMan2,
                  sizeLat = sizeLat1,
                  sizeLat2 = sizeLat2, edge.label.position = ed.pos)

layout(matrix(c(1,2,3), 1, 3, byrow = TRUE))
plot(path1)
plot(path2)
plot(path3)
graphics::layout(1) # 12,6




# merge with ffm data
sample1.pca <- merge(sample1, ffm1,  by = "id", sort = F) 
sample2.pca <- merge(sample2, ffm2,  by = "id", sort = F)  
sample3.pca <- merge(sample3, ffm3,  by = "id", sort = F)




#### 5) MULTIPLE CORRELATION

## Multiple Correlation with the mean 
model_means <- 'P.Mean + N.Mean + H.Mean'
model_sd <- 'P.Mean + N.Mean + H.Mean + P.SD + N.SD + H.SD'

Rm_4_1 <- sqrt(summary(lm(paste("P.SD~",paste(model_means)),data=sample1))$r.squared) #.09
Rm_5_1 <- sqrt(summary(lm(paste("P.rSD~",paste(model_means)),data=sample1))$r.squared) #.12
Rm_6_1 <- sqrt(summary(lm(paste("N.SD~",paste(model_means)),data=sample1))$r.squared) #.55
Rm_7_1 <- sqrt(summary(lm(paste("N.rSD~",paste(model_means)),data=sample1))$r.squared) #.06
Rm_8_1 <- sqrt(summary(lm(paste("H.SD~",paste(model_means)),data=sample1))$r.squared)#.58
Rm_9_1 <- sqrt(summary(lm(paste("H.rSD~",paste(model_means)),data=sample1))$r.squared)#.27
Rm_10_1 <- sqrt(summary(lm(paste("P.MSSD~",paste(model_means)),data=sample1))$r.squared) #.10
Rm_11_1 <- sqrt(summary(lm(paste("P.rMSSD~",paste(model_means)),data=sample1))$r.squared) #.20
Rm_12_1 <- sqrt(summary(lm(paste("N.MSSD~",paste(model_means)),data=sample1))$r.squared) #.46
Rm_13_1 <- sqrt(summary(lm(paste("N.rMSSD~",paste(model_means)),data=sample1))$r.squared)# .24
Rm_14_1 <- sqrt(summary(lm(paste("P.ALPHA~",paste(model_means)),data=sample1))$r.squared) #.08
Rm_15_1 <- sqrt(summary(lm(paste("N.ALPHA~",paste(model_means)),data=sample1))$r.squared) #.14
Rm_16_1 <- sqrt(summary(lm(paste("H.MSSD~",paste(model_means)),data=sample1))$r.squared)#.50
Rm_17_1 <- sqrt(summary(lm(paste("H.rMSSD~",paste(model_means)),data=sample1))$r.squared)#.41
Rm_18_1 <- sqrt(summary(lm(paste("H.ALPHA~",paste(model_means)),data=sample1))$r.squared)#.12
Rm_19_1 <- sqrt(summary(lm(paste("pP~",paste(model_means)),data=sample1))$r.squared) #.45
Rm_20_1 <- sqrt(summary(lm(paste("nN~",paste(model_means)),data=sample1))$r.squared) #.49
Rm_21_1 <- sqrt(summary(lm(paste("hH~",paste(model_means)),data=sample1))$r.squared)#.50
Rm_22_1 <- sqrt(summary(lm(paste("pN~",paste(model_means)),data=sample1))$r.squared)#.23
Rm_23_1 <- sqrt(summary(lm(paste("pH~",paste(model_means)),data=sample1))$r.squared)#.73
Rm_24_1 <- sqrt(summary(lm(paste("nP~",paste(model_means)),data=sample1))$r.squared)#.49
Rm_25_1 <- sqrt(summary(lm(paste("nH~",paste(model_means)),data=sample1))$r.squared)#.11
Rm_26_1 <- sqrt(summary(lm(paste("hP~",paste(model_means)),data=sample1))$r.squared)#.16
Rm_27_1 <- sqrt(summary(lm(paste("hN~",paste(model_means)),data=sample1))$r.squared)#.37
Rm_28_1 <- sqrt(summary(lm(paste("P_N~",paste(model_means)),data=sample1))$r.squared)#.27
Rm_29_1 <- sqrt(summary(lm(paste("P_H~",paste(model_means)),data=sample1))$r.squared)#.08
Rm_30_1 <- sqrt(summary(lm(paste("N_H~",paste(model_means)),data=sample1))$r.squared)#.18

Rs_10_1 <- sqrt(summary(lm(paste("P.MSSD~",paste(model_sd)),data=sample1))$r.squared) #.10
Rs_11_1 <- sqrt(summary(lm(paste("P.rMSSD~",paste(model_sd)),data=sample1))$r.squared) #.20
Rs_12_1 <- sqrt(summary(lm(paste("N.MSSD~",paste(model_sd)),data=sample1))$r.squared) #.46
Rs_13_1 <- sqrt(summary(lm(paste("N.rMSSD~",paste(model_sd)),data=sample1))$r.squared)# .24
Rs_14_1 <- sqrt(summary(lm(paste("P.ALPHA~",paste(model_sd)),data=sample1))$r.squared) #.08
Rs_15_1 <- sqrt(summary(lm(paste("N.ALPHA~",paste(model_sd)),data=sample1))$r.squared) #.14
Rs_16_1 <- sqrt(summary(lm(paste("H.MSSD~",paste(model_sd)),data=sample1))$r.squared)#.50
Rs_17_1 <- sqrt(summary(lm(paste("H.rMSSD~",paste(model_sd)),data=sample1))$r.squared)#.41
Rs_18_1 <- sqrt(summary(lm(paste("H.ALPHA~",paste(model_sd)),data=sample1))$r.squared)#.12
Rs_19_1 <- sqrt(summary(lm(paste("pP~",paste(model_sd)),data=sample1))$r.squared) #.45
Rs_20_1 <- sqrt(summary(lm(paste("nN~",paste(model_sd)),data=sample1))$r.squared) #.49
Rs_21_1 <- sqrt(summary(lm(paste("hH~",paste(model_sd)),data=sample1))$r.squared)#.50
Rs_22_1 <- sqrt(summary(lm(paste("pN~",paste(model_sd)),data=sample1))$r.squared)#.23
Rs_23_1 <- sqrt(summary(lm(paste("pH~",paste(model_sd)),data=sample1))$r.squared)#.73
Rs_24_1 <- sqrt(summary(lm(paste("nP~",paste(model_sd)),data=sample1))$r.squared)#.49
Rs_25_1 <- sqrt(summary(lm(paste("nH~",paste(model_sd)),data=sample1))$r.squared)#.11
Rs_26_1 <- sqrt(summary(lm(paste("hP~",paste(model_sd)),data=sample1))$r.squared)#.16
Rs_27_1 <- sqrt(summary(lm(paste("hN~",paste(model_sd)),data=sample1))$r.squared)#.37
Rs_28_1 <- sqrt(summary(lm(paste("P_N~",paste(model_sd)),data=sample1))$r.squared)#.27
Rs_29_1 <- sqrt(summary(lm(paste("P_H~",paste(model_sd)),data=sample1))$r.squared)#.08
Rs_30_1 <- sqrt(summary(lm(paste("N_H~",paste(model_sd)),data=sample1))$r.squared)#.18

Rm_4_2 <- sqrt(summary(lm(paste("P.SD~",paste(model_means)),data=sample2))$r.squared) #.09
Rm_5_2 <- sqrt(summary(lm(paste("P.rSD~",paste(model_means)),data=sample2))$r.squared) #.12
Rm_6_2 <- sqrt(summary(lm(paste("N.SD~",paste(model_means)),data=sample2))$r.squared) #.55
Rm_7_2 <- sqrt(summary(lm(paste("N.rSD~",paste(model_means)),data=sample2))$r.squared) #.06
Rm_8_2 <- sqrt(summary(lm(paste("H.SD~",paste(model_means)),data=sample2))$r.squared)#.58
Rm_9_2 <- sqrt(summary(lm(paste("H.rSD~",paste(model_means)),data=sample2))$r.squared)#.27
Rm_10_2 <- sqrt(summary(lm(paste("P.MSSD~",paste(model_means)),data=sample2))$r.squared) #.10
Rm_11_2 <- sqrt(summary(lm(paste("P.rMSSD~",paste(model_means)),data=sample2))$r.squared) #.20
Rm_12_2 <- sqrt(summary(lm(paste("N.MSSD~",paste(model_means)),data=sample2))$r.squared) #.46
Rm_13_2 <- sqrt(summary(lm(paste("N.rMSSD~",paste(model_means)),data=sample2))$r.squared)# .24
Rm_14_2 <- sqrt(summary(lm(paste("P.ALPHA~",paste(model_means)),data=sample2))$r.squared) #.08
Rm_15_2 <- sqrt(summary(lm(paste("N.ALPHA~",paste(model_means)),data=sample2))$r.squared) #.14
Rm_16_2 <- sqrt(summary(lm(paste("H.MSSD~",paste(model_means)),data=sample2))$r.squared)#.50
Rm_17_2 <- sqrt(summary(lm(paste("H.rMSSD~",paste(model_means)),data=sample2))$r.squared)#.41
Rm_18_2 <- sqrt(summary(lm(paste("H.ALPHA~",paste(model_means)),data=sample2))$r.squared)#.12
Rm_19_2 <- sqrt(summary(lm(paste("pP~",paste(model_means)),data=sample2))$r.squared) #.45
Rm_20_2 <- sqrt(summary(lm(paste("nN~",paste(model_means)),data=sample2))$r.squared) #.49
Rm_21_2 <- sqrt(summary(lm(paste("hH~",paste(model_means)),data=sample2))$r.squared)#.50
Rm_22_2 <- sqrt(summary(lm(paste("pN~",paste(model_means)),data=sample2))$r.squared)#.23
Rm_23_2 <- sqrt(summary(lm(paste("pH~",paste(model_means)),data=sample2))$r.squared)#.73
Rm_24_2 <- sqrt(summary(lm(paste("nP~",paste(model_means)),data=sample2))$r.squared)#.49
Rm_25_2 <- sqrt(summary(lm(paste("nH~",paste(model_means)),data=sample2))$r.squared)#.11
Rm_26_2 <- sqrt(summary(lm(paste("hP~",paste(model_means)),data=sample2))$r.squared)#.16
Rm_27_2 <- sqrt(summary(lm(paste("hN~",paste(model_means)),data=sample2))$r.squared)#.37
Rm_28_2 <- sqrt(summary(lm(paste("P_N~",paste(model_means)),data=sample2))$r.squared)#.27
Rm_29_2 <- sqrt(summary(lm(paste("P_H~",paste(model_means)),data=sample2))$r.squared)#.08
Rm_30_2 <- sqrt(summary(lm(paste("N_H~",paste(model_means)),data=sample2))$r.squared)#.18

Rs_10_2 <- sqrt(summary(lm(paste("P.MSSD~",paste(model_sd)),data=sample2))$r.squared) #.10
Rs_11_2 <- sqrt(summary(lm(paste("P.rMSSD~",paste(model_sd)),data=sample2))$r.squared) #.20
Rs_12_2 <- sqrt(summary(lm(paste("N.MSSD~",paste(model_sd)),data=sample2))$r.squared) #.46
Rs_13_2 <- sqrt(summary(lm(paste("N.rMSSD~",paste(model_sd)),data=sample2))$r.squared)# .24
Rs_14_2 <- sqrt(summary(lm(paste("P.ALPHA~",paste(model_sd)),data=sample2))$r.squared) #.08
Rs_15_2 <- sqrt(summary(lm(paste("N.ALPHA~",paste(model_sd)),data=sample2))$r.squared) #.14
Rs_16_2 <- sqrt(summary(lm(paste("H.MSSD~",paste(model_sd)),data=sample2))$r.squared)#.50
Rs_17_2 <- sqrt(summary(lm(paste("H.rMSSD~",paste(model_sd)),data=sample2))$r.squared)#.41
Rs_18_2 <- sqrt(summary(lm(paste("H.ALPHA~",paste(model_sd)),data=sample2))$r.squared)#.12
Rs_19_2 <- sqrt(summary(lm(paste("pP~",paste(model_sd)),data=sample2))$r.squared) #.45
Rs_20_2 <- sqrt(summary(lm(paste("nN~",paste(model_sd)),data=sample2))$r.squared) #.49
Rs_21_2 <- sqrt(summary(lm(paste("hH~",paste(model_sd)),data=sample2))$r.squared)#.50
Rs_22_2 <- sqrt(summary(lm(paste("pN~",paste(model_sd)),data=sample2))$r.squared)#.23
Rs_23_2 <- sqrt(summary(lm(paste("pH~",paste(model_sd)),data=sample2))$r.squared)#.73
Rs_24_2 <- sqrt(summary(lm(paste("nP~",paste(model_sd)),data=sample2))$r.squared)#.49
Rs_25_2 <- sqrt(summary(lm(paste("nH~",paste(model_sd)),data=sample2))$r.squared)#.11
Rs_26_2 <- sqrt(summary(lm(paste("hP~",paste(model_sd)),data=sample2))$r.squared)#.16
Rs_27_2 <- sqrt(summary(lm(paste("hN~",paste(model_sd)),data=sample2))$r.squared)#.37
Rs_28_2 <- sqrt(summary(lm(paste("P_N~",paste(model_sd)),data=sample2))$r.squared)#.27
Rs_29_2 <- sqrt(summary(lm(paste("P_H~",paste(model_sd)),data=sample2))$r.squared)#.08
Rs_30_2 <- sqrt(summary(lm(paste("N_H~",paste(model_sd)),data=sample2))$r.squared)#.18

Rm_4_3 <- sqrt(summary(lm(paste("P.SD~",paste(model_means)),data=sample3))$r.squared) #.09
Rm_5_3 <- sqrt(summary(lm(paste("P.rSD~",paste(model_means)),data=sample3))$r.squared) #.12
Rm_6_3 <- sqrt(summary(lm(paste("N.SD~",paste(model_means)),data=sample3))$r.squared) #.55
Rm_7_3 <- sqrt(summary(lm(paste("N.rSD~",paste(model_means)),data=sample3))$r.squared) #.06
Rm_8_3 <- sqrt(summary(lm(paste("H.SD~",paste(model_means)),data=sample3))$r.squared)#.58
Rm_9_3 <- sqrt(summary(lm(paste("H.rSD~",paste(model_means)),data=sample3))$r.squared)#.27
Rm_10_3 <- sqrt(summary(lm(paste("P.MSSD~",paste(model_means)),data=sample3))$r.squared) #.10
Rm_11_3 <- sqrt(summary(lm(paste("P.rMSSD~",paste(model_means)),data=sample3))$r.squared) #.20
Rm_12_3 <- sqrt(summary(lm(paste("N.MSSD~",paste(model_means)),data=sample3))$r.squared) #.46
Rm_13_3 <- sqrt(summary(lm(paste("N.rMSSD~",paste(model_means)),data=sample3))$r.squared)# .24
Rm_14_3 <- sqrt(summary(lm(paste("P.ALPHA~",paste(model_means)),data=sample3))$r.squared) #.08
Rm_15_3 <- sqrt(summary(lm(paste("N.ALPHA~",paste(model_means)),data=sample3))$r.squared) #.14
Rm_16_3 <- sqrt(summary(lm(paste("H.MSSD~",paste(model_means)),data=sample3))$r.squared)#.50
Rm_17_3 <- sqrt(summary(lm(paste("H.rMSSD~",paste(model_means)),data=sample3))$r.squared)#.41
Rm_18_3 <- sqrt(summary(lm(paste("H.ALPHA~",paste(model_means)),data=sample3))$r.squared)#.12
Rm_19_3 <- sqrt(summary(lm(paste("pP~",paste(model_means)),data=sample3))$r.squared) #.45
Rm_20_3 <- sqrt(summary(lm(paste("nN~",paste(model_means)),data=sample3))$r.squared) #.49
Rm_21_3 <- sqrt(summary(lm(paste("hH~",paste(model_means)),data=sample3))$r.squared)#.50
Rm_22_3 <- sqrt(summary(lm(paste("pN~",paste(model_means)),data=sample3))$r.squared)#.23
Rm_23_3 <- sqrt(summary(lm(paste("pH~",paste(model_means)),data=sample3))$r.squared)#.73
Rm_24_3 <- sqrt(summary(lm(paste("nP~",paste(model_means)),data=sample3))$r.squared)#.49
Rm_25_3 <- sqrt(summary(lm(paste("nH~",paste(model_means)),data=sample3))$r.squared)#.11
Rm_26_3 <- sqrt(summary(lm(paste("hP~",paste(model_means)),data=sample3))$r.squared)#.16
Rm_27_3 <- sqrt(summary(lm(paste("hN~",paste(model_means)),data=sample3))$r.squared)#.37
Rm_28_3 <- sqrt(summary(lm(paste("P_N~",paste(model_means)),data=sample3))$r.squared)#.27
Rm_29_3 <- sqrt(summary(lm(paste("P_H~",paste(model_means)),data=sample3))$r.squared)#.08
Rm_30_3 <- sqrt(summary(lm(paste("N_H~",paste(model_means)),data=sample3))$r.squared)#.18

Rs_10_3 <- sqrt(summary(lm(paste("P.MSSD~",paste(model_sd)),data=sample3))$r.squared) #.10
Rs_11_3 <- sqrt(summary(lm(paste("P.rMSSD~",paste(model_sd)),data=sample3))$r.squared) #.20
Rs_12_3 <- sqrt(summary(lm(paste("N.MSSD~",paste(model_sd)),data=sample3))$r.squared) #.46
Rs_13_3 <- sqrt(summary(lm(paste("N.rMSSD~",paste(model_sd)),data=sample3))$r.squared)# .24
Rs_14_3 <- sqrt(summary(lm(paste("P.ALPHA~",paste(model_sd)),data=sample3))$r.squared) #.08
Rs_15_3 <- sqrt(summary(lm(paste("N.ALPHA~",paste(model_sd)),data=sample3))$r.squared) #.14
Rs_16_3 <- sqrt(summary(lm(paste("H.MSSD~",paste(model_sd)),data=sample3))$r.squared)#.50
Rs_17_3 <- sqrt(summary(lm(paste("H.rMSSD~",paste(model_sd)),data=sample3))$r.squared)#.41
Rs_18_3 <- sqrt(summary(lm(paste("H.ALPHA~",paste(model_sd)),data=sample3))$r.squared)#.12
Rs_19_3 <- sqrt(summary(lm(paste("pP~",paste(model_sd)),data=sample3))$r.squared) #.45
Rs_20_3 <- sqrt(summary(lm(paste("nN~",paste(model_sd)),data=sample3))$r.squared) #.49
Rs_21_3 <- sqrt(summary(lm(paste("hH~",paste(model_sd)),data=sample3))$r.squared)#.50
Rs_22_3 <- sqrt(summary(lm(paste("pN~",paste(model_sd)),data=sample3))$r.squared)#.23
Rs_23_3 <- sqrt(summary(lm(paste("pH~",paste(model_sd)),data=sample3))$r.squared)#.73
Rs_24_3 <- sqrt(summary(lm(paste("nP~",paste(model_sd)),data=sample3))$r.squared)#.49
Rs_25_3 <- sqrt(summary(lm(paste("nH~",paste(model_sd)),data=sample3))$r.squared)#.11
Rs_26_3 <- sqrt(summary(lm(paste("hP~",paste(model_sd)),data=sample3))$r.squared)#.16
Rs_27_3 <- sqrt(summary(lm(paste("hN~",paste(model_sd)),data=sample3))$r.squared)#.37
Rs_28_3 <- sqrt(summary(lm(paste("P_N~",paste(model_sd)),data=sample3))$r.squared)#.27
Rs_29_3 <- sqrt(summary(lm(paste("P_H~",paste(model_sd)),data=sample3))$r.squared)#.08
Rs_30_3 <- sqrt(summary(lm(paste("N_H~",paste(model_sd)),data=sample3))$r.squared)#.18


#### 5) TEST-RETEST-RELIABILITIES ####

# standardize variables and subset PCA indicators
test.pca.z <- as.data.frame(scale(test1[,-1]))
retest.pca.z <- as.data.frame(scale(retest1[,-1]))

# ITEM TEST-RETEST RELIABILITIES
it <- cbind(test.pca.z, retest.pca.z)
for (i in 1:30) {
  colnames(it)[i] <- paste0(colnames(it)[i], "_test")
}
for (i in 31:60) {
  colnames(it)[i] <- paste0(colnames(it)[i], "_retest")
}
it_m <- it
it_sd <- it

## Multiple Correlations with the mean 
model_means_t <- 'P.Mean_test + N.Mean_test + H.Mean_test'
model_sd_t <- 'P.Mean_test + N.Mean_test + H.Mean_test + P.SD_test + N.SD_test + H.SD_test'
model_means_r <- 'P.Mean_retest + N.Mean_retest + H.Mean_retest'
model_sd_r <- 'P.Mean_retest + N.Mean_retest + H.Mean_retest + P.SD_retest + N.SD_retest + H.SD_retest'


resid.three.mean_test <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_means_t)),
                        data=x)) 
  return(x)}

resid.three.mean_rete <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_means_r)),
                        data=x)) 
  return(x)}

resid.three.sd_test <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_sd_t)),
                        data=x)) 
  return(x)}

resid.three.sd_rete <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_sd_r)),
                        data=x)) 
  return(x)}


# residualize data for mean 
for (i in 4:30){                      
  it_m <- resid.three.mean_test(it_m, i)}
for (i in 34:60){
  it_m <- resid.three.mean_rete(it_m, i)}

# residualize data for mean and mean+sd
for (i in c(4:6,10:12)){                      
  it_sd <- resid.three.mean_test(it_sd, i)}
for (i in c(7:9,13:30)){
  it_sd <- resid.three.sd_test(it_sd, i)}
for (i in c(34:36,40:42)){
  it_sd <- resid.three.mean_rete(it_sd, i)}
for (i in c(37:39,43:60)){
  it_sd <- resid.three.sd_rete(it_sd, i)}


## Residual variables test-retest reliabilites

# mea
rt_p_mean <- CorCI(spearman.brown(cor(it_m$P.Mean_test, it_m$P.Mean_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) #.88
rt_n_mean <- CorCI(spearman.brown(cor(it_m$N.Mean_test, it_m$N.Mean_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) #.91
rt_h_mean <- CorCI(spearman.brown(cor(it_m$H.Mean_test, it_m$H.Mean_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) #.86

# sd (redundant with mean)
rt_p_sd <- CorCI(spearman.brown(cor(it$P.SD_test, it$P.SD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 
rt_n_sd <- CorCI(spearman.brown(cor(it$N.SD_test, it$N.SD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 
rt_h_sd <- CorCI(spearman.brown(cor(it$H.SD_test, it$H.SD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 
rt_p_sd_m <- CorCI(spearman.brown(cor(it_m$P.SD_test, it_m$P.SD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 
rt_n_sd_m <- CorCI(spearman.brown(cor(it_m$N.SD_test, it_m$N.SD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 
rt_h_sd_m <- CorCI(spearman.brown(cor(it_m$H.SD_test, it_m$H.SD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 

# rl.sd
rt_p_rsd <- CorCI(spearman.brown(cor(it$P.rSD_test, it$P.rSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_rsd <- CorCI(spearman.brown(cor(it$N.rSD_test, it$N.rSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_rsd <- CorCI(spearman.brown(cor(it$H.rSD_test, it$H.rSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_p_rsd_m <- CorCI(spearman.brown(cor(it_m$P.rSD_test, it_m$P.rSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_rsd_m <- CorCI(spearman.brown(cor(it_m$N.rSD_test, it_m$N.rSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_rsd_m <- CorCI(spearman.brown(cor(it_m$H.rSD_test, it_m$H.rSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)

# mssd (redundant with mean)
rt_p_mssd <- CorCI(spearman.brown(cor(it$P.MSSD_test, it$P.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_mssd <- CorCI(spearman.brown(cor(it$N.MSSD_test, it$N.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_mssd <- CorCI(spearman.brown(cor(it$H.MSSD_test, it$H.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_p_mssd_m <- CorCI(spearman.brown(cor(it_m$P.MSSD_test, it_m$P.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_mssd_m <- CorCI(spearman.brown(cor(it_m$N.MSSD_test, it_m$N.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_mssd_m <- CorCI(spearman.brown(cor(it_m$H.MSSD_test, it_m$H.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_p_mssd_sd <- CorCI(spearman.brown(cor(it_sd$P.MSSD_test, it_sd$P.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_mssd_sd <- CorCI(spearman.brown(cor(it_sd$N.MSSD_test, it_sd$N.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_mssd_sd <- CorCI(spearman.brown(cor(it_sd$H.MSSD_test, it_sd$H.MSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)

# rl.mssd
rt_p_rmssd <- CorCI(spearman.brown(cor(it$P.rMSSD_test, it$P.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_rmssd <- CorCI(spearman.brown(cor(it$N.rMSSD_test, it$N.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_rmssd <- CorCI(spearman.brown(cor(it$H.rMSSD_test, it$H.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_p_rmssd_m <- CorCI(spearman.brown(cor(it_m$P.rMSSD_test, it_m$P.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_rmssd_m <- CorCI(spearman.brown(cor(it_m$N.rMSSD_test, it_m$N.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_rmssd_m <- CorCI(spearman.brown(cor(it_m$H.rMSSD_test, it_m$H.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_p_rmssd_sd <- CorCI(spearman.brown(cor(it_sd$P.rMSSD_test, it_sd$P.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_rmssd_sd <- CorCI(spearman.brown(cor(it_sd$N.rMSSD_test, it_sd$N.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_rmssd_sd <- CorCI(spearman.brown(cor(it_sd$H.rMSSD_test, it_sd$H.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)

# alpha
rt_p_alpha <- CorCI(spearman.brown(cor(it$P.ALPHA_test, it$P.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_alpha <- CorCI(spearman.brown(cor(it$N.ALPHA_test, it$N.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_alpha <- CorCI(spearman.brown(cor(it$H.ALPHA_test, it$H.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 
rt_p_alpha_m <- CorCI(spearman.brown(cor(it_m$P.ALPHA_test, it_m$P.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_alpha_m <- CorCI(spearman.brown(cor(it_m$N.ALPHA_test, it_m$N.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_alpha_m <- CorCI(spearman.brown(cor(it_m$H.ALPHA_test, it_m$H.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 
rt_p_alpha_sd <- CorCI(spearman.brown(cor(it_sd$P.ALPHA_test, it_sd$P.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_n_alpha_sd <- CorCI(spearman.brown(cor(it_sd$N.ALPHA_test, it_sd$N.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_h_alpha_sd <- CorCI(spearman.brown(cor(it_sd$H.ALPHA_test, it_sd$H.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95) 

# autoregressions
rt_pP <- CorCI(spearman.brown(cor(it$pP_test, it$pP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nN <- CorCI(spearman.brown(cor(it$nN_test, it$nN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hH <- CorCI(spearman.brown(cor(it$hH_test, it$hH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_pP_m <- CorCI(spearman.brown(cor(it_m$pP_test, it_m$pP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nN_m <- CorCI(spearman.brown(cor(it_m$nN_test, it_m$nN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hH_m <- CorCI(spearman.brown(cor(it_m$hH_test, it_m$hH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_pP_sd <- CorCI(spearman.brown(cor(it_sd$pP_test, it_sd$pP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nN_sd <- CorCI(spearman.brown(cor(it_sd$nN_test, it_sd$nN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hH_sd <- CorCI(spearman.brown(cor(it_sd$hH_test, it_sd$hH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)

# contemporaneous correlation
rt_N_H <- CorCI(spearman.brown(cor(it$N_H_test, it$N_H_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_P_H <- CorCI(spearman.brown(cor(it$P_H_test, it$P_H_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_P_N <- CorCI(spearman.brown(cor(it$P_N_test, it$P_N_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_N_H_m <- CorCI(spearman.brown(cor(it_m$N_H_test, it_m$N_H_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_P_H_m <- CorCI(spearman.brown(cor(it_m$P_H_test, it_m$P_H_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_P_N_m <- CorCI(spearman.brown(cor(it_m$P_N_test, it_m$P_N_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_N_H_sd <- CorCI(spearman.brown(cor(it_sd$N_H_test, it_sd$N_H_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_P_H_sd <- CorCI(spearman.brown(cor(it_sd$P_H_test, it_sd$P_H_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_P_N_sd <- CorCI(spearman.brown(cor(it_sd$P_N_test, it_sd$P_N_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)


# crosslagged regressions
rt_pN <-CorCI(spearman.brown(cor(it$pN_test, it$pN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_pH <-CorCI(spearman.brown(cor(it$pH_test, it$pH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nP <-CorCI(spearman.brown(cor(it$nP_test, it$nP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nH <-CorCI(spearman.brown(cor(it$nH_test, it$nH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hP <-CorCI(spearman.brown(cor(it$hP_test, it$hP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hN <-CorCI(spearman.brown(cor(it$hN_test, it$hN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_pN_m <-CorCI(spearman.brown(cor(it_m$pN_test, it_m$pN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_pH_m <-CorCI(spearman.brown(cor(it_m$pH_test, it_m$pH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nP_m <-CorCI(spearman.brown(cor(it_m$nP_test, it_m$nP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nH_m <-CorCI(spearman.brown(cor(it_m$nH_test, it_m$nH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hP_m <-CorCI(spearman.brown(cor(it_m$hP_test, it_m$hP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hN_m <-CorCI(spearman.brown(cor(it_m$hN_test, it_m$hN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_pN_sd <-CorCI(spearman.brown(cor(it_sd$pN_test, it_sd$pN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_pH_sd <-CorCI(spearman.brown(cor(it_sd$pH_test, it_sd$pH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nP_sd <-CorCI(spearman.brown(cor(it_sd$nP_test, it_sd$nP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_nH_sd <-CorCI(spearman.brown(cor(it_sd$nH_test, it_sd$nH_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hP_sd <-CorCI(spearman.brown(cor(it_sd$hP_test, it_sd$hP_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)
rt_hN_sd <-CorCI(spearman.brown(cor(it_sd$hN_test, it_sd$hN_retest, method = "spearman"),2,"n")$r.new, n=100, conf.level = 0.95)



rt_p_mean_beta <-rt_p_mean["cor"]
rt_p_mean_lb <-rt_p_mean["lwr.ci"]
rt_p_mean_ub <-rt_p_mean["upr.ci"]
rt_n_mean_beta <-rt_n_mean["cor"]
rt_n_mean_lb <-rt_n_mean["lwr.ci"]
rt_n_mean_ub <-rt_n_mean["upr.ci"]
rt_h_mean_beta <-rt_h_mean["cor"]
rt_h_mean_lb <-rt_h_mean["lwr.ci"]
rt_h_mean_ub <-rt_h_mean["upr.ci"]

rt_p_sd_beta <-rt_p_sd_m["cor"]
rt_p_sd_lb <-rt_p_sd_m["lwr.ci"]
rt_p_sd_ub <-rt_p_sd_m["upr.ci"]
rt_n_sd_beta <-rt_n_sd_m["cor"]
rt_n_sd_lb <-rt_n_sd_m["lwr.ci"]
rt_n_sd_ub <-rt_n_sd_m["upr.ci"]
rt_h_sd_beta <-rt_h_sd_m["cor"]
rt_h_sd_lb <-rt_h_sd_m["lwr.ci"]
rt_h_sd_ub <-rt_h_sd_m["upr.ci"]

rt_p_rsd_beta <-rt_p_rsd_m["cor"]
rt_p_rsd_lb <-rt_p_rsd_m["lwr.ci"]
rt_p_rsd_ub <-rt_p_rsd_m["upr.ci"]
rt_n_rsd_beta <-rt_n_rsd_m["cor"]
rt_n_rsd_lb <-rt_n_rsd_m["lwr.ci"]
rt_n_rsd_ub <-rt_n_rsd_m["upr.ci"]
rt_h_rsd_beta <-rt_h_rsd_m["cor"]
rt_h_rsd_lb <-rt_h_rsd_m["lwr.ci"]
rt_h_rsd_ub <-rt_h_rsd_m["upr.ci"]

rt_p_mssd_beta <-rt_p_mssd_sd["cor"]
rt_p_mssd_lb <-rt_p_mssd_sd["lwr.ci"]
rt_p_mssd_ub <-rt_p_mssd_sd["upr.ci"]
rt_n_mssd_beta <-rt_n_mssd_sd["cor"]
rt_n_mssd_lb <-rt_n_mssd_sd["lwr.ci"]
rt_n_mssd_ub <-rt_n_mssd_sd["upr.ci"]
rt_h_mssd_beta <-rt_h_mssd_sd["cor"]
rt_h_mssd_lb <-rt_h_mssd_sd["lwr.ci"]
rt_h_mssd_ub <-rt_h_mssd_sd["upr.ci"]

rt_p_rmssd_beta <-rt_p_rmssd_sd["cor"]
rt_p_rmssd_lb <-rt_p_rmssd_sd["lwr.ci"]
rt_p_rmssd_ub <-rt_p_rmssd_sd["upr.ci"]
rt_n_rmssd_beta <-rt_n_rmssd_sd["cor"]
rt_n_rmssd_lb <-rt_n_rmssd_sd["lwr.ci"]
rt_n_rmssd_ub <-rt_n_rmssd_sd["upr.ci"]
rt_h_rmssd_beta <-rt_h_rmssd_sd["cor"]
rt_h_rmssd_lb <-rt_h_rmssd_sd["lwr.ci"]
rt_h_rmssd_ub <-rt_h_rmssd_sd["upr.ci"]

rt_p_alpha_beta <-rt_p_alpha_sd["cor"]
rt_p_alpha_lb <-rt_p_alpha_sd["lwr.ci"]
rt_p_alpha_ub <-rt_p_alpha_sd["upr.ci"]
rt_n_alpha_beta <-rt_n_alpha_sd["cor"]
rt_n_alpha_lb <-rt_n_alpha_sd["lwr.ci"]
rt_n_alpha_ub <-rt_n_alpha_sd["upr.ci"]
rt_h_alpha_beta <-rt_h_alpha_sd["cor"]
rt_h_alpha_lb <-rt_h_alpha_sd["lwr.ci"]
rt_h_alpha_ub <-rt_h_alpha_sd["upr.ci"]

rt_pP_beta <-rt_pP_sd["cor"]
rt_pP_lb <-rt_pP_sd["lwr.ci"]
rt_pP_ub <-rt_pP_sd["upr.ci"]
rt_nN_beta <-rt_nN_sd["cor"]
rt_nN_lb <-rt_nN_sd["lwr.ci"]
rt_nN_ub <-rt_nN_sd["upr.ci"]
rt_hH_beta <-rt_hH_sd["cor"]
rt_hH_lb <-rt_hH_sd["lwr.ci"]
rt_hH_ub <-rt_hH_sd["upr.ci"]

rt_pN_beta <-rt_pN_sd["cor"]
rt_pN_lb <-rt_pN_sd["lwr.ci"]
rt_pN_ub <-rt_pN_sd["upr.ci"]
rt_pH_beta <-rt_pH_sd["cor"]
rt_pH_lb <-rt_pH_sd["lwr.ci"]
rt_pH_ub <-rt_pH_sd["upr.ci"]
rt_nP_beta <-rt_nP_sd["cor"]
rt_nP_lb <-rt_nP_sd["lwr.ci"]
rt_nP_ub <-rt_nP_sd["upr.ci"]
rt_nH_beta <-rt_nH_sd["cor"]
rt_nH_lb <-rt_nH_sd["lwr.ci"]
rt_nH_ub <-rt_nH_sd["upr.ci"]
rt_hP_beta <-rt_hP_sd["cor"]
rt_hP_lb <-rt_hP_sd["lwr.ci"]
rt_hP_ub <-rt_hP_sd["upr.ci"]
rt_hN_beta <-rt_hN_sd["cor"]
rt_hN_lb <-rt_hN_sd["lwr.ci"]
rt_hN_ub <-rt_hN_sd["upr.ci"]

rt_P_N_beta <-rt_P_N_sd["cor"]
rt_P_N_lb <-rt_P_N_sd["lwr.ci"]
rt_P_N_ub <-rt_P_N_sd["upr.ci"]
rt_P_H_beta <-rt_P_H_sd["cor"]
rt_P_H_lb <-rt_P_H_sd["lwr.ci"]
rt_P_H_ub <-rt_P_H_sd["upr.ci"]
rt_N_H_beta <-rt_N_H_sd["cor"]
rt_N_H_lb <-rt_N_H_sd["lwr.ci"]
rt_N_H_ub <- rt_N_H_sd["upr.ci"]

table_trt_1 <- paste0(format(rd(round(rt_p_mean_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_p_mean_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_p_mean_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_2 <- paste0(format(rd(round(rt_n_mean_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_n_mean_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_n_mean_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_3 <- paste0(format(rd(round(rt_h_mean_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_h_mean_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_h_mean_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_4 <- paste0(format(rd(round(rt_p_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_p_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_p_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_5 <- paste0(format(rd(round(rt_n_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_n_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_n_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_6 <- paste0(format(rd(round(rt_h_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_h_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_h_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_7 <- paste0(format(rd(round(rt_p_rsd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_p_rsd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_p_rsd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_8 <- paste0(format(rd(round(rt_n_rsd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_n_rsd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_n_rsd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_9 <- paste0(format(rd(round(rt_h_rsd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_h_rsd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_h_rsd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_10 <- paste0(format(rd(round(rt_p_mssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_p_mssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_p_mssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_11 <- paste0(format(rd(round(rt_n_mssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_n_mssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_n_mssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_12 <- paste0(format(rd(round(rt_h_mssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_h_mssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_h_mssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_13 <- paste0(format(rd(round(rt_p_rmssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_p_rmssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_p_rmssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_14 <- paste0(format(rd(round(rt_n_rmssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_n_rmssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_n_rmssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_15 <- paste0(format(rd(round(rt_h_rmssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_h_rmssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_h_rmssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_16 <- paste0(format(rd(round(rt_p_alpha_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_p_alpha_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_p_alpha_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_17 <- paste0(format(rd(round(rt_n_alpha_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_n_alpha_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_n_alpha_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_18 <- paste0(format(rd(round(rt_h_alpha_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_h_alpha_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_h_alpha_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_19 <- paste0(format(rd(round(rt_pP_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_pP_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_pP_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_20 <- paste0(format(rd(round(rt_nN_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_nN_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_nN_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_21 <- paste0(format(rd(round(rt_hH_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_hH_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_hH_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_22 <- paste0(format(rd(round(rt_pN_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_pN_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_pN_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_23 <- paste0(format(rd(round(rt_pH_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_pH_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_pH_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_24 <- paste0(format(rd(round(rt_nP_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_nP_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_nP_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_25 <- paste0(format(rd(round(rt_nH_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_nH_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_nH_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_26 <- paste0(format(rd(round(rt_hP_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_hP_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_hP_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_27 <- paste0(format(rd(round(rt_hN_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_hN_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_hN_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_28 <- paste0(format(rd(round(rt_P_N_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_P_N_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_P_N_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_29 <- paste0(format(rd(round(rt_P_H_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_P_H_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_P_H_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt_30 <- paste0(format(rd(round(rt_N_H_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt_N_H_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt_N_H_ub,2), add = FALSE, digits = 2),nsmall=2),"]")

tsize <- 3
er_height <- 0.5
label <- c("xNH", "xPH", "xPN", "xHN", "xHP", "xNH", "xNP", 
           "xPH", "xPN", "xHH", "xNN", "xPP", 
           "HA a", "NA a", "PA a","HA cMSSD", "NA cMSSD", "PA cMSSD",
           "HA MSSD", "NA MSSD", "PA MSSD", "HA cSD", "NA cSD", "PA cSD", "HA SD", "NA SD", "PA SD",
           "HA mean", "NA Mean", "PA Mean", NA)
mean_o  <- c(rt_N_H_beta, rt_P_H_beta, rt_P_N_beta, rt_hN_beta, rt_hP_beta, 
             rt_nH_beta, rt_nP_beta, rt_pH_beta, rt_pN_beta, rt_hH_beta, rt_nN_beta, rt_pP_beta, 
             rt_h_alpha_beta, rt_n_alpha_beta, rt_p_alpha_beta, rt_h_rmssd_beta, rt_n_rmssd_beta, rt_p_rmssd_beta,
             rt_h_mssd_beta, rt_n_mssd_beta, rt_p_mssd_beta, rt_h_rsd_beta, rt_n_rsd_beta, rt_p_rsd_beta, rt_h_sd_beta, rt_n_sd_beta, rt_p_sd_beta,
             rt_h_mean_beta, rt_n_mean_beta, rt_p_mean_beta, NA) 
lower_o <- c(rt_N_H_lb, rt_P_H_lb, rt_P_N_lb, rt_hN_lb, rt_hP_lb, 
             rt_nH_lb, rt_nP_lb, rt_pH_lb, rt_pN_lb, rt_hH_lb, rt_nN_lb, rt_pP_lb, 
             rt_h_alpha_lb, rt_n_alpha_lb, rt_p_alpha_lb, rt_h_rmssd_lb, rt_n_rmssd_lb, rt_p_rmssd_lb,
             rt_h_mssd_lb, rt_n_mssd_lb, rt_p_mssd_lb, rt_h_rsd_lb, rt_n_rsd_lb, rt_p_rsd_lb, rt_h_sd_lb, rt_n_sd_lb, rt_p_sd_lb,
             rt_h_mean_lb, rt_n_mean_lb, rt_p_mean_lb, NA)
upper_o <- c(rt_N_H_ub, rt_P_H_ub, rt_P_N_ub, rt_hN_ub, rt_hP_ub, 
             rt_nH_ub, rt_nP_ub, rt_pH_ub, rt_pN_ub, rt_hH_ub, rt_nN_ub, rt_pP_ub, 
             rt_h_alpha_ub, rt_n_alpha_ub, rt_p_alpha_ub, rt_h_rmssd_ub, rt_n_rmssd_ub, rt_p_rmssd_ub,
             rt_h_mssd_ub, rt_n_mssd_ub, rt_p_mssd_ub, rt_h_rsd_ub, rt_n_rsd_ub, rt_p_rsd_ub, rt_h_sd_ub, rt_n_sd_ub, rt_p_sd_ub,
             rt_h_mean_ub, rt_n_mean_ub, rt_p_mean_ub, NA)
df_o <- data.frame(label, mean_o, lower_o, upper_o)
#df_o$label <- factor(df_o$label, levels=rev(df_o$label))

dat_o <- data.frame(group = factor(c("1","2","3","4","5","6","7","8","9","10",
                                     "11","12","13","14","15","16","17","18","19","20",
                                     "21","22","23","24","25","26","27","28","29","30", "31"), 
                                   levels=c("1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30", "31")),
                    cen = mean_o,
                    low = lower_o,
                    high = upper_o,
                    color = factor(c(1,1,1,0,1,0,0,0,0,1,
                                     1,1,0,1,1,1,1,1,1,1,
                                     1,1,1,1,1,1,1,1,1,1,
                                     1)))

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  panel.background = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

trt <- ggplot(dat_o,aes(mean_o,group, colour = color)) + 
  geom_point(size=tsize, shape=16) +
  #  scale_y_discrete(labels = label)+
  theme(legend.position = "none") +
  geom_errorbarh(aes(xmax = upper_o, xmin = lower_o), height = er_height) +
  coord_cartesian(xlim = c(-.25,1))+
  scale_color_manual(values = c("grey","black")) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks=c(-.25,0,.25,.5,.75,1), labels = c(-.25,0,.25,.5,.75,1)) +
  labs(x="test-retest r", y="")


lab_trt <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13","14","15","16","17","18","19","20",
                                    "21","22","23","24","25","26","27","28","29","30","31",
                                    "1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13","14","15","16","17","18","19","20",
                                    "21","22","23","24","25","26","27","28","29","30","31",
                                    "1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13","14","15","16","17","18","19","20",
                                    "21","22","23","24","25","26","27","28","29","30","31")
                                  , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                             "20","19","18","17","16","15","14","13","12","11",
                                             "10","9","8","7","6","5","4","3","2","1")),
                      V05 = rep(c(1,2,3),each=31),
                      V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                             "","","","","","","","","","","",
                             "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                             "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA A","NA A","HA A","xPP","xNN", "xHH",
                             "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH",    
                             "r [95% CI]",table_trt_1,table_trt_2,table_trt_3,table_trt_4,table_trt_5,table_trt_6,table_trt_7,table_trt_8,table_trt_9,table_trt_10,
                             table_trt_11,table_trt_12,table_trt_13,table_trt_14,table_trt_15,table_trt_16,table_trt_17,table_trt_18,table_trt_19,table_trt_20,
                             table_trt_21,table_trt_22,table_trt_23,table_trt_24,table_trt_25,table_trt_26,table_trt_27,table_trt_28,table_trt_29,table_trt_30)
)



data_table_trt <- ggplot(lab_trt, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept=c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))
grid.arrange(data_table_trt, trt, ncol =2) # 12,6


####### TEST RETEST SAMPLE 3 (10 days vs. 10 days)

# standardize variables and subset PCA indicators
test.pca.z <- as.data.frame(test2[,-1])
retest.pca.z <- as.data.frame(retest2[,-1])

# ITEM TEST-RETEST RELIABILITIES
it <- cbind(test.pca.z, retest.pca.z)
for (i in 1:30) {
  colnames(it)[i] <- paste0(colnames(it)[i], "_test")
}
for (i in 31:60) {
  colnames(it)[i] <- paste0(colnames(it)[i], "_retest")
}
it_m <- it
it_sd <- it

## Multiple Correlations with the mean 
model_means_t <- 'P.Mean_test + N.Mean_test + H.Mean_test'
model_sd_t <- 'P.Mean_test + N.Mean_test + H.Mean_test + P.SD_test + N.SD_test + H.SD_test'
model_means_r <- 'P.Mean_retest + N.Mean_retest + H.Mean_retest'
model_sd_r <- 'P.Mean_retest + N.Mean_retest + H.Mean_retest + P.SD_retest + N.SD_retest + H.SD_retest'


resid.three.mean_test <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_means_t)),
                        data=x)) 
  return(x)}

resid.three.mean_rete <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_means_r)),
                        data=x)) 
  return(x)}

resid.three.sd_test <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_sd_t)),
                        data=x)) 
  return(x)}

resid.three.sd_rete <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_sd_r)),
                        data=x)) 
  return(x)}


# residualize data for mean 
for (i in 4:30){                      
  it_m <- resid.three.mean_test(it_m, i)}
for (i in 34:60){
  it_m <- resid.three.mean_rete(it_m, i)}

# residualize data for mean and mean+sd
for (i in c(4:6,10:12)){                      
  it_sd <- resid.three.mean_test(it_sd, i)}
for (i in c(7:9,13:30)){
  it_sd <- resid.three.sd_test(it_sd, i)}
for (i in c(34:36,40:42)){
  it_sd <- resid.three.mean_rete(it_sd, i)}
for (i in c(37:39,43:60)){
  it_sd <- resid.three.sd_rete(it_sd, i)}


## Residual variables test-retest reliabilites

# mean
rt2_p_mean <- CorCI(spearman.brown(cor(it_m$P.Mean_test, it_m$P.Mean_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) #.88
rt2_n_mean <- CorCI(spearman.brown(cor(it_m$N.Mean_test, it_m$N.Mean_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) #.91
rt2_h_mean <- CorCI(spearman.brown(cor(it_m$H.Mean_test, it_m$H.Mean_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) #.86


# sd (redundant with mean)
rt2_p_sd <- CorCI(spearman.brown(cor(it$P.SD_test, it$P.SD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 
rt2_n_sd <- CorCI(spearman.brown(cor(it$N.SD_test, it$N.SD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 
rt2_h_sd <- CorCI(spearman.brown(cor(it$H.SD_test, it$H.SD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 
rt2_p_sd_m <- CorCI(spearman.brown(cor(it_m$P.SD_test, it_m$P.SD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 
rt2_n_sd_m <- CorCI(spearman.brown(cor(it_m$N.SD_test, it_m$N.SD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 
rt2_h_sd_m <- CorCI(spearman.brown(cor(it_m$H.SD_test, it_m$H.SD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 

# rl.sd
rt2_p_rsd <- CorCI(spearman.brown(cor(it$P.rSD_test, it$P.rSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_rsd <- CorCI(spearman.brown(cor(it$N.rSD_test, it$N.rSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_rsd <- CorCI(spearman.brown(cor(it$H.rSD_test, it$H.rSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_p_rsd_m <- CorCI(spearman.brown(cor(it_m$P.rSD_test, it_m$P.rSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_rsd_m <- CorCI(spearman.brown(cor(it_m$N.rSD_test, it_m$N.rSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_rsd_m <- CorCI(spearman.brown(cor(it_m$H.rSD_test, it_m$H.rSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)

# mssd (redundant with mean)
rt2_p_mssd <- CorCI(spearman.brown(cor(it$P.MSSD_test, it$P.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_mssd <- CorCI(spearman.brown(cor(it$N.MSSD_test, it$N.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_mssd <- CorCI(spearman.brown(cor(it$H.MSSD_test, it$H.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_p_mssd_m <- CorCI(spearman.brown(cor(it_m$P.MSSD_test, it_m$P.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_mssd_m <- CorCI(spearman.brown(cor(it_m$N.MSSD_test, it_m$N.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_mssd_m <- CorCI(spearman.brown(cor(it_m$H.MSSD_test, it_m$H.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_p_mssd_sd <- CorCI(spearman.brown(cor(it_sd$P.MSSD_test, it_sd$P.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_mssd_sd <- CorCI(spearman.brown(cor(it_sd$N.MSSD_test, it_sd$N.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_mssd_sd <- CorCI(spearman.brown(cor(it_sd$H.MSSD_test, it_sd$H.MSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)

# rl.mssd
rt2_p_rmssd <- CorCI(spearman.brown(cor(it$P.rMSSD_test, it$P.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_rmssd <- CorCI(spearman.brown(cor(it$N.rMSSD_test, it$N.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_rmssd <- CorCI(spearman.brown(cor(it$H.rMSSD_test, it$H.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_p_rmssd_m <- CorCI(spearman.brown(cor(it_m$P.rMSSD_test, it_m$P.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_rmssd_m <- CorCI(spearman.brown(cor(it_m$N.rMSSD_test, it_m$N.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_rmssd_m <- CorCI(spearman.brown(cor(it_m$H.rMSSD_test, it_m$H.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_p_rmssd_sd <- CorCI(spearman.brown(cor(it_sd$P.rMSSD_test, it_sd$P.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_rmssd_sd <- CorCI(spearman.brown(cor(it_sd$N.rMSSD_test, it_sd$N.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_rmssd_sd <- CorCI(spearman.brown(cor(it_sd$H.rMSSD_test, it_sd$H.rMSSD_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)

# alpha
rt2_p_alpha <- CorCI(spearman.brown(cor(it$P.ALPHA_test, it$P.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_alpha <- CorCI(spearman.brown(cor(it$N.ALPHA_test, it$N.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_alpha <- CorCI(spearman.brown(cor(it$H.ALPHA_test, it$H.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 
rt2_p_alpha_m <- CorCI(spearman.brown(cor(it_m$P.ALPHA_test, it_m$P.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_alpha_m <- CorCI(spearman.brown(cor(it_m$N.ALPHA_test, it_m$N.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_alpha_m <- CorCI(spearman.brown(cor(it_m$H.ALPHA_test, it_m$H.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 
rt2_p_alpha_sd <- CorCI(spearman.brown(cor(it_sd$P.ALPHA_test, it_sd$P.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_n_alpha_sd <- CorCI(spearman.brown(cor(it_sd$N.ALPHA_test, it_sd$N.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_h_alpha_sd <- CorCI(spearman.brown(cor(it_sd$H.ALPHA_test, it_sd$H.ALPHA_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95) 

# autoregressions
rt2_pP <- CorCI(spearman.brown(cor(it$pP_test, it$pP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nN <- CorCI(spearman.brown(cor(it$nN_test, it$nN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hH <- CorCI(spearman.brown(cor(it$hH_test, it$hH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_pP_m <- CorCI(spearman.brown(cor(it_m$pP_test, it_m$pP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nN_m <- CorCI(spearman.brown(cor(it_m$nN_test, it_m$nN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hH_m <- CorCI(spearman.brown(cor(it_m$hH_test, it_m$hH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_pP_sd <- CorCI(spearman.brown(cor(it_sd$pP_test, it_sd$pP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nN_sd <- CorCI(spearman.brown(cor(it_sd$nN_test, it_sd$nN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hH_sd <- CorCI(spearman.brown(cor(it_sd$hH_test, it_sd$hH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)

# contemporaneous correlation
rt2_N_H <- CorCI(spearman.brown(cor(it$N_H_test, it$N_H_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_P_H <- CorCI(spearman.brown(cor(it$P_H_test, it$P_H_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_P_N <- CorCI(spearman.brown(cor(it$P_N_test, it$P_N_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_N_H_m <- CorCI(spearman.brown(cor(it_m$N_H_test, it_m$N_H_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_P_H_m <- CorCI(spearman.brown(cor(it_m$P_H_test, it_m$P_H_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_P_N_m <- CorCI(spearman.brown(cor(it_m$P_N_test, it_m$P_N_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_N_H_sd <- CorCI(spearman.brown(cor(it_sd$N_H_test, it_sd$N_H_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_P_H_sd <- CorCI(spearman.brown(cor(it_sd$P_H_test, it_sd$P_H_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_P_N_sd <- CorCI(spearman.brown(cor(it_sd$P_N_test, it_sd$P_N_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)


# crosslagged regressions
rt2_pN <- CorCI(spearman.brown(cor(it$pN_test, it$pN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_pH <-CorCI(spearman.brown(cor(it$pH_test, it$pH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nP <-CorCI(spearman.brown(cor(it$nP_test, it$nP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nH <-CorCI(spearman.brown(cor(it$nH_test, it$nH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hP <-CorCI(spearman.brown(cor(it$hP_test, it$hP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hN <-CorCI(spearman.brown(cor(it$hN_test, it$hN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_pN_m <-CorCI(spearman.brown(cor(it_m$pN_test, it_m$pN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_pH_m <-CorCI(spearman.brown(cor(it_m$pH_test, it_m$pH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nP_m <-CorCI(spearman.brown(cor(it_m$nP_test, it_m$nP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nH_m <-CorCI(spearman.brown(cor(it_m$nH_test, it_m$nH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hP_m <-CorCI(spearman.brown(cor(it_m$hP_test, it_m$hP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hN_m <-CorCI(spearman.brown(cor(it_m$hN_test, it_m$hN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_pN_sd <-CorCI(spearman.brown(cor(it_sd$pN_test, it_sd$pN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_pH_sd <-CorCI(spearman.brown(cor(it_sd$pH_test, it_sd$pH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nP_sd <-CorCI(spearman.brown(cor(it_sd$nP_test, it_sd$nP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_nH_sd <-CorCI(spearman.brown(cor(it_sd$nH_test, it_sd$nH_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hP_sd <-CorCI(spearman.brown(cor(it_sd$hP_test, it_sd$hP_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)
rt2_hN_sd <-CorCI(spearman.brown(cor(it_sd$hN_test, it_sd$hN_retest, method = "spearman"),2,"n")$r.new, n=143, conf.level = 0.95)


rt2_p_mean_beta <- rt2_p_mean["cor"]
rt2_p_mean_lb <- rt2_p_mean["lwr.ci"]
rt2_p_mean_ub <- rt2_p_mean["upr.ci"]
rt2_n_mean_beta <- rt2_n_mean["cor"]
rt2_n_mean_lb <- rt2_n_mean["lwr.ci"]
rt2_n_mean_ub <- rt2_n_mean["upr.ci"]
rt2_h_mean_beta <- rt2_h_mean["cor"]
rt2_h_mean_lb <- rt2_h_mean["lwr.ci"]
rt2_h_mean_ub <- rt2_h_mean["upr.ci"]

rt2_p_sd_beta <- rt2_p_sd_m["cor"]
rt2_p_sd_lb <- rt2_p_sd_m["lwr.ci"]
rt2_p_sd_ub <- rt2_p_sd_m["upr.ci"]
rt2_n_sd_beta <- rt2_n_sd_m["cor"]
rt2_n_sd_lb <- rt2_n_sd_m["lwr.ci"]
rt2_n_sd_ub <- rt2_n_sd_m["upr.ci"]
rt2_h_sd_beta <- rt2_h_sd_m["cor"]
rt2_h_sd_lb <- rt2_h_sd_m["lwr.ci"]
rt2_h_sd_ub <- rt2_h_sd_m["upr.ci"]

rt2_p_rsd_beta <- rt2_p_rsd_m["cor"]
rt2_p_rsd_lb <- rt2_p_rsd_m["lwr.ci"]
rt2_p_rsd_ub <- rt2_p_rsd_m["upr.ci"]
rt2_n_rsd_beta <- rt2_n_rsd_m["cor"]
rt2_n_rsd_lb <- rt2_n_rsd_m["lwr.ci"]
rt2_n_rsd_ub <- rt2_n_rsd_m["upr.ci"]
rt2_h_rsd_beta <- rt2_h_rsd_m["cor"]
rt2_h_rsd_lb <- rt2_h_rsd_m["lwr.ci"]
rt2_h_rsd_ub <- rt2_h_rsd_m["upr.ci"]

rt2_p_mssd_beta <- rt2_p_mssd_sd["cor"]
rt2_p_mssd_lb <- rt2_p_mssd_sd["lwr.ci"]
rt2_p_mssd_ub <- rt2_p_mssd_sd["upr.ci"]
rt2_n_mssd_beta <- rt2_n_mssd_sd["cor"]
rt2_n_mssd_lb <- rt2_n_mssd_sd["lwr.ci"]
rt2_n_mssd_ub <- rt2_n_mssd_sd["upr.ci"]
rt2_h_mssd_beta <- rt2_h_mssd_sd["cor"]
rt2_h_mssd_lb <- rt2_h_mssd_sd["lwr.ci"]
rt2_h_mssd_ub <- rt2_h_mssd_sd["upr.ci"]

rt2_p_rmssd_beta <- rt2_p_rmssd_sd["cor"]
rt2_p_rmssd_lb <- rt2_p_rmssd_sd["lwr.ci"]
rt2_p_rmssd_ub <- rt2_p_rmssd_sd["upr.ci"]
rt2_n_rmssd_beta <- rt2_n_rmssd_sd["cor"]
rt2_n_rmssd_lb <- rt2_n_rmssd_sd["lwr.ci"]
rt2_n_rmssd_ub <- rt2_n_rmssd_sd["upr.ci"]
rt2_h_rmssd_beta <- rt2_h_rmssd_sd["cor"]
rt2_h_rmssd_lb <- rt2_h_rmssd_sd["lwr.ci"]
rt2_h_rmssd_ub <- rt2_h_rmssd_sd["upr.ci"]

rt2_p_alpha_beta <- rt2_p_alpha_sd["cor"]
rt2_p_alpha_lb <- rt2_p_alpha_sd["lwr.ci"]
rt2_p_alpha_ub <- rt2_p_alpha_sd["upr.ci"]
rt2_n_alpha_beta <- rt2_n_alpha_sd["cor"]
rt2_n_alpha_lb <- rt2_n_alpha_sd["lwr.ci"]
rt2_n_alpha_ub <- rt2_n_alpha_sd["upr.ci"]
rt2_h_alpha_beta <- rt2_h_alpha_sd["cor"]
rt2_h_alpha_lb <- rt2_h_alpha_sd["lwr.ci"]
rt2_h_alpha_ub <- rt2_h_alpha_sd["upr.ci"]

rt2_pP_beta <- rt2_pP_sd["cor"]
rt2_pP_lb <- rt2_pP_sd["lwr.ci"]
rt2_pP_ub <- rt2_pP_sd["upr.ci"]
rt2_nN_beta <- rt2_nN_sd["cor"]
rt2_nN_lb <- rt2_nN_sd["lwr.ci"]
rt2_nN_ub <- rt2_nN_sd["upr.ci"]
rt2_hH_beta <- rt2_hH_sd["cor"]
rt2_hH_lb <- rt2_hH_sd["lwr.ci"]
rt2_hH_ub <- rt2_hH_sd["upr.ci"]

rt2_pN_beta <- rt2_pN_sd["cor"]
rt2_pN_lb <- rt2_pN_sd["lwr.ci"]
rt2_pN_ub <- rt2_pN_sd["upr.ci"]
rt2_pH_beta <- rt2_pH_sd["cor"]
rt2_pH_lb <- rt2_pH_sd["lwr.ci"]
rt2_pH_ub <- rt2_pH_sd["upr.ci"]
rt2_nP_beta <- rt2_nP_sd["cor"]
rt2_nP_lb <- rt2_nP_sd["lwr.ci"]
rt2_nP_ub <- rt2_nP_sd["upr.ci"]
rt2_nH_beta <- rt2_nH_sd["cor"]
rt2_nH_lb <- rt2_nH_sd["lwr.ci"]
rt2_nH_ub <- rt2_nH_sd["upr.ci"]
rt2_hP_beta <- rt2_hP["cor"]
rt2_hP_lb <- rt2_hP["lwr.ci"]
rt2_hP_ub <- rt2_hP["upr.ci"]
rt2_hN_beta <- rt2_hN_sd["cor"]
rt2_hN_lb <- rt2_hN_sd["lwr.ci"]
rt2_hN_ub <- rt2_hN_sd["upr.ci"]

rt2_P_N_beta <- rt2_P_N_sd["cor"]
rt2_P_N_lb <- rt2_P_N_sd["lwr.ci"]
rt2_P_N_ub <- rt2_P_N_sd["upr.ci"]
rt2_P_H_beta <- rt2_P_H_sd["cor"]
rt2_P_H_lb <- rt2_P_H_sd["lwr.ci"]
rt2_P_H_ub <- rt2_P_H_sd["upr.ci"]
rt2_N_H_beta <- rt2_N_H_sd["cor"]
rt2_N_H_lb <- rt2_N_H_sd["lwr.ci"]
rt2_N_H_ub <- rt2_N_H_sd["upr.ci"]

table_trt2_1 <- paste0(format(rd(round(rt2_p_mean_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_p_mean_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_p_mean_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_2 <- paste0(format(rd(round(rt2_n_mean_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_n_mean_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_n_mean_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_3 <- paste0(format(rd(round(rt2_h_mean_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_h_mean_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_h_mean_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_4 <- paste0(format(rd(round(rt2_p_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_p_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_p_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_5 <- paste0(format(rd(round(rt2_n_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_n_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_n_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_6 <- paste0(format(rd(round(rt2_h_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_h_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_h_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_7 <- paste0(format(rd(round(rt2_p_rsd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_p_rsd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_p_rsd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_8 <- paste0(format(rd(round(rt2_n_rsd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_n_rsd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_n_rsd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_9 <- paste0(format(rd(round(rt2_h_rsd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_h_rsd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_h_rsd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_10 <- paste0(format(rd(round(rt2_p_mssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_p_mssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_p_mssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_11 <- paste0(format(rd(round(rt2_n_mssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_n_mssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_n_mssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_12 <- paste0(format(rd(round(rt2_h_mssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_h_mssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_h_mssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_13 <- paste0(format(rd(round(rt2_p_rmssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_p_rmssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_p_rmssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_14 <- paste0(format(rd(round(rt2_n_rmssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_n_rmssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_n_rmssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_15 <- paste0(format(rd(round(rt2_h_rmssd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_h_rmssd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_h_rmssd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_16 <- paste0(format(rd(round(rt2_p_alpha_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_p_alpha_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_p_alpha_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_17 <- paste0(format(rd(round(rt2_n_alpha_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_n_alpha_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_n_alpha_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_18 <- paste0(format(rd(round(rt2_h_alpha_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_h_alpha_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_h_alpha_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_19 <- paste0(format(rd(round(rt2_pP_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_pP_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_pP_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_20 <- paste0(format(rd(round(rt2_nN_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_nN_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_nN_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_21 <- paste0(format(rd(round(rt2_hH_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_hH_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_hH_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_22 <- paste0(format(rd(round(rt2_pN_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_pN_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_pN_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_23 <- paste0(format(rd(round(rt2_pH_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_pH_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_pH_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_24 <- paste0(format(rd(round(rt2_nP_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_nP_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_nP_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_25 <- paste0(format(rd(round(rt2_nH_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_nH_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_nH_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_26 <- paste0(format(rd(round(rt2_hP_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_hP_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_hP_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_27 <- paste0(format(rd(round(rt2_hN_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_hN_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_hN_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_28 <- paste0(format(rd(round(rt2_P_N_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_P_N_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_P_N_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_29 <- paste0(format(rd(round(rt2_P_H_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_P_H_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_P_H_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_trt2_30 <- paste0(format(rd(round(rt2_N_H_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(rt2_N_H_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(rt2_N_H_ub,2), add = FALSE, digits = 2),nsmall=2),"]")

tsize <- 3
er_height <- 0.5
label <- c("xNH", "xPH", "xPN", "xHN", "xHP", "xNH", "xNP", 
           "xPH", "xPN", "xHH", "xNN", "xPP", 
           "HA a", "NA a", "PA a","HA cMSSD", "NA cMSSD", "PA cMSSD",
           "HA MSSD", "NA MSSD", "PA MSSD", "HA cSD", "NA cSD", "PA cSD", "HA SD", "NA SD", "PA SD",
           "HA mean", "NA Mean", "PA Mean", NA)
mean_o  <- c(rt2_N_H_beta, rt2_P_H_beta, rt2_P_N_beta, rt2_hN_beta, rt2_hP_beta, 
             rt2_nH_beta, rt2_nP_beta, rt2_pH_beta, rt2_pN_beta, rt2_hH_beta, rt2_nN_beta, rt2_pP_beta, 
             rt2_h_alpha_beta, rt2_n_alpha_beta, rt2_p_alpha_beta, rt2_h_rmssd_beta, rt2_n_rmssd_beta, rt2_p_rmssd_beta,
             rt2_h_mssd_beta, rt2_n_mssd_beta, rt2_p_mssd_beta, rt2_h_rsd_beta, rt2_n_rsd_beta, rt2_p_rsd_beta, rt2_h_sd_beta, rt2_n_sd_beta, rt2_p_sd_beta,
             rt2_h_mean_beta, rt2_n_mean_beta, rt2_p_mean_beta, NA) 
lower_o <- c(rt2_N_H_lb, rt2_P_H_lb, rt2_P_N_lb, rt2_hN_lb, rt2_hP_lb, 
             rt2_nH_lb, rt2_nP_lb, rt2_pH_lb, rt2_pN_lb, rt2_hH_lb, rt2_nN_lb, rt2_pP_lb, 
             rt2_h_alpha_lb, rt2_n_alpha_lb, rt2_p_alpha_lb, rt2_h_rmssd_lb, rt2_n_rmssd_lb, rt2_p_rmssd_lb,
             rt2_h_mssd_lb, rt2_n_mssd_lb, rt2_p_mssd_lb, rt2_h_rsd_lb, rt2_n_rsd_lb, rt2_p_rsd_lb, rt2_h_sd_lb, rt2_n_sd_lb, rt2_p_sd_lb,
             rt2_h_mean_lb, rt2_n_mean_lb, rt2_p_mean_lb, NA)
upper_o <- c(rt2_N_H_ub, rt2_P_H_ub, rt2_P_N_ub, rt2_hN_ub, rt2_hP_ub, 
             rt2_nH_ub, rt2_nP_ub, rt2_pH_ub, rt2_pN_ub, rt2_hH_ub, rt2_nN_ub, rt2_pP_ub, 
             rt2_h_alpha_ub, rt2_n_alpha_ub, rt2_p_alpha_ub, rt2_h_rmssd_ub, rt2_n_rmssd_ub, rt2_p_rmssd_ub,
             rt2_h_mssd_ub, rt2_n_mssd_ub, rt2_p_mssd_ub, rt2_h_rsd_ub, rt2_n_rsd_ub, rt2_p_rsd_ub, rt2_h_sd_ub, rt2_n_sd_ub, rt2_p_sd_ub,
             rt2_h_mean_ub, rt2_n_mean_ub, rt2_p_mean_ub, NA)
df_o <- data.frame(label, mean_o, lower_o, upper_o)
#df_o$label <- factor(df_o$label, levels=rev(df_o$label))

dat_o <- data.frame(group = factor(c("1","2","3","4","5","6","7","8","9","10",
                                     "11","12","13","14","15","16","17","18","19","20",
                                     "21","22","23","24","25","26","27","28","29","30", "31"), 
                                   levels=c("1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30", "31")),
                    cen = mean_o,
                    low = lower_o,
                    high = upper_o,
                    color = factor(c(1,1,1,1,0,0,0,0,1,0,
                                     0,1,1,1,1,1,1,1,1,1,
                                     1,1,1,1,1,1,1,1,1,1,
                                     1)))

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  panel.background = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

trt <- ggplot(dat_o,aes(mean_o,group, colour = color)) + 
  geom_point(size=tsize, shape=16) +
  #  scale_y_discrete(labels = label)+
  theme(legend.position = "none") +
  geom_errorbarh(aes(xmax = upper_o, xmin = lower_o), height = er_height) +
  coord_cartesian(xlim = c(-.25,1))+
  scale_color_manual(values = c("grey","black")) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks=c(-.25,0,.25,.5,.75,1), labels = c(-.25,0,.25,.5,.75,1)) +
  labs(x="test-retest r", y="")


lab_trt <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13","14","15","16","17","18","19","20",
                                    "21","22","23","24","25","26","27","28","29","30","31",
                                    "1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13","14","15","16","17","18","19","20",
                                    "21","22","23","24","25","26","27","28","29","30","31",
                                    "1","2","3","4","5","6","7","8","9","10",
                                    "11","12","13","14","15","16","17","18","19","20",
                                    "21","22","23","24","25","26","27","28","29","30","31")
                                  , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                             "20","19","18","17","16","15","14","13","12","11",
                                             "10","9","8","7","6","5","4","3","2","1")),
                      V05 = rep(c(1,2,3),each=31),
                      V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                             "","","","","","","","","","","",
                             "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                             "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA a","NA a","HA a","xPP","xNN", "xHH",
                             "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH",    
                             "r [95% CI]",table_trt2_1,table_trt2_2,table_trt2_3,table_trt2_4,table_trt2_5,table_trt2_6,table_trt2_7,table_trt2_8,table_trt2_9,table_trt2_10,
                             table_trt2_11,table_trt2_12,table_trt2_13,table_trt2_14,table_trt2_15,table_trt2_16,table_trt2_17,table_trt2_18,table_trt2_19,table_trt2_20,
                             table_trt2_21,table_trt2_22,table_trt2_23,table_trt2_24,table_trt2_25,table_trt2_26,table_trt2_27,table_trt2_28,table_trt2_29,table_trt2_30)
)



data_table_trt <- ggplot(lab_trt, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept=c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))
grid.arrange(data_table_trt, trt, ncol =2) #12,6




#### 6) RELATIONSHIP TO TRAIT PERSONALITY DOMAINS ####
# trait personality domains regressed on the seven robust principal components
options(scipen=999)

resid.three.mean <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_means)),
                        data=x)) 
  return(x)}

resid.three.sd <- function (x, y) {
  names <- colnames(x)
  var <- names[y]
  x[,y] <- residuals(lm(paste0(var,"~", paste(model_sd)),
                        data=x)) 
  return(x)}


#### residualize 1
# residualize data for mean 

cor_open1 <- sample1.pca[,c(36,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_cons1 <- sample1.pca[,c(34,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_extr1 <- sample1.pca[,c(32,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_agre1 <- sample1.pca[,c(35,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_neur1 <- sample1.pca[,c(33,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]

cor_open2 <- sample2.pca[,c(32,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_cons2 <- sample2.pca[,c(33,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_extr2 <- sample2.pca[,c(34,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_agre2 <- sample2.pca[,c(35,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_neur2 <- sample2.pca[,c(36,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]

cor_open3 <- sample3.pca[,c(34,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_cons3 <- sample3.pca[,c(36,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_extr3 <- sample3.pca[,c(33,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_agre3 <- sample3.pca[,c(35,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]
cor_neur3 <- sample3.pca[,c(32,2:7,11:13,8:10,14:16,17:19,20:22,23,24,26,25,27,28,29,31,30)]

m_cor_open1 <- cor_open1 
m_cor_cons1 <- cor_cons1 
m_cor_extr1 <- cor_extr1 
m_cor_agre1 <- cor_agre1 
m_cor_neur1 <- cor_neur1 

m_cor_open2 <- cor_open2 
m_cor_cons2 <- cor_cons2 
m_cor_extr2 <- cor_extr2 
m_cor_agre2 <- cor_agre2 
m_cor_neur2 <- cor_neur2 

m_cor_open3 <- cor_open3 
m_cor_cons3 <- cor_cons3 
m_cor_extr3 <- cor_extr3 
m_cor_agre3 <- cor_agre3 
m_cor_neur3 <- cor_neur3 

sd_cor_open1 <- cor_open1 
sd_cor_cons1 <- cor_cons1 
sd_cor_extr1 <- cor_extr1 
sd_cor_agre1 <- cor_agre1 
sd_cor_neur1 <- cor_neur1 

sd_cor_open2 <- cor_open2 
sd_cor_cons2 <- cor_cons2 
sd_cor_extr2 <- cor_extr2 
sd_cor_agre2 <- cor_agre2 
sd_cor_neur2 <- cor_neur2 

sd_cor_open3 <- cor_open3 
sd_cor_cons3 <- cor_cons3 
sd_cor_extr3 <- cor_extr3 
sd_cor_agre3 <- cor_agre3 
sd_cor_neur3 <- cor_neur3 

## bivariate corr with B5

cor_o_1 <- cor(cor_open1, use = "pairwise", method = "pearson")
cor_o_1_1 <- cor_o_1[1,2]
cor_o_2_1 <- cor_o_1[1,3]
cor_o_3_1 <- cor_o_1[1,4]
cor_o_4_1 <- cor_o_1[1,5]
cor_o_5_1 <- cor_o_1[1,6]
cor_o_6_1 <- cor_o_1[1,7]
cor_o_7_1 <- cor_o_1[1,8]
cor_o_8_1 <- cor_o_1[1,9]
cor_o_9_1 <- cor_o_1[1,10]
cor_o_10_1 <- cor_o_1[1,11]
cor_o_11_1 <- cor_o_1[1,12]
cor_o_12_1 <- cor_o_1[1,13]
cor_o_13_1 <- cor_o_1[1,14]
cor_o_14_1 <- cor_o_1[1,15]
cor_o_15_1 <- cor_o_1[1,16]
cor_o_16_1 <- cor_o_1[1,17]
cor_o_17_1 <- cor_o_1[1,18]
cor_o_18_1 <- cor_o_1[1,19]
cor_o_19_1 <- cor_o_1[1,20]
cor_o_20_1 <- cor_o_1[1,21]
cor_o_21_1 <- cor_o_1[1,22]
cor_o_22_1 <- cor_o_1[1,23]
cor_o_23_1 <- cor_o_1[1,24]
cor_o_24_1 <- cor_o_1[1,25]
cor_o_25_1 <- cor_o_1[1,26]
cor_o_26_1 <- cor_o_1[1,27]
cor_o_27_1 <- cor_o_1[1,28]
cor_o_28_1 <- cor_o_1[1,29]
cor_o_29_1 <- cor_o_1[1,30]
cor_o_30_1 <- cor_o_1[1,31]

cor_o_2 <- cor(cor_open2, use = "pairwise", method = "pearson")
cor_o_1_2 <- cor_o_2[1,2]
cor_o_2_2 <- cor_o_2[1,3]
cor_o_3_2 <- cor_o_2[1,4]
cor_o_4_2 <- cor_o_2[1,5]
cor_o_5_2 <- cor_o_2[1,6]
cor_o_6_2 <- cor_o_2[1,7]
cor_o_7_2 <- cor_o_2[1,8]
cor_o_8_2 <- cor_o_2[1,9]
cor_o_9_2 <- cor_o_2[1,10]
cor_o_10_2 <- cor_o_2[1,11]
cor_o_11_2 <- cor_o_2[1,12]
cor_o_12_2 <- cor_o_2[1,13]
cor_o_13_2 <- cor_o_2[1,14]
cor_o_14_2 <- cor_o_2[1,15]
cor_o_15_2 <- cor_o_2[1,16]
cor_o_16_2 <- cor_o_2[1,17]
cor_o_17_2 <- cor_o_2[1,18]
cor_o_18_2 <- cor_o_2[1,19]
cor_o_19_2 <- cor_o_2[1,20]
cor_o_20_2 <- cor_o_2[1,21]
cor_o_21_2 <- cor_o_2[1,22]
cor_o_22_2 <- cor_o_2[1,23]
cor_o_23_2 <- cor_o_2[1,24]
cor_o_24_2 <- cor_o_2[1,25]
cor_o_25_2 <- cor_o_2[1,26]
cor_o_26_2 <- cor_o_2[1,27]
cor_o_27_2 <- cor_o_2[1,28]
cor_o_28_2 <- cor_o_2[1,29]
cor_o_29_2 <- cor_o_2[1,30]
cor_o_30_2 <- cor_o_2[1,31]

cor_o_3 <- cor(cor_open3, use = "pairwise", method = "pearson")
cor_o_1_3 <- cor_o_3[1,2]
cor_o_2_3 <- cor_o_3[1,3]
cor_o_3_3 <- cor_o_3[1,4]
cor_o_4_3 <- cor_o_3[1,5]
cor_o_5_3 <- cor_o_3[1,6]
cor_o_6_3 <- cor_o_3[1,7]
cor_o_7_3 <- cor_o_3[1,8]
cor_o_8_3 <- cor_o_3[1,9]
cor_o_9_3 <- cor_o_3[1,10]
cor_o_10_3 <- cor_o_3[1,11]
cor_o_11_3 <- cor_o_3[1,12]
cor_o_12_3 <- cor_o_3[1,13]
cor_o_13_3 <- cor_o_3[1,14]
cor_o_14_3 <- cor_o_3[1,15]
cor_o_15_3 <- cor_o_3[1,16]
cor_o_16_3 <- cor_o_3[1,17]
cor_o_17_3 <- cor_o_3[1,18]
cor_o_18_3 <- cor_o_3[1,19]
cor_o_19_3 <- cor_o_3[1,20]
cor_o_20_3 <- cor_o_3[1,21]
cor_o_21_3 <- cor_o_3[1,22]
cor_o_22_3 <- cor_o_3[1,23]
cor_o_23_3 <- cor_o_3[1,24]
cor_o_24_3 <- cor_o_3[1,25]
cor_o_25_3 <- cor_o_3[1,26]
cor_o_26_3 <- cor_o_3[1,27]
cor_o_27_3 <- cor_o_3[1,28]
cor_o_28_3 <- cor_o_3[1,29]
cor_o_29_3 <- cor_o_3[1,30]
cor_o_30_3 <- cor_o_3[1,31]

cor_c_1 <- cor(cor_cons1, use = "pairwise", method = "pearson")
cor_c_1_1 <- cor_c_1[1,2]
cor_c_2_1 <- cor_c_1[1,3]
cor_c_3_1 <- cor_c_1[1,4]
cor_c_4_1 <- cor_c_1[1,5]
cor_c_5_1 <- cor_c_1[1,6]
cor_c_6_1 <- cor_c_1[1,7]
cor_c_7_1 <- cor_c_1[1,8]
cor_c_8_1 <- cor_c_1[1,9]
cor_c_9_1 <- cor_c_1[1,10]
cor_c_10_1 <- cor_c_1[1,11]
cor_c_11_1 <- cor_c_1[1,12]
cor_c_12_1 <- cor_c_1[1,13]
cor_c_13_1 <- cor_c_1[1,14]
cor_c_14_1 <- cor_c_1[1,15]
cor_c_15_1 <- cor_c_1[1,16]
cor_c_16_1 <- cor_c_1[1,17]
cor_c_17_1 <- cor_c_1[1,18]
cor_c_18_1 <- cor_c_1[1,19]
cor_c_19_1 <- cor_c_1[1,20]
cor_c_20_1 <- cor_c_1[1,21]
cor_c_21_1 <- cor_c_1[1,22]
cor_c_22_1 <- cor_c_1[1,23]
cor_c_23_1 <- cor_c_1[1,24]
cor_c_24_1 <- cor_c_1[1,25]
cor_c_25_1 <- cor_c_1[1,26]
cor_c_26_1 <- cor_c_1[1,27]
cor_c_27_1 <- cor_c_1[1,28]
cor_c_28_1 <- cor_c_1[1,29]
cor_c_29_1 <- cor_c_1[1,30]
cor_c_30_1 <- cor_c_1[1,31]

cor_c_2 <- cor(cor_cons2, use = "pairwise", method = "pearson")
cor_c_1_2 <- cor_c_2[1,2]
cor_c_2_2 <- cor_c_2[1,3]
cor_c_3_2 <- cor_c_2[1,4]
cor_c_4_2 <- cor_c_2[1,5]
cor_c_5_2 <- cor_c_2[1,6]
cor_c_6_2 <- cor_c_2[1,7]
cor_c_7_2 <- cor_c_2[1,8]
cor_c_8_2 <- cor_c_2[1,9]
cor_c_9_2 <- cor_c_2[1,10]
cor_c_10_2 <- cor_c_2[1,11]
cor_c_11_2 <- cor_c_2[1,12]
cor_c_12_2 <- cor_c_2[1,13]
cor_c_13_2 <- cor_c_2[1,14]
cor_c_14_2 <- cor_c_2[1,15]
cor_c_15_2 <- cor_c_2[1,16]
cor_c_16_2 <- cor_c_2[1,17]
cor_c_17_2 <- cor_c_2[1,18]
cor_c_18_2 <- cor_c_2[1,19]
cor_c_19_2 <- cor_c_2[1,20]
cor_c_20_2 <- cor_c_2[1,21]
cor_c_21_2 <- cor_c_2[1,22]
cor_c_22_2 <- cor_c_2[1,23]
cor_c_23_2 <- cor_c_2[1,24]
cor_c_24_2 <- cor_c_2[1,25]
cor_c_25_2 <- cor_c_2[1,26]
cor_c_26_2 <- cor_c_2[1,27]
cor_c_27_2 <- cor_c_2[1,28]
cor_c_28_2 <- cor_c_2[1,29]
cor_c_29_2 <- cor_c_2[1,30]
cor_c_30_2 <- cor_c_2[1,31]

cor_c_3 <- cor(cor_cons3, use = "pairwise", method = "pearson")
cor_c_1_3 <- cor_c_3[1,2]
cor_c_2_3 <- cor_c_3[1,3]
cor_c_3_3 <- cor_c_3[1,4]
cor_c_4_3 <- cor_c_3[1,5]
cor_c_5_3 <- cor_c_3[1,6]
cor_c_6_3 <- cor_c_3[1,7]
cor_c_7_3 <- cor_c_3[1,8]
cor_c_8_3 <- cor_c_3[1,9]
cor_c_9_3 <- cor_c_3[1,10]
cor_c_10_3 <- cor_c_3[1,11]
cor_c_11_3 <- cor_c_3[1,12]
cor_c_12_3 <- cor_c_3[1,13]
cor_c_13_3 <- cor_c_3[1,14]
cor_c_14_3 <- cor_c_3[1,15]
cor_c_15_3 <- cor_c_3[1,16]
cor_c_16_3 <- cor_c_3[1,17]
cor_c_17_3 <- cor_c_3[1,18]
cor_c_18_3 <- cor_c_3[1,19]
cor_c_19_3 <- cor_c_3[1,20]
cor_c_20_3 <- cor_c_3[1,21]
cor_c_21_3 <- cor_c_3[1,22]
cor_c_22_3 <- cor_c_3[1,23]
cor_c_23_3 <- cor_c_3[1,24]
cor_c_24_3 <- cor_c_3[1,25]
cor_c_25_3 <- cor_c_3[1,26]
cor_c_26_3 <- cor_c_3[1,27]
cor_c_27_3 <- cor_c_3[1,28]
cor_c_28_3 <- cor_c_3[1,29]
cor_c_29_3 <- cor_c_3[1,30]
cor_c_30_3 <- cor_c_3[1,31]

cor_e_1 <- cor(cor_extr1, use = "pairwise", method = "pearson")
cor_e_1_1 <- cor_e_1[1,2]
cor_e_2_1 <- cor_e_1[1,3]
cor_e_3_1 <- cor_e_1[1,4]
cor_e_4_1 <- cor_e_1[1,5]
cor_e_5_1 <- cor_e_1[1,6]
cor_e_6_1 <- cor_e_1[1,7]
cor_e_7_1 <- cor_e_1[1,8]
cor_e_8_1 <- cor_e_1[1,9]
cor_e_9_1 <- cor_e_1[1,10]
cor_e_10_1 <- cor_e_1[1,11]
cor_e_11_1 <- cor_e_1[1,12]
cor_e_12_1 <- cor_e_1[1,13]
cor_e_13_1 <- cor_e_1[1,14]
cor_e_14_1 <- cor_e_1[1,15]
cor_e_15_1 <- cor_e_1[1,16]
cor_e_16_1 <- cor_e_1[1,17]
cor_e_17_1 <- cor_e_1[1,18]
cor_e_18_1 <- cor_e_1[1,19]
cor_e_19_1 <- cor_e_1[1,20]
cor_e_20_1 <- cor_e_1[1,21]
cor_e_21_1 <- cor_e_1[1,22]
cor_e_22_1 <- cor_e_1[1,23]
cor_e_23_1 <- cor_e_1[1,24]
cor_e_24_1 <- cor_e_1[1,25]
cor_e_25_1 <- cor_e_1[1,26]
cor_e_26_1 <- cor_e_1[1,27]
cor_e_27_1 <- cor_e_1[1,28]
cor_e_28_1 <- cor_e_1[1,29]
cor_e_29_1 <- cor_e_1[1,30]
cor_e_30_1 <- cor_e_1[1,31]

cor_e_2 <- cor(cor_extr2, use = "pairwise", method = "pearson")
cor_e_1_2 <- cor_e_2[1,2]
cor_e_2_2 <- cor_e_2[1,3]
cor_e_3_2 <- cor_e_2[1,4]
cor_e_4_2 <- cor_e_2[1,5]
cor_e_5_2 <- cor_e_2[1,6]
cor_e_6_2 <- cor_e_2[1,7]
cor_e_7_2 <- cor_e_2[1,8]
cor_e_8_2 <- cor_e_2[1,9]
cor_e_9_2 <- cor_e_2[1,10]
cor_e_10_2 <- cor_e_2[1,11]
cor_e_11_2 <- cor_e_2[1,12]
cor_e_12_2 <- cor_e_2[1,13]
cor_e_13_2 <- cor_e_2[1,14]
cor_e_14_2 <- cor_e_2[1,15]
cor_e_15_2 <- cor_e_2[1,16]
cor_e_16_2 <- cor_e_2[1,17]
cor_e_17_2 <- cor_e_2[1,18]
cor_e_18_2 <- cor_e_2[1,19]
cor_e_19_2 <- cor_e_2[1,20]
cor_e_20_2 <- cor_e_2[1,21]
cor_e_21_2 <- cor_e_2[1,22]
cor_e_22_2 <- cor_e_2[1,23]
cor_e_23_2 <- cor_e_2[1,24]
cor_e_24_2 <- cor_e_2[1,25]
cor_e_25_2 <- cor_e_2[1,26]
cor_e_26_2 <- cor_e_2[1,27]
cor_e_27_2 <- cor_e_2[1,28]
cor_e_28_2 <- cor_e_2[1,29]
cor_e_29_2 <- cor_e_2[1,30]
cor_e_30_2 <- cor_e_2[1,31]

cor_e_3 <- cor(cor_extr3, use = "pairwise", method = "pearson")
cor_e_1_3 <- cor_e_3[1,2]
cor_e_2_3 <- cor_e_3[1,3]
cor_e_3_3 <- cor_e_3[1,4]
cor_e_4_3 <- cor_e_3[1,5]
cor_e_5_3 <- cor_e_3[1,6]
cor_e_6_3 <- cor_e_3[1,7]
cor_e_7_3 <- cor_e_3[1,8]
cor_e_8_3 <- cor_e_3[1,9]
cor_e_9_3 <- cor_e_3[1,10]
cor_e_10_3 <- cor_e_3[1,11]
cor_e_11_3 <- cor_e_3[1,12]
cor_e_12_3 <- cor_e_3[1,13]
cor_e_13_3 <- cor_e_3[1,14]
cor_e_14_3 <- cor_e_3[1,15]
cor_e_15_3 <- cor_e_3[1,16]
cor_e_16_3 <- cor_e_3[1,17]
cor_e_17_3 <- cor_e_3[1,18]
cor_e_18_3 <- cor_e_3[1,19]
cor_e_19_3 <- cor_e_3[1,20]
cor_e_20_3 <- cor_e_3[1,21]
cor_e_21_3 <- cor_e_3[1,22]
cor_e_22_3 <- cor_e_3[1,23]
cor_e_23_3 <- cor_e_3[1,24]
cor_e_24_3 <- cor_e_3[1,25]
cor_e_25_3 <- cor_e_3[1,26]
cor_e_26_3 <- cor_e_3[1,27]
cor_e_27_3 <- cor_e_3[1,28]
cor_e_28_3 <- cor_e_3[1,29]
cor_e_29_3 <- cor_e_3[1,30]
cor_e_30_3 <- cor_e_3[1,31]

cor_a_1 <- cor(cor_agre1, use = "pairwise", method = "pearson")
cor_a_1_1 <- cor_a_1[1,2]
cor_a_2_1 <- cor_a_1[1,3]
cor_a_3_1 <- cor_a_1[1,4]
cor_a_4_1 <- cor_a_1[1,5]
cor_a_5_1 <- cor_a_1[1,6]
cor_a_6_1 <- cor_a_1[1,7]
cor_a_7_1 <- cor_a_1[1,8]
cor_a_8_1 <- cor_a_1[1,9]
cor_a_9_1 <- cor_a_1[1,10]
cor_a_10_1 <- cor_a_1[1,11]
cor_a_11_1 <- cor_a_1[1,12]
cor_a_12_1 <- cor_a_1[1,13]
cor_a_13_1 <- cor_a_1[1,14]
cor_a_14_1 <- cor_a_1[1,15]
cor_a_15_1 <- cor_a_1[1,16]
cor_a_16_1 <- cor_a_1[1,17]
cor_a_17_1 <- cor_a_1[1,18]
cor_a_18_1 <- cor_a_1[1,19]
cor_a_19_1 <- cor_a_1[1,20]
cor_a_20_1 <- cor_a_1[1,21]
cor_a_21_1 <- cor_a_1[1,22]
cor_a_22_1 <- cor_a_1[1,23]
cor_a_23_1 <- cor_a_1[1,24]
cor_a_24_1 <- cor_a_1[1,25]
cor_a_25_1 <- cor_a_1[1,26]
cor_a_26_1 <- cor_a_1[1,27]
cor_a_27_1 <- cor_a_1[1,28]
cor_a_28_1 <- cor_a_1[1,29]
cor_a_29_1 <- cor_a_1[1,30]
cor_a_30_1 <- cor_a_1[1,31]

cor_a_2 <- cor(cor_agre2, use = "pairwise", method = "pearson")
cor_a_1_2 <- cor_a_2[1,2]
cor_a_2_2 <- cor_a_2[1,3]
cor_a_3_2 <- cor_a_2[1,4]
cor_a_4_2 <- cor_a_2[1,5]
cor_a_5_2 <- cor_a_2[1,6]
cor_a_6_2 <- cor_a_2[1,7]
cor_a_7_2 <- cor_a_2[1,8]
cor_a_8_2 <- cor_a_2[1,9]
cor_a_9_2 <- cor_a_2[1,10]
cor_a_10_2 <- cor_a_2[1,11]
cor_a_11_2 <- cor_a_2[1,12]
cor_a_12_2 <- cor_a_2[1,13]
cor_a_13_2 <- cor_a_2[1,14]
cor_a_14_2 <- cor_a_2[1,15]
cor_a_15_2 <- cor_a_2[1,16]
cor_a_16_2 <- cor_a_2[1,17]
cor_a_17_2 <- cor_a_2[1,18]
cor_a_18_2 <- cor_a_2[1,19]
cor_a_19_2 <- cor_a_2[1,20]
cor_a_20_2 <- cor_a_2[1,21]
cor_a_21_2 <- cor_a_2[1,22]
cor_a_22_2 <- cor_a_2[1,23]
cor_a_23_2 <- cor_a_2[1,24]
cor_a_24_2 <- cor_a_2[1,25]
cor_a_25_2 <- cor_a_2[1,26]
cor_a_26_2 <- cor_a_2[1,27]
cor_a_27_2 <- cor_a_2[1,28]
cor_a_28_2 <- cor_a_2[1,29]
cor_a_29_2 <- cor_a_2[1,30]
cor_a_30_2 <- cor_a_2[1,31]

cor_a_3 <- cor(cor_agre3, use = "pairwise", method = "pearson")
cor_a_1_3 <- cor_a_3[1,2]
cor_a_2_3 <- cor_a_3[1,3]
cor_a_3_3 <- cor_a_3[1,4]
cor_a_4_3 <- cor_a_3[1,5]
cor_a_5_3 <- cor_a_3[1,6]
cor_a_6_3 <- cor_a_3[1,7]
cor_a_7_3 <- cor_a_3[1,8]
cor_a_8_3 <- cor_a_3[1,9]
cor_a_9_3 <- cor_a_3[1,10]
cor_a_10_3 <- cor_a_3[1,11]
cor_a_11_3 <- cor_a_3[1,12]
cor_a_12_3 <- cor_a_3[1,13]
cor_a_13_3 <- cor_a_3[1,14]
cor_a_14_3 <- cor_a_3[1,15]
cor_a_15_3 <- cor_a_3[1,16]
cor_a_16_3 <- cor_a_3[1,17]
cor_a_17_3 <- cor_a_3[1,18]
cor_a_18_3 <- cor_a_3[1,19]
cor_a_19_3 <- cor_a_3[1,20]
cor_a_20_3 <- cor_a_3[1,21]
cor_a_21_3 <- cor_a_3[1,22]
cor_a_22_3 <- cor_a_3[1,23]
cor_a_23_3 <- cor_a_3[1,24]
cor_a_24_3 <- cor_a_3[1,25]
cor_a_25_3 <- cor_a_3[1,26]
cor_a_26_3 <- cor_a_3[1,27]
cor_a_27_3 <- cor_a_3[1,28]
cor_a_28_3 <- cor_a_3[1,29]
cor_a_29_3 <- cor_a_3[1,30]
cor_a_30_3 <- cor_a_3[1,31]

cor_n_1 <- cor(cor_neur1, use = "pairwise", method = "pearson")
cor_n_1_1 <- cor_n_1[1,2]
cor_n_2_1 <- cor_n_1[1,3]
cor_n_3_1 <- cor_n_1[1,4]
cor_n_4_1 <- cor_n_1[1,5]
cor_n_5_1 <- cor_n_1[1,6]
cor_n_6_1 <- cor_n_1[1,7]
cor_n_7_1 <- cor_n_1[1,8]
cor_n_8_1 <- cor_n_1[1,9]
cor_n_9_1 <- cor_n_1[1,10]
cor_n_10_1 <- cor_n_1[1,11]
cor_n_11_1 <- cor_n_1[1,12]
cor_n_12_1 <- cor_n_1[1,13]
cor_n_13_1 <- cor_n_1[1,14]
cor_n_14_1 <- cor_n_1[1,15]
cor_n_15_1 <- cor_n_1[1,16]
cor_n_16_1 <- cor_n_1[1,17]
cor_n_17_1 <- cor_n_1[1,18]
cor_n_18_1 <- cor_n_1[1,19]
cor_n_19_1 <- cor_n_1[1,20]
cor_n_20_1 <- cor_n_1[1,21]
cor_n_21_1 <- cor_n_1[1,22]
cor_n_22_1 <- cor_n_1[1,23]
cor_n_23_1 <- cor_n_1[1,24]
cor_n_24_1 <- cor_n_1[1,25]
cor_n_25_1 <- cor_n_1[1,26]
cor_n_26_1 <- cor_n_1[1,27]
cor_n_27_1 <- cor_n_1[1,28]
cor_n_28_1 <- cor_n_1[1,29]
cor_n_29_1 <- cor_n_1[1,30]
cor_n_30_1 <- cor_n_1[1,31]

cor_n_2 <- cor(cor_neur2, use = "pairwise", method = "pearson")
cor_n_1_2 <- cor_n_2[1,2]
cor_n_2_2 <- cor_n_2[1,3]
cor_n_3_2 <- cor_n_2[1,4]
cor_n_4_2 <- cor_n_2[1,5]
cor_n_5_2 <- cor_n_2[1,6]
cor_n_6_2 <- cor_n_2[1,7]
cor_n_7_2 <- cor_n_2[1,8]
cor_n_8_2 <- cor_n_2[1,9]
cor_n_9_2 <- cor_n_2[1,10]
cor_n_10_2 <- cor_n_2[1,11]
cor_n_11_2 <- cor_n_2[1,12]
cor_n_12_2 <- cor_n_2[1,13]
cor_n_13_2 <- cor_n_2[1,14]
cor_n_14_2 <- cor_n_2[1,15]
cor_n_15_2 <- cor_n_2[1,16]
cor_n_16_2 <- cor_n_2[1,17]
cor_n_17_2 <- cor_n_2[1,18]
cor_n_18_2 <- cor_n_2[1,19]
cor_n_19_2 <- cor_n_2[1,20]
cor_n_20_2 <- cor_n_2[1,21]
cor_n_21_2 <- cor_n_2[1,22]
cor_n_22_2 <- cor_n_2[1,23]
cor_n_23_2 <- cor_n_2[1,24]
cor_n_24_2 <- cor_n_2[1,25]
cor_n_25_2 <- cor_n_2[1,26]
cor_n_26_2 <- cor_n_2[1,27]
cor_n_27_2 <- cor_n_2[1,28]
cor_n_28_2 <- cor_n_2[1,29]
cor_n_29_2 <- cor_n_2[1,30]
cor_n_30_2 <- cor_n_2[1,31]

cor_n_3 <- cor(cor_neur3, use = "pairwise", method = "pearson")
cor_n_1_3 <- cor_n_3[1,2]
cor_n_2_3 <- cor_n_3[1,3]
cor_n_3_3 <- cor_n_3[1,4]
cor_n_4_3 <- cor_n_3[1,5]
cor_n_5_3 <- cor_n_3[1,6]
cor_n_6_3 <- cor_n_3[1,7]
cor_n_7_3 <- cor_n_3[1,8]
cor_n_8_3 <- cor_n_3[1,9]
cor_n_9_3 <- cor_n_3[1,10]
cor_n_10_3 <- cor_n_3[1,11]
cor_n_11_3 <- cor_n_3[1,12]
cor_n_12_3 <- cor_n_3[1,13]
cor_n_13_3 <- cor_n_3[1,14]
cor_n_14_3 <- cor_n_3[1,15]
cor_n_15_3 <- cor_n_3[1,16]
cor_n_16_3 <- cor_n_3[1,17]
cor_n_17_3 <- cor_n_3[1,18]
cor_n_18_3 <- cor_n_3[1,19]
cor_n_19_3 <- cor_n_3[1,20]
cor_n_20_3 <- cor_n_3[1,21]
cor_n_21_3 <- cor_n_3[1,22]
cor_n_22_3 <- cor_n_3[1,23]
cor_n_23_3 <- cor_n_3[1,24]
cor_n_24_3 <- cor_n_3[1,25]
cor_n_25_3 <- cor_n_3[1,26]
cor_n_26_3 <- cor_n_3[1,27]
cor_n_27_3 <- cor_n_3[1,28]
cor_n_28_3 <- cor_n_3[1,29]
cor_n_29_3 <- cor_n_3[1,30]
cor_n_30_3 <- cor_n_3[1,31]

# resid mean
for (i in 5:31){m_cor_open1 <- resid.three.mean(m_cor_open1, i)}
for (i in 5:31){m_cor_cons1 <- resid.three.mean(m_cor_cons1, i)}
for (i in 5:31){m_cor_extr1 <- resid.three.mean(m_cor_extr1, i)}
for (i in 5:31){m_cor_agre1 <- resid.three.mean(m_cor_agre1, i)}
for (i in 5:31){m_cor_neur1 <- resid.three.mean(m_cor_neur1, i)}

for (i in 5:31){m_cor_open2 <- resid.three.mean(m_cor_open2, i)}
for (i in 5:31){m_cor_cons2 <- resid.three.mean(m_cor_cons2, i)}
for (i in 5:31){m_cor_extr2 <- resid.three.mean(m_cor_extr2, i)}
for (i in 5:31){m_cor_agre2 <- resid.three.mean(m_cor_agre2, i)}
for (i in 5:31){m_cor_neur2 <- resid.three.mean(m_cor_neur2, i)}

for (i in 5:31){m_cor_open3 <- resid.three.mean(m_cor_open3, i)}
for (i in 5:31){m_cor_cons3 <- resid.three.mean(m_cor_cons3, i)}
for (i in 5:31){m_cor_extr3 <- resid.three.mean(m_cor_extr3, i)}
for (i in 5:31){m_cor_agre3 <- resid.three.mean(m_cor_agre3, i)}
for (i in 5:31){m_cor_neur3 <- resid.three.mean(m_cor_neur3, i)}

m_cor_o_1 <- cor(m_cor_open1, use = "pairwise", method = "pearson")
m_cor_o_4_1 <- m_cor_o_1[1,5]
m_cor_o_5_1 <- m_cor_o_1[1,6]
m_cor_o_6_1 <- m_cor_o_1[1,7]
m_cor_o_7_1 <- m_cor_o_1[1,8]
m_cor_o_8_1 <- m_cor_o_1[1,9]
m_cor_o_9_1 <- m_cor_o_1[1,10]
m_cor_o_10_1 <- m_cor_o_1[1,11]
m_cor_o_11_1 <- m_cor_o_1[1,12]
m_cor_o_12_1 <- m_cor_o_1[1,13]
m_cor_o_13_1 <- m_cor_o_1[1,14]
m_cor_o_14_1 <- m_cor_o_1[1,15]
m_cor_o_15_1 <- m_cor_o_1[1,16]
m_cor_o_16_1 <- m_cor_o_1[1,17]
m_cor_o_17_1 <- m_cor_o_1[1,18]
m_cor_o_18_1 <- m_cor_o_1[1,19]
m_cor_o_19_1 <- m_cor_o_1[1,20]
m_cor_o_20_1 <- m_cor_o_1[1,21]
m_cor_o_21_1 <- m_cor_o_1[1,22]
m_cor_o_22_1 <- m_cor_o_1[1,23]
m_cor_o_23_1 <- m_cor_o_1[1,24]
m_cor_o_24_1 <- m_cor_o_1[1,25]
m_cor_o_25_1 <- m_cor_o_1[1,26]
m_cor_o_26_1 <- m_cor_o_1[1,27]
m_cor_o_27_1 <- m_cor_o_1[1,28]
m_cor_o_28_1 <- m_cor_o_1[1,29]
m_cor_o_29_1 <- m_cor_o_1[1,30]
m_cor_o_30_1 <- m_cor_o_1[1,31]

m_cor_o_2 <- cor(m_cor_open2, use = "pairwise", method = "pearson")
m_cor_o_4_2 <- m_cor_o_2[1,5]
m_cor_o_5_2 <- m_cor_o_2[1,6]
m_cor_o_6_2 <- m_cor_o_2[1,7]
m_cor_o_7_2 <- m_cor_o_2[1,8]
m_cor_o_8_2 <- m_cor_o_2[1,9]
m_cor_o_9_2 <- m_cor_o_2[1,10]
m_cor_o_10_2 <- m_cor_o_2[1,11]
m_cor_o_11_2 <- m_cor_o_2[1,12]
m_cor_o_12_2 <- m_cor_o_2[1,13]
m_cor_o_13_2 <- m_cor_o_2[1,14]
m_cor_o_14_2 <- m_cor_o_2[1,15]
m_cor_o_15_2 <- m_cor_o_2[1,16]
m_cor_o_16_2 <- m_cor_o_2[1,17]
m_cor_o_17_2 <- m_cor_o_2[1,18]
m_cor_o_18_2 <- m_cor_o_2[1,19]
m_cor_o_19_2 <- m_cor_o_2[1,20]
m_cor_o_20_2 <- m_cor_o_2[1,21]
m_cor_o_21_2 <- m_cor_o_2[1,22]
m_cor_o_22_2 <- m_cor_o_2[1,23]
m_cor_o_23_2 <- m_cor_o_2[1,24]
m_cor_o_24_2 <- m_cor_o_2[1,25]
m_cor_o_25_2 <- m_cor_o_2[1,26]
m_cor_o_26_2 <- m_cor_o_2[1,27]
m_cor_o_27_2 <- m_cor_o_2[1,28]
m_cor_o_28_2 <- m_cor_o_2[1,29]
m_cor_o_29_2 <- m_cor_o_2[1,30]
m_cor_o_30_2 <- m_cor_o_2[1,31]

m_cor_o_3 <- cor(m_cor_open3, use = "pairwise", method = "pearson")
m_cor_o_4_3 <- m_cor_o_3[1,5]
m_cor_o_5_3 <- m_cor_o_3[1,6]
m_cor_o_6_3 <- m_cor_o_3[1,7]
m_cor_o_7_3 <- m_cor_o_3[1,8]
m_cor_o_8_3 <- m_cor_o_3[1,9]
m_cor_o_9_3 <- m_cor_o_3[1,10]
m_cor_o_10_3 <- m_cor_o_3[1,11]
m_cor_o_11_3 <- m_cor_o_3[1,12]
m_cor_o_12_3 <- m_cor_o_3[1,13]
m_cor_o_13_3 <- m_cor_o_3[1,14]
m_cor_o_14_3 <- m_cor_o_3[1,15]
m_cor_o_15_3 <- m_cor_o_3[1,16]
m_cor_o_16_3 <- m_cor_o_3[1,17]
m_cor_o_17_3 <- m_cor_o_3[1,18]
m_cor_o_18_3 <- m_cor_o_3[1,19]
m_cor_o_19_3 <- m_cor_o_3[1,20]
m_cor_o_20_3 <- m_cor_o_3[1,21]
m_cor_o_21_3 <- m_cor_o_3[1,22]
m_cor_o_22_3 <- m_cor_o_3[1,23]
m_cor_o_23_3 <- m_cor_o_3[1,24]
m_cor_o_24_3 <- m_cor_o_3[1,25]
m_cor_o_25_3 <- m_cor_o_3[1,26]
m_cor_o_26_3 <- m_cor_o_3[1,27]
m_cor_o_27_3 <- m_cor_o_3[1,28]
m_cor_o_28_3 <- m_cor_o_3[1,29]
m_cor_o_29_3 <- m_cor_o_3[1,30]
m_cor_o_30_3 <- m_cor_o_3[1,31]

m_cor_c_1 <- cor(m_cor_cons1, use = "pairwise", method = "pearson")
m_cor_c_4_1 <- m_cor_c_1[1,5]
m_cor_c_5_1 <- m_cor_c_1[1,6]
m_cor_c_6_1 <- m_cor_c_1[1,7]
m_cor_c_7_1 <- m_cor_c_1[1,8]
m_cor_c_8_1 <- m_cor_c_1[1,9]
m_cor_c_9_1 <- m_cor_c_1[1,10]
m_cor_c_10_1 <- m_cor_c_1[1,11]
m_cor_c_11_1 <- m_cor_c_1[1,12]
m_cor_c_12_1 <- m_cor_c_1[1,13]
m_cor_c_13_1 <- m_cor_c_1[1,14]
m_cor_c_14_1 <- m_cor_c_1[1,15]
m_cor_c_15_1 <- m_cor_c_1[1,16]
m_cor_c_16_1 <- m_cor_c_1[1,17]
m_cor_c_17_1 <- m_cor_c_1[1,18]
m_cor_c_18_1 <- m_cor_c_1[1,19]
m_cor_c_19_1 <- m_cor_c_1[1,20]
m_cor_c_20_1 <- m_cor_c_1[1,21]
m_cor_c_21_1 <- m_cor_c_1[1,22]
m_cor_c_22_1 <- m_cor_c_1[1,23]
m_cor_c_23_1 <- m_cor_c_1[1,24]
m_cor_c_24_1 <- m_cor_c_1[1,25]
m_cor_c_25_1 <- m_cor_c_1[1,26]
m_cor_c_26_1 <- m_cor_c_1[1,27]
m_cor_c_27_1 <- m_cor_c_1[1,28]
m_cor_c_28_1 <- m_cor_c_1[1,29]
m_cor_c_29_1 <- m_cor_c_1[1,30]
m_cor_c_30_1 <- m_cor_c_1[1,31]

m_cor_c_2 <- cor(m_cor_cons2, use = "pairwise", method = "pearson")
m_cor_c_4_2 <- m_cor_c_2[1,5]
m_cor_c_5_2 <- m_cor_c_2[1,6]
m_cor_c_6_2 <- m_cor_c_2[1,7]
m_cor_c_7_2 <- m_cor_c_2[1,8]
m_cor_c_8_2 <- m_cor_c_2[1,9]
m_cor_c_9_2 <- m_cor_c_2[1,10]
m_cor_c_10_2 <- m_cor_c_2[1,11]
m_cor_c_11_2 <- m_cor_c_2[1,12]
m_cor_c_12_2 <- m_cor_c_2[1,13]
m_cor_c_13_2 <- m_cor_c_2[1,14]
m_cor_c_14_2 <- m_cor_c_2[1,15]
m_cor_c_15_2 <- m_cor_c_2[1,16]
m_cor_c_16_2 <- m_cor_c_2[1,17]
m_cor_c_17_2 <- m_cor_c_2[1,18]
m_cor_c_18_2 <- m_cor_c_2[1,19]
m_cor_c_19_2 <- m_cor_c_2[1,20]
m_cor_c_20_2 <- m_cor_c_2[1,21]
m_cor_c_21_2 <- m_cor_c_2[1,22]
m_cor_c_22_2 <- m_cor_c_2[1,23]
m_cor_c_23_2 <- m_cor_c_2[1,24]
m_cor_c_24_2 <- m_cor_c_2[1,25]
m_cor_c_25_2 <- m_cor_c_2[1,26]
m_cor_c_26_2 <- m_cor_c_2[1,27]
m_cor_c_27_2 <- m_cor_c_2[1,28]
m_cor_c_28_2 <- m_cor_c_2[1,29]
m_cor_c_29_2 <- m_cor_c_2[1,30]
m_cor_c_30_2 <- m_cor_c_2[1,31]

m_cor_c_3 <- cor(m_cor_cons3, use = "pairwise", method = "pearson")
m_cor_c_4_3 <- m_cor_c_3[1,5]
m_cor_c_5_3 <- m_cor_c_3[1,6]
m_cor_c_6_3 <- m_cor_c_3[1,7]
m_cor_c_7_3 <- m_cor_c_3[1,8]
m_cor_c_8_3 <- m_cor_c_3[1,9]
m_cor_c_9_3 <- m_cor_c_3[1,10]
m_cor_c_10_3 <- m_cor_c_3[1,11]
m_cor_c_11_3 <- m_cor_c_3[1,12]
m_cor_c_12_3 <- m_cor_c_3[1,13]
m_cor_c_13_3 <- m_cor_c_3[1,14]
m_cor_c_14_3 <- m_cor_c_3[1,15]
m_cor_c_15_3 <- m_cor_c_3[1,16]
m_cor_c_16_3 <- m_cor_c_3[1,17]
m_cor_c_17_3 <- m_cor_c_3[1,18]
m_cor_c_18_3 <- m_cor_c_3[1,19]
m_cor_c_19_3 <- m_cor_c_3[1,20]
m_cor_c_20_3 <- m_cor_c_3[1,21]
m_cor_c_21_3 <- m_cor_c_3[1,22]
m_cor_c_22_3 <- m_cor_c_3[1,23]
m_cor_c_23_3 <- m_cor_c_3[1,24]
m_cor_c_24_3 <- m_cor_c_3[1,25]
m_cor_c_25_3 <- m_cor_c_3[1,26]
m_cor_c_26_3 <- m_cor_c_3[1,27]
m_cor_c_27_3 <- m_cor_c_3[1,28]
m_cor_c_28_3 <- m_cor_c_3[1,29]
m_cor_c_29_3 <- m_cor_c_3[1,30]
m_cor_c_30_3 <- m_cor_c_3[1,31]

m_cor_e_1 <- cor(m_cor_extr1, use = "pairwise", method = "pearson")
m_cor_e_4_1 <- m_cor_e_1[1,5]
m_cor_e_5_1 <- m_cor_e_1[1,6]
m_cor_e_6_1 <- m_cor_e_1[1,7]
m_cor_e_7_1 <- m_cor_e_1[1,8]
m_cor_e_8_1 <- m_cor_e_1[1,9]
m_cor_e_9_1 <- m_cor_e_1[1,10]
m_cor_e_10_1 <- m_cor_e_1[1,11]
m_cor_e_11_1 <- m_cor_e_1[1,12]
m_cor_e_12_1 <- m_cor_e_1[1,13]
m_cor_e_13_1 <- m_cor_e_1[1,14]
m_cor_e_14_1 <- m_cor_e_1[1,15]
m_cor_e_15_1 <- m_cor_e_1[1,16]
m_cor_e_16_1 <- m_cor_e_1[1,17]
m_cor_e_17_1 <- m_cor_e_1[1,18]
m_cor_e_18_1 <- m_cor_e_1[1,19]
m_cor_e_19_1 <- m_cor_e_1[1,20]
m_cor_e_20_1 <- m_cor_e_1[1,21]
m_cor_e_21_1 <- m_cor_e_1[1,22]
m_cor_e_22_1 <- m_cor_e_1[1,23]
m_cor_e_23_1 <- m_cor_e_1[1,24]
m_cor_e_24_1 <- m_cor_e_1[1,25]
m_cor_e_25_1 <- m_cor_e_1[1,26]
m_cor_e_26_1 <- m_cor_e_1[1,27]
m_cor_e_27_1 <- m_cor_e_1[1,28]
m_cor_e_28_1 <- m_cor_e_1[1,29]
m_cor_e_29_1 <- m_cor_e_1[1,30]
m_cor_e_30_1 <- m_cor_e_1[1,31]

m_cor_e_2 <- cor(m_cor_extr2, use = "pairwise", method = "pearson")
m_cor_e_4_2 <- m_cor_e_2[1,5]
m_cor_e_5_2 <- m_cor_e_2[1,6]
m_cor_e_6_2 <- m_cor_e_2[1,7]
m_cor_e_7_2 <- m_cor_e_2[1,8]
m_cor_e_8_2 <- m_cor_e_2[1,9]
m_cor_e_9_2 <- m_cor_e_2[1,10]
m_cor_e_10_2 <- m_cor_e_2[1,11]
m_cor_e_11_2 <- m_cor_e_2[1,12]
m_cor_e_12_2 <- m_cor_e_2[1,13]
m_cor_e_13_2 <- m_cor_e_2[1,14]
m_cor_e_14_2 <- m_cor_e_2[1,15]
m_cor_e_15_2 <- m_cor_e_2[1,16]
m_cor_e_16_2 <- m_cor_e_2[1,17]
m_cor_e_17_2 <- m_cor_e_2[1,18]
m_cor_e_18_2 <- m_cor_e_2[1,19]
m_cor_e_19_2 <- m_cor_e_2[1,20]
m_cor_e_20_2 <- m_cor_e_2[1,21]
m_cor_e_21_2 <- m_cor_e_2[1,22]
m_cor_e_22_2 <- m_cor_e_2[1,23]
m_cor_e_23_2 <- m_cor_e_2[1,24]
m_cor_e_24_2 <- m_cor_e_2[1,25]
m_cor_e_25_2 <- m_cor_e_2[1,26]
m_cor_e_26_2 <- m_cor_e_2[1,27]
m_cor_e_27_2 <- m_cor_e_2[1,28]
m_cor_e_28_2 <- m_cor_e_2[1,29]
m_cor_e_29_2 <- m_cor_e_2[1,30]
m_cor_e_30_2 <- m_cor_e_2[1,31]

m_cor_e_3 <- cor(m_cor_extr3, use = "pairwise", method = "pearson")
m_cor_e_4_3 <- m_cor_e_3[1,5]
m_cor_e_5_3 <- m_cor_e_3[1,6]
m_cor_e_6_3 <- m_cor_e_3[1,7]
m_cor_e_7_3 <- m_cor_e_3[1,8]
m_cor_e_8_3 <- m_cor_e_3[1,9]
m_cor_e_9_3 <- m_cor_e_3[1,10]
m_cor_e_10_3 <- m_cor_e_3[1,11]
m_cor_e_11_3 <- m_cor_e_3[1,12]
m_cor_e_12_3 <- m_cor_e_3[1,13]
m_cor_e_13_3 <- m_cor_e_3[1,14]
m_cor_e_14_3 <- m_cor_e_3[1,15]
m_cor_e_15_3 <- m_cor_e_3[1,16]
m_cor_e_16_3 <- m_cor_e_3[1,17]
m_cor_e_17_3 <- m_cor_e_3[1,18]
m_cor_e_18_3 <- m_cor_e_3[1,19]
m_cor_e_19_3 <- m_cor_e_3[1,20]
m_cor_e_20_3 <- m_cor_e_3[1,21]
m_cor_e_21_3 <- m_cor_e_3[1,22]
m_cor_e_22_3 <- m_cor_e_3[1,23]
m_cor_e_23_3 <- m_cor_e_3[1,24]
m_cor_e_24_3 <- m_cor_e_3[1,25]
m_cor_e_25_3 <- m_cor_e_3[1,26]
m_cor_e_26_3 <- m_cor_e_3[1,27]
m_cor_e_27_3 <- m_cor_e_3[1,28]
m_cor_e_28_3 <- m_cor_e_3[1,29]
m_cor_e_29_3 <- m_cor_e_3[1,30]
m_cor_e_30_3 <- m_cor_e_3[1,31]

m_cor_a_1 <- cor(m_cor_agre1, use = "pairwise", method = "pearson")
m_cor_a_4_1 <- m_cor_a_1[1,5]
m_cor_a_5_1 <- m_cor_a_1[1,6]
m_cor_a_6_1 <- m_cor_a_1[1,7]
m_cor_a_7_1 <- m_cor_a_1[1,8]
m_cor_a_8_1 <- m_cor_a_1[1,9]
m_cor_a_9_1 <- m_cor_a_1[1,10]
m_cor_a_10_1 <- m_cor_a_1[1,11]
m_cor_a_11_1 <- m_cor_a_1[1,12]
m_cor_a_12_1 <- m_cor_a_1[1,13]
m_cor_a_13_1 <- m_cor_a_1[1,14]
m_cor_a_14_1 <- m_cor_a_1[1,15]
m_cor_a_15_1 <- m_cor_a_1[1,16]
m_cor_a_16_1 <- m_cor_a_1[1,17]
m_cor_a_17_1 <- m_cor_a_1[1,18]
m_cor_a_18_1 <- m_cor_a_1[1,19]
m_cor_a_19_1 <- m_cor_a_1[1,20]
m_cor_a_20_1 <- m_cor_a_1[1,21]
m_cor_a_21_1 <- m_cor_a_1[1,22]
m_cor_a_22_1 <- m_cor_a_1[1,23]
m_cor_a_23_1 <- m_cor_a_1[1,24]
m_cor_a_24_1 <- m_cor_a_1[1,25]
m_cor_a_25_1 <- m_cor_a_1[1,26]
m_cor_a_26_1 <- m_cor_a_1[1,27]
m_cor_a_27_1 <- m_cor_a_1[1,28]
m_cor_a_28_1 <- m_cor_a_1[1,29]
m_cor_a_29_1 <- m_cor_a_1[1,30]
m_cor_a_30_1 <- m_cor_a_1[1,31]

m_cor_a_2 <- cor(m_cor_agre2, use = "pairwise", method = "pearson")
m_cor_a_4_2 <- m_cor_a_2[1,5]
m_cor_a_5_2 <- m_cor_a_2[1,6]
m_cor_a_6_2 <- m_cor_a_2[1,7]
m_cor_a_7_2 <- m_cor_a_2[1,8]
m_cor_a_8_2 <- m_cor_a_2[1,9]
m_cor_a_9_2 <- m_cor_a_2[1,10]
m_cor_a_10_2 <- m_cor_a_2[1,11]
m_cor_a_11_2 <- m_cor_a_2[1,12]
m_cor_a_12_2 <- m_cor_a_2[1,13]
m_cor_a_13_2 <- m_cor_a_2[1,14]
m_cor_a_14_2 <- m_cor_a_2[1,15]
m_cor_a_15_2 <- m_cor_a_2[1,16]
m_cor_a_16_2 <- m_cor_a_2[1,17]
m_cor_a_17_2 <- m_cor_a_2[1,18]
m_cor_a_18_2 <- m_cor_a_2[1,19]
m_cor_a_19_2 <- m_cor_a_2[1,20]
m_cor_a_20_2 <- m_cor_a_2[1,21]
m_cor_a_21_2 <- m_cor_a_2[1,22]
m_cor_a_22_2 <- m_cor_a_2[1,23]
m_cor_a_23_2 <- m_cor_a_2[1,24]
m_cor_a_24_2 <- m_cor_a_2[1,25]
m_cor_a_25_2 <- m_cor_a_2[1,26]
m_cor_a_26_2 <- m_cor_a_2[1,27]
m_cor_a_27_2 <- m_cor_a_2[1,28]
m_cor_a_28_2 <- m_cor_a_2[1,29]
m_cor_a_29_2 <- m_cor_a_2[1,30]
m_cor_a_30_2 <- m_cor_a_2[1,31]

m_cor_a_3 <- cor(m_cor_agre3, use = "pairwise", method = "pearson")
m_cor_a_4_3 <- m_cor_a_3[1,5]
m_cor_a_5_3 <- m_cor_a_3[1,6]
m_cor_a_6_3 <- m_cor_a_3[1,7]
m_cor_a_7_3 <- m_cor_a_3[1,8]
m_cor_a_8_3 <- m_cor_a_3[1,9]
m_cor_a_9_3 <- m_cor_a_3[1,10]
m_cor_a_10_3 <- m_cor_a_3[1,11]
m_cor_a_11_3 <- m_cor_a_3[1,12]
m_cor_a_12_3 <- m_cor_a_3[1,13]
m_cor_a_13_3 <- m_cor_a_3[1,14]
m_cor_a_14_3 <- m_cor_a_3[1,15]
m_cor_a_15_3 <- m_cor_a_3[1,16]
m_cor_a_16_3 <- m_cor_a_3[1,17]
m_cor_a_17_3 <- m_cor_a_3[1,18]
m_cor_a_18_3 <- m_cor_a_3[1,19]
m_cor_a_19_3 <- m_cor_a_3[1,20]
m_cor_a_20_3 <- m_cor_a_3[1,21]
m_cor_a_21_3 <- m_cor_a_3[1,22]
m_cor_a_22_3 <- m_cor_a_3[1,23]
m_cor_a_23_3 <- m_cor_a_3[1,24]
m_cor_a_24_3 <- m_cor_a_3[1,25]
m_cor_a_25_3 <- m_cor_a_3[1,26]
m_cor_a_26_3 <- m_cor_a_3[1,27]
m_cor_a_27_3 <- m_cor_a_3[1,28]
m_cor_a_28_3 <- m_cor_a_3[1,29]
m_cor_a_29_3 <- m_cor_a_3[1,30]
m_cor_a_30_3 <- m_cor_a_3[1,31]

m_cor_n_1 <- cor(m_cor_neur1, use = "pairwise", method = "pearson")
m_cor_n_4_1 <- m_cor_n_1[1,5]
m_cor_n_5_1 <- m_cor_n_1[1,6]
m_cor_n_6_1 <- m_cor_n_1[1,7]
m_cor_n_7_1 <- m_cor_n_1[1,8]
m_cor_n_8_1 <- m_cor_n_1[1,9]
m_cor_n_9_1 <- m_cor_n_1[1,10]
m_cor_n_10_1 <- m_cor_n_1[1,11]
m_cor_n_11_1 <- m_cor_n_1[1,12]
m_cor_n_12_1 <- m_cor_n_1[1,13]
m_cor_n_13_1 <- m_cor_n_1[1,14]
m_cor_n_14_1 <- m_cor_n_1[1,15]
m_cor_n_15_1 <- m_cor_n_1[1,16]
m_cor_n_16_1 <- m_cor_n_1[1,17]
m_cor_n_17_1 <- m_cor_n_1[1,18]
m_cor_n_18_1 <- m_cor_n_1[1,19]
m_cor_n_19_1 <- m_cor_n_1[1,20]
m_cor_n_20_1 <- m_cor_n_1[1,21]
m_cor_n_21_1 <- m_cor_n_1[1,22]
m_cor_n_22_1 <- m_cor_n_1[1,23]
m_cor_n_23_1 <- m_cor_n_1[1,24]
m_cor_n_24_1 <- m_cor_n_1[1,25]
m_cor_n_25_1 <- m_cor_n_1[1,26]
m_cor_n_26_1 <- m_cor_n_1[1,27]
m_cor_n_27_1 <- m_cor_n_1[1,28]
m_cor_n_28_1 <- m_cor_n_1[1,29]
m_cor_n_29_1 <- m_cor_n_1[1,30]
m_cor_n_30_1 <- m_cor_n_1[1,31]

m_cor_n_2 <- cor(m_cor_neur2, use = "pairwise", method = "pearson")
m_cor_n_4_2 <- m_cor_n_2[1,5]
m_cor_n_5_2 <- m_cor_n_2[1,6]
m_cor_n_6_2 <- m_cor_n_2[1,7]
m_cor_n_7_2 <- m_cor_n_2[1,8]
m_cor_n_8_2 <- m_cor_n_2[1,9]
m_cor_n_9_2 <- m_cor_n_2[1,10]
m_cor_n_10_2 <- m_cor_n_2[1,11]
m_cor_n_11_2 <- m_cor_n_2[1,12]
m_cor_n_12_2 <- m_cor_n_2[1,13]
m_cor_n_13_2 <- m_cor_n_2[1,14]
m_cor_n_14_2 <- m_cor_n_2[1,15]
m_cor_n_15_2 <- m_cor_n_2[1,16]
m_cor_n_16_2 <- m_cor_n_2[1,17]
m_cor_n_17_2 <- m_cor_n_2[1,18]
m_cor_n_18_2 <- m_cor_n_2[1,19]
m_cor_n_19_2 <- m_cor_n_2[1,20]
m_cor_n_20_2 <- m_cor_n_2[1,21]
m_cor_n_21_2 <- m_cor_n_2[1,22]
m_cor_n_22_2 <- m_cor_n_2[1,23]
m_cor_n_23_2 <- m_cor_n_2[1,24]
m_cor_n_24_2 <- m_cor_n_2[1,25]
m_cor_n_25_2 <- m_cor_n_2[1,26]
m_cor_n_26_2 <- m_cor_n_2[1,27]
m_cor_n_27_2 <- m_cor_n_2[1,28]
m_cor_n_28_2 <- m_cor_n_2[1,29]
m_cor_n_29_2 <- m_cor_n_2[1,30]
m_cor_n_30_2 <- m_cor_n_2[1,31]

m_cor_n_3 <- cor(m_cor_neur3, use = "pairwise", method = "pearson")
m_cor_n_4_3 <- m_cor_n_3[1,5]
m_cor_n_5_3 <- m_cor_n_3[1,6]
m_cor_n_6_3 <- m_cor_n_3[1,7]
m_cor_n_7_3 <- m_cor_n_3[1,8]
m_cor_n_8_3 <- m_cor_n_3[1,9]
m_cor_n_9_3 <- m_cor_n_3[1,10]
m_cor_n_10_3 <- m_cor_n_3[1,11]
m_cor_n_11_3 <- m_cor_n_3[1,12]
m_cor_n_12_3 <- m_cor_n_3[1,13]
m_cor_n_13_3 <- m_cor_n_3[1,14]
m_cor_n_14_3 <- m_cor_n_3[1,15]
m_cor_n_15_3 <- m_cor_n_3[1,16]
m_cor_n_16_3 <- m_cor_n_3[1,17]
m_cor_n_17_3 <- m_cor_n_3[1,18]
m_cor_n_18_3 <- m_cor_n_3[1,19]
m_cor_n_19_3 <- m_cor_n_3[1,20]
m_cor_n_20_3 <- m_cor_n_3[1,21]
m_cor_n_21_3 <- m_cor_n_3[1,22]
m_cor_n_22_3 <- m_cor_n_3[1,23]
m_cor_n_23_3 <- m_cor_n_3[1,24]
m_cor_n_24_3 <- m_cor_n_3[1,25]
m_cor_n_25_3 <- m_cor_n_3[1,26]
m_cor_n_26_3 <- m_cor_n_3[1,27]
m_cor_n_27_3 <- m_cor_n_3[1,28]
m_cor_n_28_3 <- m_cor_n_3[1,29]
m_cor_n_29_3 <- m_cor_n_3[1,30]
m_cor_n_30_3 <- m_cor_n_3[1,31]

# resid sd
# resid mean
for (i in 11:31){sd_cor_open1 <- resid.three.sd(sd_cor_open1, i)}
for (i in 11:31){sd_cor_cons1 <- resid.three.sd(sd_cor_cons1, i)}
for (i in 11:31){sd_cor_extr1 <- resid.three.sd(sd_cor_extr1, i)}
for (i in 11:31){sd_cor_agre1 <- resid.three.sd(sd_cor_agre1, i)}
for (i in 11:31){sd_cor_neur1 <- resid.three.sd(sd_cor_neur1, i)}

for (i in 11:31){sd_cor_open2 <- resid.three.sd(sd_cor_open2, i)}
for (i in 11:31){sd_cor_cons2 <- resid.three.sd(sd_cor_cons2, i)}
for (i in 11:31){sd_cor_extr2 <- resid.three.sd(sd_cor_extr2, i)}
for (i in 11:31){sd_cor_agre2 <- resid.three.sd(sd_cor_agre2, i)}
for (i in 11:31){sd_cor_neur2 <- resid.three.sd(sd_cor_neur2, i)}

for (i in 11:31){sd_cor_open3 <- resid.three.sd(sd_cor_open3, i)}
for (i in 11:31){sd_cor_cons3 <- resid.three.sd(sd_cor_cons3, i)}
for (i in 11:31){sd_cor_extr3 <- resid.three.sd(sd_cor_extr3, i)}
for (i in 11:31){sd_cor_agre3 <- resid.three.sd(sd_cor_agre3, i)}
for (i in 11:31){sd_cor_neur3 <- resid.three.sd(sd_cor_neur3, i)}


sd_cor_o_1 <- cor(sd_cor_open1, use = "pairwise", method = "pearson")
sd_cor_o_4_1 <- sd_cor_o_1[1,5]
sd_cor_o_5_1 <- sd_cor_o_1[1,6]
sd_cor_o_6_1 <- sd_cor_o_1[1,7]
sd_cor_o_7_1 <- sd_cor_o_1[1,8]
sd_cor_o_8_1 <- sd_cor_o_1[1,9]
sd_cor_o_9_1 <- sd_cor_o_1[1,10]
sd_cor_o_10_1 <- sd_cor_o_1[1,11]
sd_cor_o_11_1 <- sd_cor_o_1[1,12]
sd_cor_o_12_1 <- sd_cor_o_1[1,13]
sd_cor_o_13_1 <- sd_cor_o_1[1,14]
sd_cor_o_14_1 <- sd_cor_o_1[1,15]
sd_cor_o_15_1 <- sd_cor_o_1[1,16]
sd_cor_o_16_1 <- sd_cor_o_1[1,17]
sd_cor_o_17_1 <- sd_cor_o_1[1,18]
sd_cor_o_18_1 <- sd_cor_o_1[1,19]
sd_cor_o_19_1 <- sd_cor_o_1[1,20]
sd_cor_o_20_1 <- sd_cor_o_1[1,21]
sd_cor_o_21_1 <- sd_cor_o_1[1,22]
sd_cor_o_22_1 <- sd_cor_o_1[1,23]
sd_cor_o_23_1 <- sd_cor_o_1[1,24]
sd_cor_o_24_1 <- sd_cor_o_1[1,25]
sd_cor_o_25_1 <- sd_cor_o_1[1,26]
sd_cor_o_26_1 <- sd_cor_o_1[1,27]
sd_cor_o_27_1 <- sd_cor_o_1[1,28]
sd_cor_o_28_1 <- sd_cor_o_1[1,29]
sd_cor_o_29_1 <- sd_cor_o_1[1,30]
sd_cor_o_30_1 <- sd_cor_o_1[1,31]

sd_cor_o_2 <- cor(sd_cor_open2, use = "pairwise", method = "pearson")
sd_cor_o_4_2 <- sd_cor_o_2[1,5]
sd_cor_o_5_2 <- sd_cor_o_2[1,6]
sd_cor_o_6_2 <- sd_cor_o_2[1,7]
sd_cor_o_7_2 <- sd_cor_o_2[1,8]
sd_cor_o_8_2 <- sd_cor_o_2[1,9]
sd_cor_o_9_2 <- sd_cor_o_2[1,10]
sd_cor_o_10_2 <- sd_cor_o_2[1,11]
sd_cor_o_11_2 <- sd_cor_o_2[1,12]
sd_cor_o_12_2 <- sd_cor_o_2[1,13]
sd_cor_o_13_2 <- sd_cor_o_2[1,14]
sd_cor_o_14_2 <- sd_cor_o_2[1,15]
sd_cor_o_15_2 <- sd_cor_o_2[1,16]
sd_cor_o_16_2 <- sd_cor_o_2[1,17]
sd_cor_o_17_2 <- sd_cor_o_2[1,18]
sd_cor_o_18_2 <- sd_cor_o_2[1,19]
sd_cor_o_19_2 <- sd_cor_o_2[1,20]
sd_cor_o_20_2 <- sd_cor_o_2[1,21]
sd_cor_o_21_2 <- sd_cor_o_2[1,22]
sd_cor_o_22_2 <- sd_cor_o_2[1,23]
sd_cor_o_23_2 <- sd_cor_o_2[1,24]
sd_cor_o_24_2 <- sd_cor_o_2[1,25]
sd_cor_o_25_2 <- sd_cor_o_2[1,26]
sd_cor_o_26_2 <- sd_cor_o_2[1,27]
sd_cor_o_27_2 <- sd_cor_o_2[1,28]
sd_cor_o_28_2 <- sd_cor_o_2[1,29]
sd_cor_o_29_2 <- sd_cor_o_2[1,30]
sd_cor_o_30_2 <- sd_cor_o_2[1,31]

sd_cor_o_3 <- cor(sd_cor_open3, use = "pairwise", method = "pearson")
sd_cor_o_4_3 <- sd_cor_o_3[1,5]
sd_cor_o_5_3 <- sd_cor_o_3[1,6]
sd_cor_o_6_3 <- sd_cor_o_3[1,7]
sd_cor_o_7_3 <- sd_cor_o_3[1,8]
sd_cor_o_8_3 <- sd_cor_o_3[1,9]
sd_cor_o_9_3 <- sd_cor_o_3[1,10]
sd_cor_o_10_3 <- sd_cor_o_3[1,11]
sd_cor_o_11_3 <- sd_cor_o_3[1,12]
sd_cor_o_12_3 <- sd_cor_o_3[1,13]
sd_cor_o_13_3 <- sd_cor_o_3[1,14]
sd_cor_o_14_3 <- sd_cor_o_3[1,15]
sd_cor_o_15_3 <- sd_cor_o_3[1,16]
sd_cor_o_16_3 <- sd_cor_o_3[1,17]
sd_cor_o_17_3 <- sd_cor_o_3[1,18]
sd_cor_o_18_3 <- sd_cor_o_3[1,19]
sd_cor_o_19_3 <- sd_cor_o_3[1,20]
sd_cor_o_20_3 <- sd_cor_o_3[1,21]
sd_cor_o_21_3 <- sd_cor_o_3[1,22]
sd_cor_o_22_3 <- sd_cor_o_3[1,23]
sd_cor_o_23_3 <- sd_cor_o_3[1,24]
sd_cor_o_24_3 <- sd_cor_o_3[1,25]
sd_cor_o_25_3 <- sd_cor_o_3[1,26]
sd_cor_o_26_3 <- sd_cor_o_3[1,27]
sd_cor_o_27_3 <- sd_cor_o_3[1,28]
sd_cor_o_28_3 <- sd_cor_o_3[1,29]
sd_cor_o_29_3 <- sd_cor_o_3[1,30]
sd_cor_o_30_3 <- sd_cor_o_3[1,31]

sd_cor_c_1 <- cor(sd_cor_cons1, use = "pairwise", method = "pearson")
sd_cor_c_4_1 <- sd_cor_c_1[1,5]
sd_cor_c_5_1 <- sd_cor_c_1[1,6]
sd_cor_c_6_1 <- sd_cor_c_1[1,7]
sd_cor_c_7_1 <- sd_cor_c_1[1,8]
sd_cor_c_8_1 <- sd_cor_c_1[1,9]
sd_cor_c_9_1 <- sd_cor_c_1[1,10]
sd_cor_c_10_1 <- sd_cor_c_1[1,11]
sd_cor_c_11_1 <- sd_cor_c_1[1,12]
sd_cor_c_12_1 <- sd_cor_c_1[1,13]
sd_cor_c_13_1 <- sd_cor_c_1[1,14]
sd_cor_c_14_1 <- sd_cor_c_1[1,15]
sd_cor_c_15_1 <- sd_cor_c_1[1,16]
sd_cor_c_16_1 <- sd_cor_c_1[1,17]
sd_cor_c_17_1 <- sd_cor_c_1[1,18]
sd_cor_c_18_1 <- sd_cor_c_1[1,19]
sd_cor_c_19_1 <- sd_cor_c_1[1,20]
sd_cor_c_20_1 <- sd_cor_c_1[1,21]
sd_cor_c_21_1 <- sd_cor_c_1[1,22]
sd_cor_c_22_1 <- sd_cor_c_1[1,23]
sd_cor_c_23_1 <- sd_cor_c_1[1,24]
sd_cor_c_24_1 <- sd_cor_c_1[1,25]
sd_cor_c_25_1 <- sd_cor_c_1[1,26]
sd_cor_c_26_1 <- sd_cor_c_1[1,27]
sd_cor_c_27_1 <- sd_cor_c_1[1,28]
sd_cor_c_28_1 <- sd_cor_c_1[1,29]
sd_cor_c_29_1 <- sd_cor_c_1[1,30]
sd_cor_c_30_1 <- sd_cor_c_1[1,31]

sd_cor_c_2 <- cor(sd_cor_cons2, use = "pairwise", method = "pearson")
sd_cor_c_4_2 <- sd_cor_c_2[1,5]
sd_cor_c_5_2 <- sd_cor_c_2[1,6]
sd_cor_c_6_2 <- sd_cor_c_2[1,7]
sd_cor_c_7_2 <- sd_cor_c_2[1,8]
sd_cor_c_8_2 <- sd_cor_c_2[1,9]
sd_cor_c_9_2 <- sd_cor_c_2[1,10]
sd_cor_c_10_2 <- sd_cor_c_2[1,11]
sd_cor_c_11_2 <- sd_cor_c_2[1,12]
sd_cor_c_12_2 <- sd_cor_c_2[1,13]
sd_cor_c_13_2 <- sd_cor_c_2[1,14]
sd_cor_c_14_2 <- sd_cor_c_2[1,15]
sd_cor_c_15_2 <- sd_cor_c_2[1,16]
sd_cor_c_16_2 <- sd_cor_c_2[1,17]
sd_cor_c_17_2 <- sd_cor_c_2[1,18]
sd_cor_c_18_2 <- sd_cor_c_2[1,19]
sd_cor_c_19_2 <- sd_cor_c_2[1,20]
sd_cor_c_20_2 <- sd_cor_c_2[1,21]
sd_cor_c_21_2 <- sd_cor_c_2[1,22]
sd_cor_c_22_2 <- sd_cor_c_2[1,23]
sd_cor_c_23_2 <- sd_cor_c_2[1,24]
sd_cor_c_24_2 <- sd_cor_c_2[1,25]
sd_cor_c_25_2 <- sd_cor_c_2[1,26]
sd_cor_c_26_2 <- sd_cor_c_2[1,27]
sd_cor_c_27_2 <- sd_cor_c_2[1,28]
sd_cor_c_28_2 <- sd_cor_c_2[1,29]
sd_cor_c_29_2 <- sd_cor_c_2[1,30]
sd_cor_c_30_2 <- sd_cor_c_2[1,31]

sd_cor_c_3 <- cor(sd_cor_cons3, use = "pairwise", method = "pearson")
sd_cor_c_4_3 <- sd_cor_c_3[1,5]
sd_cor_c_5_3 <- sd_cor_c_3[1,6]
sd_cor_c_6_3 <- sd_cor_c_3[1,7]
sd_cor_c_7_3 <- sd_cor_c_3[1,8]
sd_cor_c_8_3 <- sd_cor_c_3[1,9]
sd_cor_c_9_3 <- sd_cor_c_3[1,10]
sd_cor_c_10_3 <- sd_cor_c_3[1,11]
sd_cor_c_11_3 <- sd_cor_c_3[1,12]
sd_cor_c_12_3 <- sd_cor_c_3[1,13]
sd_cor_c_13_3 <- sd_cor_c_3[1,14]
sd_cor_c_14_3 <- sd_cor_c_3[1,15]
sd_cor_c_15_3 <- sd_cor_c_3[1,16]
sd_cor_c_16_3 <- sd_cor_c_3[1,17]
sd_cor_c_17_3 <- sd_cor_c_3[1,18]
sd_cor_c_18_3 <- sd_cor_c_3[1,19]
sd_cor_c_19_3 <- sd_cor_c_3[1,20]
sd_cor_c_20_3 <- sd_cor_c_3[1,21]
sd_cor_c_21_3 <- sd_cor_c_3[1,22]
sd_cor_c_22_3 <- sd_cor_c_3[1,23]
sd_cor_c_23_3 <- sd_cor_c_3[1,24]
sd_cor_c_24_3 <- sd_cor_c_3[1,25]
sd_cor_c_25_3 <- sd_cor_c_3[1,26]
sd_cor_c_26_3 <- sd_cor_c_3[1,27]
sd_cor_c_27_3 <- sd_cor_c_3[1,28]
sd_cor_c_28_3 <- sd_cor_c_3[1,29]
sd_cor_c_29_3 <- sd_cor_c_3[1,30]
sd_cor_c_30_3 <- sd_cor_c_3[1,31]

sd_cor_e_1 <- cor(sd_cor_extr1, use = "pairwise", method = "pearson")
sd_cor_e_4_1 <- sd_cor_e_1[1,5]
sd_cor_e_5_1 <- sd_cor_e_1[1,6]
sd_cor_e_6_1 <- sd_cor_e_1[1,7]
sd_cor_e_7_1 <- sd_cor_e_1[1,8]
sd_cor_e_8_1 <- sd_cor_e_1[1,9]
sd_cor_e_9_1 <- sd_cor_e_1[1,10]
sd_cor_e_10_1 <- sd_cor_e_1[1,11]
sd_cor_e_11_1 <- sd_cor_e_1[1,12]
sd_cor_e_12_1 <- sd_cor_e_1[1,13]
sd_cor_e_13_1 <- sd_cor_e_1[1,14]
sd_cor_e_14_1 <- sd_cor_e_1[1,15]
sd_cor_e_15_1 <- sd_cor_e_1[1,16]
sd_cor_e_16_1 <- sd_cor_e_1[1,17]
sd_cor_e_17_1 <- sd_cor_e_1[1,18]
sd_cor_e_18_1 <- sd_cor_e_1[1,19]
sd_cor_e_19_1 <- sd_cor_e_1[1,20]
sd_cor_e_20_1 <- sd_cor_e_1[1,21]
sd_cor_e_21_1 <- sd_cor_e_1[1,22]
sd_cor_e_22_1 <- sd_cor_e_1[1,23]
sd_cor_e_23_1 <- sd_cor_e_1[1,24]
sd_cor_e_24_1 <- sd_cor_e_1[1,25]
sd_cor_e_25_1 <- sd_cor_e_1[1,26]
sd_cor_e_26_1 <- sd_cor_e_1[1,27]
sd_cor_e_27_1 <- sd_cor_e_1[1,28]
sd_cor_e_28_1 <- sd_cor_e_1[1,29]
sd_cor_e_29_1 <- sd_cor_e_1[1,30]
sd_cor_e_30_1 <- sd_cor_e_1[1,31]

sd_cor_e_2 <- cor(sd_cor_extr2, use = "pairwise", method = "pearson")
sd_cor_e_4_2 <- sd_cor_e_2[1,5]
sd_cor_e_5_2 <- sd_cor_e_2[1,6]
sd_cor_e_6_2 <- sd_cor_e_2[1,7]
sd_cor_e_7_2 <- sd_cor_e_2[1,8]
sd_cor_e_8_2 <- sd_cor_e_2[1,9]
sd_cor_e_9_2 <- sd_cor_e_2[1,10]
sd_cor_e_10_2 <- sd_cor_e_2[1,11]
sd_cor_e_11_2 <- sd_cor_e_2[1,12]
sd_cor_e_12_2 <- sd_cor_e_2[1,13]
sd_cor_e_13_2 <- sd_cor_e_2[1,14]
sd_cor_e_14_2 <- sd_cor_e_2[1,15]
sd_cor_e_15_2 <- sd_cor_e_2[1,16]
sd_cor_e_16_2 <- sd_cor_e_2[1,17]
sd_cor_e_17_2 <- sd_cor_e_2[1,18]
sd_cor_e_18_2 <- sd_cor_e_2[1,19]
sd_cor_e_19_2 <- sd_cor_e_2[1,20]
sd_cor_e_20_2 <- sd_cor_e_2[1,21]
sd_cor_e_21_2 <- sd_cor_e_2[1,22]
sd_cor_e_22_2 <- sd_cor_e_2[1,23]
sd_cor_e_23_2 <- sd_cor_e_2[1,24]
sd_cor_e_24_2 <- sd_cor_e_2[1,25]
sd_cor_e_25_2 <- sd_cor_e_2[1,26]
sd_cor_e_26_2 <- sd_cor_e_2[1,27]
sd_cor_e_27_2 <- sd_cor_e_2[1,28]
sd_cor_e_28_2 <- sd_cor_e_2[1,29]
sd_cor_e_29_2 <- sd_cor_e_2[1,30]
sd_cor_e_30_2 <- sd_cor_e_2[1,31]

sd_cor_e_3 <- cor(sd_cor_extr3, use = "pairwise", method = "pearson")
sd_cor_e_4_3 <- sd_cor_e_3[1,5]
sd_cor_e_5_3 <- sd_cor_e_3[1,6]
sd_cor_e_6_3 <- sd_cor_e_3[1,7]
sd_cor_e_7_3 <- sd_cor_e_3[1,8]
sd_cor_e_8_3 <- sd_cor_e_3[1,9]
sd_cor_e_9_3 <- sd_cor_e_3[1,10]
sd_cor_e_10_3 <- sd_cor_e_3[1,11]
sd_cor_e_11_3 <- sd_cor_e_3[1,12]
sd_cor_e_12_3 <- sd_cor_e_3[1,13]
sd_cor_e_13_3 <- sd_cor_e_3[1,14]
sd_cor_e_14_3 <- sd_cor_e_3[1,15]
sd_cor_e_15_3 <- sd_cor_e_3[1,16]
sd_cor_e_16_3 <- sd_cor_e_3[1,17]
sd_cor_e_17_3 <- sd_cor_e_3[1,18]
sd_cor_e_18_3 <- sd_cor_e_3[1,19]
sd_cor_e_19_3 <- sd_cor_e_3[1,20]
sd_cor_e_20_3 <- sd_cor_e_3[1,21]
sd_cor_e_21_3 <- sd_cor_e_3[1,22]
sd_cor_e_22_3 <- sd_cor_e_3[1,23]
sd_cor_e_23_3 <- sd_cor_e_3[1,24]
sd_cor_e_24_3 <- sd_cor_e_3[1,25]
sd_cor_e_25_3 <- sd_cor_e_3[1,26]
sd_cor_e_26_3 <- sd_cor_e_3[1,27]
sd_cor_e_27_3 <- sd_cor_e_3[1,28]
sd_cor_e_28_3 <- sd_cor_e_3[1,29]
sd_cor_e_29_3 <- sd_cor_e_3[1,30]
sd_cor_e_30_3 <- sd_cor_e_3[1,31]

sd_cor_a_1 <- cor(sd_cor_agre1, use = "pairwise", method = "pearson")
sd_cor_a_4_1 <- sd_cor_a_1[1,5]
sd_cor_a_5_1 <- sd_cor_a_1[1,6]
sd_cor_a_6_1 <- sd_cor_a_1[1,7]
sd_cor_a_7_1 <- sd_cor_a_1[1,8]
sd_cor_a_8_1 <- sd_cor_a_1[1,9]
sd_cor_a_9_1 <- sd_cor_a_1[1,10]
sd_cor_a_10_1 <- sd_cor_a_1[1,11]
sd_cor_a_11_1 <- sd_cor_a_1[1,12]
sd_cor_a_12_1 <- sd_cor_a_1[1,13]
sd_cor_a_13_1 <- sd_cor_a_1[1,14]
sd_cor_a_14_1 <- sd_cor_a_1[1,15]
sd_cor_a_15_1 <- sd_cor_a_1[1,16]
sd_cor_a_16_1 <- sd_cor_a_1[1,17]
sd_cor_a_17_1 <- sd_cor_a_1[1,18]
sd_cor_a_18_1 <- sd_cor_a_1[1,19]
sd_cor_a_19_1 <- sd_cor_a_1[1,20]
sd_cor_a_20_1 <- sd_cor_a_1[1,21]
sd_cor_a_21_1 <- sd_cor_a_1[1,22]
sd_cor_a_22_1 <- sd_cor_a_1[1,23]
sd_cor_a_23_1 <- sd_cor_a_1[1,24]
sd_cor_a_24_1 <- sd_cor_a_1[1,25]
sd_cor_a_25_1 <- sd_cor_a_1[1,26]
sd_cor_a_26_1 <- sd_cor_a_1[1,27]
sd_cor_a_27_1 <- sd_cor_a_1[1,28]
sd_cor_a_28_1 <- sd_cor_a_1[1,29]
sd_cor_a_29_1 <- sd_cor_a_1[1,30]
sd_cor_a_30_1 <- sd_cor_a_1[1,31]

sd_cor_a_2 <- cor(sd_cor_agre2, use = "pairwise", method = "pearson")
sd_cor_a_4_2 <- sd_cor_a_2[1,5]
sd_cor_a_5_2 <- sd_cor_a_2[1,6]
sd_cor_a_6_2 <- sd_cor_a_2[1,7]
sd_cor_a_7_2 <- sd_cor_a_2[1,8]
sd_cor_a_8_2 <- sd_cor_a_2[1,9]
sd_cor_a_9_2 <- sd_cor_a_2[1,10]
sd_cor_a_10_2 <- sd_cor_a_2[1,11]
sd_cor_a_11_2 <- sd_cor_a_2[1,12]
sd_cor_a_12_2 <- sd_cor_a_2[1,13]
sd_cor_a_13_2 <- sd_cor_a_2[1,14]
sd_cor_a_14_2 <- sd_cor_a_2[1,15]
sd_cor_a_15_2 <- sd_cor_a_2[1,16]
sd_cor_a_16_2 <- sd_cor_a_2[1,17]
sd_cor_a_17_2 <- sd_cor_a_2[1,18]
sd_cor_a_18_2 <- sd_cor_a_2[1,19]
sd_cor_a_19_2 <- sd_cor_a_2[1,20]
sd_cor_a_20_2 <- sd_cor_a_2[1,21]
sd_cor_a_21_2 <- sd_cor_a_2[1,22]
sd_cor_a_22_2 <- sd_cor_a_2[1,23]
sd_cor_a_23_2 <- sd_cor_a_2[1,24]
sd_cor_a_24_2 <- sd_cor_a_2[1,25]
sd_cor_a_25_2 <- sd_cor_a_2[1,26]
sd_cor_a_26_2 <- sd_cor_a_2[1,27]
sd_cor_a_27_2 <- sd_cor_a_2[1,28]
sd_cor_a_28_2 <- sd_cor_a_2[1,29]
sd_cor_a_29_2 <- sd_cor_a_2[1,30]
sd_cor_a_30_2 <- sd_cor_a_2[1,31]

sd_cor_a_3 <- cor(sd_cor_agre3, use = "pairwise", method = "pearson")
sd_cor_a_4_3 <- sd_cor_a_3[1,5]
sd_cor_a_5_3 <- sd_cor_a_3[1,6]
sd_cor_a_6_3 <- sd_cor_a_3[1,7]
sd_cor_a_7_3 <- sd_cor_a_3[1,8]
sd_cor_a_8_3 <- sd_cor_a_3[1,9]
sd_cor_a_9_3 <- sd_cor_a_3[1,10]
sd_cor_a_10_3 <- sd_cor_a_3[1,11]
sd_cor_a_11_3 <- sd_cor_a_3[1,12]
sd_cor_a_12_3 <- sd_cor_a_3[1,13]
sd_cor_a_13_3 <- sd_cor_a_3[1,14]
sd_cor_a_14_3 <- sd_cor_a_3[1,15]
sd_cor_a_15_3 <- sd_cor_a_3[1,16]
sd_cor_a_16_3 <- sd_cor_a_3[1,17]
sd_cor_a_17_3 <- sd_cor_a_3[1,18]
sd_cor_a_18_3 <- sd_cor_a_3[1,19]
sd_cor_a_19_3 <- sd_cor_a_3[1,20]
sd_cor_a_20_3 <- sd_cor_a_3[1,21]
sd_cor_a_21_3 <- sd_cor_a_3[1,22]
sd_cor_a_22_3 <- sd_cor_a_3[1,23]
sd_cor_a_23_3 <- sd_cor_a_3[1,24]
sd_cor_a_24_3 <- sd_cor_a_3[1,25]
sd_cor_a_25_3 <- sd_cor_a_3[1,26]
sd_cor_a_26_3 <- sd_cor_a_3[1,27]
sd_cor_a_27_3 <- sd_cor_a_3[1,28]
sd_cor_a_28_3 <- sd_cor_a_3[1,29]
sd_cor_a_29_3 <- sd_cor_a_3[1,30]
sd_cor_a_30_3 <- sd_cor_a_3[1,31]

sd_cor_n_1 <- cor(sd_cor_neur1, use = "pairwise", method = "pearson")
sd_cor_n_4_1 <- sd_cor_n_1[1,5]
sd_cor_n_5_1 <- sd_cor_n_1[1,6]
sd_cor_n_6_1 <- sd_cor_n_1[1,7]
sd_cor_n_7_1 <- sd_cor_n_1[1,8]
sd_cor_n_8_1 <- sd_cor_n_1[1,9]
sd_cor_n_9_1 <- sd_cor_n_1[1,10]
sd_cor_n_10_1 <- sd_cor_n_1[1,11]
sd_cor_n_11_1 <- sd_cor_n_1[1,12]
sd_cor_n_12_1 <- sd_cor_n_1[1,13]
sd_cor_n_13_1 <- sd_cor_n_1[1,14]
sd_cor_n_14_1 <- sd_cor_n_1[1,15]
sd_cor_n_15_1 <- sd_cor_n_1[1,16]
sd_cor_n_16_1 <- sd_cor_n_1[1,17]
sd_cor_n_17_1 <- sd_cor_n_1[1,18]
sd_cor_n_18_1 <- sd_cor_n_1[1,19]
sd_cor_n_19_1 <- sd_cor_n_1[1,20]
sd_cor_n_20_1 <- sd_cor_n_1[1,21]
sd_cor_n_21_1 <- sd_cor_n_1[1,22]
sd_cor_n_22_1 <- sd_cor_n_1[1,23]
sd_cor_n_23_1 <- sd_cor_n_1[1,24]
sd_cor_n_24_1 <- sd_cor_n_1[1,25]
sd_cor_n_25_1 <- sd_cor_n_1[1,26]
sd_cor_n_26_1 <- sd_cor_n_1[1,27]
sd_cor_n_27_1 <- sd_cor_n_1[1,28]
sd_cor_n_28_1 <- sd_cor_n_1[1,29]
sd_cor_n_29_1 <- sd_cor_n_1[1,30]
sd_cor_n_30_1 <- sd_cor_n_1[1,31]

sd_cor_n_2 <- cor(sd_cor_neur2, use = "pairwise", method = "pearson")
sd_cor_n_4_2 <- sd_cor_n_2[1,5]
sd_cor_n_5_2 <- sd_cor_n_2[1,6]
sd_cor_n_6_2 <- sd_cor_n_2[1,7]
sd_cor_n_7_2 <- sd_cor_n_2[1,8]
sd_cor_n_8_2 <- sd_cor_n_2[1,9]
sd_cor_n_9_2 <- sd_cor_n_2[1,10]
sd_cor_n_10_2 <- sd_cor_n_2[1,11]
sd_cor_n_11_2 <- sd_cor_n_2[1,12]
sd_cor_n_12_2 <- sd_cor_n_2[1,13]
sd_cor_n_13_2 <- sd_cor_n_2[1,14]
sd_cor_n_14_2 <- sd_cor_n_2[1,15]
sd_cor_n_15_2 <- sd_cor_n_2[1,16]
sd_cor_n_16_2 <- sd_cor_n_2[1,17]
sd_cor_n_17_2 <- sd_cor_n_2[1,18]
sd_cor_n_18_2 <- sd_cor_n_2[1,19]
sd_cor_n_19_2 <- sd_cor_n_2[1,20]
sd_cor_n_20_2 <- sd_cor_n_2[1,21]
sd_cor_n_21_2 <- sd_cor_n_2[1,22]
sd_cor_n_22_2 <- sd_cor_n_2[1,23]
sd_cor_n_23_2 <- sd_cor_n_2[1,24]
sd_cor_n_24_2 <- sd_cor_n_2[1,25]
sd_cor_n_25_2 <- sd_cor_n_2[1,26]
sd_cor_n_26_2 <- sd_cor_n_2[1,27]
sd_cor_n_27_2 <- sd_cor_n_2[1,28]
sd_cor_n_28_2 <- sd_cor_n_2[1,29]
sd_cor_n_29_2 <- sd_cor_n_2[1,30]
sd_cor_n_30_2 <- sd_cor_n_2[1,31]

sd_cor_n_3 <- cor(sd_cor_neur3, use = "pairwise", method = "pearson")
sd_cor_n_4_3 <- sd_cor_n_3[1,5]
sd_cor_n_5_3 <- sd_cor_n_3[1,6]
sd_cor_n_6_3 <- sd_cor_n_3[1,7]
sd_cor_n_7_3 <- sd_cor_n_3[1,8]
sd_cor_n_8_3 <- sd_cor_n_3[1,9]
sd_cor_n_9_3 <- sd_cor_n_3[1,10]
sd_cor_n_10_3 <- sd_cor_n_3[1,11]
sd_cor_n_11_3 <- sd_cor_n_3[1,12]
sd_cor_n_12_3 <- sd_cor_n_3[1,13]
sd_cor_n_13_3 <- sd_cor_n_3[1,14]
sd_cor_n_14_3 <- sd_cor_n_3[1,15]
sd_cor_n_15_3 <- sd_cor_n_3[1,16]
sd_cor_n_16_3 <- sd_cor_n_3[1,17]
sd_cor_n_17_3 <- sd_cor_n_3[1,18]
sd_cor_n_18_3 <- sd_cor_n_3[1,19]
sd_cor_n_19_3 <- sd_cor_n_3[1,20]
sd_cor_n_20_3 <- sd_cor_n_3[1,21]
sd_cor_n_21_3 <- sd_cor_n_3[1,22]
sd_cor_n_22_3 <- sd_cor_n_3[1,23]
sd_cor_n_23_3 <- sd_cor_n_3[1,24]
sd_cor_n_24_3 <- sd_cor_n_3[1,25]
sd_cor_n_25_3 <- sd_cor_n_3[1,26]
sd_cor_n_26_3 <- sd_cor_n_3[1,27]
sd_cor_n_27_3 <- sd_cor_n_3[1,28]
sd_cor_n_28_3 <- sd_cor_n_3[1,29]
sd_cor_n_29_3 <- sd_cor_n_3[1,30]
sd_cor_n_30_3 <- sd_cor_n_3[1,31]

### put into dataframe
n <- c(872,99,193)
meta <- data.frame(n = n,
                   o_P.Mean = c(cor_o_1_1, cor_o_1_2, cor_o_1_3), o_N.Mean = c(cor_o_2_1, cor_o_2_2, cor_o_2_3),
                   o_H.Mean = c(cor_o_3_1, cor_o_3_2, cor_o_3_3), o_P.SD = c(cor_o_4_1, cor_o_4_2, cor_o_4_3),
                   o_N.SD = c(cor_o_5_1, cor_o_5_2, cor_o_5_3), o_H.SD= c(cor_o_6_1, cor_o_6_2, cor_o_6_3),
                   o_P.rSD = c(cor_o_7_1, cor_o_7_2, cor_o_7_3), o_N.rSD = c(cor_o_8_1, cor_o_8_2, cor_o_8_3),
                   o_H.rSD = c(cor_o_9_1, cor_o_9_2, cor_o_9_3), o_P.MSSD = c(cor_o_10_1, cor_o_10_2, cor_o_10_3),
                   o_N.MSSD = c(cor_o_11_1, cor_o_11_2, cor_o_11_3), o_H.MSSD = c(cor_o_12_1, cor_o_12_2, cor_o_12_3),
                   o_P.rMSSD = c(cor_o_13_1, cor_o_13_2, cor_o_13_3), o_N.rMSSD = c(cor_o_14_1, cor_o_14_2, cor_o_14_3),
                   o_H.rMSSD = c(cor_o_15_1, cor_o_15_2, cor_o_15_3), o_P.ALPHA = c(cor_o_16_1, cor_o_16_2, cor_o_16_3),
                   o_N.ALPHA = c(cor_o_17_1, cor_o_17_2, cor_o_17_3), o_H.ALPHA = c(cor_o_18_1, cor_o_18_2, cor_o_18_3),
                   o_pP = c(cor_o_19_1, cor_o_19_2, cor_o_19_3), o_nN = c(cor_o_20_1, cor_o_20_2, cor_o_20_3),
                   o_hH = c(cor_o_21_1, cor_o_21_2, cor_o_21_3), o_pN = c(cor_o_22_1, cor_o_22_2, cor_o_22_3),
                   o_pH = c(cor_o_23_1, cor_o_23_2, cor_o_23_3), o_nP = c(cor_o_24_1, cor_o_24_2, cor_o_24_3),
                   o_nH = c(cor_o_25_1, cor_o_25_2, cor_o_25_3), o_hP = c(cor_o_26_1, cor_o_26_2, cor_o_26_3),
                   o_hN = c(cor_o_27_1, cor_o_27_2, cor_o_27_3), o_P_N = c(cor_o_28_1, cor_o_28_2, cor_o_28_3),
                   o_P_H = c(cor_o_29_1, cor_o_29_2, cor_o_29_3), o_N_H = c(cor_o_30_1, cor_o_30_2, cor_o_30_3),
                   
                   m_o_P.SD = c(m_cor_o_4_1, m_cor_o_4_2, m_cor_o_4_3),
                   m_o_N.SD = c(m_cor_o_5_1, m_cor_o_5_2, m_cor_o_5_3), m_o_H.SD= c(m_cor_o_6_1, m_cor_o_6_2, m_cor_o_6_3),
                   m_o_P.rSD = c(m_cor_o_7_1, m_cor_o_7_2, m_cor_o_7_3), m_o_N.rSD = c(m_cor_o_8_1, m_cor_o_8_2, m_cor_o_8_3),
                   m_o_H.rSD = c(m_cor_o_9_1, m_cor_o_9_2, m_cor_o_9_3), m_o_P.MSSD = c(m_cor_o_10_1, m_cor_o_10_2, m_cor_o_10_3),
                   m_o_N.MSSD = c(m_cor_o_11_1, m_cor_o_11_2, m_cor_o_11_3), m_o_H.MSSD = c(m_cor_o_12_1, m_cor_o_12_2, m_cor_o_12_3),
                   m_o_P.rMSSD = c(m_cor_o_13_1, m_cor_o_13_2, m_cor_o_13_3), m_o_N.rMSSD = c(m_cor_o_14_1, m_cor_o_14_2, m_cor_o_14_3),
                   m_o_H.rMSSD = c(m_cor_o_15_1, m_cor_o_15_2, m_cor_o_15_3), m_o_P.ALPHA = c(m_cor_o_16_1, m_cor_o_16_2, m_cor_o_16_3),
                   m_o_N.ALPHA = c(m_cor_o_17_1, m_cor_o_17_2, m_cor_o_17_3), m_o_H.ALPHA = c(m_cor_o_18_1, m_cor_o_18_2, m_cor_o_18_3),
                   m_o_pP = c(m_cor_o_19_1, m_cor_o_19_2, m_cor_o_19_3), m_o_nN = c(m_cor_o_20_1, m_cor_o_20_2, m_cor_o_20_3),
                   m_o_hH = c(m_cor_o_21_1, m_cor_o_21_2, m_cor_o_21_3), m_o_pN = c(m_cor_o_22_1, m_cor_o_22_2, m_cor_o_22_3),
                   m_o_pH = c(m_cor_o_23_1, m_cor_o_23_2, m_cor_o_23_3), m_o_nP = c(m_cor_o_24_1, m_cor_o_24_2, m_cor_o_24_3),
                   m_o_nH = c(m_cor_o_25_1, m_cor_o_25_2, m_cor_o_25_3), m_o_hP = c(m_cor_o_26_1, m_cor_o_26_2, m_cor_o_26_3),
                   m_o_hN = c(m_cor_o_27_1, m_cor_o_27_2, m_cor_o_27_3), m_o_P_N = c(m_cor_o_28_1, m_cor_o_28_2, m_cor_o_28_3),
                   m_o_P_H = c(m_cor_o_29_1, m_cor_o_29_2, m_cor_o_29_3), m_o_N_H = c(m_cor_o_30_1, m_cor_o_30_2, m_cor_o_30_3),
                   
                   sd_o_P.MSSD = c(sd_cor_o_10_1, sd_cor_o_10_2, sd_cor_o_10_3),
                   sd_o_N.MSSD = c(sd_cor_o_11_1, sd_cor_o_11_2, sd_cor_o_11_3), sd_o_H.MSSD = c(sd_cor_o_12_1, sd_cor_o_12_2, sd_cor_o_12_3),
                   sd_o_P.rMSSD = c(sd_cor_o_13_1, sd_cor_o_13_2, sd_cor_o_13_3), sd_o_N.rMSSD = c(sd_cor_o_14_1, sd_cor_o_14_2, sd_cor_o_14_3),
                   sd_o_H.rMSSD = c(sd_cor_o_15_1, sd_cor_o_15_2, sd_cor_o_15_3), sd_o_P.ALPHA = c(sd_cor_o_16_1, sd_cor_o_16_2, sd_cor_o_16_3),
                   sd_o_N.ALPHA = c(sd_cor_o_17_1, sd_cor_o_17_2, sd_cor_o_17_3), sd_o_H.ALPHA = c(sd_cor_o_18_1, sd_cor_o_18_2, sd_cor_o_18_3),
                   sd_o_pP = c(sd_cor_o_19_1, sd_cor_o_19_2, sd_cor_o_19_3), sd_o_nN = c(sd_cor_o_20_1, sd_cor_o_20_2, sd_cor_o_20_3),
                   sd_o_hH = c(sd_cor_o_21_1, sd_cor_o_21_2, sd_cor_o_21_3), sd_o_pN = c(sd_cor_o_22_1, sd_cor_o_22_2, sd_cor_o_22_3),
                   sd_o_pH = c(sd_cor_o_23_1, sd_cor_o_23_2, sd_cor_o_23_3), sd_o_nP = c(sd_cor_o_24_1, sd_cor_o_24_2, sd_cor_o_24_3),
                   sd_o_nH = c(sd_cor_o_25_1, sd_cor_o_25_2, sd_cor_o_25_3), sd_o_hP = c(sd_cor_o_26_1, sd_cor_o_26_2, sd_cor_o_26_3),
                   sd_o_hN = c(sd_cor_o_27_1, sd_cor_o_27_2, sd_cor_o_27_3), sd_o_P_N = c(sd_cor_o_28_1, sd_cor_o_28_2, sd_cor_o_28_3),
                   sd_o_P_H = c(sd_cor_o_29_1, sd_cor_o_29_2, sd_cor_o_29_3), sd_o_N_H = c(sd_cor_o_30_1, sd_cor_o_30_2, sd_cor_o_30_3),
                   
                   c_P.Mean = c(cor_c_1_1, cor_c_1_2, cor_c_1_3), c_N.Mean = c(cor_c_2_1, cor_c_2_2, cor_c_2_3),
                   c_H.Mean = c(cor_c_3_1, cor_c_3_2, cor_c_3_3), c_P.SD = c(cor_c_4_1, cor_c_4_2, cor_c_4_3),
                   c_N.SD = c(cor_c_5_1, cor_c_5_2, cor_c_5_3), c_H.SD= c(cor_c_6_1, cor_c_6_2, cor_c_6_3),
                   c_P.rSD = c(cor_c_7_1, cor_c_7_2, cor_c_7_3), c_N.rSD = c(cor_c_8_1, cor_c_8_2, cor_c_8_3),
                   c_H.rSD = c(cor_c_9_1, cor_c_9_2, cor_c_9_3), c_P.MSSD = c(cor_c_10_1, cor_c_10_2, cor_c_10_3),
                   c_N.MSSD = c(cor_c_11_1, cor_c_11_2, cor_c_11_3), c_H.MSSD = c(cor_c_12_1, cor_c_12_2, cor_c_12_3),
                   c_P.rMSSD = c(cor_c_13_1, cor_c_13_2, cor_c_13_3), c_N.rMSSD = c(cor_c_14_1, cor_c_14_2, cor_c_14_3),
                   c_H.rMSSD = c(cor_c_15_1, cor_c_15_2, cor_c_15_3), c_P.ALPHA = c(cor_c_16_1, cor_c_16_2, cor_c_16_3),
                   c_N.ALPHA = c(cor_c_17_1, cor_c_17_2, cor_c_17_3), c_H.ALPHA = c(cor_c_18_1, cor_c_18_2, cor_c_18_3),
                   c_pP = c(cor_c_19_1, cor_c_19_2, cor_c_19_3), c_nN = c(cor_c_20_1, cor_c_20_2, cor_c_20_3),
                   c_hH = c(cor_c_21_1, cor_c_21_2, cor_c_21_3), c_pN = c(cor_c_22_1, cor_c_22_2, cor_c_22_3),
                   c_pH = c(cor_c_23_1, cor_c_23_2, cor_c_23_3), c_nP = c(cor_c_24_1, cor_c_24_2, cor_c_24_3),
                   c_nH = c(cor_c_25_1, cor_c_25_2, cor_c_25_3), c_hP = c(cor_c_26_1, cor_c_26_2, cor_c_26_3),
                   c_hN = c(cor_c_27_1, cor_c_27_2, cor_c_27_3), c_P_N = c(cor_c_28_1, cor_c_28_2, cor_c_28_3),
                   c_P_H = c(cor_c_29_1, cor_c_29_2, cor_c_29_3), c_N_H = c(cor_c_30_1, cor_c_30_2, cor_c_30_3),
                   
                   m_c_P.SD = c(m_cor_c_4_1, m_cor_c_4_2, m_cor_c_4_3),
                   m_c_N.SD = c(m_cor_c_5_1, m_cor_c_5_2, m_cor_c_5_3), m_c_H.SD= c(m_cor_c_6_1, m_cor_c_6_2, m_cor_c_6_3),
                   m_c_P.rSD = c(m_cor_c_7_1, m_cor_c_7_2, m_cor_c_7_3), m_c_N.rSD = c(m_cor_c_8_1, m_cor_c_8_2, m_cor_c_8_3),
                   m_c_H.rSD = c(m_cor_c_9_1, m_cor_c_9_2, m_cor_c_9_3), m_c_P.MSSD = c(m_cor_c_10_1, m_cor_c_10_2, m_cor_c_10_3),
                   m_c_N.MSSD = c(m_cor_c_11_1, m_cor_c_11_2, m_cor_c_11_3), m_c_H.MSSD = c(m_cor_c_12_1, m_cor_c_12_2, m_cor_c_12_3),
                   m_c_P.rMSSD = c(m_cor_c_13_1, m_cor_c_13_2, m_cor_c_13_3), m_c_N.rMSSD = c(m_cor_c_14_1, m_cor_c_14_2, m_cor_c_14_3),
                   m_c_H.rMSSD = c(m_cor_c_15_1, m_cor_c_15_2, m_cor_c_15_3), m_c_P.ALPHA = c(m_cor_c_16_1, m_cor_c_16_2, m_cor_c_16_3),
                   m_c_N.ALPHA = c(m_cor_c_17_1, m_cor_c_17_2, m_cor_c_17_3), m_c_H.ALPHA = c(m_cor_c_18_1, m_cor_c_18_2, m_cor_c_18_3),
                   m_c_pP = c(m_cor_c_19_1, m_cor_c_19_2, m_cor_c_19_3), m_c_nN = c(m_cor_c_20_1, m_cor_c_20_2, m_cor_c_20_3),
                   m_c_hH = c(m_cor_c_21_1, m_cor_c_21_2, m_cor_c_21_3), m_c_pN = c(m_cor_c_22_1, m_cor_c_22_2, m_cor_c_22_3),
                   m_c_pH = c(m_cor_c_23_1, m_cor_c_23_2, m_cor_c_23_3), m_c_nP = c(m_cor_c_24_1, m_cor_c_24_2, m_cor_c_24_3),
                   m_c_nH = c(m_cor_c_25_1, m_cor_c_25_2, m_cor_c_25_3), m_c_hP = c(m_cor_c_26_1, m_cor_c_26_2, m_cor_c_26_3),
                   m_c_hN = c(m_cor_c_27_1, m_cor_c_27_2, m_cor_c_27_3), m_c_P_N = c(m_cor_c_28_1, m_cor_c_28_2, m_cor_c_28_3),
                   m_c_P_H = c(m_cor_c_29_1, m_cor_c_29_2, m_cor_c_29_3), m_c_N_H = c(m_cor_c_30_1, m_cor_c_30_2, m_cor_c_30_3),
                   
                   sd_c_P.MSSD = c(sd_cor_c_10_1, sd_cor_c_10_2, sd_cor_c_10_3),
                   sd_c_N.MSSD = c(sd_cor_c_11_1, sd_cor_c_11_2, sd_cor_c_11_3), sd_c_H.MSSD = c(sd_cor_c_12_1, sd_cor_c_12_2, sd_cor_c_12_3),
                   sd_c_P.rMSSD = c(sd_cor_c_13_1, sd_cor_c_13_2, sd_cor_c_13_3), sd_c_N.rMSSD = c(sd_cor_c_14_1, sd_cor_c_14_2, sd_cor_c_14_3),
                   sd_c_H.rMSSD = c(sd_cor_c_15_1, sd_cor_c_15_2, sd_cor_c_15_3), sd_c_P.ALPHA = c(sd_cor_c_16_1, sd_cor_c_16_2, sd_cor_c_16_3),
                   sd_c_N.ALPHA = c(sd_cor_c_17_1, sd_cor_c_17_2, sd_cor_c_17_3), sd_c_H.ALPHA = c(sd_cor_c_18_1, sd_cor_c_18_2, sd_cor_c_18_3),
                   sd_c_pP = c(sd_cor_c_19_1, sd_cor_c_19_2, sd_cor_c_19_3), sd_c_nN = c(sd_cor_c_20_1, sd_cor_c_20_2, sd_cor_c_20_3),
                   sd_c_hH = c(sd_cor_c_21_1, sd_cor_c_21_2, sd_cor_c_21_3), sd_c_pN = c(sd_cor_c_22_1, sd_cor_c_22_2, sd_cor_c_22_3),
                   sd_c_pH = c(sd_cor_c_23_1, sd_cor_c_23_2, sd_cor_c_23_3), sd_c_nP = c(sd_cor_c_24_1, sd_cor_c_24_2, sd_cor_c_24_3),
                   sd_c_nH = c(sd_cor_c_25_1, sd_cor_c_25_2, sd_cor_c_25_3), sd_c_hP = c(sd_cor_c_26_1, sd_cor_c_26_2, sd_cor_c_26_3),
                   sd_c_hN = c(sd_cor_c_27_1, sd_cor_c_27_2, sd_cor_c_27_3), sd_c_P_N = c(sd_cor_c_28_1, sd_cor_c_28_2, sd_cor_c_28_3),
                   sd_c_P_H = c(sd_cor_c_29_1, sd_cor_c_29_2, sd_cor_c_29_3), sd_c_N_H = c(sd_cor_c_30_1, sd_cor_c_30_2, sd_cor_c_30_3),
                   
                   e_P.Mean = c(cor_e_1_1, cor_e_1_2, cor_e_1_3), e_N.Mean = c(cor_e_2_1, cor_e_2_2, cor_e_2_3),
                   e_H.Mean = c(cor_e_3_1, cor_e_3_2, cor_e_3_3), e_P.SD = c(cor_e_4_1, cor_e_4_2, cor_e_4_3),
                   e_N.SD = c(cor_e_5_1, cor_e_5_2, cor_e_5_3), e_H.SD= c(cor_e_6_1, cor_e_6_2, cor_e_6_3),
                   e_P.rSD = c(cor_e_7_1, cor_e_7_2, cor_e_7_3), e_N.rSD = c(cor_e_8_1, cor_e_8_2, cor_e_8_3),
                   e_H.rSD = c(cor_e_9_1, cor_e_9_2, cor_e_9_3), e_P.MSSD = c(cor_e_10_1, cor_e_10_2, cor_e_10_3),
                   e_N.MSSD = c(cor_e_11_1, cor_e_11_2, cor_e_11_3), e_H.MSSD = c(cor_e_12_1, cor_e_12_2, cor_e_12_3),
                   e_P.rMSSD = c(cor_e_13_1, cor_e_13_2, cor_e_13_3), e_N.rMSSD = c(cor_e_14_1, cor_e_14_2, cor_e_14_3),
                   e_H.rMSSD = c(cor_e_15_1, cor_e_15_2, cor_e_15_3), e_P.ALPHA = c(cor_e_16_1, cor_e_16_2, cor_e_16_3),
                   e_N.ALPHA = c(cor_e_17_1, cor_e_17_2, cor_e_17_3), e_H.ALPHA = c(cor_e_18_1, cor_e_18_2, cor_e_18_3),
                   e_pP = c(cor_e_19_1, cor_e_19_2, cor_e_19_3), e_nN = c(cor_e_20_1, cor_e_20_2, cor_e_20_3),
                   e_hH = c(cor_e_21_1, cor_e_21_2, cor_e_21_3), e_pN = c(cor_e_22_1, cor_e_22_2, cor_e_22_3),
                   e_pH = c(cor_e_23_1, cor_e_23_2, cor_e_23_3), e_nP = c(cor_e_24_1, cor_e_24_2, cor_e_24_3),
                   e_nH = c(cor_e_25_1, cor_e_25_2, cor_e_25_3), e_hP = c(cor_e_26_1, cor_e_26_2, cor_e_26_3),
                   e_hN = c(cor_e_27_1, cor_e_27_2, cor_e_27_3), e_P_N = c(cor_e_28_1, cor_e_28_2, cor_e_28_3),
                   e_P_H = c(cor_e_29_1, cor_e_29_2, cor_e_29_3), e_N_H = c(cor_e_30_1, cor_e_30_2, cor_e_30_3),
                   
                   m_e_P.SD = c(m_cor_e_4_1, m_cor_e_4_2, m_cor_e_4_3),
                   m_e_N.SD = c(m_cor_e_5_1, m_cor_e_5_2, m_cor_e_5_3), m_e_H.SD= c(m_cor_e_6_1, m_cor_e_6_2, m_cor_e_6_3),
                   m_e_P.rSD = c(m_cor_e_7_1, m_cor_e_7_2, m_cor_e_7_3), m_e_N.rSD = c(m_cor_e_8_1, m_cor_e_8_2, m_cor_e_8_3),
                   m_e_H.rSD = c(m_cor_e_9_1, m_cor_e_9_2, m_cor_e_9_3), m_e_P.MSSD = c(m_cor_e_10_1, m_cor_e_10_2, m_cor_e_10_3),
                   m_e_N.MSSD = c(m_cor_e_11_1, m_cor_e_11_2, m_cor_e_11_3), m_e_H.MSSD = c(m_cor_e_12_1, m_cor_e_12_2, m_cor_e_12_3),
                   m_e_P.rMSSD = c(m_cor_e_13_1, m_cor_e_13_2, m_cor_e_13_3), m_e_N.rMSSD = c(m_cor_e_14_1, m_cor_e_14_2, m_cor_e_14_3),
                   m_e_H.rMSSD = c(m_cor_e_15_1, m_cor_e_15_2, m_cor_e_15_3), m_e_P.ALPHA = c(m_cor_e_16_1, m_cor_e_16_2, m_cor_e_16_3),
                   m_e_N.ALPHA = c(m_cor_e_17_1, m_cor_e_17_2, m_cor_e_17_3), m_e_H.ALPHA = c(m_cor_e_18_1, m_cor_e_18_2, m_cor_e_18_3),
                   m_e_pP = c(m_cor_e_19_1, m_cor_e_19_2, m_cor_e_19_3), m_e_nN = c(m_cor_e_20_1, m_cor_e_20_2, m_cor_e_20_3),
                   m_e_hH = c(m_cor_e_21_1, m_cor_e_21_2, m_cor_e_21_3), m_e_pN = c(m_cor_e_22_1, m_cor_e_22_2, m_cor_e_22_3),
                   m_e_pH = c(m_cor_e_23_1, m_cor_e_23_2, m_cor_e_23_3), m_e_nP = c(m_cor_e_24_1, m_cor_e_24_2, m_cor_e_24_3),
                   m_e_nH = c(m_cor_e_25_1, m_cor_e_25_2, m_cor_e_25_3), m_e_hP = c(m_cor_e_26_1, m_cor_e_26_2, m_cor_e_26_3),
                   m_e_hN = c(m_cor_e_27_1, m_cor_e_27_2, m_cor_e_27_3), m_e_P_N = c(m_cor_e_28_1, m_cor_e_28_2, m_cor_e_28_3),
                   m_e_P_H = c(m_cor_e_29_1, m_cor_e_29_2, m_cor_e_29_3), m_e_N_H = c(m_cor_e_30_1, m_cor_e_30_2, m_cor_e_30_3),
                   
                   sd_e_P.MSSD = c(sd_cor_e_10_1, sd_cor_e_10_2, sd_cor_e_10_3),
                   sd_e_N.MSSD = c(sd_cor_e_11_1, sd_cor_e_11_2, sd_cor_e_11_3), sd_e_H.MSSD = c(sd_cor_e_12_1, sd_cor_e_12_2, sd_cor_e_12_3),
                   sd_e_P.rMSSD = c(sd_cor_e_13_1, sd_cor_e_13_2, sd_cor_e_13_3), sd_e_N.rMSSD = c(sd_cor_e_14_1, sd_cor_e_14_2, sd_cor_e_14_3),
                   sd_e_H.rMSSD = c(sd_cor_e_15_1, sd_cor_e_15_2, sd_cor_e_15_3), sd_e_P.ALPHA = c(sd_cor_e_16_1, sd_cor_e_16_2, sd_cor_e_16_3),
                   sd_e_N.ALPHA = c(sd_cor_e_17_1, sd_cor_e_17_2, sd_cor_e_17_3), sd_e_H.ALPHA = c(sd_cor_e_18_1, sd_cor_e_18_2, sd_cor_e_18_3),
                   sd_e_pP = c(sd_cor_e_19_1, sd_cor_e_19_2, sd_cor_e_19_3), sd_e_nN = c(sd_cor_e_20_1, sd_cor_e_20_2, sd_cor_e_20_3),
                   sd_e_hH = c(sd_cor_e_21_1, sd_cor_e_21_2, sd_cor_e_21_3), sd_e_pN = c(sd_cor_e_22_1, sd_cor_e_22_2, sd_cor_e_22_3),
                   sd_e_pH = c(sd_cor_e_23_1, sd_cor_e_23_2, sd_cor_e_23_3), sd_e_nP = c(sd_cor_e_24_1, sd_cor_e_24_2, sd_cor_e_24_3),
                   sd_e_nH = c(sd_cor_e_25_1, sd_cor_e_25_2, sd_cor_e_25_3), sd_e_hP = c(sd_cor_e_26_1, sd_cor_e_26_2, sd_cor_e_26_3),
                   sd_e_hN = c(sd_cor_e_27_1, sd_cor_e_27_2, sd_cor_e_27_3), sd_e_P_N = c(sd_cor_e_28_1, sd_cor_e_28_2, sd_cor_e_28_3),
                   sd_e_P_H = c(sd_cor_e_29_1, sd_cor_e_29_2, sd_cor_e_29_3), sd_e_N_H = c(sd_cor_e_30_1, sd_cor_e_30_2, sd_cor_e_30_3),
                   
                   a_P.Mean = c(cor_a_1_1, cor_a_1_2, cor_a_1_3), a_N.Mean = c(cor_a_2_1, cor_a_2_2, cor_a_2_3),
                   a_H.Mean = c(cor_a_3_1, cor_a_3_2, cor_a_3_3), a_P.SD = c(cor_a_4_1, cor_a_4_2, cor_a_4_3),
                   a_N.SD = c(cor_a_5_1, cor_a_5_2, cor_a_5_3), a_H.SD= c(cor_a_6_1, cor_a_6_2, cor_a_6_3),
                   a_P.rSD = c(cor_a_7_1, cor_a_7_2, cor_a_7_3), a_N.rSD = c(cor_a_8_1, cor_a_8_2, cor_a_8_3),
                   a_H.rSD = c(cor_a_9_1, cor_a_9_2, cor_a_9_3), a_P.MSSD = c(cor_a_10_1, cor_a_10_2, cor_a_10_3),
                   a_N.MSSD = c(cor_a_11_1, cor_a_11_2, cor_a_11_3), a_H.MSSD = c(cor_a_12_1, cor_a_12_2, cor_a_12_3),
                   a_P.rMSSD = c(cor_a_13_1, cor_a_13_2, cor_a_13_3), a_N.rMSSD = c(cor_a_14_1, cor_a_14_2, cor_a_14_3),
                   a_H.rMSSD = c(cor_a_15_1, cor_a_15_2, cor_a_15_3), a_P.ALPHA = c(cor_a_16_1, cor_a_16_2, cor_a_16_3),
                   a_N.ALPHA = c(cor_a_17_1, cor_a_17_2, cor_a_17_3), a_H.ALPHA = c(cor_a_18_1, cor_a_18_2, cor_a_18_3),
                   a_pP = c(cor_a_19_1, cor_a_19_2, cor_a_19_3), a_nN = c(cor_a_20_1, cor_a_20_2, cor_a_20_3),
                   a_hH = c(cor_a_21_1, cor_a_21_2, cor_a_21_3), a_pN = c(cor_a_22_1, cor_a_22_2, cor_a_22_3),
                   a_pH = c(cor_a_23_1, cor_a_23_2, cor_a_23_3), a_nP = c(cor_a_24_1, cor_a_24_2, cor_a_24_3),
                   a_nH = c(cor_a_25_1, cor_a_25_2, cor_a_25_3), a_hP = c(cor_a_26_1, cor_a_26_2, cor_a_26_3),
                   a_hN = c(cor_a_27_1, cor_a_27_2, cor_a_27_3), a_P_N = c(cor_a_28_1, cor_a_28_2, cor_a_28_3),
                   a_P_H = c(cor_a_29_1, cor_a_29_2, cor_a_29_3), a_N_H = c(cor_a_30_1, cor_a_30_2, cor_a_30_3),
                   
                   m_a_P.SD = c(m_cor_a_4_1, m_cor_a_4_2, m_cor_a_4_3),
                   m_a_N.SD = c(m_cor_a_5_1, m_cor_a_5_2, m_cor_a_5_3), m_a_H.SD= c(m_cor_a_6_1, m_cor_a_6_2, m_cor_a_6_3),
                   m_a_P.rSD = c(m_cor_a_7_1, m_cor_a_7_2, m_cor_a_7_3), m_a_N.rSD = c(m_cor_a_8_1, m_cor_a_8_2, m_cor_a_8_3),
                   m_a_H.rSD = c(m_cor_a_9_1, m_cor_a_9_2, m_cor_a_9_3), m_a_P.MSSD = c(m_cor_a_10_1, m_cor_a_10_2, m_cor_a_10_3),
                   m_a_N.MSSD = c(m_cor_a_11_1, m_cor_a_11_2, m_cor_a_11_3), m_a_H.MSSD = c(m_cor_a_12_1, m_cor_a_12_2, m_cor_a_12_3),
                   m_a_P.rMSSD = c(m_cor_a_13_1, m_cor_a_13_2, m_cor_a_13_3), m_a_N.rMSSD = c(m_cor_a_14_1, m_cor_a_14_2, m_cor_a_14_3),
                   m_a_H.rMSSD = c(m_cor_a_15_1, m_cor_a_15_2, m_cor_a_15_3), m_a_P.ALPHA = c(m_cor_a_16_1, m_cor_a_16_2, m_cor_a_16_3),
                   m_a_N.ALPHA = c(m_cor_a_17_1, m_cor_a_17_2, m_cor_a_17_3), m_a_H.ALPHA = c(m_cor_a_18_1, m_cor_a_18_2, m_cor_a_18_3),
                   m_a_pP = c(m_cor_a_19_1, m_cor_a_19_2, m_cor_a_19_3), m_a_nN = c(m_cor_a_20_1, m_cor_a_20_2, m_cor_a_20_3),
                   m_a_hH = c(m_cor_a_21_1, m_cor_a_21_2, m_cor_a_21_3), m_a_pN = c(m_cor_a_22_1, m_cor_a_22_2, m_cor_a_22_3),
                   m_a_pH = c(m_cor_a_23_1, m_cor_a_23_2, m_cor_a_23_3), m_a_nP = c(m_cor_a_24_1, m_cor_a_24_2, m_cor_a_24_3),
                   m_a_nH = c(m_cor_a_25_1, m_cor_a_25_2, m_cor_a_25_3), m_a_hP = c(m_cor_a_26_1, m_cor_a_26_2, m_cor_a_26_3),
                   m_a_hN = c(m_cor_a_27_1, m_cor_a_27_2, m_cor_a_27_3), m_a_P_N = c(m_cor_a_28_1, m_cor_a_28_2, m_cor_a_28_3),
                   m_a_P_H = c(m_cor_a_29_1, m_cor_a_29_2, m_cor_a_29_3), m_a_N_H = c(m_cor_a_30_1, m_cor_a_30_2, m_cor_a_30_3),
                   
                   sd_a_P.MSSD = c(sd_cor_a_10_1, sd_cor_a_10_2, sd_cor_a_10_3),
                   sd_a_N.MSSD = c(sd_cor_a_11_1, sd_cor_a_11_2, sd_cor_a_11_3), sd_a_H.MSSD = c(sd_cor_a_12_1, sd_cor_a_12_2, sd_cor_a_12_3),
                   sd_a_P.rMSSD = c(sd_cor_a_13_1, sd_cor_a_13_2, sd_cor_a_13_3), sd_a_N.rMSSD = c(sd_cor_a_14_1, sd_cor_a_14_2, sd_cor_a_14_3),
                   sd_a_H.rMSSD = c(sd_cor_a_15_1, sd_cor_a_15_2, sd_cor_a_15_3), sd_a_P.ALPHA = c(sd_cor_a_16_1, sd_cor_a_16_2, sd_cor_a_16_3),
                   sd_a_N.ALPHA = c(sd_cor_a_17_1, sd_cor_a_17_2, sd_cor_a_17_3), sd_a_H.ALPHA = c(sd_cor_a_18_1, sd_cor_a_18_2, sd_cor_a_18_3),
                   sd_a_pP = c(sd_cor_a_19_1, sd_cor_a_19_2, sd_cor_a_19_3), sd_a_nN = c(sd_cor_a_20_1, sd_cor_a_20_2, sd_cor_a_20_3),
                   sd_a_hH = c(sd_cor_a_21_1, sd_cor_a_21_2, sd_cor_a_21_3), sd_a_pN = c(sd_cor_a_22_1, sd_cor_a_22_2, sd_cor_a_22_3),
                   sd_a_pH = c(sd_cor_a_23_1, sd_cor_a_23_2, sd_cor_a_23_3), sd_a_nP = c(sd_cor_a_24_1, sd_cor_a_24_2, sd_cor_a_24_3),
                   sd_a_nH = c(sd_cor_a_25_1, sd_cor_a_25_2, sd_cor_a_25_3), sd_a_hP = c(sd_cor_a_26_1, sd_cor_a_26_2, sd_cor_a_26_3),
                   sd_a_hN = c(sd_cor_a_27_1, sd_cor_a_27_2, sd_cor_a_27_3), sd_a_P_N = c(sd_cor_a_28_1, sd_cor_a_28_2, sd_cor_a_28_3),
                   sd_a_P_H = c(sd_cor_a_29_1, sd_cor_a_29_2, sd_cor_a_29_3), sd_a_N_H = c(sd_cor_a_30_1, sd_cor_a_30_2, sd_cor_a_30_3),
                   
                   n_P.Mean = c(cor_n_1_1, cor_n_1_2, cor_n_1_3), n_N.Mean = c(cor_n_2_1, cor_n_2_2, cor_n_2_3),
                   n_H.Mean = c(cor_n_3_1, cor_n_3_2, cor_n_3_3), n_P.SD = c(cor_n_4_1, cor_n_4_2, cor_n_4_3),
                   n_N.SD = c(cor_n_5_1, cor_n_5_2, cor_n_5_3), n_H.SD= c(cor_n_6_1, cor_n_6_2, cor_n_6_3),
                   n_P.rSD = c(cor_n_7_1, cor_n_7_2, cor_n_7_3), n_N.rSD = c(cor_n_8_1, cor_n_8_2, cor_n_8_3),
                   n_H.rSD = c(cor_n_9_1, cor_n_9_2, cor_n_9_3), n_P.MSSD = c(cor_n_10_1, cor_n_10_2, cor_n_10_3),
                   n_N.MSSD = c(cor_n_11_1, cor_n_11_2, cor_n_11_3), n_H.MSSD = c(cor_n_12_1, cor_n_12_2, cor_n_12_3),
                   n_P.rMSSD = c(cor_n_13_1, cor_n_13_2, cor_n_13_3), n_N.rMSSD = c(cor_n_14_1, cor_n_14_2, cor_n_14_3),
                   n_H.rMSSD = c(cor_n_15_1, cor_n_15_2, cor_n_15_3), n_P.ALPHA = c(cor_n_16_1, cor_n_16_2, cor_n_16_3),
                   n_N.ALPHA = c(cor_n_17_1, cor_n_17_2, cor_n_17_3), n_H.ALPHA = c(cor_n_18_1, cor_n_18_2, cor_n_18_3),
                   n_pP = c(cor_n_19_1, cor_n_19_2, cor_n_19_3), n_nN = c(cor_n_20_1, cor_n_20_2, cor_n_20_3),
                   n_hH = c(cor_n_21_1, cor_n_21_2, cor_n_21_3), n_pN = c(cor_n_22_1, cor_n_22_2, cor_n_22_3),
                   n_pH = c(cor_n_23_1, cor_n_23_2, cor_n_23_3), n_nP = c(cor_n_24_1, cor_n_24_2, cor_n_24_3),
                   n_nH = c(cor_n_25_1, cor_n_25_2, cor_n_25_3), n_hP = c(cor_n_26_1, cor_n_26_2, cor_n_26_3),
                   n_hN = c(cor_n_27_1, cor_n_27_2, cor_n_27_3), n_P_N = c(cor_n_28_1, cor_n_28_2, cor_n_28_3),
                   n_P_H = c(cor_n_29_1, cor_n_29_2, cor_n_29_3), n_N_H = c(cor_n_30_1, cor_n_30_2, cor_n_30_3),
                   
                   m_n_P.SD = c(m_cor_n_4_1, m_cor_n_4_2, m_cor_n_4_3),
                   m_n_N.SD = c(m_cor_n_5_1, m_cor_n_5_2, m_cor_n_5_3), m_n_H.SD= c(m_cor_n_6_1, m_cor_n_6_2, m_cor_n_6_3),
                   m_n_P.rSD = c(m_cor_n_7_1, m_cor_n_7_2, m_cor_n_7_3), m_n_N.rSD = c(m_cor_n_8_1, m_cor_n_8_2, m_cor_n_8_3),
                   m_n_H.rSD = c(m_cor_n_9_1, m_cor_n_9_2, m_cor_n_9_3), m_n_P.MSSD = c(m_cor_n_10_1, m_cor_n_10_2, m_cor_n_10_3),
                   m_n_N.MSSD = c(m_cor_n_11_1, m_cor_n_11_2, m_cor_n_11_3), m_n_H.MSSD = c(m_cor_n_12_1, m_cor_n_12_2, m_cor_n_12_3),
                   m_n_P.rMSSD = c(m_cor_n_13_1, m_cor_n_13_2, m_cor_n_13_3), m_n_N.rMSSD = c(m_cor_n_14_1, m_cor_n_14_2, m_cor_n_14_3),
                   m_n_H.rMSSD = c(m_cor_n_15_1, m_cor_n_15_2, m_cor_n_15_3), m_n_P.ALPHA = c(m_cor_n_16_1, m_cor_n_16_2, m_cor_n_16_3),
                   m_n_N.ALPHA = c(m_cor_n_17_1, m_cor_n_17_2, m_cor_n_17_3), m_n_H.ALPHA = c(m_cor_n_18_1, m_cor_n_18_2, m_cor_n_18_3),
                   m_n_pP = c(m_cor_n_19_1, m_cor_n_19_2, m_cor_n_19_3), m_n_nN = c(m_cor_n_20_1, m_cor_n_20_2, m_cor_n_20_3),
                   m_n_hH = c(m_cor_n_21_1, m_cor_n_21_2, m_cor_n_21_3), m_n_pN = c(m_cor_n_22_1, m_cor_n_22_2, m_cor_n_22_3),
                   m_n_pH = c(m_cor_n_23_1, m_cor_n_23_2, m_cor_n_23_3), m_n_nP = c(m_cor_n_24_1, m_cor_n_24_2, m_cor_n_24_3),
                   m_n_nH = c(m_cor_n_25_1, m_cor_n_25_2, m_cor_n_25_3), m_n_hP = c(m_cor_n_26_1, m_cor_n_26_2, m_cor_n_26_3),
                   m_n_hN = c(m_cor_n_27_1, m_cor_n_27_2, m_cor_n_27_3), m_n_P_N = c(m_cor_n_28_1, m_cor_n_28_2, m_cor_n_28_3),
                   m_n_P_H = c(m_cor_n_29_1, m_cor_n_29_2, m_cor_n_29_3), m_n_N_H = c(m_cor_n_30_1, m_cor_n_30_2, m_cor_n_30_3),
                   
                   sd_n_P.MSSD = c(sd_cor_n_10_1, sd_cor_n_10_2, sd_cor_n_10_3),
                   sd_n_N.MSSD = c(sd_cor_n_11_1, sd_cor_n_11_2, sd_cor_n_11_3), sd_n_H.MSSD = c(sd_cor_n_12_1, sd_cor_n_12_2, sd_cor_n_12_3),
                   sd_n_P.rMSSD = c(sd_cor_n_13_1, sd_cor_n_13_2, sd_cor_n_13_3), sd_n_N.rMSSD = c(sd_cor_n_14_1, sd_cor_n_14_2, sd_cor_n_14_3),
                   sd_n_H.rMSSD = c(sd_cor_n_15_1, sd_cor_n_15_2, sd_cor_n_15_3), sd_n_P.ALPHA = c(sd_cor_n_16_1, sd_cor_n_16_2, sd_cor_n_16_3),
                   sd_n_N.ALPHA = c(sd_cor_n_17_1, sd_cor_n_17_2, sd_cor_n_17_3), sd_n_H.ALPHA = c(sd_cor_n_18_1, sd_cor_n_18_2, sd_cor_n_18_3),
                   sd_n_pP = c(sd_cor_n_19_1, sd_cor_n_19_2, sd_cor_n_19_3), sd_n_nN = c(sd_cor_n_20_1, sd_cor_n_20_2, sd_cor_n_20_3),
                   sd_n_hH = c(sd_cor_n_21_1, sd_cor_n_21_2, sd_cor_n_21_3), sd_n_pN = c(sd_cor_n_22_1, sd_cor_n_22_2, sd_cor_n_22_3),
                   sd_n_pH = c(sd_cor_n_23_1, sd_cor_n_23_2, sd_cor_n_23_3), sd_n_nP = c(sd_cor_n_24_1, sd_cor_n_24_2, sd_cor_n_24_3),
                   sd_n_nH = c(sd_cor_n_25_1, sd_cor_n_25_2, sd_cor_n_25_3), sd_n_hP = c(sd_cor_n_26_1, sd_cor_n_26_2, sd_cor_n_26_3),
                   sd_n_hN = c(sd_cor_n_27_1, sd_cor_n_27_2, sd_cor_n_27_3), sd_n_P_N = c(sd_cor_n_28_1, sd_cor_n_28_2, sd_cor_n_28_3),
                   sd_n_P_H = c(sd_cor_n_29_1, sd_cor_n_29_2, sd_cor_n_29_3), sd_n_N_H = c(sd_cor_n_30_1, sd_cor_n_30_2, sd_cor_n_30_3)
)

for (i in 2:31) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_o_",i-1),
                                                                           paste0("vi_o_",i-1)))} # +29 #-1
for (i in 32:58) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_o_m_",i-28),
                                                                           paste0("vi_o_m_",i-28)))} # +26 # -4
for (i in 59:79) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_o_sd_",i-49),
                                                                           paste0("vi_o_sd_",i-49)))} # +20 -10
for (i in 80:109) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_c_",i-79),
                                                                           paste0("vi_c_",i-79)))}#-1
for (i in 110:136) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_c_m_",i-106),
                                                                           paste0("vi_c_m_",i-106)))}# -4
for (i in 137:157) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_c_sd_",i-127),
                                                                           paste0("vi_c_sd_",i-127)))}# -10
for (i in 158:187) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_e_",i-157),
                                                                           paste0("vi_e_",i-157)))}#-1
for (i in 188:214) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_e_m_",i-184),
                                                                           paste0("vi_e_m_",i-184)))}# -4
for (i in 215:235) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_e_sd_",i-205),
                                                                           paste0("vi_e_sd_",i-205)))}# -10
for (i in 236:265) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_a_",i-235),
                                                                           paste0("vi_a_",i-235)))}# -1
for (i in 266:292) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_a_m_",i-262),
                                                                           paste0("vi_a_m_",i-262)))}# -4
for (i in 293:313) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_a_sd_",i-283),
                                                                           paste0("vi_a_sd_",i-283)))}# -10
for (i in 314:343) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_n_",i-313),
                                                                           paste0("vi_n_",i-313)))}# -1
for (i in 344:370) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_n_m_",i-340),
                                                                           paste0("vi_n_m_",i-340)))}# -4
for (i in 371:391) {
  meta <- escalc(measure="ZCOR", ri=meta[,i], ni=n, data=meta, var.names=c(paste0("yi_n_sd_",i-361),
                                                                           paste0("vi_n_sd_",i-361)))}# -10


###### META ANALYSIS


## OPENNESS
## OPENNESS
ma_o_1 <- rma(yi=yi_o_1, vi=vi_o_1, data=meta, weights = n, level = 95)
ma_o_1_beta <- fisherz2r(ma_o_1$beta)
ma_o_1_lb <- fisherz2r(ma_o_1$ci.lb)
ma_o_1_ub <- fisherz2r(ma_o_1$ci.ub)
ma_o_2 <- rma(yi=yi_o_2, vi=vi_o_2, data=meta, weights = n, level = 95)
ma_o_2_beta <- fisherz2r(ma_o_2$beta)
ma_o_2_lb <- fisherz2r(ma_o_2$ci.lb)
ma_o_2_ub <- fisherz2r(ma_o_2$ci.ub)
ma_o_3 <- rma(yi=yi_o_3, vi=vi_o_3, data=meta, weights = n, level = 95)
ma_o_3_beta <- fisherz2r(ma_o_3$beta)
ma_o_3_lb <- fisherz2r(ma_o_3$ci.lb)
ma_o_3_ub <- fisherz2r(ma_o_3$ci.ub)

# mean
ma_o_4_m <- rma(yi=yi_o_m_4, vi=vi_o_m_4, data=meta, weights = n, level = 95)
ma_o_4_m_beta <- fisherz2r(ma_o_4_m$beta)
ma_o_4_m_lb <- fisherz2r(ma_o_4_m$ci.lb)
ma_o_4_m_ub <- fisherz2r(ma_o_4_m$ci.ub)
ma_o_5_m <- rma(yi=yi_o_m_5, vi=vi_o_m_5, data=meta, weights = n, level = 95)
ma_o_5_m_beta <- fisherz2r(ma_o_5_m$beta)
ma_o_5_m_lb <- fisherz2r(ma_o_5_m$ci.lb)
ma_o_5_m_ub <- fisherz2r(ma_o_5_m$ci.ub)
ma_o_6_m <- rma(yi=yi_o_m_6, vi=vi_o_m_6, data=meta, weights = n, level = 95)
ma_o_6_m_beta <- fisherz2r(ma_o_6_m$beta)
ma_o_6_m_lb <- fisherz2r(ma_o_6_m$ci.lb)
ma_o_6_m_ub <- fisherz2r(ma_o_6_m$ci.ub)
ma_o_7_m <- rma(yi=yi_o_m_7, vi=vi_o_m_7, data=meta, weights = n, level = 95)
ma_o_7_m_beta <- fisherz2r(ma_o_7_m$beta)
ma_o_7_m_lb <- fisherz2r(ma_o_7_m$ci.lb)
ma_o_7_m_ub <- fisherz2r(ma_o_7_m$ci.ub)
ma_o_8_m <- rma(yi=yi_o_m_8, vi=vi_o_m_8, data=meta, weights = n, level = 95)
ma_o_8_m_beta <- fisherz2r(ma_o_8_m$beta)
ma_o_8_m_lb <- fisherz2r(ma_o_8_m$ci.lb)
ma_o_8_m_ub <- fisherz2r(ma_o_8_m$ci.ub)
ma_o_9_m <- rma(yi=yi_o_m_9, vi=vi_o_m_9, data=meta, weights = n, level = 95)
ma_o_9_m_beta <- fisherz2r(ma_o_9_m$beta)
ma_o_9_m_lb <- fisherz2r(ma_o_9_m$ci.lb)
ma_o_9_m_ub <- fisherz2r(ma_o_9_m$ci.ub)

#sd
ma_o_10_sd <- rma(yi=yi_o_sd_10, vi=vi_o_sd_10, data=meta, weights = n, level = 95)
ma_o_10_sd_beta <- fisherz2r(ma_o_10_sd$beta)
ma_o_10_sd_lb <- fisherz2r(ma_o_10_sd$ci.lb)
ma_o_10_sd_ub <- fisherz2r(ma_o_10_sd$ci.ub)
ma_o_11_sd <- rma(yi=yi_o_sd_11, vi=vi_o_sd_11, data=meta, weights = n, level = 95)
ma_o_11_sd_beta <- fisherz2r(ma_o_11_sd$beta)
ma_o_11_sd_lb <- fisherz2r(ma_o_11_sd$ci.lb)
ma_o_11_sd_ub <- fisherz2r(ma_o_11_sd$ci.ub)
ma_o_12_sd <- rma(yi=yi_o_sd_12, vi=vi_o_sd_12, data=meta, weights = n, level = 95)
ma_o_12_sd_beta <- fisherz2r(ma_o_12_sd$beta)
ma_o_12_sd_lb <- fisherz2r(ma_o_12_sd$ci.lb)
ma_o_12_sd_ub <- fisherz2r(ma_o_12_sd$ci.ub)
ma_o_13_sd <- rma(yi=yi_o_sd_13, vi=vi_o_sd_13, data=meta, weights = n, level = 95)
ma_o_13_sd_beta <- fisherz2r(ma_o_13_sd$beta)
ma_o_13_sd_lb <- fisherz2r(ma_o_13_sd$ci.lb)
ma_o_13_sd_ub <- fisherz2r(ma_o_13_sd$ci.ub)
ma_o_14_sd <- rma(yi=yi_o_sd_14, vi=vi_o_sd_14, data=meta, weights = n, level = 95)
ma_o_14_sd_beta <- fisherz2r(ma_o_14_sd$beta)
ma_o_14_sd_lb <- fisherz2r(ma_o_14_sd$ci.lb)
ma_o_14_sd_ub <- fisherz2r(ma_o_14_sd$ci.ub)
ma_o_15_sd <- rma(yi=yi_o_sd_15, vi=vi_o_sd_15, data=meta, weights = n, level = 95)
ma_o_15_sd_beta <- fisherz2r(ma_o_15_sd$beta)
ma_o_15_sd_lb <- fisherz2r(ma_o_15_sd$ci.lb)
ma_o_15_sd_ub <- fisherz2r(ma_o_15_sd$ci.ub)
ma_o_16_sd <- rma(yi=yi_o_sd_16, vi=vi_o_sd_16, data=meta, weights = n, level = 95)
ma_o_16_sd_beta <- fisherz2r(ma_o_16_sd$beta)
ma_o_16_sd_lb <- fisherz2r(ma_o_16_sd$ci.lb)
ma_o_16_sd_ub <- fisherz2r(ma_o_16_sd$ci.ub)
ma_o_17_sd <- rma(yi=yi_o_sd_17, vi=vi_o_sd_17, data=meta, weights = n, level = 95)
ma_o_17_sd_beta <- fisherz2r(ma_o_17_sd$beta)
ma_o_17_sd_lb <- fisherz2r(ma_o_17_sd$ci.lb)
ma_o_17_sd_ub <- fisherz2r(ma_o_17_sd$ci.ub)
ma_o_18_sd <- rma(yi=yi_o_sd_18, vi=vi_o_sd_18, data=meta, weights = n, level = 95)
ma_o_18_sd_beta <- fisherz2r(ma_o_18_sd$beta)
ma_o_18_sd_lb <- fisherz2r(ma_o_18_sd$ci.lb)
ma_o_18_sd_ub <- fisherz2r(ma_o_18_sd$ci.ub)
ma_o_19_sd <- rma(yi=yi_o_sd_19, vi=vi_o_sd_19, data=meta, weights = n, level = 95)
ma_o_19_sd_beta <- fisherz2r(ma_o_19_sd$beta)
ma_o_19_sd_lb <- fisherz2r(ma_o_19_sd$ci.lb)
ma_o_19_sd_ub <- fisherz2r(ma_o_19_sd$ci.ub)
ma_o_20_sd <- rma(yi=yi_o_sd_20, vi=vi_o_sd_20, data=meta, weights = n, level = 95)
ma_o_20_sd_beta <- fisherz2r(ma_o_20_sd$beta)
ma_o_20_sd_lb <- fisherz2r(ma_o_20_sd$ci.lb)
ma_o_20_sd_ub <- fisherz2r(ma_o_20_sd$ci.ub)
ma_o_21_sd <- rma(yi=yi_o_sd_21, vi=vi_o_sd_21, data=meta, weights = n, level = 95)
ma_o_21_sd_beta <- fisherz2r(ma_o_21_sd$beta)
ma_o_21_sd_lb <- fisherz2r(ma_o_21_sd$ci.lb)
ma_o_21_sd_ub <- fisherz2r(ma_o_21_sd$ci.ub)
ma_o_22_sd <- rma(yi=yi_o_sd_22, vi=vi_o_sd_22, data=meta, weights = n, level = 95)
ma_o_22_sd_beta <- fisherz2r(ma_o_22_sd$beta)
ma_o_22_sd_lb <- fisherz2r(ma_o_22_sd$ci.lb)
ma_o_22_sd_ub <- fisherz2r(ma_o_22_sd$ci.ub)
ma_o_23_sd <- rma(yi=yi_o_sd_23, vi=vi_o_sd_23, data=meta, weights = n, level = 95)
ma_o_23_sd_beta <- fisherz2r(ma_o_23_sd$beta)
ma_o_23_sd_lb <- fisherz2r(ma_o_23_sd$ci.lb)
ma_o_23_sd_ub <- fisherz2r(ma_o_23_sd$ci.ub)
ma_o_24_sd <- rma(yi=yi_o_sd_24, vi=vi_o_sd_24, data=meta, weights = n, level = 95)
ma_o_24_sd_beta <- fisherz2r(ma_o_24_sd$beta)
ma_o_24_sd_lb <- fisherz2r(ma_o_24_sd$ci.lb)
ma_o_24_sd_ub <- fisherz2r(ma_o_24_sd$ci.ub)
ma_o_25_sd <- rma(yi=yi_o_sd_25, vi=vi_o_sd_25, data=meta, weights = n, level = 95)
ma_o_25_sd_beta <- fisherz2r(ma_o_25_sd$beta)
ma_o_25_sd_lb <- fisherz2r(ma_o_25_sd$ci.lb)
ma_o_25_sd_ub <- fisherz2r(ma_o_25_sd$ci.ub)
ma_o_26_sd <- rma(yi=yi_o_sd_26, vi=vi_o_sd_26, data=meta, weights = n, level = 95)
ma_o_26_sd_beta <- fisherz2r(ma_o_26_sd$beta)
ma_o_26_sd_lb <- fisherz2r(ma_o_26_sd$ci.lb)
ma_o_26_sd_ub <- fisherz2r(ma_o_26_sd$ci.ub)
ma_o_27_sd <- rma(yi=yi_o_sd_27, vi=vi_o_sd_27, data=meta, weights = n, level = 95)
ma_o_27_sd_beta <- fisherz2r(ma_o_27_sd$beta)
ma_o_27_sd_lb <- fisherz2r(ma_o_27_sd$ci.lb)
ma_o_27_sd_ub <- fisherz2r(ma_o_27_sd$ci.ub)
ma_o_28_sd <- rma(yi=yi_o_sd_28, vi=vi_o_sd_28, data=meta, weights = n, level = 95)
ma_o_28_sd_beta <- fisherz2r(ma_o_28_sd$beta)
ma_o_28_sd_lb <- fisherz2r(ma_o_28_sd$ci.lb)
ma_o_28_sd_ub <- fisherz2r(ma_o_28_sd$ci.ub)
ma_o_29_sd <- rma(yi=yi_o_sd_29, vi=vi_o_sd_29, data=meta, weights = n, level = 95)
ma_o_29_sd_beta <- fisherz2r(ma_o_29_sd$beta)
ma_o_29_sd_lb <- fisherz2r(ma_o_29_sd$ci.lb)
ma_o_29_sd_ub <- fisherz2r(ma_o_29_sd$ci.ub)
ma_o_30_sd <- rma(yi=yi_o_sd_30, vi=vi_o_sd_30, data=meta, weights = n, level = 95)
ma_o_30_sd_beta <- fisherz2r(ma_o_30_sd$beta)
ma_o_30_sd_lb <- fisherz2r(ma_o_30_sd$ci.lb)
ma_o_30_sd_ub <- fisherz2r(ma_o_30_sd$ci.ub)

## CONSCIENTIOUSNESS

ma_c_1 <- rma(yi=yi_c_1, vi=vi_c_1, data=meta, weights = n, level = 95)
ma_c_1_beta <- fisherz2r(ma_c_1$beta)
ma_c_1_lb <- fisherz2r(ma_c_1$ci.lb)
ma_c_1_ub <- fisherz2r(ma_c_1$ci.ub)
ma_c_2 <- rma(yi=yi_c_2, vi=vi_c_2, data=meta, weights = n, level = 95)
ma_c_2_beta <- fisherz2r(ma_c_2$beta)
ma_c_2_lb <- fisherz2r(ma_c_2$ci.lb)
ma_c_2_ub <- fisherz2r(ma_c_2$ci.ub)
ma_c_3 <- rma(yi=yi_c_3, vi=vi_c_3, data=meta, weights = n, level = 95)
ma_c_3_beta <- fisherz2r(ma_c_3$beta)
ma_c_3_lb <- fisherz2r(ma_c_3$ci.lb)
ma_c_3_ub <- fisherz2r(ma_c_3$ci.ub)

# mean
ma_c_4_m <- rma(yi=yi_c_m_4, vi=vi_c_m_4, data=meta, weights = n, level = 95)
ma_c_4_m_beta <- fisherz2r(ma_c_4_m$beta)
ma_c_4_m_lb <- fisherz2r(ma_c_4_m$ci.lb)
ma_c_4_m_ub <- fisherz2r(ma_c_4_m$ci.ub)
ma_c_5_m <- rma(yi=yi_c_m_5, vi=vi_c_m_5, data=meta, weights = n, level = 95)
ma_c_5_m_beta <- fisherz2r(ma_c_5_m$beta)
ma_c_5_m_lb <- fisherz2r(ma_c_5_m$ci.lb)
ma_c_5_m_ub <- fisherz2r(ma_c_5_m$ci.ub)
ma_c_6_m <- rma(yi=yi_c_m_6, vi=vi_c_m_6, data=meta, weights = n, level = 95)
ma_c_6_m_beta <- fisherz2r(ma_c_6_m$beta)
ma_c_6_m_lb <- fisherz2r(ma_c_6_m$ci.lb)
ma_c_6_m_ub <- fisherz2r(ma_c_6_m$ci.ub)
ma_c_7_m <- rma(yi=yi_c_m_7, vi=vi_c_m_7, data=meta, weights = n, level = 95)
ma_c_7_m_beta <- fisherz2r(ma_c_7_m$beta)
ma_c_7_m_lb <- fisherz2r(ma_c_7_m$ci.lb)
ma_c_7_m_ub <- fisherz2r(ma_c_7_m$ci.ub)
ma_c_8_m <- rma(yi=yi_c_m_8, vi=vi_c_m_8, data=meta, weights = n, level = 95)
ma_c_8_m_beta <- fisherz2r(ma_c_8_m$beta)
ma_c_8_m_lb <- fisherz2r(ma_c_8_m$ci.lb)
ma_c_8_m_ub <- fisherz2r(ma_c_8_m$ci.ub)
ma_c_9_m <- rma(yi=yi_c_m_9, vi=vi_c_m_9, data=meta, weights = n, level = 95)
ma_c_9_m_beta <- fisherz2r(ma_c_9_m$beta)
ma_c_9_m_lb <- fisherz2r(ma_c_9_m$ci.lb)
ma_c_9_m_ub <- fisherz2r(ma_c_9_m$ci.ub)

#sd
ma_c_10_sd <- rma(yi=yi_c_sd_10, vi=vi_c_sd_10, data=meta, weights = n, level = 95)
ma_c_10_sd_beta <- fisherz2r(ma_c_10_sd$beta)
ma_c_10_sd_lb <- fisherz2r(ma_c_10_sd$ci.lb)
ma_c_10_sd_ub <- fisherz2r(ma_c_10_sd$ci.ub)
ma_c_11_sd <- rma(yi=yi_c_sd_11, vi=vi_c_sd_11, data=meta, weights = n, level = 95)
ma_c_11_sd_beta <- fisherz2r(ma_c_11_sd$beta)
ma_c_11_sd_lb <- fisherz2r(ma_c_11_sd$ci.lb)
ma_c_11_sd_ub <- fisherz2r(ma_c_11_sd$ci.ub)
ma_c_12_sd <- rma(yi=yi_c_sd_12, vi=vi_c_sd_12, data=meta, weights = n, level = 95)
ma_c_12_sd_beta <- fisherz2r(ma_c_12_sd$beta)
ma_c_12_sd_lb <- fisherz2r(ma_c_12_sd$ci.lb)
ma_c_12_sd_ub <- fisherz2r(ma_c_12_sd$ci.ub)
ma_c_13_sd <- rma(yi=yi_c_sd_13, vi=vi_c_sd_13, data=meta, weights = n, level = 95)
ma_c_13_sd_beta <- fisherz2r(ma_c_13_sd$beta)
ma_c_13_sd_lb <- fisherz2r(ma_c_13_sd$ci.lb)
ma_c_13_sd_ub <- fisherz2r(ma_c_13_sd$ci.ub)
ma_c_14_sd <- rma(yi=yi_c_sd_14, vi=vi_c_sd_14, data=meta, weights = n, level = 95)
ma_c_14_sd_beta <- fisherz2r(ma_c_14_sd$beta)
ma_c_14_sd_lb <- fisherz2r(ma_c_14_sd$ci.lb)
ma_c_14_sd_ub <- fisherz2r(ma_c_14_sd$ci.ub)
ma_c_15_sd <- rma(yi=yi_c_sd_15, vi=vi_c_sd_15, data=meta, weights = n, level = 95)
ma_c_15_sd_beta <- fisherz2r(ma_c_15_sd$beta)
ma_c_15_sd_lb <- fisherz2r(ma_c_15_sd$ci.lb)
ma_c_15_sd_ub <- fisherz2r(ma_c_15_sd$ci.ub)
ma_c_16_sd <- rma(yi=yi_c_sd_16, vi=vi_c_sd_16, data=meta, weights = n, level = 95)
ma_c_16_sd_beta <- fisherz2r(ma_c_16_sd$beta)
ma_c_16_sd_lb <- fisherz2r(ma_c_16_sd$ci.lb)
ma_c_16_sd_ub <- fisherz2r(ma_c_16_sd$ci.ub)
ma_c_17_sd <- rma(yi=yi_c_sd_17, vi=vi_c_sd_17, data=meta, weights = n, level = 95)
ma_c_17_sd_beta <- fisherz2r(ma_c_17_sd$beta)
ma_c_17_sd_lb <- fisherz2r(ma_c_17_sd$ci.lb)
ma_c_17_sd_ub <- fisherz2r(ma_c_17_sd$ci.ub)
ma_c_18_sd <- rma(yi=yi_c_sd_18, vi=vi_c_sd_18, data=meta, weights = n, level = 95)
ma_c_18_sd_beta <- fisherz2r(ma_c_18_sd$beta)
ma_c_18_sd_lb <- fisherz2r(ma_c_18_sd$ci.lb)
ma_c_18_sd_ub <- fisherz2r(ma_c_18_sd$ci.ub)
ma_c_19_sd <- rma(yi=yi_c_sd_19, vi=vi_c_sd_19, data=meta, weights = n, level = 95)
ma_c_19_sd_beta <- fisherz2r(ma_c_19_sd$beta)
ma_c_19_sd_lb <- fisherz2r(ma_c_19_sd$ci.lb)
ma_c_19_sd_ub <- fisherz2r(ma_c_19_sd$ci.ub)
ma_c_20_sd <- rma(yi=yi_c_sd_20, vi=vi_c_sd_20, data=meta, weights = n, level = 95)
ma_c_20_sd_beta <- fisherz2r(ma_c_20_sd$beta)
ma_c_20_sd_lb <- fisherz2r(ma_c_20_sd$ci.lb)
ma_c_20_sd_ub <- fisherz2r(ma_c_20_sd$ci.ub)
ma_c_21_sd <- rma(yi=yi_c_sd_21, vi=vi_c_sd_21, data=meta, weights = n, level = 95)
ma_c_21_sd_beta <- fisherz2r(ma_c_21_sd$beta)
ma_c_21_sd_lb <- fisherz2r(ma_c_21_sd$ci.lb)
ma_c_21_sd_ub <- fisherz2r(ma_c_21_sd$ci.ub)
ma_c_22_sd <- rma(yi=yi_c_sd_22, vi=vi_c_sd_22, data=meta, weights = n, level = 95)
ma_c_22_sd_beta <- fisherz2r(ma_c_22_sd$beta)
ma_c_22_sd_lb <- fisherz2r(ma_c_22_sd$ci.lb)
ma_c_22_sd_ub <- fisherz2r(ma_c_22_sd$ci.ub)
ma_c_23_sd <- rma(yi=yi_c_sd_23, vi=vi_c_sd_23, data=meta, weights = n, level = 95)
ma_c_23_sd_beta <- fisherz2r(ma_c_23_sd$beta)
ma_c_23_sd_lb <- fisherz2r(ma_c_23_sd$ci.lb)
ma_c_23_sd_ub <- fisherz2r(ma_c_23_sd$ci.ub)
ma_c_24_sd <- rma(yi=yi_c_sd_24, vi=vi_c_sd_24, data=meta, weights = n, level = 95)
ma_c_24_sd_beta <- fisherz2r(ma_c_24_sd$beta)
ma_c_24_sd_lb <- fisherz2r(ma_c_24_sd$ci.lb)
ma_c_24_sd_ub <- fisherz2r(ma_c_24_sd$ci.ub)
ma_c_25_sd <- rma(yi=yi_c_sd_25, vi=vi_c_sd_25, data=meta, weights = n, level = 95)
ma_c_25_sd_beta <- fisherz2r(ma_c_25_sd$beta)
ma_c_25_sd_lb <- fisherz2r(ma_c_25_sd$ci.lb)
ma_c_25_sd_ub <- fisherz2r(ma_c_25_sd$ci.ub)
ma_c_26_sd <- rma(yi=yi_c_sd_26, vi=vi_c_sd_26, data=meta, weights = n, level = 95)
ma_c_26_sd_beta <- fisherz2r(ma_c_26_sd$beta)
ma_c_26_sd_lb <- fisherz2r(ma_c_26_sd$ci.lb)
ma_c_26_sd_ub <- fisherz2r(ma_c_26_sd$ci.ub)
ma_c_27_sd <- rma(yi=yi_c_sd_27, vi=vi_c_sd_27, data=meta, weights = n, level = 95)
ma_c_27_sd_beta <- fisherz2r(ma_c_27_sd$beta)
ma_c_27_sd_lb <- fisherz2r(ma_c_27_sd$ci.lb)
ma_c_27_sd_ub <- fisherz2r(ma_c_27_sd$ci.ub)
ma_c_28_sd <- rma(yi=yi_c_sd_28, vi=vi_c_sd_28, data=meta, weights = n, level = 95)
ma_c_28_sd_beta <- fisherz2r(ma_c_28_sd$beta)
ma_c_28_sd_lb <- fisherz2r(ma_c_28_sd$ci.lb)
ma_c_28_sd_ub <- fisherz2r(ma_c_28_sd$ci.ub)
ma_c_29_sd <- rma(yi=yi_c_sd_29, vi=vi_c_sd_29, data=meta, weights = n, level = 95)
ma_c_29_sd_beta <- fisherz2r(ma_c_29_sd$beta)
ma_c_29_sd_lb <- fisherz2r(ma_c_29_sd$ci.lb)
ma_c_29_sd_ub <- fisherz2r(ma_c_29_sd$ci.ub)
ma_c_30_sd <- rma(yi=yi_c_sd_30, vi=vi_c_sd_30, data=meta, weights = n, level = 95)
ma_c_30_sd_beta <- fisherz2r(ma_c_30_sd$beta)
ma_c_30_sd_lb <- fisherz2r(ma_c_30_sd$ci.lb)
ma_c_30_sd_ub <- fisherz2r(ma_c_30_sd$ci.ub)

# EXTRAVERSION

ma_e_1 <- rma(yi=yi_e_1, vi=vi_e_1, data=meta, weights = n, level = 95)
ma_e_1_beta <- fisherz2r(ma_e_1$beta)
ma_e_1_lb <- fisherz2r(ma_e_1$ci.lb)
ma_e_1_ub <- fisherz2r(ma_e_1$ci.ub)
ma_e_2 <- rma(yi=yi_e_2, vi=vi_e_2, data=meta, weights = n, level = 95)
ma_e_2_beta <- fisherz2r(ma_e_2$beta)
ma_e_2_lb <- fisherz2r(ma_e_2$ci.lb)
ma_e_2_ub <- fisherz2r(ma_e_2$ci.ub)
ma_e_3 <- rma(yi=yi_e_3, vi=vi_e_3, data=meta, weights = n, level = 95)
ma_e_3_beta <- fisherz2r(ma_e_3$beta)
ma_e_3_lb <- fisherz2r(ma_e_3$ci.lb)
ma_e_3_ub <- fisherz2r(ma_e_3$ci.ub)

# mean
ma_e_4_m <- rma(yi=yi_e_m_4, vi=vi_e_m_4, data=meta, weights = n, level = 95)
ma_e_4_m_beta <- fisherz2r(ma_e_4_m$beta)
ma_e_4_m_lb <- fisherz2r(ma_e_4_m$ci.lb)
ma_e_4_m_ub <- fisherz2r(ma_e_4_m$ci.ub)
ma_e_5_m <- rma(yi=yi_e_m_5, vi=vi_e_m_5, data=meta, weights = n, level = 95)
ma_e_5_m_beta <- fisherz2r(ma_e_5_m$beta)
ma_e_5_m_lb <- fisherz2r(ma_e_5_m$ci.lb)
ma_e_5_m_ub <- fisherz2r(ma_e_5_m$ci.ub)
ma_e_6_m <- rma(yi=yi_e_m_6, vi=vi_e_m_6, data=meta, weights = n, level = 95)
ma_e_6_m_beta <- fisherz2r(ma_e_6_m$beta)
ma_e_6_m_lb <- fisherz2r(ma_e_6_m$ci.lb)
ma_e_6_m_ub <- fisherz2r(ma_e_6_m$ci.ub)
ma_e_7_m <- rma(yi=yi_e_m_7, vi=vi_e_m_7, data=meta, weights = n, level = 95)
ma_e_7_m_beta <- fisherz2r(ma_e_7_m$beta)
ma_e_7_m_lb <- fisherz2r(ma_e_7_m$ci.lb)
ma_e_7_m_ub <- fisherz2r(ma_e_7_m$ci.ub)
ma_e_8_m <- rma(yi=yi_e_m_8, vi=vi_e_m_8, data=meta, weights = n, level = 95)
ma_e_8_m_beta <- fisherz2r(ma_e_8_m$beta)
ma_e_8_m_lb <- fisherz2r(ma_e_8_m$ci.lb)
ma_e_8_m_ub <- fisherz2r(ma_e_8_m$ci.ub)
ma_e_9_m <- rma(yi=yi_e_m_9, vi=vi_e_m_9, data=meta, weights = n, level = 95)
ma_e_9_m_beta <- fisherz2r(ma_e_9_m$beta)
ma_e_9_m_lb <- fisherz2r(ma_e_9_m$ci.lb)
ma_e_9_m_ub <- fisherz2r(ma_e_9_m$ci.ub)

#sd
ma_e_10_sd <- rma(yi=yi_e_sd_10, vi=vi_e_sd_10, data=meta, weights = n, level = 95)
ma_e_10_sd_beta <- fisherz2r(ma_e_10_sd$beta)
ma_e_10_sd_lb <- fisherz2r(ma_e_10_sd$ci.lb)
ma_e_10_sd_ub <- fisherz2r(ma_e_10_sd$ci.ub)
ma_e_11_sd <- rma(yi=yi_e_sd_11, vi=vi_e_sd_11, data=meta, weights = n, level = 95)
ma_e_11_sd_beta <- fisherz2r(ma_e_11_sd$beta)
ma_e_11_sd_lb <- fisherz2r(ma_e_11_sd$ci.lb)
ma_e_11_sd_ub <- fisherz2r(ma_e_11_sd$ci.ub)
ma_e_12_sd <- rma(yi=yi_e_sd_12, vi=vi_e_sd_12, data=meta, weights = n, level = 95)
ma_e_12_sd_beta <- fisherz2r(ma_e_12_sd$beta)
ma_e_12_sd_lb <- fisherz2r(ma_e_12_sd$ci.lb)
ma_e_12_sd_ub <- fisherz2r(ma_e_12_sd$ci.ub)
ma_e_13_sd <- rma(yi=yi_e_sd_13, vi=vi_e_sd_13, data=meta, weights = n, level = 95)
ma_e_13_sd_beta <- fisherz2r(ma_e_13_sd$beta)
ma_e_13_sd_lb <- fisherz2r(ma_e_13_sd$ci.lb)
ma_e_13_sd_ub <- fisherz2r(ma_e_13_sd$ci.ub)
ma_e_14_sd <- rma(yi=yi_e_sd_14, vi=vi_e_sd_14, data=meta, weights = n, level = 95)
ma_e_14_sd_beta <- fisherz2r(ma_e_14_sd$beta)
ma_e_14_sd_lb <- fisherz2r(ma_e_14_sd$ci.lb)
ma_e_14_sd_ub <- fisherz2r(ma_e_14_sd$ci.ub)
ma_e_15_sd <- rma(yi=yi_e_sd_15, vi=vi_e_sd_15, data=meta, weights = n, level = 95)
ma_e_15_sd_beta <- fisherz2r(ma_e_15_sd$beta)
ma_e_15_sd_lb <- fisherz2r(ma_e_15_sd$ci.lb)
ma_e_15_sd_ub <- fisherz2r(ma_e_15_sd$ci.ub)
ma_e_16_sd <- rma(yi=yi_e_sd_16, vi=vi_e_sd_16, data=meta, weights = n, level = 95)
ma_e_16_sd_beta <- fisherz2r(ma_e_16_sd$beta)
ma_e_16_sd_lb <- fisherz2r(ma_e_16_sd$ci.lb)
ma_e_16_sd_ub <- fisherz2r(ma_e_16_sd$ci.ub)
ma_e_17_sd <- rma(yi=yi_e_sd_17, vi=vi_e_sd_17, data=meta, weights = n, level = 95)
ma_e_17_sd_beta <- fisherz2r(ma_e_17_sd$beta)
ma_e_17_sd_lb <- fisherz2r(ma_e_17_sd$ci.lb)
ma_e_17_sd_ub <- fisherz2r(ma_e_17_sd$ci.ub)
ma_e_18_sd <- rma(yi=yi_e_sd_18, vi=vi_e_sd_18, data=meta, weights = n, level = 95)
ma_e_18_sd_beta <- fisherz2r(ma_e_18_sd$beta)
ma_e_18_sd_lb <- fisherz2r(ma_e_18_sd$ci.lb)
ma_e_18_sd_ub <- fisherz2r(ma_e_18_sd$ci.ub)
ma_e_19_sd <- rma(yi=yi_e_sd_19, vi=vi_e_sd_19, data=meta, weights = n, level = 95)
ma_e_19_sd_beta <- fisherz2r(ma_e_19_sd$beta)
ma_e_19_sd_lb <- fisherz2r(ma_e_19_sd$ci.lb)
ma_e_19_sd_ub <- fisherz2r(ma_e_19_sd$ci.ub)
ma_e_20_sd <- rma(yi=yi_e_sd_20, vi=vi_e_sd_20, data=meta, weights = n, level = 95)
ma_e_20_sd_beta <- fisherz2r(ma_e_20_sd$beta)
ma_e_20_sd_lb <- fisherz2r(ma_e_20_sd$ci.lb)
ma_e_20_sd_ub <- fisherz2r(ma_e_20_sd$ci.ub)
ma_e_21_sd <- rma(yi=yi_e_sd_21, vi=vi_e_sd_21, data=meta, weights = n, level = 95)
ma_e_21_sd_beta <- fisherz2r(ma_e_21_sd$beta)
ma_e_21_sd_lb <- fisherz2r(ma_e_21_sd$ci.lb)
ma_e_21_sd_ub <- fisherz2r(ma_e_21_sd$ci.ub)
ma_e_22_sd <- rma(yi=yi_e_sd_22, vi=vi_e_sd_22, data=meta, weights = n, level = 95)
ma_e_22_sd_beta <- fisherz2r(ma_e_22_sd$beta)
ma_e_22_sd_lb <- fisherz2r(ma_e_22_sd$ci.lb)
ma_e_22_sd_ub <- fisherz2r(ma_e_22_sd$ci.ub)
ma_e_23_sd <- rma(yi=yi_e_sd_23, vi=vi_e_sd_23, data=meta, weights = n, level = 95)
ma_e_23_sd_beta <- fisherz2r(ma_e_23_sd$beta)
ma_e_23_sd_lb <- fisherz2r(ma_e_23_sd$ci.lb)
ma_e_23_sd_ub <- fisherz2r(ma_e_23_sd$ci.ub)
ma_e_24_sd <- rma(yi=yi_e_sd_24, vi=vi_e_sd_24, data=meta, weights = n, level = 95)
ma_e_24_sd_beta <- fisherz2r(ma_e_24_sd$beta)
ma_e_24_sd_lb <- fisherz2r(ma_e_24_sd$ci.lb)
ma_e_24_sd_ub <- fisherz2r(ma_e_24_sd$ci.ub)
ma_e_25_sd <- rma(yi=yi_e_sd_25, vi=vi_e_sd_25, data=meta, weights = n, level = 95)
ma_e_25_sd_beta <- fisherz2r(ma_e_25_sd$beta)
ma_e_25_sd_lb <- fisherz2r(ma_e_25_sd$ci.lb)
ma_e_25_sd_ub <- fisherz2r(ma_e_25_sd$ci.ub)
ma_e_26_sd <- rma(yi=yi_e_sd_26, vi=vi_e_sd_26, data=meta, weights = n, level = 95)
ma_e_26_sd_beta <- fisherz2r(ma_e_26_sd$beta)
ma_e_26_sd_lb <- fisherz2r(ma_e_26_sd$ci.lb)
ma_e_26_sd_ub <- fisherz2r(ma_e_26_sd$ci.ub)
ma_e_27_sd <- rma(yi=yi_e_sd_27, vi=vi_e_sd_27, data=meta, weights = n, level = 95)
ma_e_27_sd_beta <- fisherz2r(ma_e_27_sd$beta)
ma_e_27_sd_lb <- fisherz2r(ma_e_27_sd$ci.lb)
ma_e_27_sd_ub <- fisherz2r(ma_e_27_sd$ci.ub)
ma_e_28_sd <- rma(yi=yi_e_sd_28, vi=vi_e_sd_28, data=meta, weights = n, level = 95)
ma_e_28_sd_beta <- fisherz2r(ma_e_28_sd$beta)
ma_e_28_sd_lb <- fisherz2r(ma_e_28_sd$ci.lb)
ma_e_28_sd_ub <- fisherz2r(ma_e_28_sd$ci.ub)
ma_e_29_sd <- rma(yi=yi_e_sd_29, vi=vi_e_sd_29, data=meta, weights = n, level = 95)
ma_e_29_sd_beta <- fisherz2r(ma_e_29_sd$beta)
ma_e_29_sd_lb <- fisherz2r(ma_e_29_sd$ci.lb)
ma_e_29_sd_ub <- fisherz2r(ma_e_29_sd$ci.ub)
ma_e_30_sd <- rma(yi=yi_e_sd_30, vi=vi_e_sd_30, data=meta, weights = n, level = 95)
ma_e_30_sd_beta <- fisherz2r(ma_e_30_sd$beta)
ma_e_30_sd_lb <- fisherz2r(ma_e_30_sd$ci.lb)
ma_e_30_sd_ub <- fisherz2r(ma_e_30_sd$ci.ub)

# AGREEABLENESS

ma_a_1 <- rma(yi=yi_a_1, vi=vi_a_1, data=meta, weights = n, level = 95)
ma_a_1_beta <- fisherz2r(ma_a_1$beta)
ma_a_1_lb <- fisherz2r(ma_a_1$ci.lb)
ma_a_1_ub <- fisherz2r(ma_a_1$ci.ub)
ma_a_2 <- rma(yi=yi_a_2, vi=vi_a_2, data=meta, weights = n, level = 95)
ma_a_2_beta <- fisherz2r(ma_a_2$beta)
ma_a_2_lb <- fisherz2r(ma_a_2$ci.lb)
ma_a_2_ub <- fisherz2r(ma_a_2$ci.ub)
ma_a_3 <- rma(yi=yi_a_3, vi=vi_a_3, data=meta, weights = n, level = 95)
ma_a_3_beta <- fisherz2r(ma_a_3$beta)
ma_a_3_lb <- fisherz2r(ma_a_3$ci.lb)
ma_a_3_ub <- fisherz2r(ma_a_3$ci.ub)

# mean
ma_a_4_m <- rma(yi=yi_a_m_4, vi=vi_a_m_4, data=meta, weights = n, level = 95)
ma_a_4_m_beta <- fisherz2r(ma_a_4_m$beta)
ma_a_4_m_lb <- fisherz2r(ma_a_4_m$ci.lb)
ma_a_4_m_ub <- fisherz2r(ma_a_4_m$ci.ub)
ma_a_5_m <- rma(yi=yi_a_m_5, vi=vi_a_m_5, data=meta, weights = n, level = 95)
ma_a_5_m_beta <- fisherz2r(ma_a_5_m$beta)
ma_a_5_m_lb <- fisherz2r(ma_a_5_m$ci.lb)
ma_a_5_m_ub <- fisherz2r(ma_a_5_m$ci.ub)
ma_a_6_m <- rma(yi=yi_a_m_6, vi=vi_a_m_6, data=meta, weights = n, level = 95)
ma_a_6_m_beta <- fisherz2r(ma_a_6_m$beta)
ma_a_6_m_lb <- fisherz2r(ma_a_6_m$ci.lb)
ma_a_6_m_ub <- fisherz2r(ma_a_6_m$ci.ub)
ma_a_7_m <- rma(yi=yi_a_m_7, vi=vi_a_m_7, data=meta, weights = n, level = 95)
ma_a_7_m_beta <- fisherz2r(ma_a_7_m$beta)
ma_a_7_m_lb <- fisherz2r(ma_a_7_m$ci.lb)
ma_a_7_m_ub <- fisherz2r(ma_a_7_m$ci.ub)
ma_a_8_m <- rma(yi=yi_a_m_8, vi=vi_a_m_8, data=meta, weights = n, level = 95)
ma_a_8_m_beta <- fisherz2r(ma_a_8_m$beta)
ma_a_8_m_lb <- fisherz2r(ma_a_8_m$ci.lb)
ma_a_8_m_ub <- fisherz2r(ma_a_8_m$ci.ub)
ma_a_9_m <- rma(yi=yi_a_m_9, vi=vi_a_m_9, data=meta, weights = n, level = 95)
ma_a_9_m_beta <- fisherz2r(ma_a_9_m$beta)
ma_a_9_m_lb <- fisherz2r(ma_a_9_m$ci.lb)
ma_a_9_m_ub <- fisherz2r(ma_a_9_m$ci.ub)

#sd
ma_a_10_sd <- rma(yi=yi_a_sd_10, vi=vi_a_sd_10, data=meta, weights = n, level = 95)
ma_a_10_sd_beta <- fisherz2r(ma_a_10_sd$beta)
ma_a_10_sd_lb <- fisherz2r(ma_a_10_sd$ci.lb)
ma_a_10_sd_ub <- fisherz2r(ma_a_10_sd$ci.ub)
ma_a_11_sd <- rma(yi=yi_a_sd_11, vi=vi_a_sd_11, data=meta, weights = n, level = 95)
ma_a_11_sd_beta <- fisherz2r(ma_a_11_sd$beta)
ma_a_11_sd_lb <- fisherz2r(ma_a_11_sd$ci.lb)
ma_a_11_sd_ub <- fisherz2r(ma_a_11_sd$ci.ub)
ma_a_12_sd <- rma(yi=yi_a_sd_12, vi=vi_a_sd_12, data=meta, weights = n, level = 95)
ma_a_12_sd_beta <- fisherz2r(ma_a_12_sd$beta)
ma_a_12_sd_lb <- fisherz2r(ma_a_12_sd$ci.lb)
ma_a_12_sd_ub <- fisherz2r(ma_a_12_sd$ci.ub)
ma_a_13_sd <- rma(yi=yi_a_sd_13, vi=vi_a_sd_13, data=meta, weights = n, level = 95)
ma_a_13_sd_beta <- fisherz2r(ma_a_13_sd$beta)
ma_a_13_sd_lb <- fisherz2r(ma_a_13_sd$ci.lb)
ma_a_13_sd_ub <- fisherz2r(ma_a_13_sd$ci.ub)
ma_a_14_sd <- rma(yi=yi_a_sd_14, vi=vi_a_sd_14, data=meta, weights = n, level = 95)
ma_a_14_sd_beta <- fisherz2r(ma_a_14_sd$beta)
ma_a_14_sd_lb <- fisherz2r(ma_a_14_sd$ci.lb)
ma_a_14_sd_ub <- fisherz2r(ma_a_14_sd$ci.ub)
ma_a_15_sd <- rma(yi=yi_a_sd_15, vi=vi_a_sd_15, data=meta, weights = n, level = 95)
ma_a_15_sd_beta <- fisherz2r(ma_a_15_sd$beta)
ma_a_15_sd_lb <- fisherz2r(ma_a_15_sd$ci.lb)
ma_a_15_sd_ub <- fisherz2r(ma_a_15_sd$ci.ub)
ma_a_16_sd <- rma(yi=yi_a_sd_16, vi=vi_a_sd_16, data=meta, weights = n, level = 95)
ma_a_16_sd_beta <- fisherz2r(ma_a_16_sd$beta)
ma_a_16_sd_lb <- fisherz2r(ma_a_16_sd$ci.lb)
ma_a_16_sd_ub <- fisherz2r(ma_a_16_sd$ci.ub)
ma_a_17_sd <- rma(yi=yi_a_sd_17, vi=vi_a_sd_17, data=meta, weights = n, level = 95)
ma_a_17_sd_beta <- fisherz2r(ma_a_17_sd$beta)
ma_a_17_sd_lb <- fisherz2r(ma_a_17_sd$ci.lb)
ma_a_17_sd_ub <- fisherz2r(ma_a_17_sd$ci.ub)
ma_a_18_sd <- rma(yi=yi_a_sd_18, vi=vi_a_sd_18, data=meta, weights = n, level = 95)
ma_a_18_sd_beta <- fisherz2r(ma_a_18_sd$beta)
ma_a_18_sd_lb <- fisherz2r(ma_a_18_sd$ci.lb)
ma_a_18_sd_ub <- fisherz2r(ma_a_18_sd$ci.ub)
ma_a_19_sd <- rma(yi=yi_a_sd_19, vi=vi_a_sd_19, data=meta, weights = n, level = 95)
ma_a_19_sd_beta <- fisherz2r(ma_a_19_sd$beta)
ma_a_19_sd_lb <- fisherz2r(ma_a_19_sd$ci.lb)
ma_a_19_sd_ub <- fisherz2r(ma_a_19_sd$ci.ub)
ma_a_20_sd <- rma(yi=yi_a_sd_20, vi=vi_a_sd_20, data=meta, weights = n, level = 95)
ma_a_20_sd_beta <- fisherz2r(ma_a_20_sd$beta)
ma_a_20_sd_lb <- fisherz2r(ma_a_20_sd$ci.lb)
ma_a_20_sd_ub <- fisherz2r(ma_a_20_sd$ci.ub)
ma_a_21_sd <- rma(yi=yi_a_sd_21, vi=vi_a_sd_21, data=meta, weights = n, level = 95)
ma_a_21_sd_beta <- fisherz2r(ma_a_21_sd$beta)
ma_a_21_sd_lb <- fisherz2r(ma_a_21_sd$ci.lb)
ma_a_21_sd_ub <- fisherz2r(ma_a_21_sd$ci.ub)
ma_a_22_sd <- rma(yi=yi_a_sd_22, vi=vi_a_sd_22, data=meta, weights = n, level = 95)
ma_a_22_sd_beta <- fisherz2r(ma_a_22_sd$beta)
ma_a_22_sd_lb <- fisherz2r(ma_a_22_sd$ci.lb)
ma_a_22_sd_ub <- fisherz2r(ma_a_22_sd$ci.ub)
ma_a_23_sd <- rma(yi=yi_a_sd_23, vi=vi_a_sd_23, data=meta, weights = n, level = 95)
ma_a_23_sd_beta <- fisherz2r(ma_a_23_sd$beta)
ma_a_23_sd_lb <- fisherz2r(ma_a_23_sd$ci.lb)
ma_a_23_sd_ub <- fisherz2r(ma_a_23_sd$ci.ub)
ma_a_24_sd <- rma(yi=yi_a_sd_24, vi=vi_a_sd_24, data=meta, weights = n, level = 95)
ma_a_24_sd_beta <- fisherz2r(ma_a_24_sd$beta)
ma_a_24_sd_lb <- fisherz2r(ma_a_24_sd$ci.lb)
ma_a_24_sd_ub <- fisherz2r(ma_a_24_sd$ci.ub)
ma_a_25_sd <- rma(yi=yi_a_sd_25, vi=vi_a_sd_25, data=meta, weights = n, level = 95)
ma_a_25_sd_beta <- fisherz2r(ma_a_25_sd$beta)
ma_a_25_sd_lb <- fisherz2r(ma_a_25_sd$ci.lb)
ma_a_25_sd_ub <- fisherz2r(ma_a_25_sd$ci.ub)
ma_a_26_sd <- rma(yi=yi_a_sd_26, vi=vi_a_sd_26, data=meta, weights = n, level = 95)
ma_a_26_sd_beta <- fisherz2r(ma_a_26_sd$beta)
ma_a_26_sd_lb <- fisherz2r(ma_a_26_sd$ci.lb)
ma_a_26_sd_ub <- fisherz2r(ma_a_26_sd$ci.ub)
ma_a_27_sd <- rma(yi=yi_a_sd_27, vi=vi_a_sd_27, data=meta, weights = n, level = 95)
ma_a_27_sd_beta <- fisherz2r(ma_a_27_sd$beta)
ma_a_27_sd_lb <- fisherz2r(ma_a_27_sd$ci.lb)
ma_a_27_sd_ub <- fisherz2r(ma_a_27_sd$ci.ub)
ma_a_28_sd <- rma(yi=yi_a_sd_28, vi=vi_a_sd_28, data=meta, weights = n, level = 95)
ma_a_28_sd_beta <- fisherz2r(ma_a_28_sd$beta)
ma_a_28_sd_lb <- fisherz2r(ma_a_28_sd$ci.lb)
ma_a_28_sd_ub <- fisherz2r(ma_a_28_sd$ci.ub)
ma_a_29_sd <- rma(yi=yi_a_sd_29, vi=vi_a_sd_29, data=meta, weights = n, level = 95)
ma_a_29_sd_beta <- fisherz2r(ma_a_29_sd$beta)
ma_a_29_sd_lb <- fisherz2r(ma_a_29_sd$ci.lb)
ma_a_29_sd_ub <- fisherz2r(ma_a_29_sd$ci.ub)
ma_a_30_sd <- rma(yi=yi_a_sd_30, vi=vi_a_sd_30, data=meta, weights = n, level = 95)
ma_a_30_sd_beta <- fisherz2r(ma_a_30_sd$beta)
ma_a_30_sd_lb <- fisherz2r(ma_a_30_sd$ci.lb)
ma_a_30_sd_ub <- fisherz2r(ma_a_30_sd$ci.ub)

## NEUROTICISM

ma_n_1 <- rma(yi=yi_n_1, vi=vi_n_1, data=meta, weights = n, level = 95)
ma_n_1_beta <- fisherz2r(ma_n_1$beta)
ma_n_1_lb <- fisherz2r(ma_n_1$ci.lb)
ma_n_1_ub <- fisherz2r(ma_n_1$ci.ub)
ma_n_2 <- rma(yi=yi_n_2, vi=vi_n_2, data=meta, weights = n, level = 95)
ma_n_2_beta <- fisherz2r(ma_n_2$beta)
ma_n_2_lb <- fisherz2r(ma_n_2$ci.lb)
ma_n_2_ub <- fisherz2r(ma_n_2$ci.ub)
ma_n_3 <- rma(yi=yi_n_3, vi=vi_n_3, data=meta, weights = n, level = 95)
ma_n_3_beta <- fisherz2r(ma_n_3$beta)
ma_n_3_lb <- fisherz2r(ma_n_3$ci.lb)
ma_n_3_ub <- fisherz2r(ma_n_3$ci.ub)

# mean
ma_n_4_m <- rma(yi=yi_n_m_4, vi=vi_n_m_4, data=meta, weights = n, level = 95)
ma_n_4_m_beta <- fisherz2r(ma_n_4_m$beta)
ma_n_4_m_lb <- fisherz2r(ma_n_4_m$ci.lb)
ma_n_4_m_ub <- fisherz2r(ma_n_4_m$ci.ub)
ma_n_5_m <- rma(yi=yi_n_m_5, vi=vi_n_m_5, data=meta, weights = n, level = 95)
ma_n_5_m_beta <- fisherz2r(ma_n_5_m$beta)
ma_n_5_m_lb <- fisherz2r(ma_n_5_m$ci.lb)
ma_n_5_m_ub <- fisherz2r(ma_n_5_m$ci.ub)
ma_n_6_m <- rma(yi=yi_n_m_6, vi=vi_n_m_6, data=meta, weights = n, level = 95)
ma_n_6_m_beta <- fisherz2r(ma_n_6_m$beta)
ma_n_6_m_lb <- fisherz2r(ma_n_6_m$ci.lb)
ma_n_6_m_ub <- fisherz2r(ma_n_6_m$ci.ub)
ma_n_7_m <- rma(yi=yi_n_m_7, vi=vi_n_m_7, data=meta, weights = n, level = 95)
ma_n_7_m_beta <- fisherz2r(ma_n_7_m$beta)
ma_n_7_m_lb <- fisherz2r(ma_n_7_m$ci.lb)
ma_n_7_m_ub <- fisherz2r(ma_n_7_m$ci.ub)
ma_n_8_m <- rma(yi=yi_n_m_8, vi=vi_n_m_8, data=meta, weights = n, level = 95)
ma_n_8_m_beta <- fisherz2r(ma_n_8_m$beta)
ma_n_8_m_lb <- fisherz2r(ma_n_8_m$ci.lb)
ma_n_8_m_ub <- fisherz2r(ma_n_8_m$ci.ub)
ma_n_9_m <- rma(yi=yi_n_m_9, vi=vi_n_m_9, data=meta, weights = n, level = 95)
ma_n_9_m_beta <- fisherz2r(ma_n_9_m$beta)
ma_n_9_m_lb <- fisherz2r(ma_n_9_m$ci.lb)
ma_n_9_m_ub <- fisherz2r(ma_n_9_m$ci.ub)

#sd
ma_n_10_sd <- rma(yi=yi_n_sd_10, vi=vi_n_sd_10, data=meta, weights = n, level = 95)
ma_n_10_sd_beta <- fisherz2r(ma_n_10_sd$beta)
ma_n_10_sd_lb <- fisherz2r(ma_n_10_sd$ci.lb)
ma_n_10_sd_ub <- fisherz2r(ma_n_10_sd$ci.ub)
ma_n_11_sd <- rma(yi=yi_n_sd_11, vi=vi_n_sd_11, data=meta, weights = n, level = 95)
ma_n_11_sd_beta <- fisherz2r(ma_n_11_sd$beta)
ma_n_11_sd_lb <- fisherz2r(ma_n_11_sd$ci.lb)
ma_n_11_sd_ub <- fisherz2r(ma_n_11_sd$ci.ub)
ma_n_12_sd <- rma(yi=yi_n_sd_12, vi=vi_n_sd_12, data=meta, weights = n, level = 95)
ma_n_12_sd_beta <- fisherz2r(ma_n_12_sd$beta)
ma_n_12_sd_lb <- fisherz2r(ma_n_12_sd$ci.lb)
ma_n_12_sd_ub <- fisherz2r(ma_n_12_sd$ci.ub)
ma_n_13_sd <- rma(yi=yi_n_sd_13, vi=vi_n_sd_13, data=meta, weights = n, level = 95)
ma_n_13_sd_beta <- fisherz2r(ma_n_13_sd$beta)
ma_n_13_sd_lb <- fisherz2r(ma_n_13_sd$ci.lb)
ma_n_13_sd_ub <- fisherz2r(ma_n_13_sd$ci.ub)
ma_n_14_sd <- rma(yi=yi_n_sd_14, vi=vi_n_sd_14, data=meta, weights = n, level = 95)
ma_n_14_sd_beta <- fisherz2r(ma_n_14_sd$beta)
ma_n_14_sd_lb <- fisherz2r(ma_n_14_sd$ci.lb)
ma_n_14_sd_ub <- fisherz2r(ma_n_14_sd$ci.ub)
ma_n_15_sd <- rma(yi=yi_n_sd_15, vi=vi_n_sd_15, data=meta, weights = n, level = 95)
ma_n_15_sd_beta <- fisherz2r(ma_n_15_sd$beta)
ma_n_15_sd_lb <- fisherz2r(ma_n_15_sd$ci.lb)
ma_n_15_sd_ub <- fisherz2r(ma_n_15_sd$ci.ub)
ma_n_16_sd <- rma(yi=yi_n_sd_16, vi=vi_n_sd_16, data=meta, weights = n, level = 95)
ma_n_16_sd_beta <- fisherz2r(ma_n_16_sd$beta)
ma_n_16_sd_lb <- fisherz2r(ma_n_16_sd$ci.lb)
ma_n_16_sd_ub <- fisherz2r(ma_n_16_sd$ci.ub)
ma_n_17_sd <- rma(yi=yi_n_sd_17, vi=vi_n_sd_17, data=meta, weights = n, level = 95)
ma_n_17_sd_beta <- fisherz2r(ma_n_17_sd$beta)
ma_n_17_sd_lb <- fisherz2r(ma_n_17_sd$ci.lb)
ma_n_17_sd_ub <- fisherz2r(ma_n_17_sd$ci.ub)
ma_n_18_sd <- rma(yi=yi_n_sd_18, vi=vi_n_sd_18, data=meta, weights = n, level = 95)
ma_n_18_sd_beta <- fisherz2r(ma_n_18_sd$beta)
ma_n_18_sd_lb <- fisherz2r(ma_n_18_sd$ci.lb)
ma_n_18_sd_ub <- fisherz2r(ma_n_18_sd$ci.ub)
ma_n_19_sd <- rma(yi=yi_n_sd_19, vi=vi_n_sd_19, data=meta, weights = n, level = 95)
ma_n_19_sd_beta <- fisherz2r(ma_n_19_sd$beta)
ma_n_19_sd_lb <- fisherz2r(ma_n_19_sd$ci.lb)
ma_n_19_sd_ub <- fisherz2r(ma_n_19_sd$ci.ub)
ma_n_20_sd <- rma(yi=yi_n_sd_20, vi=vi_n_sd_20, data=meta, weights = n, level = 95)
ma_n_20_sd_beta <- fisherz2r(ma_n_20_sd$beta)
ma_n_20_sd_lb <- fisherz2r(ma_n_20_sd$ci.lb)
ma_n_20_sd_ub <- fisherz2r(ma_n_20_sd$ci.ub)
ma_n_21_sd <- rma(yi=yi_n_sd_21, vi=vi_n_sd_21, data=meta, weights = n, level = 95)
ma_n_21_sd_beta <- fisherz2r(ma_n_21_sd$beta)
ma_n_21_sd_lb <- fisherz2r(ma_n_21_sd$ci.lb)
ma_n_21_sd_ub <- fisherz2r(ma_n_21_sd$ci.ub)
ma_n_22_sd <- rma(yi=yi_n_sd_22, vi=vi_n_sd_22, data=meta, weights = n, level = 95)
ma_n_22_sd_beta <- fisherz2r(ma_n_22_sd$beta)
ma_n_22_sd_lb <- fisherz2r(ma_n_22_sd$ci.lb)
ma_n_22_sd_ub <- fisherz2r(ma_n_22_sd$ci.ub)
ma_n_23_sd <- rma(yi=yi_n_sd_23, vi=vi_n_sd_23, data=meta, weights = n, level = 95)
ma_n_23_sd_beta <- fisherz2r(ma_n_23_sd$beta)
ma_n_23_sd_lb <- fisherz2r(ma_n_23_sd$ci.lb)
ma_n_23_sd_ub <- fisherz2r(ma_n_23_sd$ci.ub)
ma_n_24_sd <- rma(yi=yi_n_sd_24, vi=vi_n_sd_24, data=meta, weights = n, level = 95)
ma_n_24_sd_beta <- fisherz2r(ma_n_24_sd$beta)
ma_n_24_sd_lb <- fisherz2r(ma_n_24_sd$ci.lb)
ma_n_24_sd_ub <- fisherz2r(ma_n_24_sd$ci.ub)
ma_n_25_sd <- rma(yi=yi_n_sd_25, vi=vi_n_sd_25, data=meta, weights = n, level = 95)
ma_n_25_sd_beta <- fisherz2r(ma_n_25_sd$beta)
ma_n_25_sd_lb <- fisherz2r(ma_n_25_sd$ci.lb)
ma_n_25_sd_ub <- fisherz2r(ma_n_25_sd$ci.ub)
ma_n_26_sd <- rma(yi=yi_n_sd_26, vi=vi_n_sd_26, data=meta, weights = n, level = 95)
ma_n_26_sd_beta <- fisherz2r(ma_n_26_sd$beta)
ma_n_26_sd_lb <- fisherz2r(ma_n_26_sd$ci.lb)
ma_n_26_sd_ub <- fisherz2r(ma_n_26_sd$ci.ub)
ma_n_27_sd <- rma(yi=yi_n_sd_27, vi=vi_n_sd_27, data=meta, weights = n, level = 95)
ma_n_27_sd_beta <- fisherz2r(ma_n_27_sd$beta)
ma_n_27_sd_lb <- fisherz2r(ma_n_27_sd$ci.lb)
ma_n_27_sd_ub <- fisherz2r(ma_n_27_sd$ci.ub)
ma_n_28_sd <- rma(yi=yi_n_sd_28, vi=vi_n_sd_28, data=meta, weights = n, level = 95)
ma_n_28_sd_beta <- fisherz2r(ma_n_28_sd$beta)
ma_n_28_sd_lb <- fisherz2r(ma_n_28_sd$ci.lb)
ma_n_28_sd_ub <- fisherz2r(ma_n_28_sd$ci.ub)
ma_n_29_sd <- rma(yi=yi_n_sd_29, vi=vi_n_sd_29, data=meta, weights = n, level = 95)
ma_n_29_sd_beta <- fisherz2r(ma_n_29_sd$beta)
ma_n_29_sd_lb <- fisherz2r(ma_n_29_sd$ci.lb)
ma_n_29_sd_ub <- fisherz2r(ma_n_29_sd$ci.ub)
ma_n_30_sd <- rma(yi=yi_n_sd_30, vi=vi_n_sd_30, data=meta, weights = n, level = 95)
ma_n_30_sd_beta <- fisherz2r(ma_n_30_sd$beta)
ma_n_30_sd_lb <- fisherz2r(ma_n_30_sd$ci.lb)
ma_n_30_sd_ub <- fisherz2r(ma_n_30_sd$ci.ub)

i2_o_1<- format(round(ma_o_1$I2,0),nsmall=0)
i2_o_2<- format(round(ma_o_2$I2,0),nsmall=0)
i2_o_3<- format(round(ma_o_3$I2,0),nsmall=0)
i2_o_4<- format(round(ma_o_4_m$I2,0),nsmall=0)
i2_o_5<- format(round(ma_o_5_m$I2,0),nsmall=0)
i2_o_6<- format(round(ma_o_6_m$I2,0),nsmall=0)
i2_o_7<- format(round(ma_o_7_m$I2,0),nsmall=0)
i2_o_8<- format(round(ma_o_8_m$I2,0),nsmall=0)
i2_o_9<- format(round(ma_o_9_m$I2,0),nsmall=0)
i2_o_10<- format(round(ma_o_10_sd$I2,0),nsmall=0)
i2_o_11<- format(round(ma_o_11_sd$I2,0),nsmall=0)
i2_o_12<- format(round(ma_o_12_sd$I2,0),nsmall=0)
i2_o_13<- format(round(ma_o_13_sd$I2,0),nsmall=0)
i2_o_14<- format(round(ma_o_14_sd$I2,0),nsmall=0)
i2_o_15<- format(round(ma_o_15_sd$I2,0),nsmall=0)
i2_o_16<- format(round(ma_o_16_sd$I2,0),nsmall=0)
i2_o_17<- format(round(ma_o_17_sd$I2,0),nsmall=0)
i2_o_18<- format(round(ma_o_18_sd$I2,0),nsmall=0)
i2_o_19<- format(round(ma_o_19_sd$I2,0),nsmall=0)
i2_o_20<- format(round(ma_o_20_sd$I2,0),nsmall=0)
i2_o_21<- format(round(ma_o_21_sd$I2,0),nsmall=0)
i2_o_22<- format(round(ma_o_22_sd$I2,0),nsmall=0)
i2_o_23<- format(round(ma_o_23_sd$I2,0),nsmall=0)
i2_o_24<- format(round(ma_o_24_sd$I2,0),nsmall=0)
i2_o_25<- format(round(ma_o_25_sd$I2,0),nsmall=0)
i2_o_26<- format(round(ma_o_26_sd$I2,0),nsmall=0)
i2_o_27<- format(round(ma_o_27_sd$I2,0),nsmall=0)
i2_o_28<- format(round(ma_o_28_sd$I2,0),nsmall=0)
i2_o_29<- format(round(ma_o_29_sd$I2,0),nsmall=0)
i2_o_30<- format(round(ma_o_30_sd$I2,0),nsmall=0)

i2_c_1<- format(round(ma_c_1$I2,0),nsmall=0)
i2_c_2<- format(round(ma_c_2$I2,0),nsmall=0)
i2_c_3<- format(round(ma_c_3$I2,0),nsmall=0)
i2_c_4<- format(round(ma_c_4_m$I2,0),nsmall=0)
i2_c_5<- format(round(ma_c_5_m$I2,0),nsmall=0)
i2_c_6<- format(round(ma_c_6_m$I2,0),nsmall=0)
i2_c_7<- format(round(ma_c_7_m$I2,0),nsmall=0)
i2_c_8<- format(round(ma_c_8_m$I2,0),nsmall=0)
i2_c_9<- format(round(ma_c_9_m$I2,0),nsmall=0)
i2_c_10<- format(round(ma_c_10_sd$I2,0),nsmall=0)
i2_c_11<- format(round(ma_c_11_sd$I2,0),nsmall=0)
i2_c_12<- format(round(ma_c_12_sd$I2,0),nsmall=0)
i2_c_13<- format(round(ma_c_13_sd$I2,0),nsmall=0)
i2_c_14<- format(round(ma_c_14_sd$I2,0),nsmall=0)
i2_c_15<- format(round(ma_c_15_sd$I2,0),nsmall=0)
i2_c_16<- format(round(ma_c_16_sd$I2,0),nsmall=0)
i2_c_17<- format(round(ma_c_17_sd$I2,0),nsmall=0)
i2_c_18<- format(round(ma_c_18_sd$I2,0),nsmall=0)
i2_c_19<- format(round(ma_c_19_sd$I2,0),nsmall=0)
i2_c_20<- format(round(ma_c_20_sd$I2,0),nsmall=0)
i2_c_21<- format(round(ma_c_21_sd$I2,0),nsmall=0)
i2_c_22<- format(round(ma_c_22_sd$I2,0),nsmall=0)
i2_c_23<- format(round(ma_c_23_sd$I2,0),nsmall=0)
i2_c_24<- format(round(ma_c_24_sd$I2,0),nsmall=0)
i2_c_25<- format(round(ma_c_25_sd$I2,0),nsmall=0)
i2_c_26<- format(round(ma_c_26_sd$I2,0),nsmall=0)
i2_c_27<- format(round(ma_c_27_sd$I2,0),nsmall=0)
i2_c_28<- format(round(ma_c_28_sd$I2,0),nsmall=0)
i2_c_29<- format(round(ma_c_29_sd$I2,0),nsmall=0)
i2_c_30<- format(round(ma_c_30_sd$I2,0),nsmall=0)

i2_e_1<- format(round(ma_e_1$I2,0),nsmall=0)
i2_e_2<- format(round(ma_e_2$I2,0),nsmall=0)
i2_e_3<- format(round(ma_e_3$I2,0),nsmall=0)
i2_e_4<- format(round(ma_e_4_m$I2,0),nsmall=0)
i2_e_5<- format(round(ma_e_5_m$I2,0),nsmall=0)
i2_e_6<- format(round(ma_e_6_m$I2,0),nsmall=0)
i2_e_7<- format(round(ma_e_7_m$I2,0),nsmall=0)
i2_e_8<- format(round(ma_e_8_m$I2,0),nsmall=0)
i2_e_9<- format(round(ma_e_9_m$I2,0),nsmall=0)
i2_e_10<- format(round(ma_e_10_sd$I2,0),nsmall=0)
i2_e_11<- format(round(ma_e_11_sd$I2,0),nsmall=0)
i2_e_12<- format(round(ma_e_12_sd$I2,0),nsmall=0)
i2_e_13<- format(round(ma_e_13_sd$I2,0),nsmall=0)
i2_e_14<- format(round(ma_e_14_sd$I2,0),nsmall=0)
i2_e_15<- format(round(ma_e_15_sd$I2,0),nsmall=0)
i2_e_16<- format(round(ma_e_16_sd$I2,0),nsmall=0)
i2_e_17<- format(round(ma_e_17_sd$I2,0),nsmall=0)
i2_e_18<- format(round(ma_e_18_sd$I2,0),nsmall=0)
i2_e_19<- format(round(ma_e_19_sd$I2,0),nsmall=0)
i2_e_20<- format(round(ma_e_20_sd$I2,0),nsmall=0)
i2_e_21<- format(round(ma_e_21_sd$I2,0),nsmall=0)
i2_e_22<- format(round(ma_e_22_sd$I2,0),nsmall=0)
i2_e_23<- format(round(ma_e_23_sd$I2,0),nsmall=0)
i2_e_24<- format(round(ma_e_24_sd$I2,0),nsmall=0)
i2_e_25<- format(round(ma_e_25_sd$I2,0),nsmall=0)
i2_e_26<- format(round(ma_e_26_sd$I2,0),nsmall=0)
i2_e_27<- format(round(ma_e_27_sd$I2,0),nsmall=0)
i2_e_28<- format(round(ma_e_28_sd$I2,0),nsmall=0)
i2_e_29<- format(round(ma_e_29_sd$I2,0),nsmall=0)
i2_e_30<- format(round(ma_e_30_sd$I2,0),nsmall=0)

i2_a_1<- format(round(ma_a_1$I2,0),nsmall=0)
i2_a_2<- format(round(ma_a_2$I2,0),nsmall=0)
i2_a_3<- format(round(ma_a_3$I2,0),nsmall=0)
i2_a_4<- format(round(ma_a_4_m$I2,0),nsmall=0)
i2_a_5<- format(round(ma_a_5_m$I2,0),nsmall=0)
i2_a_6<- format(round(ma_a_6_m$I2,0),nsmall=0)
i2_a_7<- format(round(ma_a_7_m$I2,0),nsmall=0)
i2_a_8<- format(round(ma_a_8_m$I2,0),nsmall=0)
i2_a_9<- format(round(ma_a_9_m$I2,0),nsmall=0)
i2_a_10<- format(round(ma_a_10_sd$I2,0),nsmall=0)
i2_a_11<- format(round(ma_a_11_sd$I2,0),nsmall=0)
i2_a_12<- format(round(ma_a_12_sd$I2,0),nsmall=0)
i2_a_13<- format(round(ma_a_13_sd$I2,0),nsmall=0)
i2_a_14<- format(round(ma_a_14_sd$I2,0),nsmall=0)
i2_a_15<- format(round(ma_a_15_sd$I2,0),nsmall=0)
i2_a_16<- format(round(ma_a_16_sd$I2,0),nsmall=0)
i2_a_17<- format(round(ma_a_17_sd$I2,0),nsmall=0)
i2_a_18<- format(round(ma_a_18_sd$I2,0),nsmall=0)
i2_a_19<- format(round(ma_a_19_sd$I2,0),nsmall=0)
i2_a_20<- format(round(ma_a_20_sd$I2,0),nsmall=0)
i2_a_21<- format(round(ma_a_21_sd$I2,0),nsmall=0)
i2_a_22<- format(round(ma_a_22_sd$I2,0),nsmall=0)
i2_a_23<- format(round(ma_a_23_sd$I2,0),nsmall=0)
i2_a_24<- format(round(ma_a_24_sd$I2,0),nsmall=0)
i2_a_25<- format(round(ma_a_25_sd$I2,0),nsmall=0)
i2_a_26<- format(round(ma_a_26_sd$I2,0),nsmall=0)
i2_a_27<- format(round(ma_a_27_sd$I2,0),nsmall=0)
i2_a_28<- format(round(ma_a_28_sd$I2,0),nsmall=0)
i2_a_29<- format(round(ma_a_29_sd$I2,0),nsmall=0)
i2_a_30<- format(round(ma_a_30_sd$I2,0),nsmall=0)

i2_n_1<- format(round(ma_n_1$I2,0),nsmall=0)
i2_n_2<- format(round(ma_n_2$I2,0),nsmall=0)
i2_n_3<- format(round(ma_n_3$I2,0),nsmall=0)
i2_n_4<- format(round(ma_n_4_m$I2,0),nsmall=0)
i2_n_5<- format(round(ma_n_5_m$I2,0),nsmall=0)
i2_n_6<- format(round(ma_n_6_m$I2,0),nsmall=0)
i2_n_7<- format(round(ma_n_7_m$I2,0),nsmall=0)
i2_n_8<- format(round(ma_n_8_m$I2,0),nsmall=0)
i2_n_9<- format(round(ma_n_9_m$I2,0),nsmall=0)
i2_n_10<- format(round(ma_n_10_sd$I2,0),nsmall=0)
i2_n_11<- format(round(ma_n_11_sd$I2,0),nsmall=0)
i2_n_12<- format(round(ma_n_12_sd$I2,0),nsmall=0)
i2_n_13<- format(round(ma_n_13_sd$I2,0),nsmall=0)
i2_n_14<- format(round(ma_n_14_sd$I2,0),nsmall=0)
i2_n_15<- format(round(ma_n_15_sd$I2,0),nsmall=0)
i2_n_16<- format(round(ma_n_16_sd$I2,0),nsmall=0)
i2_n_17<- format(round(ma_n_17_sd$I2,0),nsmall=0)
i2_n_18<- format(round(ma_n_18_sd$I2,0),nsmall=0)
i2_n_19<- format(round(ma_n_19_sd$I2,0),nsmall=0)
i2_n_20<- format(round(ma_n_20_sd$I2,0),nsmall=0)
i2_n_21<- format(round(ma_n_21_sd$I2,0),nsmall=0)
i2_n_22<- format(round(ma_n_22_sd$I2,0),nsmall=0)
i2_n_23<- format(round(ma_n_23_sd$I2,0),nsmall=0)
i2_n_24<- format(round(ma_n_24_sd$I2,0),nsmall=0)
i2_n_25<- format(round(ma_n_25_sd$I2,0),nsmall=0)
i2_n_26<- format(round(ma_n_26_sd$I2,0),nsmall=0)
i2_n_27<- format(round(ma_n_27_sd$I2,0),nsmall=0)
i2_n_28<- format(round(ma_n_28_sd$I2,0),nsmall=0)
i2_n_29<- format(round(ma_n_29_sd$I2,0),nsmall=0)
i2_n_30<- format(round(ma_n_30_sd$I2,0),nsmall=0)

# data table
####
table_o_1 <- paste0(format(rd(round(ma_o_1_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_1_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_1_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_2 <- paste0(format(rd(round(ma_o_2_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_2_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_2_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_3 <- paste0(format(rd(round(ma_o_3_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_3_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_3_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_4 <- paste0(format(rd(round(ma_o_4_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_4_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_4_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_5 <- paste0(format(rd(round(ma_o_5_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_5_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_5_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_6 <- paste0(format(rd(round(ma_o_6_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_6_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_6_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_7 <- paste0(format(rd(round(ma_o_7_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_7_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_7_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_8 <- paste0(format(rd(round(ma_o_8_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_8_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_8_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_9 <- paste0(format(rd(round(ma_o_9_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_9_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_9_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_10 <- paste0(format(rd(round(ma_o_10_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_10_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_10_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_11 <- paste0(format(rd(round(ma_o_11_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_11_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_11_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_12 <- paste0(format(rd(round(ma_o_12_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_12_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_12_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_13 <- paste0(format(rd(round(ma_o_13_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_13_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_13_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_14 <- paste0(format(rd(round(ma_o_14_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_14_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_14_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_15 <- paste0(format(rd(round(ma_o_15_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_15_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_15_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_16 <- paste0(format(rd(round(ma_o_16_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_16_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_16_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_17 <- paste0(format(rd(round(ma_o_17_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_17_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_17_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_18 <- paste0(format(rd(round(ma_o_18_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_18_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_18_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_19 <- paste0(format(rd(round(ma_o_19_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_19_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_19_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_20 <- paste0(format(rd(round(ma_o_20_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_20_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_20_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_21 <- paste0(format(rd(round(ma_o_21_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_21_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_21_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_22 <- paste0(format(rd(round(ma_o_22_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_22_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_22_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_23 <- paste0(format(rd(round(ma_o_23_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_23_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_23_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_24 <- paste0(format(rd(round(ma_o_24_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_24_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_24_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_25 <- paste0(format(rd(round(ma_o_25_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_25_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_25_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_26 <- paste0(format(rd(round(ma_o_26_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_26_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_26_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_27 <- paste0(format(rd(round(ma_o_27_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_27_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_27_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_28 <- paste0(format(rd(round(ma_o_28_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_28_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_28_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_29 <- paste0(format(rd(round(ma_o_29_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_29_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_29_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_o_30 <- paste0(format(rd(round(ma_o_30_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_o_30_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_o_30_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")

####
table_c_1 <- paste0(format(rd(round(ma_c_1_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_1_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_1_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_2 <- paste0(format(rd(round(ma_c_2_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_2_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_2_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_3 <- paste0(format(rd(round(ma_c_3_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_3_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_3_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_4 <- paste0(format(rd(round(ma_c_4_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_4_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_4_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_5 <- paste0(format(rd(round(ma_c_5_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_5_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_5_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_6 <- paste0(format(rd(round(ma_c_6_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_6_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_6_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_7 <- paste0(format(rd(round(ma_c_7_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_7_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_7_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_8 <- paste0(format(rd(round(ma_c_8_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_8_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_8_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_9 <- paste0(format(rd(round(ma_c_9_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_9_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_9_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_10 <- paste0(format(rd(round(ma_c_10_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_10_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_10_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_11 <- paste0(format(rd(round(ma_c_11_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_11_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_11_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_12 <- paste0(format(rd(round(ma_c_12_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_12_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_12_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_13 <- paste0(format(rd(round(ma_c_13_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_13_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_13_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_14 <- paste0(format(rd(round(ma_c_14_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_14_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_14_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_15 <- paste0(format(rd(round(ma_c_15_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_15_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_15_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_16 <- paste0(format(rd(round(ma_c_16_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_16_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_16_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_17 <- paste0(format(rd(round(ma_c_17_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_17_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_17_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_18 <- paste0(format(rd(round(ma_c_18_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_18_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_18_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_19 <- paste0(format(rd(round(ma_c_19_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_19_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_19_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_20 <- paste0(format(rd(round(ma_c_20_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_20_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_20_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_21 <- paste0(format(rd(round(ma_c_21_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_21_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_21_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_22 <- paste0(format(rd(round(ma_c_22_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_22_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_22_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_23 <- paste0(format(rd(round(ma_c_23_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_23_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_23_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_24 <- paste0(format(rd(round(ma_c_24_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_24_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_24_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_25 <- paste0(format(rd(round(ma_c_25_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_25_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_25_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_26 <- paste0(format(rd(round(ma_c_26_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_26_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_26_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_27 <- paste0(format(rd(round(ma_c_27_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_27_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_27_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_28 <- paste0(format(rd(round(ma_c_28_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_28_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_28_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_29 <- paste0(format(rd(round(ma_c_29_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_29_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_29_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_c_30 <- paste0(format(rd(round(ma_c_30_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_c_30_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_c_30_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")

####
table_e_1 <- paste0(format(rd(round(ma_e_1_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_1_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_1_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_2 <- paste0(format(rd(round(ma_e_2_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_2_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_2_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_3 <- paste0(format(rd(round(ma_e_3_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_3_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_3_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_4 <- paste0(format(rd(round(ma_e_4_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_4_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_4_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_5 <- paste0(format(rd(round(ma_e_5_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_5_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_5_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_6 <- paste0(format(rd(round(ma_e_6_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_6_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_6_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_7 <- paste0(format(rd(round(ma_e_7_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_7_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_7_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_8 <- paste0(format(rd(round(ma_e_8_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_8_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_8_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_9 <- paste0(format(rd(round(ma_e_9_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_9_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_9_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_10 <- paste0(format(rd(round(ma_e_10_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_10_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_10_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_11 <- paste0(format(rd(round(ma_e_11_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_11_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_11_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_12 <- paste0(format(rd(round(ma_e_12_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_12_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_12_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_13 <- paste0(format(rd(round(ma_e_13_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_13_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_13_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_14 <- paste0(format(rd(round(ma_e_14_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_14_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_14_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_15 <- paste0(format(rd(round(ma_e_15_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_15_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_15_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_16 <- paste0(format(rd(round(ma_e_16_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_16_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_16_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_17 <- paste0(format(rd(round(ma_e_17_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_17_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_17_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_18 <- paste0(format(rd(round(ma_e_18_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_18_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_18_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_19 <- paste0(format(rd(round(ma_e_19_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_19_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_19_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_20 <- paste0(format(rd(round(ma_e_20_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_20_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_20_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_21 <- paste0(format(rd(round(ma_e_21_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_21_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_21_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_22 <- paste0(format(rd(round(ma_e_22_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_22_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_22_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_23 <- paste0(format(rd(round(ma_e_23_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_23_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_23_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_24 <- paste0(format(rd(round(ma_e_24_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_24_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_24_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_25 <- paste0(format(rd(round(ma_e_25_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_25_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_25_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_26 <- paste0(format(rd(round(ma_e_26_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_26_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_26_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_27 <- paste0(format(rd(round(ma_e_27_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_27_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_27_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_28 <- paste0(format(rd(round(ma_e_28_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_28_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_28_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_29 <- paste0(format(rd(round(ma_e_29_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_29_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_29_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_e_30 <- paste0(format(rd(round(ma_e_30_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_e_30_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_e_30_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")

####
table_a_1 <- paste0(format(rd(round(ma_a_1_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_1_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_1_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_2 <- paste0(format(rd(round(ma_a_2_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_2_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_2_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_3 <- paste0(format(rd(round(ma_a_3_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_3_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_3_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_4 <- paste0(format(rd(round(ma_a_4_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_4_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_4_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_5 <- paste0(format(rd(round(ma_a_5_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_5_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_5_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_6 <- paste0(format(rd(round(ma_a_6_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_6_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_6_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_7 <- paste0(format(rd(round(ma_a_7_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_7_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_7_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_8 <- paste0(format(rd(round(ma_a_8_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_8_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_8_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_9 <- paste0(format(rd(round(ma_a_9_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_9_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_9_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_10 <- paste0(format(rd(round(ma_a_10_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_10_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_10_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_11 <- paste0(format(rd(round(ma_a_11_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_11_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_11_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_12 <- paste0(format(rd(round(ma_a_12_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_12_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_12_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_13 <- paste0(format(rd(round(ma_a_13_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_13_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_13_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_14 <- paste0(format(rd(round(ma_a_14_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_14_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_14_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_15 <- paste0(format(rd(round(ma_a_15_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_15_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_15_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_16 <- paste0(format(rd(round(ma_a_16_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_16_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_16_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_17 <- paste0(format(rd(round(ma_a_17_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_17_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_17_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_18 <- paste0(format(rd(round(ma_a_18_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_18_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_18_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_19 <- paste0(format(rd(round(ma_a_19_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_19_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_19_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_20 <- paste0(format(rd(round(ma_a_20_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_20_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_20_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_21 <- paste0(format(rd(round(ma_a_21_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_21_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_21_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_22 <- paste0(format(rd(round(ma_a_22_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_22_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_22_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_23 <- paste0(format(rd(round(ma_a_23_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_23_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_23_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_24 <- paste0(format(rd(round(ma_a_24_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_24_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_24_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_25 <- paste0(format(rd(round(ma_a_25_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_25_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_25_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_26 <- paste0(format(rd(round(ma_a_26_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_26_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_26_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_27 <- paste0(format(rd(round(ma_a_27_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_27_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_27_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_28 <- paste0(format(rd(round(ma_a_28_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_28_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_28_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_29 <- paste0(format(rd(round(ma_a_29_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_29_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_29_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_a_30 <- paste0(format(rd(round(ma_a_30_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_a_30_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_a_30_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")

table_n_1 <- paste0(format(rd(round(ma_n_1_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_1_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_1_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_2 <- paste0(format(rd(round(ma_n_2_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_2_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_2_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_3 <- paste0(format(rd(round(ma_n_3_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_3_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_3_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_4 <- paste0(format(rd(round(ma_n_4_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_4_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_4_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_5 <- paste0(format(rd(round(ma_n_5_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_5_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_5_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_6 <- paste0(format(rd(round(ma_n_6_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_6_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_6_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_7 <- paste0(format(rd(round(ma_n_7_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_7_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_7_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_8 <- paste0(format(rd(round(ma_n_8_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_8_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_8_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_9 <- paste0(format(rd(round(ma_n_9_m_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_9_m_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_9_m_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_10 <- paste0(format(rd(round(ma_n_10_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_10_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_10_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_11 <- paste0(format(rd(round(ma_n_11_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_11_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_11_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_12 <- paste0(format(rd(round(ma_n_12_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_12_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_12_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_13 <- paste0(format(rd(round(ma_n_13_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_13_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_13_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_14 <- paste0(format(rd(round(ma_n_14_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_14_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_14_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_15 <- paste0(format(rd(round(ma_n_15_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_15_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_15_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_16 <- paste0(format(rd(round(ma_n_16_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_16_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_16_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_17 <- paste0(format(rd(round(ma_n_17_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_17_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_17_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_18 <- paste0(format(rd(round(ma_n_18_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_18_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_18_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_19 <- paste0(format(rd(round(ma_n_19_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_19_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_19_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_20 <- paste0(format(rd(round(ma_n_20_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_20_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_20_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_21 <- paste0(format(rd(round(ma_n_21_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_21_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_21_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_22 <- paste0(format(rd(round(ma_n_22_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_22_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_22_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_23 <- paste0(format(rd(round(ma_n_23_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_23_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_23_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_24 <- paste0(format(rd(round(ma_n_24_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_24_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_24_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_25 <- paste0(format(rd(round(ma_n_25_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_25_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_25_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_26 <- paste0(format(rd(round(ma_n_26_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_26_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_26_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_27 <- paste0(format(rd(round(ma_n_27_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_27_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_27_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_28 <- paste0(format(rd(round(ma_n_28_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_28_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_28_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_29 <- paste0(format(rd(round(ma_n_29_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_29_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_29_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")
table_n_30 <- paste0(format(rd(round(ma_n_30_sd_beta,2), add = FALSE, digits = 2),nsmall=2)," [",format(rd(round(ma_n_30_sd_lb,2), add = FALSE, digits = 2),nsmall=2),"; ",format(rd(round(ma_n_30_sd_ub,2), add = FALSE, digits = 2),nsmall=2),"]")

# GGPLOTNOW OPENNESS

tsize <- 3
er_height <- 0.5


label <- c("xNH", "xPH", "xPN", "xHN", "xHP", "xNH", "xNP", 
           "xPH", "xPN", "xHH", "xNN", "xPP", 
           "HA a", "NA a", "PA a","HA cMSSD", "NA cMSSD", "PA cMSSD",
           "HA MSSD", "NA MSSD", "PA MSSD", "HA cSD", "NA cSD", "PA cSD", "HA SD", "NA SD", "PA SD",
           "HA mean", "NA Mean", "PA Mean", NA)
mean_o  <- c(ma_o_30_sd_beta, ma_o_29_sd_beta, ma_o_28_sd_beta, ma_o_27_sd_beta, ma_o_26_sd_beta, 
             ma_o_25_sd_beta, ma_o_24_sd_beta, ma_o_23_sd_beta, ma_o_22_sd_beta, ma_o_21_sd_beta, 
             ma_o_20_sd_beta, ma_o_19_sd_beta, ma_o_18_sd_beta, ma_o_17_sd_beta, ma_o_16_sd_beta,
             ma_o_15_sd_beta, ma_o_14_sd_beta, ma_o_13_sd_beta, ma_o_12_sd_beta, ma_o_11_sd_beta,
             ma_o_10_sd_beta, ma_o_9_m_beta, ma_o_8_m_beta, ma_o_7_m_beta, ma_o_6_m_beta,
             ma_o_5_m_beta, ma_o_4_m_beta, ma_o_3_beta, ma_o_2_beta, ma_o_1_beta, NA) 
lower_o <- c(ma_o_30_sd_lb, ma_o_29_sd_lb, ma_o_28_sd_lb, ma_o_27_sd_lb, ma_o_26_sd_lb, 
             ma_o_25_sd_lb, ma_o_24_sd_lb, ma_o_23_sd_lb, ma_o_22_sd_lb, ma_o_21_sd_lb, 
             ma_o_20_sd_lb, ma_o_19_sd_lb, ma_o_18_sd_lb, ma_o_17_sd_lb, ma_o_16_sd_lb,
             ma_o_15_sd_lb, ma_o_14_sd_lb, ma_o_13_sd_lb, ma_o_12_sd_lb, ma_o_11_sd_lb,
             ma_o_10_sd_lb, ma_o_9_m_lb, ma_o_8_m_lb, ma_o_7_m_lb, ma_o_6_m_lb,
             ma_o_5_m_lb, ma_o_4_m_lb, ma_o_3_lb, ma_o_2_lb, ma_o_1_lb, NA)
upper_o <- c(ma_o_30_sd_ub, ma_o_29_sd_ub, ma_o_28_sd_ub, ma_o_27_sd_ub, ma_o_26_sd_ub, 
             ma_o_25_sd_ub, ma_o_24_sd_ub, ma_o_23_sd_ub, ma_o_22_sd_ub, ma_o_21_sd_ub, 
             ma_o_20_sd_ub, ma_o_19_sd_ub, ma_o_18_sd_ub, ma_o_17_sd_ub, ma_o_16_sd_ub,
             ma_o_15_sd_ub, ma_o_14_sd_ub, ma_o_13_sd_ub, ma_o_12_sd_ub, ma_o_11_sd_ub,
             ma_o_10_sd_ub, ma_o_9_m_ub, ma_o_8_m_ub, ma_o_7_m_ub, ma_o_6_m_ub,
             ma_o_5_m_ub, ma_o_4_m_ub, ma_o_3_ub, ma_o_2_ub, ma_o_1_ub, NA)
df_o <- data.frame(label, mean_o, lower_o, upper_o)
#df_o$label <- factor(df_o$label, levels=rev(df_o$label))

dat_o <- data.frame(group = factor(c("1","2","3","4","5","6","7","8","9","10",
                                     "11","12","13","14","15","16","17","18","19","20",
                                     "21","22","23","24","25","26","27","28","29","30","31"), 
                                   levels=c("1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30","31")),
                    cen = mean_o,
                    low = lower_o,
                    high = upper_o,
                    color = factor(c(0,0,0,0,0,0,0,0,0,0,
                                     0,0,0,0,0,0,0,0,0,0,
                                     0,1,0,1,1,0,1,0,0,1,
                                     0)))

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

cart <- .75
nudge <- 0.62
size <- 3
p1 <- ggplot(dat_o,aes(mean_o,group, colour = color)) + 
  geom_point(size=tsize, shape=18) +
  theme(legend.position = "none") +
  geom_errorbarh(aes(xmax = upper_o, xmin = lower_o), height = er_height) +

  geom_text(data=dat_o[dat_o$color == 1,],
            aes(high,
                label= paste0(f_num(cen,2)," [",f_num(low,2),"; ",f_num(high,2),"]")
                ), 
                #nudge_x = -dat_o$cen[dat_o$color == 1]+dat_o$high[dat_o$color == 1],
                nudge_x = -dat_o$high[dat_o$color == 1]+nudge,
                #nudge_x = .07,
                size = size) +
  coord_cartesian(xlim = c(-.5,cart))+
  scale_color_manual(values = c("grey","black")) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks=c(-.5,-.3,-.1,0,.1,.3,.5), labels = c(-.5,-.3,-.1,0,.1,.3,.5)) +
  labs(x="r with openness", y="")


lab_o <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31")
                                , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                           "20","19","18","17","16","15","14","13","12","11",
                                           "10","9","8","7","6","5","4","3","2","1")),
                    V05 = rep(c(1,2,3,4),each=31),
                    V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                           "","","","","","","","","","","",
                           "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                           "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA A","NA A","HA A","xPP","xNN", "xHH",
                           "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH",    
                           "r [95% CI]",table_o_1,table_o_2,table_o_3,table_o_4,table_o_5,table_o_6,table_o_7,table_o_8,table_o_9,table_o_10,
                           table_o_11,table_o_12,table_o_13,table_o_14,table_o_15,table_o_16,table_o_17,table_o_18,table_o_19,table_o_20,
                           table_o_21,table_o_22,table_o_23,table_o_24,table_o_25,table_o_26,table_o_27,table_o_28,table_o_29,table_o_30,
                           "I2",i2_o_1,i2_o_2,i2_o_3,i2_o_4,i2_o_5,i2_o_6,i2_o_7,i2_o_8,i2_o_9,i2_o_10,
                           i2_o_11,i2_o_12,i2_o_13,i2_o_14,i2_o_15,i2_o_16,i2_o_17,i2_o_18,i2_o_19,i2_o_20,
                           i2_o_21,i2_o_22,i2_o_23,i2_o_24,i2_o_25,i2_o_26,i2_o_27,i2_o_28,i2_o_29,i2_o_30)
)



data_table1 <- ggplot(lab_o, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  theme(legend.position = "none") +
  geom_hline(aes(yintercept=c(30.5))) +  
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))

## GGPLOT CONSCIENTIOUSNESS
label <- c("xNH", "xPH", "xPN", "xHN", "xHP", "xNH", "xNP", 
           "xPH", "xPN", "xHH", "xNN", "xPP", 
           "HA a", "NA a", "PA a","HA cMSSD", "NA cMSSD", "PA cMSSD",
           "HA MSSD", "NA MSSD", "PA MSSD", "HA cSD", "NA cSD", "PA cSD", "HA SD", "NA SD", "PA SD",
           "HA mean", "NA Mean", "PA Mean", NA)
mean_c  <- c(ma_c_30_sd_beta, ma_c_29_sd_beta, ma_c_28_sd_beta, ma_c_27_sd_beta, ma_c_26_sd_beta, 
             ma_c_25_sd_beta, ma_c_24_sd_beta, ma_c_23_sd_beta, ma_c_22_sd_beta, ma_c_21_sd_beta, 
             ma_c_20_sd_beta, ma_c_19_sd_beta, ma_c_18_sd_beta, ma_c_17_sd_beta, ma_c_16_sd_beta,
             ma_c_15_sd_beta, ma_c_14_sd_beta, ma_c_13_sd_beta, ma_c_12_sd_beta, ma_c_11_sd_beta,
             ma_c_10_sd_beta, ma_c_9_m_beta, ma_c_8_m_beta, ma_c_7_m_beta, ma_c_6_m_beta,
             ma_c_5_m_beta, ma_c_4_m_beta, ma_c_3_beta, ma_c_2_beta, ma_c_1_beta, NA) 
lower_c <- c(ma_c_30_sd_lb, ma_c_29_sd_lb, ma_c_28_sd_lb, ma_c_27_sd_lb, ma_c_26_sd_lb, 
             ma_c_25_sd_lb, ma_c_24_sd_lb, ma_c_23_sd_lb, ma_c_22_sd_lb, ma_c_21_sd_lb, 
             ma_c_20_sd_lb, ma_c_19_sd_lb, ma_c_18_sd_lb, ma_c_17_sd_lb, ma_c_16_sd_lb,
             ma_c_15_sd_lb, ma_c_14_sd_lb, ma_c_13_sd_lb, ma_c_12_sd_lb, ma_c_11_sd_lb,
             ma_c_10_sd_lb, ma_c_9_m_lb, ma_c_8_m_lb, ma_c_7_m_lb, ma_c_6_m_lb,
             ma_c_5_m_lb, ma_c_4_m_lb, ma_c_3_lb, ma_c_2_lb, ma_c_1_lb, NA)
upper_c <- c(ma_c_30_sd_ub, ma_c_29_sd_ub, ma_c_28_sd_ub, ma_c_27_sd_ub, ma_c_26_sd_ub, 
             ma_c_25_sd_ub, ma_c_24_sd_ub, ma_c_23_sd_ub, ma_c_22_sd_ub, ma_c_21_sd_ub, 
             ma_c_20_sd_ub, ma_c_19_sd_ub, ma_c_18_sd_ub, ma_c_17_sd_ub, ma_c_16_sd_ub,
             ma_c_15_sd_ub, ma_c_14_sd_ub, ma_c_13_sd_ub, ma_c_12_sd_ub, ma_c_11_sd_ub,
             ma_c_10_sd_ub, ma_c_9_m_ub, ma_c_8_m_ub, ma_c_7_m_ub, ma_c_6_m_ub,
             ma_c_5_m_ub, ma_c_4_m_ub, ma_c_3_ub, ma_c_2_ub, ma_c_1_ub, NA)
df_c <- data.frame(label, mean_c, lower_c, upper_c)
#df_c$label <- factor(df_c$label, levels=rev(df_c$label))


dat_c <- data.frame(group = factor(c("1","2","3","4","5","6","7","8","9","10",
                                     "11","12","13","14","15","16","17","18","19","20",
                                     "21","22","23","24","25","26","27","28","29","30","31"), 
                                   levels=c("1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30","31")),
                    cen = mean_c,
                    low = lower_c,
                    high = upper_c,
                    color = factor(c(0,0,0,0,0,0,0,0,0,0,
                                     0,0,0,0,0,0,0,0,0,0,
                                     0,0,0,0,0,0,0,1,1,1,
                                     0)))

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

p2 <- ggplot(dat_c,aes(mean_c,group, colour = color)) + 
  geom_point(size=tsize, shape=18) +
  theme(legend.position = "none") +
  geom_errorbarh(aes(xmax = upper_c, xmin = lower_c), height = er_height) +
  geom_text(data=dat_c[dat_c$color == 1,],
            aes(high,
                label= paste0(f_num(cen,2)," [",f_num(low,2),"; ",f_num(high,2),"]")
            ), 
            nudge_x = -dat_c$high[dat_c$color == 1]+nudge,
            size = size) +
  coord_cartesian(xlim = c(-.5,cart))+
  scale_color_manual(values = c("grey","black")) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks=c(-.5,-.3,-.1,0,.1,.3,.5), labels = c(-.5,-.3,-.1,0,.1,.3,.5)) +
  labs(x="r with conscientiousness", y="")

lab_c <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31")
                                , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                           "20","19","18","17","16","15","14","13","12","11",
                                           "10","9","8","7","6","5","4","3","2","1")),
                    V05 = rep(c(1,2,3,4),each=31),
                    V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                           "","","","","","","","","","","",
                           "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                           "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA A","NA A","HA A","xPP","xNN", "xHH",
                           "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH",     
                           "r [95% CI]",table_c_1,table_c_2,table_c_3,table_c_4,table_c_5,table_c_6,table_c_7,table_c_8,table_c_9,table_c_10,
                           table_c_11,table_c_12,table_c_13,table_c_14,table_c_15,table_c_16,table_c_17,table_c_18,table_c_19,table_c_20,
                           table_c_21,table_c_22,table_c_23,table_c_24,table_c_25,table_c_26,table_c_27,table_c_28,table_c_29,table_c_30,
                           "I2",i2_c_1,i2_c_2,i2_c_3,i2_c_4,i2_c_5,i2_c_6,i2_c_7,i2_c_8,i2_c_9,i2_c_10,
                           i2_c_11,i2_c_12,i2_c_13,i2_c_14,i2_c_15,i2_c_16,i2_c_17,i2_c_18,i2_c_19,i2_c_20,
                           i2_c_21,i2_c_22,i2_c_23,i2_c_24,i2_c_25,i2_c_26,i2_c_27,i2_c_28,i2_c_29,i2_c_30)
)



data_table2 <- ggplot(lab_c, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  geom_hline(aes(yintercept=c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))


## GGPLOT CONSCIENTIOUSNESS
label <- c("xNH", "xPH", "xPN", "xHN", "xHP", "xNH", "xNP", 
           "xPH", "xPN", "xHH", "xNN", "xPP", 
           "HA a", "NA a", "PA a","HA cMSSD", "NA cMSSD", "PA cMSSD",
           "HA MSSD", "NA MSSD", "PA MSSD", "HA cSD", "NA cSD", "PA cSD", "HA SD", "NA SD", "PA SD",
           "HA mean", "NA Mean", "PA Mean", NA)
mean_e  <- c(ma_e_30_sd_beta, ma_e_29_sd_beta, ma_e_28_sd_beta, ma_e_27_sd_beta, ma_e_26_sd_beta, 
             ma_e_25_sd_beta, ma_e_24_sd_beta, ma_e_23_sd_beta, ma_e_22_sd_beta, ma_e_21_sd_beta, 
             ma_e_20_sd_beta, ma_e_19_sd_beta, ma_e_18_sd_beta, ma_e_17_sd_beta, ma_e_16_sd_beta,
             ma_e_15_sd_beta, ma_e_14_sd_beta, ma_e_13_sd_beta, ma_e_12_sd_beta, ma_e_11_sd_beta,
             ma_e_10_sd_beta, ma_e_9_m_beta, ma_e_8_m_beta, ma_e_7_m_beta, ma_e_6_m_beta,
             ma_e_5_m_beta, ma_e_4_m_beta, ma_e_3_beta, ma_e_2_beta, ma_e_1_beta, NA) 
lower_e <- c(ma_e_30_sd_lb, ma_e_29_sd_lb, ma_e_28_sd_lb, ma_e_27_sd_lb, ma_e_26_sd_lb, 
             ma_e_25_sd_lb, ma_e_24_sd_lb, ma_e_23_sd_lb, ma_e_22_sd_lb, ma_e_21_sd_lb, 
             ma_e_20_sd_lb, ma_e_19_sd_lb, ma_e_18_sd_lb, ma_e_17_sd_lb, ma_e_16_sd_lb,
             ma_e_15_sd_lb, ma_e_14_sd_lb, ma_e_13_sd_lb, ma_e_12_sd_lb, ma_e_11_sd_lb,
             ma_e_10_sd_lb, ma_e_9_m_lb, ma_e_8_m_lb, ma_e_7_m_lb, ma_e_6_m_lb,
             ma_e_5_m_lb, ma_e_4_m_lb, ma_e_3_lb, ma_e_2_lb, ma_e_1_lb, NA)
upper_e <- c(ma_e_30_sd_ub, ma_e_29_sd_ub, ma_e_28_sd_ub, ma_e_27_sd_ub, ma_e_26_sd_ub, 
             ma_e_25_sd_ub, ma_e_24_sd_ub, ma_e_23_sd_ub, ma_e_22_sd_ub, ma_e_21_sd_ub, 
             ma_e_20_sd_ub, ma_e_19_sd_ub, ma_e_18_sd_ub, ma_e_17_sd_ub, ma_e_16_sd_ub,
             ma_e_15_sd_ub, ma_e_14_sd_ub, ma_e_13_sd_ub, ma_e_12_sd_ub, ma_e_11_sd_ub,
             ma_e_10_sd_ub, ma_e_9_m_ub, ma_e_8_m_ub, ma_e_7_m_ub, ma_e_6_m_ub,
             ma_e_5_m_ub, ma_e_4_m_ub, ma_e_3_ub, ma_e_2_ub, ma_e_1_ub, NA)
df_e <- data.frame(label, mean_e, lower_e, upper_e)
#df_e$label <- factor(df_e$label, levels=rev(df_e$label))


dat_e <- data.frame(group = factor(c("1","2","3","4","5","6","7","8","9","10",
                                     "11","12","13","14","15","16","17","18","19","20",
                                     "21","22","23","24","25","26","27","28","29","30","31"), 
                                   levels=c("1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30","31")),
                    cen = mean_e,
                    low = lower_e,
                    high = upper_e,
                    color = factor(c(0,0,0,0,0,0,0,0,0,0,
                                     0,0,0,0,0,0,0,0,0,0,
                                     0,0,1,1,0,0,1,1,1,1,
                                     1)))

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

p3 <- ggplot(dat_e,aes(mean_e,group, colour = color)) + 
  geom_point(size=tsize, shape=18) +
  theme(legend.position = "none") +
  geom_errorbarh(aes(xmax = upper_e, xmin = lower_e), height = er_height) +
  geom_text(data=dat_e[dat_e$color == 1,],
            aes(high,
                label= paste0(f_num(cen,2)," [",f_num(low,2),"; ",f_num(high,2),"]")
            ), 
            nudge_x = -dat_e$high[dat_e$color == 1]+nudge,
            size = size) +
  coord_cartesian(xlim = c(-.5,cart))+
  scale_color_manual(values = c("grey","black")) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks=c(-.5,-.3,-.1,0,.1,.3,.5), labels = c(-.5,-.3,-.1,0,.1,.3,.5)) +
  labs(x="r  with extraversion", y="")

lab_e <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31")
                                , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                           "20","19","18","17","16","15","14","13","12","11",
                                           "10","9","8","7","6","5","4","3","2","1")),
                    V05 = rep(c(1,2,3,4),each=31),
                    V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                           "","","","","","","","","","","",
                           "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                           "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA A","NA A","HA A","xPP","xNN", "xHH",
                           "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH",     
                           "r [95% CI]",table_e_1,table_e_2,table_e_3,table_e_4,table_e_5,table_e_6,table_e_7,table_e_8,table_e_9,table_e_10,
                           table_e_11,table_e_12,table_e_13,table_e_14,table_e_15,table_e_16,table_e_17,table_e_18,table_e_19,table_e_20,
                           table_e_21,table_e_22,table_e_23,table_e_24,table_e_25,table_e_26,table_e_27,table_e_28,table_e_29,table_e_30,
                           "I2",i2_e_1,i2_e_2,i2_e_3,i2_e_4,i2_e_5,i2_e_6,i2_e_7,i2_e_8,i2_e_9,i2_e_10,
                           i2_e_11,i2_e_12,i2_e_13,i2_e_14,i2_e_15,i2_e_16,i2_e_17,i2_e_18,i2_e_19,i2_e_20,
                           i2_e_21,i2_e_22,i2_e_23,i2_e_24,i2_e_25,i2_e_26,i2_e_27,i2_e_28,i2_e_29,i2_e_30)
)



data_table3 <- ggplot(lab_e, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  geom_hline(aes(yintercept=c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))

## GGPLOT AGREEABLENESS
label <- c("xNH", "xPH", "xPN", "xHN", "xHP", "xNH", "xNP", 
           "xPH", "xPN", "xHH", "xNN", "xPP", 
           "HA a", "NA a", "PA a","HA cMSSD", "NA cMSSD", "PA cMSSD",
           "HA MSSD", "NA MSSD", "PA MSSD", "HA cSD", "NA cSD", "PA cSD", "HA SD", "NA SD", "PA SD",
           "HA mean", "NA Mean", "PA Mean", NA)
mean_a  <- c(ma_a_30_sd_beta, ma_a_29_sd_beta, ma_a_28_sd_beta, ma_a_27_sd_beta, ma_a_26_sd_beta, 
             ma_a_25_sd_beta, ma_a_24_sd_beta, ma_a_23_sd_beta, ma_a_22_sd_beta, ma_a_21_sd_beta, 
             ma_a_20_sd_beta, ma_a_19_sd_beta, ma_a_18_sd_beta, ma_a_17_sd_beta, ma_a_16_sd_beta,
             ma_a_15_sd_beta, ma_a_14_sd_beta, ma_a_13_sd_beta, ma_a_12_sd_beta, ma_a_11_sd_beta,
             ma_a_10_sd_beta, ma_a_9_m_beta, ma_a_8_m_beta, ma_a_7_m_beta, ma_a_6_m_beta,
             ma_a_5_m_beta, ma_a_4_m_beta, ma_a_3_beta, ma_a_2_beta, ma_a_1_beta, NA) 
lower_a <- c(ma_a_30_sd_lb, ma_a_29_sd_lb, ma_a_28_sd_lb, ma_a_27_sd_lb, ma_a_26_sd_lb, 
             ma_a_25_sd_lb, ma_a_24_sd_lb, ma_a_23_sd_lb, ma_a_22_sd_lb, ma_a_21_sd_lb, 
             ma_a_20_sd_lb, ma_a_19_sd_lb, ma_a_18_sd_lb, ma_a_17_sd_lb, ma_a_16_sd_lb,
             ma_a_15_sd_lb, ma_a_14_sd_lb, ma_a_13_sd_lb, ma_a_12_sd_lb, ma_a_11_sd_lb,
             ma_a_10_sd_lb, ma_a_9_m_lb, ma_a_8_m_lb, ma_a_7_m_lb, ma_a_6_m_lb,
             ma_a_5_m_lb, ma_a_4_m_lb, ma_a_3_lb, ma_a_2_lb, ma_a_1_lb, NA)
upper_a <- c(ma_a_30_sd_ub, ma_a_29_sd_ub, ma_a_28_sd_ub, ma_a_27_sd_ub, ma_a_26_sd_ub, 
             ma_a_25_sd_ub, ma_a_24_sd_ub, ma_a_23_sd_ub, ma_a_22_sd_ub, ma_a_21_sd_ub, 
             ma_a_20_sd_ub, ma_a_19_sd_ub, ma_a_18_sd_ub, ma_a_17_sd_ub, ma_a_16_sd_ub,
             ma_a_15_sd_ub, ma_a_14_sd_ub, ma_a_13_sd_ub, ma_a_12_sd_ub, ma_a_11_sd_ub,
             ma_a_10_sd_ub, ma_a_9_m_ub, ma_a_8_m_ub, ma_a_7_m_ub, ma_a_6_m_ub,
             ma_a_5_m_ub, ma_a_4_m_ub, ma_a_3_ub, ma_a_2_ub, ma_a_1_ub, NA)
df_a <- data.frame(label, mean_a, lower_a, upper_a)
#df_a$label <- factor(df_a$label, levels=rev(df_a$label))


dat_a <- data.frame(group = factor(c("1","2","3","4","5","6","7","8","9","10",
                                     "11","12","13","14","15","16","17","18","19","20",
                                     "21","22","23","24","25","26","27","28","29","30","31"), 
                                   levels=c("1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30","31")),
                    cen = mean_a,
                    low = lower_a,
                    high = upper_a,
                    color = factor(c(1,0,0,0,0,0,0,0,0,0,
                                     0,0,1,0,0,0,0,0,0,0,
                                     0,0,0,1,0,0,0,1,1,1,
                                     1)))

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

p4 <- ggplot(dat_a,aes(mean_a,group, colour = color)) + 
  geom_point(size=tsize, shape=18) +
  theme(legend.position = "none") +
  geom_errorbarh(aes(xmax = upper_a, xmin = lower_a), height = er_height) +
  
  geom_text(data=dat_a[dat_a$color == 1,],
            aes(high,
                label= paste0(f_num(cen,2)," [",f_num(low,2),"; ",f_num(high,2),"]")
            ), 
            #nudge_x = -dat_o$cen[dat_o$color == 1]+dat_o$high[dat_o$color == 1],
            nudge_x = -dat_a$high[dat_a$color == 1]+nudge,
            #nudge_x = .07,
            size = size) +
  coord_cartesian(xlim = c(-.5,cart))+
  scale_color_manual(values = c("grey","black")) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks=c(-.5,-.3,-.1,0,.1,.3,.5), labels = c(-.5,-.3,-.1,0,.1,.3,.5)) +
  labs(x="r  with agreeableness", y="")

lab_a <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31",
                                  "1","2","3","4","5","6","7","8","9","10",
                                  "11","12","13","14","15","16","17","18","19","20",
                                  "21","22","23","24","25","26","27","28","29","30","31")
                                , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                           "20","19","18","17","16","15","14","13","12","11",
                                           "10","9","8","7","6","5","4","3","2","1")),
                    V05 = rep(c(1,2,3,4),each=31),
                    V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                           "","","","","","","","","","","",
                           "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                           "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA A","NA A","HA A","xPP","xNN", "xHH",
                           "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH",     
                           "r [95% CI]",table_a_1,table_a_2,table_a_3,table_a_4,table_a_5,table_a_6,table_a_7,table_a_8,table_a_9,table_a_10,
                           table_a_11,table_a_12,table_a_13,table_a_14,table_a_15,table_a_16,table_a_17,table_a_18,table_a_19,table_a_20,
                           table_a_21,table_a_22,table_a_23,table_a_24,table_a_25,table_a_26,table_a_27,table_a_28,table_a_29,table_a_30,
                           "I2",i2_a_1,i2_a_2,i2_a_3,i2_a_4,i2_a_5,i2_a_6,i2_a_7,i2_a_8,i2_a_9,i2_a_10,
                           i2_a_11,i2_a_12,i2_a_13,i2_a_14,i2_a_15,i2_a_16,i2_a_17,i2_a_18,i2_a_19,i2_a_20,
                           i2_a_21,i2_a_22,i2_a_23,i2_a_24,i2_a_25,i2_a_26,i2_a_27,i2_a_28,i2_a_29,i2_a_30)
)



data_table4 <- ggplot(lab_a, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  geom_hline(aes(yintercept=c(30.5))) +  
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))





########################

# GGPLOT NEUROTICISM
label <- c("xNH", "xPH", "xPN", "xHN", "xHP", "xNH", "xNP", 
           "xPH", "xPN", "xHH", "xNN", "xPP", 
           "HA a", "NA a", "PA a","HA cMSSD", "NA cMSSD", "PA cMSSD",
           "HA MSSD", "NA MSSD", "PA MSSD", "HA cSD", "NA cSD", "PA cSD", "HA SD", "NA SD", "PA SD",
           "HA mean", "NA Mean", "PA Mean", NA)
mean_n  <- c(ma_n_30_sd_beta, ma_n_29_sd_beta, ma_n_28_sd_beta, ma_n_27_sd_beta, ma_n_26_sd_beta, 
             ma_n_25_sd_beta, ma_n_24_sd_beta, ma_n_23_sd_beta, ma_n_22_sd_beta, ma_n_21_sd_beta, 
             ma_n_20_sd_beta, ma_n_19_sd_beta, ma_n_18_sd_beta, ma_n_17_sd_beta, ma_n_16_sd_beta,
             ma_n_15_sd_beta, ma_n_14_sd_beta, ma_n_13_sd_beta, ma_n_12_sd_beta, ma_n_11_sd_beta,
             ma_n_10_sd_beta, ma_n_9_m_beta, ma_n_8_m_beta, ma_n_7_m_beta, ma_n_6_m_beta,
             ma_n_5_m_beta, ma_n_4_m_beta, ma_n_3_beta, ma_n_2_beta, ma_n_1_beta, NA) 
lower_n <- c(ma_n_30_sd_lb, ma_n_29_sd_lb, ma_n_28_sd_lb, ma_n_27_sd_lb, ma_n_26_sd_lb, 
             ma_n_25_sd_lb, ma_n_24_sd_lb, ma_n_23_sd_lb, ma_n_22_sd_lb, ma_n_21_sd_lb, 
             ma_n_20_sd_lb, ma_n_19_sd_lb, ma_n_18_sd_lb, ma_n_17_sd_lb, ma_n_16_sd_lb,
             ma_n_15_sd_lb, ma_n_14_sd_lb, ma_n_13_sd_lb, ma_n_12_sd_lb, ma_n_11_sd_lb,
             ma_n_10_sd_lb, ma_n_9_m_lb, ma_n_8_m_lb, ma_n_7_m_lb, ma_n_6_m_lb,
             ma_n_5_m_lb, ma_n_4_m_lb, ma_n_3_lb, ma_n_2_lb, ma_n_1_lb, NA)
upper_n <- c(ma_n_30_sd_ub, ma_n_29_sd_ub, ma_n_28_sd_ub, ma_n_27_sd_ub, ma_n_26_sd_ub, 
             ma_n_25_sd_ub, ma_n_24_sd_ub, ma_n_23_sd_ub, ma_n_22_sd_ub, ma_n_21_sd_ub, 
             ma_n_20_sd_ub, ma_n_19_sd_ub, ma_n_18_sd_ub, ma_n_17_sd_ub, ma_n_16_sd_ub,
             ma_n_15_sd_ub, ma_n_14_sd_ub, ma_n_13_sd_ub, ma_n_12_sd_ub, ma_n_11_sd_ub,
             ma_n_10_sd_ub, ma_n_9_m_ub, ma_n_8_m_ub, ma_n_7_m_ub, ma_n_6_m_ub,
             ma_n_5_m_ub, ma_n_4_m_ub, ma_n_3_ub, ma_n_2_ub, ma_n_1_ub, NA)
df <- data.frame(label, mean_n, lower_n, upper_n)
#df$label <- factor(df$label, levels=rev(df$label))


dat_n <- data.frame(group = factor(c("1","2","3","4","5","6","7","8","9","10",
                                     "11","12","13","14","15","16","17","18","19","20",
                                     "21","22","23","24","25","26","27","28","29","30","31"), 
                                   levels=c("1","2","3","4","5","6","7","8","9","10",
                                            "11","12","13","14","15","16","17","18","19","20",
                                            "21","22","23","24","25","26","27","28","29","30","31")),
                    cen = mean_n,
                    low = lower_n,
                    high = upper_n,
                    color = factor(c(0,0,0,0,0,0,0,0,0,0,
                                     0,0,0,1,0,1,1,0,0,0,
                                     0,0,0,0,1,1,0,1,1,1,
                                     1)))

theme_set(theme_bw())
theme_update(
  axis.line = element_line(colour = "black"),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  panel.background = element_blank(),
  axis.text.y = element_blank(),
  axis.ticks.y = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines")
)

p5 <- ggplot(dat_n,aes(mean_n,group, colour = color)) + 
  geom_point(size=tsize, shape=18) +
  theme(legend.position = "none") +
  geom_errorbarh(aes(xmax = upper_n, xmin = lower_n), height = er_height) +
  
  geom_text(data=dat_n[dat_n$color == 1,],
            aes(high,
                label= paste0(f_num(cen,2)," [",f_num(low,2),"; ",f_num(high,2),"]")
            ), 
            #nudge_x = -dat_o$cen[dat_o$color == 1]+dat_o$high[dat_o$color == 1],
            nudge_x = -dat_n$high[dat_n$color == 1]+nudge,
            #nudge_x = .07,
            size = size) +
  coord_cartesian(xlim = c(-.5,cart))+
  scale_color_manual(values = c("grey","black")) +
  geom_vline(xintercept = 0, linetype = "longdash") +
  scale_x_continuous(breaks=c(-.5,-.3,-.1,0,.1,.3,.5), labels = c(-.5,-.3,-.1,0,.1,.3,.5)) +
  labs(x="r with neuroticism", y="")

lab <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                "11","12","13","14","15","16","17","18","19","20",
                                "21","22","23","24","25","26","27","28","29","30","31",
                                "1","2","3","4","5","6","7","8","9","10",
                                "11","12","13","14","15","16","17","18","19","20",
                                "21","22","23","24","25","26","27","28","29","30","31",
                                "1","2","3","4","5","6","7","8","9","10",
                                "11","12","13","14","15","16","17","18","19","20",
                                "21","22","23","24","25","26","27","28","29","30","31",
                                "1","2","3","4","5","6","7","8","9","10",
                                "11","12","13","14","15","16","17","18","19","20",
                                "21","22","23","24","25","26","27","28","29","30","31")
                              , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                         "20","19","18","17","16","15","14","13","12","11",
                                         "10","9","8","7","6","5","4","3","2","1")),
                  V05 = rep(c(1,2,3,4),each=31),
                  V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                         "","","","","","","","","","","",
                         "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                         "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA A","NA A","HA A","xPP","xNN", "xHH",
                         "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH",    
                         "r [95% CI]",table_n_1,table_n_2,table_n_3,table_n_4,table_n_5,table_n_6,table_n_7,table_n_8,table_n_9,table_n_10,
                         table_n_11,table_n_12,table_n_13,table_n_14,table_n_15,table_n_16,table_n_17,table_n_18,table_n_19,table_n_20,
                         table_n_21,table_n_22,table_n_23,table_n_24,table_n_25,table_n_26,table_n_27,table_n_28,table_n_29,table_n_30,
                         "I2",i2_n_1,i2_n_2,i2_n_3,i2_n_4,i2_n_5,i2_n_6,i2_n_7,i2_n_8,i2_n_9,i2_n_10,
                         i2_n_11,i2_n_12,i2_n_13,i2_n_14,i2_n_15,i2_n_16,i2_n_17,i2_n_18,i2_n_19,i2_n_20,
                         i2_n_21,i2_n_22,i2_n_23,i2_n_24,i2_n_25,i2_n_26,i2_n_27,i2_n_28,i2_n_29,i2_n_30)
)



data_table5 <- ggplot(lab, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  geom_hline(aes(yintercept=c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,4.5))


labx <- data.frame(V0 = factor(c("1","2","3","4","5","6","7","8","9","10",
                                 "11","12","13","14","15","16","17","18","19","20",
                                 "21","22","23","24","25","26","27","28","29","30","31",
                                 "1","2","3","4","5","6","7","8","9","10",
                                 "11","12","13","14","15","16","17","18","19","20",
                                 "21","22","23","24","25","26","27","28","29","30","31")
                               , levels=c("31","30","29","28","27","26","25","24","23","22","21",
                                          "20","19","18","17","16","15","14","13","12","11",
                                          "10","9","8","7","6","5","4","3","2","1")),
                   V05 = rep(c(1,2),each=31),
                   V1 = c("Type","Raw Variable","","","Residual Variable I","","","","", "", "Residual Variable II","","","","","","","","","",
                          "","","","","","","","","","","",
                          "Statistic","PA M","NA M","HA M","PA SD","NA SD","HA SD","PA SDc","NA SDc","HA SDc",
                          "PA MSSD","NA MSSD","HA MSSD","PA MSSDc","NA MSSDc","HA MSSDc","PA A","NA A","HA A","xPP","xNN", "xHH",
                          "xPN","xPH","xNP","xNH","xHP","xHN","oPN","oPH","oNH")
)

data_tablex <- ggplot(labx, aes(x = V05, y = V0, label = format(V1, nsmall = 1))) +
  geom_text(size = tsize, hjust=0, vjust=0.5) + theme_bw() +
  geom_hline(aes(yintercept=c(30.5))) + 
  theme(panel.grid.major = element_blank(), 
        legend.position = "none",
        panel.border = element_blank(), 
        axis.text.x = element_text(colour="white"),element_blank(),
        axis.text.y = element_blank(), 
        axis.ticks = element_line(colour="white"),element_blank(),
        plot.margin = unit(c(0,0,0,0,0,0,0), "lines")) +
  labs(x="",y="") +
  coord_cartesian(xlim=c(1,3))
data_tablex



# grid.arrange(data_table3, p3,data_table4, p4, ncol =2)
# 
# pdf("Table3_SOED.pdf", width = 8.27, height=11.69, pointsize =8)
# grid.arrange(data_table5, p5, data_table4, p4, ncol =2)
#dev.off()


########################
# Variance Decomposition Sample 1

a_1_1 <- 0
a_2_1 <- 0
a_3_1 <- 0
a_4_1 <- summary(lm(paste("P.SD~",paste(model_means)),data=sample1))$r.squared #.09
a_5_1 <- summary(lm(paste("N.SD~",paste(model_means)),data=sample1))$r.squared #.12
a_6_1 <- summary(lm(paste("H.SD~",paste(model_means)),data=sample1))$r.squared #.55
a_7_1 <- summary(lm(paste("P.rSD~",paste(model_means)),data=sample1))$r.squared #.06
a_8_1 <- summary(lm(paste("N.rSD~",paste(model_means)),data=sample1))$r.squared#.58
a_9_1 <- summary(lm(paste("H.rSD~",paste(model_means)),data=sample1))$r.squared#.27
a_10_1 <- summary(lm(paste("P.MSSD~",paste(model_means)),data=sample1))$r.squared #.10
a_11_1 <- summary(lm(paste("N.MSSD~",paste(model_means)),data=sample1))$r.squared #.20
a_12_1 <- summary(lm(paste("H.MSSD~",paste(model_means)),data=sample1))$r.squared #.46
a_13_1 <- summary(lm(paste("P.rMSSD~",paste(model_means)),data=sample1))$r.squared# .24
a_14_1 <- summary(lm(paste("N.rMSSD~",paste(model_means)),data=sample1))$r.squared #.08
a_15_1 <- summary(lm(paste("H.rMSSD~",paste(model_means)),data=sample1))$r.squared #.14
a_16_1 <- summary(lm(paste("P.ALPHA~",paste(model_means)),data=sample1))$r.squared#.50
a_17_1 <- summary(lm(paste("N.ALPHA~",paste(model_means)),data=sample1))$r.squared#.41
a_18_1 <- summary(lm(paste("H.ALPHA~",paste(model_means)),data=sample1))$r.squared#.12
a_19_1 <- summary(lm(paste("pP~",paste(model_means)),data=sample1))$r.squared #.45
a_20_1 <- summary(lm(paste("nN~",paste(model_means)),data=sample1))$r.squared #.49
a_21_1 <- summary(lm(paste("hH~",paste(model_means)),data=sample1))$r.squared#.50
a_22_1 <- summary(lm(paste("pN~",paste(model_means)),data=sample1))$r.squared#.23
a_23_1 <- summary(lm(paste("pH~",paste(model_means)),data=sample1))$r.squared#.73
a_24_1 <- summary(lm(paste("nP~",paste(model_means)),data=sample1))$r.squared#.49
a_25_1 <- summary(lm(paste("nH~",paste(model_means)),data=sample1))$r.squared#.11
a_26_1 <- summary(lm(paste("hP~",paste(model_means)),data=sample1))$r.squared#.16
a_27_1 <- summary(lm(paste("hN~",paste(model_means)),data=sample1))$r.squared#.37
a_28_1 <- summary(lm(paste("P_N~",paste(model_means)),data=sample1))$r.squared#.27
a_29_1 <- summary(lm(paste("P_H~",paste(model_means)),data=sample1))$r.squared#.08
a_30_1 <- summary(lm(paste("N_H~",paste(model_means)),data=sample1))$r.squared#.18

b_1_1 <- 0
b_2_1 <- 0
b_3_1 <- 0
b_4_1 <- 0
b_5_1 <- 0
b_6_1 <- 0
b_7_1 <- 0
b_8_1 <- 0
b_9_1 <- 0
b_10_1 <- (summary(lm(paste("P.MSSD~",paste(model_sd)),data=sample1))$r.squared)-a_10_1
b_11_1 <- (summary(lm(paste("N.MSSD~",paste(model_sd)),data=sample1))$r.squared)-a_11_1
b_12_1 <- (summary(lm(paste("H.MSSD~",paste(model_sd)),data=sample1))$r.squared)-a_12_1 
b_13_1 <- (summary(lm(paste("P.rMSSD~",paste(model_sd)),data=sample1))$r.squared)-a_13_1
b_14_1 <- (summary(lm(paste("N.rMSSD~",paste(model_sd)),data=sample1))$r.squared)-a_14_1 
b_15_1 <- (summary(lm(paste("H.rMSSD~",paste(model_sd)),data=sample1))$r.squared)-a_15_1 
b_16_1 <- (summary(lm(paste("P.ALPHA~",paste(model_sd)),data=sample1))$r.squared)-a_16_1
b_17_1 <- (summary(lm(paste("N.ALPHA~",paste(model_sd)),data=sample1))$r.squared)-a_17_1
b_18_1 <- (summary(lm(paste("H.ALPHA~",paste(model_sd)),data=sample1))$r.squared)-a_18_1
b_19_1 <- (summary(lm(paste("pP~",paste(model_sd)),data=sample1))$r.squared)-a_19_1 
b_20_1 <- (summary(lm(paste("nN~",paste(model_sd)),data=sample1))$r.squared)-a_20_1 
b_21_1 <- (summary(lm(paste("hH~",paste(model_sd)),data=sample1))$r.squared)-a_21_1
b_22_1 <- (summary(lm(paste("pN~",paste(model_sd)),data=sample1))$r.squared)-a_22_1
b_23_1 <- (summary(lm(paste("pH~",paste(model_sd)),data=sample1))$r.squared)-a_23_1
b_24_1 <- (summary(lm(paste("nP~",paste(model_sd)),data=sample1))$r.squared)-a_24_1
b_25_1 <- (summary(lm(paste("nH~",paste(model_sd)),data=sample1))$r.squared)-a_25_1
b_26_1 <- (summary(lm(paste("hP~",paste(model_sd)),data=sample1))$r.squared)-a_26_1
b_27_1 <- (summary(lm(paste("hN~",paste(model_sd)),data=sample1))$r.squared)-a_27_1
b_28_1 <- (summary(lm(paste("P_N~",paste(model_sd)),data=sample1))$r.squared)-a_28_1
b_29_1 <- (summary(lm(paste("P_H~",paste(model_sd)),data=sample1))$r.squared)-a_29_1
b_30_1 <- (summary(lm(paste("N_H~",paste(model_sd)),data=sample1))$r.squared)-a_30_1



d_1_1 <- 1-(a_1_1 + b_1_1)
d_2_1 <- 1-(a_2_1 + b_2_1)
d_3_1 <- 1-(a_3_1 + b_3_1)
d_4_1 <- 1-(a_4_1 + b_4_1)
d_5_1 <- 1-(a_5_1 + b_5_1)
d_6_1 <- 1-(a_6_1 + b_6_1)
d_7_1 <- 1-(a_7_1 + b_7_1)
d_8_1 <- 1-(a_8_1 + b_8_1)
d_9_1 <- 1-(a_9_1 + b_9_1)
d_10_1 <- 1-(a_10_1 + b_10_1)
d_11_1 <- 1-(a_11_1 + b_11_1)
d_12_1 <- 1-(a_12_1 + b_12_1)
d_13_1 <- 1-(a_13_1 + b_13_1)
d_14_1 <- 1-(a_14_1 + b_14_1)
d_15_1 <- 1-(a_15_1 + b_15_1)
d_16_1 <- 1-(a_16_1 + b_16_1)
d_17_1 <- 1-(a_17_1 + b_17_1)
d_18_1 <- 1-(a_18_1 + b_18_1)
d_19_1 <- 1-(a_19_1 + b_19_1)
d_20_1 <- 1-(a_20_1 + b_20_1)
d_21_1 <- 1-(a_21_1 + b_21_1)
d_22_1 <- 1-(a_22_1 + b_22_1)
d_23_1 <- 1-(a_23_1 + b_23_1)
d_24_1 <- 1-(a_24_1 + b_24_1)
d_25_1 <- 1-(a_25_1 + b_25_1)
d_26_1 <- 1-(a_26_1 + b_26_1)
d_27_1 <- 1-(a_27_1 + b_27_1)
d_28_1 <- 1-(a_28_1 + b_28_1)
d_29_1 <- 1-(a_29_1 + b_29_1)
d_30_1 <- 1-(a_30_1 + b_30_1)

stats <- c("PA M","NA M","HA M","PA SD",  "NA SD",  "HA SD",  "PA SDc", "NA SDc", "HA SDc","PA MSSD","NA MSSD", 
           "HA MSSD", "PA MSSDc","NA MSSDc",
           "HA MSSDc","PA a","NA a","HA a" ,"xPP" , "xNN" , 
           "xHH","xPN","xPH","xNP","xNH", "xHP","xHN","oPN","oPH","oNH")
mean_red <- c(a_1_1, a_2_1, a_3_1, a_4_1, a_5_1, a_6_1, a_7_1, a_8_1, a_9_1, a_10_1,
              a_11_1, a_12_1, a_13_1, a_14_1, a_15_1, a_16_1, a_17_1, a_18_1, a_19_1, a_20_1,
              a_21_1, a_22_1, a_23_1, a_24_1, a_25_1, a_26_1, a_27_1, a_28_1, a_29_1, a_30_1)
sd_red <- c(b_1_1, b_2_1, b_3_1, b_4_1, b_5_1, b_6_1, b_7_1, b_8_1, b_9_1, b_10_1,
            b_11_1, b_12_1, b_13_1, b_14_1, b_15_1, b_16_1, b_17_1, b_18_1, b_19_1, b_20_1,
            b_21_1, b_22_1, b_23_1, b_24_1, b_25_1, b_26_1, b_27_1, b_28_1, b_29_1, b_30_1)
unique_err <- c(d_1_1, d_2_1, d_3_1, d_4_1, d_5_1, d_6_1, d_7_1, d_8_1, d_9_1, d_10_1,
                d_11_1, d_12_1, d_13_1, d_14_1, d_15_1, d_16_1, d_17_1, d_18_1, d_19_1, d_20_1,
                d_21_1, d_22_1, d_23_1, d_24_1, d_25_1, d_26_1, d_27_1, d_28_1, d_29_1, d_30_1)



DF <- data.frame(stats,  unique_err, sd_red, mean_red )

DF$stats <- factor(stats, levels = rev(stats))
DF1 <- melt(DF, id.var="stats")
DF1$variable <- factor(DF1$variable, levels = c("sd_red", "mean_red","unique_err"), labels = c("Redundant with scale SD", "Redundant with scale M", "Unique"))
DF1$value <- DF1$value*100

p1x <- ggplot(DF1, aes(x = stats, y = value, fill = variable)) +
  coord_flip() +
  geom_bar(stat = "identity")+
  
  geom_text(aes(x = stats, y = value, label = format(rd((round(value,2)), add = FALSE, digits = 0),nsmall=2), group = variable),
            position = position_stack(vjust = .5), size = 3)+
  
  theme_classic() +
  theme(legend.position = "none") +
  xlab("")+
  ylab("% of total variance") +
  scale_fill_manual(values=c("#ffc595","#ff8080","#E0F0E0","#90e7ea"), 
                    name = "Components of variance"
                    , guide = guide_legend(reverse = TRUE)
  ) 





########################
# Variance Decomposition Sample 2

a_1_2 <- 0
a_2_2 <- 0
a_3_2 <- 0
a_4_2 <- summary(lm(paste("P.SD~",paste(model_means)),data=sample2))$r.squared #.09
a_5_2 <- summary(lm(paste("N.SD~",paste(model_means)),data=sample2))$r.squared #.12
a_6_2 <- summary(lm(paste("H.SD~",paste(model_means)),data=sample2))$r.squared #.55
a_7_2 <- summary(lm(paste("P.rSD~",paste(model_means)),data=sample2))$r.squared #.06
a_8_2 <- summary(lm(paste("N.rSD~",paste(model_means)),data=sample2))$r.squared#.58
a_9_2 <- summary(lm(paste("H.rSD~",paste(model_means)),data=sample2))$r.squared#.27
a_10_2 <- summary(lm(paste("P.MSSD~",paste(model_means)),data=sample2))$r.squared #.10
a_11_2 <- summary(lm(paste("N.MSSD~",paste(model_means)),data=sample2))$r.squared #.20
a_12_2 <- summary(lm(paste("H.MSSD~",paste(model_means)),data=sample2))$r.squared #.46
a_13_2 <- summary(lm(paste("P.rMSSD~",paste(model_means)),data=sample2))$r.squared# .24
a_14_2 <- summary(lm(paste("N.rMSSD~",paste(model_means)),data=sample2))$r.squared #.08
a_15_2 <- summary(lm(paste("H.rMSSD~",paste(model_means)),data=sample2))$r.squared #.14
a_16_2 <- summary(lm(paste("P.ALPHA~",paste(model_means)),data=sample2))$r.squared#.50
a_17_2 <- summary(lm(paste("N.ALPHA~",paste(model_means)),data=sample2))$r.squared#.41
a_18_2 <- summary(lm(paste("H.ALPHA~",paste(model_means)),data=sample2))$r.squared#.12
a_19_2 <- summary(lm(paste("pP~",paste(model_means)),data=sample2))$r.squared #.45
a_20_2 <- summary(lm(paste("nN~",paste(model_means)),data=sample2))$r.squared #.49
a_21_2 <- summary(lm(paste("hH~",paste(model_means)),data=sample2))$r.squared#.50
a_22_2 <- summary(lm(paste("pN~",paste(model_means)),data=sample2))$r.squared#.23
a_23_2 <- summary(lm(paste("pH~",paste(model_means)),data=sample2))$r.squared#.73
a_24_2 <- summary(lm(paste("nP~",paste(model_means)),data=sample2))$r.squared#.49
a_25_2 <- summary(lm(paste("nH~",paste(model_means)),data=sample2))$r.squared#.11
a_26_2 <- summary(lm(paste("hP~",paste(model_means)),data=sample2))$r.squared#.16
a_27_2 <- summary(lm(paste("hN~",paste(model_means)),data=sample2))$r.squared#.37
a_28_2 <- summary(lm(paste("P_N~",paste(model_means)),data=sample2))$r.squared#.27
a_29_2 <- summary(lm(paste("P_H~",paste(model_means)),data=sample2))$r.squared#.08
a_30_2 <- summary(lm(paste("N_H~",paste(model_means)),data=sample2))$r.squared#.18

b_1_2 <- 0
b_2_2 <- 0
b_3_2 <- 0
b_4_2 <- 0
b_5_2 <- 0
b_6_2 <- 0
b_7_2 <- 0
b_8_2 <- 0
b_9_2 <- 0
b_10_2 <- (summary(lm(paste("P.MSSD~",paste(model_sd)),data=sample2))$r.squared)-a_10_2
b_11_2 <- (summary(lm(paste("N.MSSD~",paste(model_sd)),data=sample2))$r.squared)-a_11_2
b_12_2 <- (summary(lm(paste("H.MSSD~",paste(model_sd)),data=sample2))$r.squared)-a_12_2 
b_13_2 <- (summary(lm(paste("P.rMSSD~",paste(model_sd)),data=sample2))$r.squared)-a_13_2
b_14_2 <- (summary(lm(paste("N.rMSSD~",paste(model_sd)),data=sample2))$r.squared)-a_14_2 
b_15_2 <- (summary(lm(paste("H.rMSSD~",paste(model_sd)),data=sample2))$r.squared)-a_15_2 
b_16_2 <- (summary(lm(paste("P.ALPHA~",paste(model_sd)),data=sample2))$r.squared)-a_16_2
b_17_2 <- (summary(lm(paste("N.ALPHA~",paste(model_sd)),data=sample2))$r.squared)-a_17_2
b_18_2 <- (summary(lm(paste("H.ALPHA~",paste(model_sd)),data=sample2))$r.squared)-a_18_2
b_19_2 <- (summary(lm(paste("pP~",paste(model_sd)),data=sample2))$r.squared)-a_19_2 
b_20_2 <- (summary(lm(paste("nN~",paste(model_sd)),data=sample2))$r.squared)-a_20_2 
b_21_2 <- (summary(lm(paste("hH~",paste(model_sd)),data=sample2))$r.squared)-a_21_2
b_22_2 <- (summary(lm(paste("pN~",paste(model_sd)),data=sample2))$r.squared)-a_22_2
b_23_2 <- (summary(lm(paste("pH~",paste(model_sd)),data=sample2))$r.squared)-a_23_2
b_24_2 <- (summary(lm(paste("nP~",paste(model_sd)),data=sample2))$r.squared)-a_24_2
b_25_2 <- (summary(lm(paste("nH~",paste(model_sd)),data=sample2))$r.squared)-a_25_2
b_26_2 <- (summary(lm(paste("hP~",paste(model_sd)),data=sample2))$r.squared)-a_26_2
b_27_2 <- (summary(lm(paste("hN~",paste(model_sd)),data=sample2))$r.squared)-a_27_2
b_28_2 <- (summary(lm(paste("P_N~",paste(model_sd)),data=sample2))$r.squared)-a_28_2
b_29_2 <- (summary(lm(paste("P_H~",paste(model_sd)),data=sample2))$r.squared)-a_29_2
b_30_2 <- (summary(lm(paste("N_H~",paste(model_sd)),data=sample2))$r.squared)-a_30_2

c_1_2 <- (1-(a_1_2 + b_1_2))*(spearman.brown(rt_p_mean["cor"],2,"n")$r.new)
c_2_2 <- (1-(a_2_2 + b_2_2))*(spearman.brown(rt_n_mean["cor"],2,"n")$r.new)
c_3_2 <- (1-(a_3_2 + b_3_2))*(spearman.brown(rt_h_mean["cor"],2,"n")$r.new)
c_4_2 <- (1-(a_4_2 + b_4_2))*(spearman.brown(rt_p_sd_m["cor"],2,"n")$r.new)
c_5_2 <- (1-(a_5_2 + b_5_2))*(spearman.brown(rt_n_sd_m["cor"],2,"n")$r.new)
c_6_2 <- (1-(a_6_2 + b_6_2))*(spearman.brown(rt_h_sd_m["cor"],2,"n")$r.new)
c_7_2 <- (1-(a_7_2 + b_7_2))*(spearman.brown(rt_p_rsd_m["cor"],2,"n")$r.new)
c_8_2 <- (1-(a_8_2 + b_8_2))*(spearman.brown(rt_n_rsd_m["cor"],2,"n")$r.new)
c_9_2 <- (1-(a_9_2 + b_9_2))*(spearman.brown(rt_h_rsd_m["cor"],2,"n")$r.new)
c_10_2 <- (1-(a_10_2 + b_10_2))*(spearman.brown(rt_p_mssd_sd["cor"],2,"n")$r.new)
c_11_2 <- (1-(a_11_2 + b_11_2))*(spearman.brown(rt_n_mssd_sd["cor"],2,"n")$r.new)
c_12_2 <- (1-(a_12_2 + b_12_2))*(spearman.brown(rt_h_mssd_sd["cor"],2,"n")$r.new)
c_13_2 <- (1-(a_13_2 + b_13_2))*(spearman.brown(rt_p_rmssd_sd["cor"],2,"n")$r.new)
c_14_2 <- (1-(a_14_2 + b_14_2))*(spearman.brown(rt_n_rmssd_sd["cor"],2,"n")$r.new)
c_15_2 <- (1-(a_15_2 + b_15_2))*(spearman.brown(rt_h_rmssd_sd["cor"],2,"n")$r.new)
c_16_2 <- (1-(a_16_2 + b_16_2))*(spearman.brown(rt_p_alpha_sd["cor"],2,"n")$r.new)
c_17_2 <- (1-(a_17_2 + b_17_2))*(spearman.brown(rt_n_alpha_sd["cor"],2,"n")$r.new)
c_18_2 <- (1-(a_18_2 + b_18_2))*(spearman.brown(rt_h_alpha_sd["cor"],2,"n")$r.new)
c_19_2 <- (1-(a_19_2 + b_19_2))*(spearman.brown(rt_pP_sd["cor"],2,"n")$r.new)
c_20_2 <- (1-(a_20_2 + b_20_2))*(spearman.brown(rt_nN_sd["cor"],2,"n")$r.new)
c_21_2 <- (1-(a_21_2 + b_21_2))*(spearman.brown(rt_hH_sd["cor"],2,"n")$r.new)
c_22_2 <- (1-(a_22_2 + b_22_2))*(spearman.brown(rt_pN_sd["cor"],2,"n")$r.new)
c_23_2 <- (1-(a_23_2 + b_23_2))*(spearman.brown(rt_pH_sd["cor"],2,"n")$r.new)
c_24_2 <- (1-(a_24_2 + b_24_2))*(spearman.brown(rt_nP_sd["cor"],2,"n")$r.new)
c_25_2 <- (1-(a_25_2 + b_25_2))*(spearman.brown(rt_nH_sd["cor"],2,"n")$r.new)
c_26_2 <- (1-(a_26_2 + b_26_2))*(spearman.brown(rt_hP_sd["cor"],2,"n")$r.new)
c_27_2 <- (1-(a_27_2 + b_27_2))*(spearman.brown(rt_hN_sd["cor"],2,"n")$r.new)
c_28_2 <- (1-(a_28_2 + b_28_2))*(spearman.brown(rt_P_N_sd["cor"],2,"n")$r.new)
c_29_2 <- (1-(a_29_2 + b_29_2))*(spearman.brown(rt_P_H_sd["cor"],2,"n")$r.new)
c_30_2 <- (1-(a_30_2 + b_30_2))*(spearman.brown(rt_N_H_sd["cor"],2,"n")$r.new)

c_1_2 <- if (c_1_2 < 0) 0 else c_1_2
c_2_2 <- if (c_2_2 < 0) 0 else c_2_2
c_3_2 <- if (c_3_2 < 0) 0 else c_3_2
c_4_2 <- if (c_4_2 < 0) 0 else c_4_2
c_5_2 <- if (c_5_2 < 0) 0 else c_5_2
c_6_2 <- if (c_6_2 < 0) 0 else c_6_2
c_7_2 <- if (c_7_2 < 0) 0 else c_7_2
c_8_2 <- if (c_8_2 < 0) 0 else c_8_2
c_9_2 <- if (c_9_2 < 0) 0 else c_9_2
c_10_2 <- if (c_10_2 < 0) 0 else c_10_2
c_11_2 <- if (c_11_2 < 0) 0 else c_11_2
c_12_2 <- if (c_12_2 < 0) 0 else c_12_2
c_13_2 <- if (c_13_2 < 0) 0 else c_13_2
c_14_2 <- if (c_14_2 < 0) 0 else c_14_2
c_15_2 <- if (c_15_2 < 0) 0 else c_15_2
c_16_2 <- if (c_16_2 < 0) 0 else c_16_2
c_17_2 <- if (c_17_2 < 0) 0 else c_17_2
c_18_2 <- if (c_18_2 < 0) 0 else c_18_2
c_19_2 <- if (c_19_2 < 0) 0 else c_19_2
c_20_2 <- if (c_20_2 < 0) 0 else c_20_2
c_21_2 <- if (c_21_2 < 0) 0 else c_21_2
c_22_2 <- if (c_22_2 < 0) 0 else c_22_2
c_23_2 <- if (c_23_2 < 0) 0 else c_23_2
c_24_2 <- if (c_24_2 < 0) 0 else c_24_2
c_25_2 <- if (c_25_2 < 0) 0 else c_25_2
c_26_2 <- if (c_26_2 < 0) 0 else c_26_2
c_27_2 <- if (c_27_2 < 0) 0 else c_27_2
c_28_2 <- if (c_28_2 < 0) 0 else c_28_2
c_29_2 <- if (c_29_2 < 0) 0 else c_29_2
c_30_2 <- if (c_30_2 < 0) 0 else c_30_2


d_1_2 <- 1-(a_1_2 + b_1_2 + c_1_2)
d_2_2 <- 1-(a_2_2 + b_2_2 + c_2_2)
d_3_2 <- 1-(a_3_2 + b_3_2 + c_3_2)
d_4_2 <- 1-(a_4_2 + b_4_2 + c_4_2)
d_5_2 <- 1-(a_5_2 + b_5_2 + c_5_2)
d_6_2 <- 1-(a_6_2 + b_6_2 + c_6_2)
d_7_2 <- 1-(a_7_2 + b_7_2 + c_7_2)
d_8_2 <- 1-(a_8_2 + b_8_2 + c_8_2)
d_9_2 <- 1-(a_9_2 + b_9_2 + c_9_2)
d_10_2 <- 1-(a_10_2 + b_10_2 + c_10_2)
d_11_2 <- 1-(a_11_2 + b_11_2 + c_11_2)
d_12_2 <- 1-(a_12_2 + b_12_2 + c_12_2)
d_13_2 <- 1-(a_13_2 + b_13_2 + c_13_2)
d_14_2 <- 1-(a_14_2 + b_14_2 + c_14_2)
d_15_2 <- 1-(a_15_2 + b_15_2 + c_15_2)
d_16_2 <- 1-(a_16_2 + b_16_2 + c_16_2)
d_17_2 <- 1-(a_17_2 + b_17_2 + c_17_2)
d_18_2 <- 1-(a_18_2 + b_18_2 + c_18_2)
d_19_2 <- 1-(a_19_2 + b_19_2 + c_19_2)
d_20_2 <- 1-(a_20_2 + b_20_2 + c_20_2)
d_21_2 <- 1-(a_21_2 + b_21_2 + c_21_2)
d_22_2 <- 1-(a_22_2 + b_22_2 + c_22_2)
d_23_2 <- 1-(a_23_2 + b_23_2 + c_23_2)
d_24_2 <- 1-(a_24_2 + b_24_2 + c_24_2)
d_25_2 <- 1-(a_25_2 + b_25_2 + c_25_2)
d_26_2 <- 1-(a_26_2 + b_26_2 + c_26_2)
d_27_2 <- 1-(a_27_2 + b_27_2 + c_27_2)
d_28_2 <- 1-(a_28_2 + b_28_2 + c_28_2)
d_29_2 <- 1-(a_29_2 + b_29_2 + c_29_2)
d_30_2 <- 1-(a_30_2 + b_30_2 + c_30_2)


stats <- c("PA M","NA M","HA M","PA SD",  "NA SD",  "HA SD",  "PA SDc", "NA SDc", "HA SDc","PA MSSD","NA MSSD", 
           "HA MSSD", "PA MSSDc","NA MSSDc",
           "HA MSSDc","PA a","NA a","HA a" ,"xPP" , "xNN" , 
           "xHH","xPN","xPH","xNP","xNH", "xHP","xHN","oPN","oPH","oNH")
mean_red <- c(a_1_2, a_2_2, a_3_2, a_4_2, a_5_2, a_6_2, a_7_2, a_8_2, a_9_2, a_10_2,
              a_11_2, a_12_2, a_13_2, a_14_2, a_15_2, a_16_2, a_17_2, a_18_2, a_19_2, a_20_2,
              a_21_2, a_22_2, a_23_2, a_24_2, a_25_2, a_26_2, a_27_2, a_28_2, a_29_2, a_30_2)
sd_red <- c(b_1_2, b_2_2, b_3_2, b_4_2, b_5_2, b_6_2, b_7_2, b_8_2, b_9_2, b_10_2,
            b_11_2, b_12_2, b_13_2, b_14_2, b_15_2, b_16_2, b_17_2, b_18_2, b_19_2, b_20_2,
            b_21_2, b_22_2, b_23_2, b_24_2, b_25_2, b_26_2, b_27_2, b_28_2, b_29_2, b_30_2)
unique_stab <- c(c_1_2, c_2_2, c_3_2, c_4_2, c_5_2, c_6_2, c_7_2, c_8_2, c_9_2, c_10_2,
                 c_11_2, c_12_2, c_13_2, c_14_2, c_15_2, c_16_2, c_17_2, c_18_2, c_19_2, c_20_2,
                 c_21_2, c_22_2, c_23_2, c_24_2, c_25_2, c_26_2, c_27_2, c_28_2, c_29_2, c_30_2)
unique_err <- c(d_1_2, d_2_2, d_3_2, d_4_2, d_5_2, d_6_2, d_7_2, d_8_2, d_9_2, d_10_2,
                d_11_2, d_12_2, d_13_2, d_14_2, d_15_2, d_16_2, d_17_2, d_18_2, d_19_2, d_20_2,
                d_21_2, d_22_2, d_23_2, d_24_2, d_25_2, d_26_2, d_27_2, d_28_2, d_29_2, d_30_2)



DF <- data.frame(stats,  unique_err, unique_stab, sd_red, mean_red )

DF$stats <- factor(stats, levels = rev(stats))
DF1 <- melt(DF, id.var="stats")
DF1$variable <- factor(DF1$variable, levels = c("sd_red", "mean_red","unique_err","unique_stab"), labels = c("Redundant with scale SD", "Redundant with scale M", "Unique", "Unique & reliable"))
DF1$value <- DF1$value*100

p2x <- ggplot(DF1, aes(x = stats, y = value, fill = variable)) +
  coord_flip() +
  geom_bar(stat = "identity")+
  
  geom_text(aes(x = stats, y = value, label = format(rd((round(value,2)), add = FALSE, digits = 0),nsmall=2), group = variable),
            position = position_stack(vjust = .5), size = 3)+
  theme_classic() +
  theme(legend.position = "none") +
  xlab("")+
  ylab("% of total variance") +
  scale_fill_manual(values=c("#ffc595","#ff8080","#E0F0E0","#90e7ea"), 
                    name = "Components of variance"
                    , guide = guide_legend(reverse = TRUE)
  ) 



########################
# Variance Decomposition Sample III

a_1_3 <- 0
a_2_3 <- 0
a_3_3 <- 0
a_4_3 <- summary(lm(paste("P.SD~",paste(model_means)),data=sample3))$r.squared #.09
a_5_3 <- summary(lm(paste("N.SD~",paste(model_means)),data=sample3))$r.squared #.12
a_6_3 <- summary(lm(paste("H.SD~",paste(model_means)),data=sample3))$r.squared #.55
a_7_3 <- summary(lm(paste("P.rSD~",paste(model_means)),data=sample3))$r.squared #.06
a_8_3 <- summary(lm(paste("N.rSD~",paste(model_means)),data=sample3))$r.squared#.58
a_9_3 <- summary(lm(paste("H.rSD~",paste(model_means)),data=sample3))$r.squared#.27
a_10_3 <- summary(lm(paste("P.MSSD~",paste(model_means)),data=sample3))$r.squared #.10
a_11_3 <- summary(lm(paste("N.MSSD~",paste(model_means)),data=sample3))$r.squared #.20
a_12_3 <- summary(lm(paste("H.MSSD~",paste(model_means)),data=sample3))$r.squared #.46
a_13_3 <- summary(lm(paste("P.rMSSD~",paste(model_means)),data=sample3))$r.squared# .24
a_14_3 <- summary(lm(paste("N.rMSSD~",paste(model_means)),data=sample3))$r.squared #.08
a_15_3 <- summary(lm(paste("H.rMSSD~",paste(model_means)),data=sample3))$r.squared #.14
a_16_3 <- summary(lm(paste("P.ALPHA~",paste(model_means)),data=sample3))$r.squared#.50
a_17_3 <- summary(lm(paste("N.ALPHA~",paste(model_means)),data=sample3))$r.squared#.41
a_18_3 <- summary(lm(paste("H.ALPHA~",paste(model_means)),data=sample3))$r.squared#.12
a_19_3 <- summary(lm(paste("pP~",paste(model_means)),data=sample3))$r.squared #.45
a_20_3 <- summary(lm(paste("nN~",paste(model_means)),data=sample3))$r.squared #.49
a_21_3 <- summary(lm(paste("hH~",paste(model_means)),data=sample3))$r.squared#.50
a_22_3 <- summary(lm(paste("pN~",paste(model_means)),data=sample3))$r.squared#.23
a_23_3 <- summary(lm(paste("pH~",paste(model_means)),data=sample3))$r.squared#.73
a_24_3 <- summary(lm(paste("nP~",paste(model_means)),data=sample3))$r.squared#.49
a_25_3 <- summary(lm(paste("nH~",paste(model_means)),data=sample3))$r.squared#.11
a_26_3 <- summary(lm(paste("hP~",paste(model_means)),data=sample3))$r.squared#.16
a_27_3 <- summary(lm(paste("hN~",paste(model_means)),data=sample3))$r.squared#.37
a_28_3 <- summary(lm(paste("P_N~",paste(model_means)),data=sample3))$r.squared#.27
a_29_3 <- summary(lm(paste("P_H~",paste(model_means)),data=sample3))$r.squared#.08
a_30_3 <- summary(lm(paste("N_H~",paste(model_means)),data=sample3))$r.squared#.18

b_1_3 <- 0
b_2_3 <- 0
b_3_3 <- 0
b_4_3 <- 0
b_5_3 <- 0
b_6_3 <- 0
b_7_3 <- 0
b_8_3 <- 0
b_9_3 <- 0
b_10_3 <- (summary(lm(paste("P.MSSD~",paste(model_sd)),data=sample3))$r.squared)-a_10_3
b_11_3 <- (summary(lm(paste("N.MSSD~",paste(model_sd)),data=sample3))$r.squared)-a_11_3
b_12_3 <- (summary(lm(paste("H.MSSD~",paste(model_sd)),data=sample3))$r.squared)-a_12_3 
b_13_3 <- (summary(lm(paste("P.rMSSD~",paste(model_sd)),data=sample3))$r.squared)-a_13_3
b_14_3 <- (summary(lm(paste("N.rMSSD~",paste(model_sd)),data=sample3))$r.squared)-a_14_3 
b_15_3 <- (summary(lm(paste("H.rMSSD~",paste(model_sd)),data=sample3))$r.squared)-a_15_3 
b_16_3 <- (summary(lm(paste("P.ALPHA~",paste(model_sd)),data=sample3))$r.squared)-a_16_3
b_17_3 <- (summary(lm(paste("N.ALPHA~",paste(model_sd)),data=sample3))$r.squared)-a_17_3
b_18_3 <- (summary(lm(paste("H.ALPHA~",paste(model_sd)),data=sample3))$r.squared)-a_18_3
b_19_3 <- (summary(lm(paste("pP~",paste(model_sd)),data=sample3))$r.squared)-a_19_3 
b_20_3 <- (summary(lm(paste("nN~",paste(model_sd)),data=sample3))$r.squared)-a_20_3 
b_21_3 <- (summary(lm(paste("hH~",paste(model_sd)),data=sample3))$r.squared)-a_21_3
b_22_3 <- (summary(lm(paste("pN~",paste(model_sd)),data=sample3))$r.squared)-a_22_3
b_23_3 <- (summary(lm(paste("pH~",paste(model_sd)),data=sample3))$r.squared)-a_23_3
b_24_3 <- (summary(lm(paste("nP~",paste(model_sd)),data=sample3))$r.squared)-a_24_3
b_25_3 <- (summary(lm(paste("nH~",paste(model_sd)),data=sample3))$r.squared)-a_25_3
b_26_3 <- (summary(lm(paste("hP~",paste(model_sd)),data=sample3))$r.squared)-a_26_3
b_27_3 <- (summary(lm(paste("hN~",paste(model_sd)),data=sample3))$r.squared)-a_27_3
b_28_3 <- (summary(lm(paste("P_N~",paste(model_sd)),data=sample3))$r.squared)-a_28_3
b_29_3 <- (summary(lm(paste("P_H~",paste(model_sd)),data=sample3))$r.squared)-a_29_3
b_30_3 <- (summary(lm(paste("N_H~",paste(model_sd)),data=sample3))$r.squared)-a_30_3

                             

c_1_3 <- (1-(a_1_3 + b_1_3))*(spearman.brown(rt2_p_mean["cor"],2,"n")$r.new)
c_2_3 <- (1-(a_2_3 + b_2_3))*(spearman.brown(rt2_n_mean["cor"],2,"n")$r.new)
c_3_3 <- (1-(a_3_3 + b_3_3))*(spearman.brown(rt2_h_mean["cor"],2,"n")$r.new)
c_4_3 <- (1-(a_4_3 + b_4_3))*(spearman.brown(rt2_p_sd_m["cor"],2,"n")$r.new)
c_5_3 <- (1-(a_5_3 + b_5_3))*(spearman.brown(rt2_n_sd_m["cor"],2,"n")$r.new)
c_6_3 <- (1-(a_6_3 + b_6_3))*(spearman.brown(rt2_h_sd_m["cor"],2,"n")$r.new)
c_7_3 <- (1-(a_7_3 + b_7_3))*(spearman.brown(rt2_p_rsd_m["cor"],2,"n")$r.new)
c_8_3 <- (1-(a_8_3 + b_8_3))*(spearman.brown(rt2_n_rsd_m["cor"],2,"n")$r.new)
c_9_3 <- (1-(a_9_3 + b_9_3))*(spearman.brown(rt2_h_rsd_m["cor"],2,"n")$r.new)
c_10_3 <- (1-(a_10_3 + b_10_3))*(spearman.brown(rt2_p_mssd_sd["cor"],2,"n")$r.new)
c_11_3 <- (1-(a_11_3 + b_11_3))*(spearman.brown(rt2_n_mssd_sd["cor"],2,"n")$r.new)
c_12_3 <- (1-(a_12_3 + b_12_3))*(spearman.brown(rt2_h_mssd_sd["cor"],2,"n")$r.new)
c_13_3 <- (1-(a_13_3 + b_13_3))*(spearman.brown(rt2_p_rmssd_sd["cor"],2,"n")$r.new)
c_14_3 <- (1-(a_14_3 + b_14_3))*(spearman.brown(rt2_n_rmssd_sd["cor"],2,"n")$r.new)
c_15_3 <- (1-(a_15_3 + b_15_3))*(spearman.brown(rt2_h_rmssd_sd["cor"],2,"n")$r.new)
c_16_3 <- (1-(a_16_3 + b_16_3))*(spearman.brown(rt2_p_alpha_sd["cor"],2,"n")$r.new)
c_17_3 <- (1-(a_17_3 + b_17_3))*(spearman.brown(rt2_n_alpha_sd["cor"],2,"n")$r.new)
c_18_3 <- (1-(a_18_3 + b_18_3))*(spearman.brown(rt2_h_alpha_sd["cor"],2,"n")$r.new)
c_19_3 <- (1-(a_19_3 + b_19_3))*(spearman.brown(rt2_pP_sd["cor"],2,"n")$r.new)
c_20_3 <- (1-(a_20_3 + b_20_3))*(spearman.brown(rt2_nN_sd["cor"],2,"n")$r.new)
c_21_3 <- (1-(a_21_3 + b_21_3))*(spearman.brown(rt2_hH_sd["cor"],2,"n")$r.new)
c_22_3 <- (1-(a_22_3 + b_22_3))*(spearman.brown(rt2_pN_sd["cor"],2,"n")$r.new)
c_23_3 <- (1-(a_23_3 + b_23_3))*(spearman.brown(rt2_pH_sd["cor"],2,"n")$r.new)
c_24_3 <- (1-(a_24_3 + b_24_3))*(spearman.brown(rt2_nP_sd["cor"],2,"n")$r.new)
c_25_3 <- (1-(a_25_3 + b_25_3))*(spearman.brown(rt2_nH_sd["cor"],2,"n")$r.new)
c_26_3 <- (1-(a_26_3 + b_26_3))*(spearman.brown(rt2_hP_sd["cor"],2,"n")$r.new)
c_27_3 <- (1-(a_27_3 + b_27_3))*(spearman.brown(rt2_hN_sd["cor"],2,"n")$r.new)
c_28_3 <- (1-(a_28_3 + b_28_3))*(spearman.brown(rt2_P_N_sd["cor"],2,"n")$r.new)
c_29_3 <- (1-(a_29_3 + b_29_3))*(spearman.brown(rt2_P_H_sd["cor"],2,"n")$r.new)
c_30_3 <- (1-(a_30_3 + b_30_3))*(spearman.brown(rt2_N_H_sd["cor"],2,"n")$r.new)

c_1_3 <- if (c_1_3 < 0) 0 else c_1_3
c_2_3 <- if (c_2_3 < 0) 0 else c_2_3
c_3_3 <- if (c_3_3 < 0) 0 else c_3_3
c_4_3 <- if (c_4_3 < 0) 0 else c_4_3
c_5_3 <- if (c_5_3 < 0) 0 else c_5_3
c_6_3 <- if (c_6_3 < 0) 0 else c_6_3
c_7_3 <- if (c_7_3 < 0) 0 else c_7_3
c_8_3 <- if (c_8_3 < 0) 0 else c_8_3
c_9_3 <- if (c_9_3 < 0) 0 else c_9_3
c_10_3 <- if (c_10_3 < 0) 0 else c_10_3
c_11_3 <- if (c_11_3 < 0) 0 else c_11_3
c_12_3 <- if (c_12_3 < 0) 0 else c_12_3
c_13_3 <- if (c_13_3 < 0) 0 else c_13_3
c_14_3 <- if (c_14_3 < 0) 0 else c_14_3
c_15_3 <- if (c_15_3 < 0) 0 else c_15_3
c_16_3 <- if (c_16_3 < 0) 0 else c_16_3
c_17_3 <- if (c_17_3 < 0) 0 else c_17_3
c_18_3 <- if (c_18_3 < 0) 0 else c_18_3
c_19_3 <- if (c_19_3 < 0) 0 else c_19_3
c_20_3 <- if (c_20_3 < 0) 0 else c_20_3
c_21_3 <- if (c_21_3 < 0) 0 else c_21_3
c_22_3 <- if (c_22_3 < 0) 0 else c_22_3
c_23_3 <- if (c_23_3 < 0) 0 else c_23_3
c_24_3 <- if (c_24_3 < 0) 0 else c_24_3
c_25_3 <- if (c_25_3 < 0) 0 else c_25_3
c_26_3 <- if (c_26_3 < 0) 0 else c_26_3
c_27_3 <- if (c_27_3 < 0) 0 else c_27_3
c_28_3 <- if (c_28_3 < 0) 0 else c_28_3
c_29_3 <- if (c_29_3 < 0) 0 else c_29_3
c_30_3 <- if (c_30_3 < 0) 0 else c_30_3

d_1_3 <- 1-(a_1_3 + b_1_3 + c_1_3)
d_2_3 <- 1-(a_2_3 + b_2_3 + c_2_3)
d_3_3 <- 1-(a_3_3 + b_3_3 + c_3_3)
d_4_3 <- 1-(a_4_3 + b_4_3 + c_4_3)
d_5_3 <- 1-(a_5_3 + b_5_3 + c_5_3)
d_6_3 <- 1-(a_6_3 + b_6_3 + c_6_3)
d_7_3 <- 1-(a_7_3 + b_7_3 + c_7_3)
d_8_3 <- 1-(a_8_3 + b_8_3 + c_8_3)
d_9_3 <- 1-(a_9_3 + b_9_3 + c_9_3)
d_10_3 <- 1-(a_10_3 + b_10_3 + c_10_3)
d_11_3 <- 1-(a_11_3 + b_11_3 + c_11_3)
d_12_3 <- 1-(a_12_3 + b_12_3 + c_12_3)
d_13_3 <- 1-(a_13_3 + b_13_3 + c_13_3)
d_14_3 <- 1-(a_14_3 + b_14_3 + c_14_3)
d_15_3 <- 1-(a_15_3 + b_15_3 + c_15_3)
d_16_3 <- 1-(a_16_3 + b_16_3 + c_16_3)
d_17_3 <- 1-(a_17_3 + b_17_3 + c_17_3)
d_18_3 <- 1-(a_18_3 + b_18_3 + c_18_3)
d_19_3 <- 1-(a_19_3 + b_19_3 + c_19_3)
d_20_3 <- 1-(a_20_3 + b_20_3 + c_20_3)
d_21_3 <- 1-(a_21_3 + b_21_3 + c_21_3)
d_22_3 <- 1-(a_22_3 + b_22_3 + c_22_3)
d_23_3 <- 1-(a_23_3 + b_23_3 + c_23_3)
d_24_3 <- 1-(a_24_3 + b_24_3 + c_24_3)
d_25_3 <- 1-(a_25_3 + b_25_3 + c_25_3)
d_26_3 <- 1-(a_26_3 + b_26_3 + c_26_3)
d_27_3 <- 1-(a_27_3 + b_27_3 + c_27_3)
d_28_3 <- 1-(a_28_3 + b_28_3 + c_28_3)
d_29_3 <- 1-(a_29_3 + b_29_3 + c_29_3)
d_30_3 <- 1-(a_30_3 + b_30_3 + c_30_3)


stats <- c("PA M","NA M","HA M","PA SD",  "NA SD",  "HA SD",  "PA SDc", "NA SDc", "HA SDc","PA MSSD","NA MSSD", 
           "HA MSSD", "PA MSSDc","NA MSSDc",
           "HA MSSDc","PA a","NA a","HA a" ,"xPP" , "xNN" , 
           "xHH","xPN","xPH","xNP","xNH", "xHP","xHN","oPN","oPH","oNH")
mean_red_3 <- c(a_1_3, a_2_3, a_3_3, a_4_3, a_5_3, a_6_3, a_7_3, a_8_3, a_9_3, a_10_3,
                a_11_3, a_12_3, a_13_3, a_14_3, a_15_3, a_16_3, a_17_3, a_18_3, a_19_3, a_20_3,
                a_21_3, a_22_3, a_23_3, a_24_3, a_25_3, a_26_3, a_27_3, a_28_3, a_29_3, a_30_3)
sd_red_3 <- c(b_1_3, b_2_3, b_3_3, b_4_3, b_5_3, b_6_3, b_7_3, b_8_3, b_9_3, b_10_3,
              b_11_3, b_12_3, b_13_3, b_14_3, b_15_3, b_16_3, b_17_3, b_18_3, b_19_3, b_20_3,
              b_21_3, b_22_3, b_23_3, b_24_3, b_25_3, b_26_3, b_27_3, b_28_3, b_29_3, b_30_3)
unique_stab_3 <- c(c_1_3, c_2_3, c_3_3, c_4_3, c_5_3, c_6_3, c_7_3, c_8_3, c_9_3, c_10_3,
                   c_11_3, c_12_3, c_13_3, c_14_3, c_15_3, c_16_3, c_17_3, c_18_3, c_19_3, c_20_3,
                   c_21_3, c_22_3, c_23_3, c_24_3, c_25_3, c_26_3, c_27_3, c_28_3, c_29_3, c_30_3)
unique_err_3 <- c(d_1_3, d_2_3, d_3_3, d_4_3, d_5_3, d_6_3, d_7_3, d_8_3, d_9_3, d_10_3,
                  d_11_3, d_12_3, d_13_3, d_14_3, d_15_3, d_16_3, d_17_3, d_18_3, d_19_3, d_20_3,
                  d_21_3, d_22_3, d_23_3, d_24_3, d_25_3, d_26_3, d_27_3, d_28_3, d_29_3, d_30_3)



DF <- data.frame(stats,  unique_err_3, unique_stab_3, sd_red_3, mean_red_3 )

DF$stats <- factor(stats, levels = rev(stats))
DF1 <- melt(DF, id.var="stats")
DF1$variable <- factor(DF1$variable, levels = c("sd_red_3", "mean_red_3","unique_err_3","unique_stab_3"), labels = c("Redundant with scale SD", "Redundant with scale M", "Unique", "Unique & reliable"))
DF1$value <- DF1$value*100

p3x <- ggplot(DF1, aes(x = stats, y = value, fill = variable)) +
  coord_flip() +
  geom_bar(stat = "identity")+
  geom_text(aes(x = stats, y = value, label = format(rd((round(value,2)), add = FALSE, digits = 0),nsmall=2), group = variable),
            position = position_stack(vjust = .5), size = 3)+
  theme_classic() +
  theme(legend.position = "none") +
  xlab("")+
  ylab("% of total variance") +
  scale_fill_manual(values=c("#ffc595","#ff8080","#E0F0E0","#90e7ea"), 
                    name = "Components of variance"
                    , guide = guide_legend(reverse = TRUE)
  ) 



p4x <- ggplot(DF1, aes(x = stats, y = value, fill = variable)) +
  coord_flip() +
  geom_bar(stat = "identity")+
  theme_classic() +
  scale_fill_manual(values=c("#ffc595","#ff8080","#E0F0E0","#90e7ea"), 
                    name = "Components of variance"
                    , guide = guide_legend(reverse = TRUE)
  ) 


grid.arrange(data_table1, p1, ncol =2)
grid.arrange(data_table2, p2, ncol =2)
grid.arrange(data_table3, p3, ncol =2)
grid.arrange(data_table4, p4, ncol =2)
grid.arrange(data_table5, p5, ncol =2)

grid.arrange(p1x, p2x, p3x, p4x, ncol =4) # 20,8



grid.arrange(data_tablex, p1, p2, p3, p4, p5,  ncol =6) # 20,8

###

# PCA                 20, 10
# TRAIT PERSONALITY   15, 6 
# VAR DECOM           15, 8 ( 14.5, 8, Sample 1)
# ODD EVEN

round(sd_cor_e_1[,1],2)
round(sd_cor_e_2[,1],2)
round(sd_cor_e_3[,1],2)


round(cor_n_3,2)[,1]

