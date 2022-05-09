##################################################################################
#                                                                                #
#                      COVID-19 student mental health study                      #
#           to be published in Clinical Psychological Science, 2021              #
#                                                                                #
#                         Eiko Fried, eikofried@gmail.com                        #
#             with code from Faidra Papanikolaou & Sacha Epskamp                 #
#                                                                                #
#                          Last updated: April 13 2021                           #
#                           (date of paper acceptance)                           #
#                                                                                #
#                  Part IV: Descriptives & pre vs post comparison                #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- 1. Loading packages & Data ------------------------------
# -------------------------------------------------------------------------

library("ggplot2")
library("bootnet")
library("dplyr")
library("tidyr")
library("qgraph")
library("viridis")
library("Rmisc")
library("summarytools")
library("e1071")
# library("rstatix")

figs <- "./figures/" # figure directory
datapath <- "./data/" # data directory



# -------------------------------------------------------------------------
# --------------- 2. Descriptives -----------------------------------------
# -------------------------------------------------------------------------

### load 
load(paste0(datapath, "clean_prepost.RData"))
df <- rawdata_full

### make sense of data, double check everything 

df$Gender
mean(df$Gender, na.rm=TRUE)  # 1.175
count(df$Gender)

df$Age
mean(df$Age, na.rm=TRUE)     # 20.375
range(df$Age)
sd(df$Age)

df$Nationality
unique(count(df$Nationality))

df$RelationshipStatus
count(df$RelationshipStatus)

df$StudyProgram
count(df$StudyProgram)

df$StudyYear
count(df$StudyYear)

df$WorkStatus
count(df$WorkStatus)   # 26yes, 54 no

df$MentalHealth
count(df$MentalHealth) # 59, 17 yes, 4 NA

# DASS-21, questionnaire 1 in pre assessment
df[,c(14:34)] #range 1-4, check

# disorganization, questionnaire 2 in pre assessment
# anger, questionnaire 3 in pre assessment

# lonely scale, questionnaire 4 in pre assessment
df[,c(54:58)] #range 1-5, check
mean(rowSums(df[,c(54:58)])) # 11.65
sd((rowSums(df[,c(54:58)]))) #  3.92

# perceived self efficacy, questionnaire 5 in pre assessment
df[,c(59:68)] #range 1-5, check
mean(rowSums(df[,c(59:68)])) # 28.9
sd((rowSums(df[,c(59:68)]))) #  3.93

# mindfulness, questionnaire 6 in pre assessment
df[,c(69:80)] #range 1-5, check

# perceived stress scale, questionnaire 7 in pre assessment
df[,c(81:90)] #range 1-5, check
mean(rowSums(df[,c(81:90)])) # 30.34
sd((rowSums(df[,c(81:90)]))) #  3.36

# fatigue scale, questionnaire 8 in pre assessment
df[,c(91:98)] #range 1-4, check

# motivation scale, questionnaire 9 in pre assessment
df[,c(99:114)] #range 1-7, check

# smartphone use, questionnaire 10 in pre assessment
df[,c(115:124)] #range 1-6, check

# smartphone use2, questionnaire 11 in pre assessment
df[,c(125:128)] #range 1-6, check

# music, questionnaire 12 in pre assessment
df[,c(129:135)]

# social activity, questionnaire 13 in pre assessment
df[,c(136:139)]
df[,136] #average hours per day social activity
mean(df[,136]) # 3.775   
sd(df[,136])   # 1.3

# hungry, questionnaire 14
# procrastination, questionnaire 15

# 189:193: lonely post
# 194:197: outdoors post

df[,199] # Post27: 1-5 NA "started washing hands more"
mean(df[,199], na.rm=TRUE)     # 3.49
CI(na.omit(df[,199]), ci=0.95) # [3.32-3.67]

df[,200] # Post28: 1-5 NA "avoid social act"
mean(df[,200], na.rm=TRUE)     # 3.70
CI(na.omit(df[,200]), ci=0.95) # [3.56-3.84]

df[,201] # Post29: 1-5 NA "covid had impact on my mental health"
mean(df[,201], na.rm=TRUE)     # 3.34
CI(na.omit(df[,201]), ci=0.95) # [3.62-3.06]

df[,202] # Post30: 1-5 NA "well informed uni"
mean(df[,202], na.rm=TRUE)     # 3.61
CI(na.omit(df[,202]), ci=0.95) # [3.37-3.85]

df[,203] # Post31: 1-5 NA "well informed gov"
mean(df[,203], na.rm=TRUE)     # 3.39
CI(na.omit(df[,203]), ci=0.95) # [3.11-3.67]

df[,204] # Post32: 1-5 NA "actions of leiden uni&gov reduced stress; 3=neutral, 1=total disagree,5=total agree
mean(df[,204], na.rm=TRUE)     # 2.88
CI(na.omit(df[,204]), ci=0.95) # [2.60-3.16]

df[,205] # Post33: 1,2,NA "did you have COVID19 Sx"
mean(df[,205], na.rm=TRUE)     # 80.5% no

df[,206] # Post34: 1,NA "did you get diagnosed"
mean(df[,206], na.rm=TRUE)     # 100% no

df[,207] # Post35: broken (only 0s and some text) "if yes when"

df[,208] # Post36: 1, 2, NA "friends or fam with corona"
mean(df[,208], na.rm=TRUE)     # 94.9% no

df[,209] # Post37: 1x3, 3x1, rest 0: "How many"
# ID 25292: 3
# ID 25325: 1
# ID 25360: 1
# ID 25372: 1



# -------------------------------------------------------------------------
# --------------- 3. Pre post comparison DASS-21 and Loneliness -----------
# -------------------------------------------------------------------------

# note that we coded DASS-21 on scale 1-4, original scale is 0-3
# so let's recode that quickly
df[,c(14:34)] <- df[,c(14:34)]-1      #DASS pre
df[,c(210:230)] <- df[,c(210:230)]-1  #DASS post

### Compare DASS-21 pre post
df$DASS_pre  <- rowSums(df[,c(14:34)])   # Q1-Q22
df$DASS_post <- rowSums(df[,c(210:230)]) # Q38-58

# plot for sanity check, looks good
plot(df$DASS_pre, type="l", col="red")
lines(df$DASS_post, type="l", col="blue")

# t-test & effect size
t.test(df$DASS_pre, df$DASS_post, paired=TRUE)  # total: t=0.45, df=76, p=0.66, mean diff 0.44
df2 <- df[c("ID", "DASS_pre", "DASS_post")]
df2 <- df2 %>% gather(key = "type", value = "value", -ID); head(df2)
df2 %>% rstatix::cohens_d(value ~ type, paired = TRUE) # cohen's D= 0.05, negligible

# descriptives
mean(df$DASS_pre)                  # 15.03
sd(df$DASS_pre)                    # 9.08           
range(df$DASS_pre, na.rm=TRUE)     # 0 - 53

mean(df$DASS_post, na.rm=TRUE)     # 14.57   
sd(df$DASS_post, na.rm=TRUE)       # 8.75
range(df$DASS_post, na.rm=TRUE)    # 0 - 43


### DASS-21 subscales 
dep1 <- c(3, 5, 10, 13, 16, 17, 21)+13   # depression time 1
dep2 <- c(3, 5, 10, 13, 16, 17, 21)+209  # depression time 2
anx1 <- c(2, 4, 7, 9, 15, 19, 20)+13     # anxiety time 1
anx2 <- c(2, 4, 7, 9, 15, 19, 20)+209    # anxiety time 2
str1 <- c(1, 6, 8, 11, 12, 14, 18)+13    # stress time 1
str2 <- c(1, 6, 8, 11, 12, 14, 18)+209   # stress time 2

df$DASS_pre1  <- rowSums(df[,dep1])   
df$DASS_post1 <- rowSums(df[,dep2]) 
df$DASS_pre2  <- rowSums(df[,anx1])   
df$DASS_post2 <- rowSums(df[,anx2]) 
df$DASS_pre3  <- rowSums(df[,str1])   
df$DASS_post3 <- rowSums(df[,str2])

# sanity check
cor((df$DASS_pre1 + df$DASS_pre2 + df$DASS_pre3), df$DASS_pre)
cor((df$DASS_post1[-c(8,28,64)] + df$DASS_post2[-c(8,28,64)] + df$DASS_post3[-c(8,28,64)]), df$DASS_post[-c(8,28,64)]) # data missing for persons 8, 28, 64
plot(df$DASS_pre1, type="l", col="red", main="DASS-21 depression; red=pre, blue=post")
lines(df$DASS_post1, type="l", col="blue")
plot(df$DASS_pre2, type="l", col="red", main="DASS-21 anxiety; red=pre, blue=post")
lines(df$DASS_post2, type="l", col="blue")
plot(df$DASS_pre1, type="l", col="red", main="DASS-21 stress; red=pre, blue=post")
lines(df$DASS_post1, type="l", col="blue")


### compare
t.test(df$DASS_pre1, df$DASS_post1, paired=TRUE)  # depression: t=-2.33, df=76, p=0.02,  mean diff -1.23* increase;    cohen's d=0.27, small
df3 <- df[c("ID", "DASS_pre1", "DASS_post1")]; df3 <- df3 %>% gather(key = "type", value = "value", -ID); df3 %>% rstatix::cohens_d(value ~ type, paired = TRUE) 
t.test(df$DASS_pre2,df$DASS_post2, paired=TRUE)  # anxiety   :  t= 2.87, df=76, p=0.005, mean diff  0.94** decrease;   cohen's d=0.33, small
df4 <- df[c("ID", "DASS_pre2", "DASS_post2")]; df4 <- df4 %>% gather(key = "type", value = "value", -ID); df4 %>% rstatix::cohens_d(value ~ type, paired = TRUE) 
t.test(df$DASS_pre3,df$DASS_post3, paired=TRUE)  # stress:      t= 1.72, df=76, p=0.09,  mean diff  0.7    stationary; cohen's d=0.20, negligible
df5 <- df[c("ID", "DASS_pre3", "DASS_post3")]; df5 <- df5 %>% gather(key = "type", value = "value", -ID); df5 %>% rstatix::cohens_d(value ~ type, paired = TRUE) 

# depression (0-4 normal, 5-6 mild, 7-10 moderate, see manual)
mean(df$DASS_pre1, na.rm=TRUE)   # 4.46
mean(df$DASS_post1, na.rm=TRUE)  # 5.62
sd(df$DASS_pre1)                 # 3.80
sd(df$DASS_post1, na.rm=TRUE)    # 4.29
range(df$DASS_pre1, na.rm=TRUE)  # 0-21
range(df$DASS_post1, na.rm=TRUE) # 0-16

# anxiety (0-3 normal, 4-5 mild, 6-7 moderate, see manual)
mean(df$DASS_pre2)               # 3.64
mean(df$DASS_post2, na.rm=TRUE)  # 2.73
sd(df$DASS_pre2)                 # 3.22
sd(df$DASS_post2, na.rm=TRUE)    # 2.85
range(df$DASS_pre2, na.rm=TRUE)  # 0-15
range(df$DASS_post2, na.rm=TRUE) # 0-17

# stress  (0-7 normal, 8-9 mild, 10-12 moderate, see manual)
mean(df$DASS_pre3)               # 6.93
mean(df$DASS_post3, na.rm=TRUE)  # 6.22
sd(df$DASS_pre3)                 # 4.09
sd(df$DASS_post3, na.rm=TRUE)    # 3.74
range(df$DASS_pre3, na.rm=TRUE)  # 0-20
range(df$DASS_post3, na.rm=TRUE) # 0-16

# post hoc analysis: which items in the depression subscale drive the depression scores going up?
# DASS21 D items are: 3, 5, 10, 13, 16, 17, 21
item1D <-  mean(df[,dep2[[1]]] - df[,dep1[[1]]], na.rm=TRUE); item1D  # -0.08    no pos feelings
item2D <-  mean(df[,dep2[[2]]] - df[,dep1[[2]]], na.rm=TRUE); item2D  #  0.44 << difficult to work up initiative
item3D <-  mean(df[,dep2[[3]]] - df[,dep1[[3]]], na.rm=TRUE); item3D  #  0.39 << nothing to look forward to
item4D <-  mean(df[,dep2[[4]]] - df[,dep1[[4]]], na.rm=TRUE); item4D  #  0.08    blue
item5D <-  mean(df[,dep2[[5]]] - df[,dep1[[5]]], na.rm=TRUE); item5D  #  0.18    no enthusiasm
item6D <-  mean(df[,dep2[[6]]] - df[,dep1[[6]]], na.rm=TRUE); item6D  #  0.03    selfworth
item7D <-  mean(df[,dep2[[7]]] - df[,dep1[[7]]], na.rm=TRUE); item7D  #  0.20    life meaningless


# post hoc analysis: which items in the anxiety subscale drive the anxiety scores going down?
# DASS21 A items are: 2, 4, 7, 9, 15, 19, 20
item1A <-  mean(df[,anx2[[1]]] - df[,anx1[[1]]], na.rm=TRUE); item1A  #  0.04    dry mouth
item2A <-  mean(df[,anx2[[2]]] - df[,anx1[[2]]], na.rm=TRUE); item2A  # -0.03    diff breathing
item3A <-  mean(df[,anx2[[3]]] - df[,anx1[[3]]], na.rm=TRUE); item3A  # -0.17    trembling
item4A <-  mean(df[,anx2[[4]]] - df[,anx1[[4]]], na.rm=TRUE); item4A  # -0.44 << worried about situations to make fool of myself
item5A <-  mean(df[,anx2[[5]]] - df[,anx1[[5]]], na.rm=TRUE); item5A  # -0.18 << close to panic
item6A <-  mean(df[,anx2[[6]]] - df[,anx1[[6]]], na.rm=TRUE); item6A  # -0.05    heart rate aware
item7A <-  mean(df[,anx2[[7]]] - df[,anx1[[7]]], na.rm=TRUE); item7A  # -0.10    scared without good reason
   


### Compare loneliness pre post
df$lone_pre  <- rowSums(df[,c(54:58)])   # Q41-Q45
df$lone_post <- rowSums(df[,c(190:194)]) # Q18-Q22
plot(df$lone_pre, type="l", col="red", main="Loneliness; red=pre, blue=post")
lines(df$lone_post, type="l", col="blue")

t.test(df$lone_pre,df$lone_post, paired=TRUE) #t=3.14, df=76, p=0.002, mean diff 0.857* decrease; cohen's D=0.36
df6 <- df[c("ID", "lone_pre", "lone_post")]; df6 <- df6 %>% gather(key = "type", value = "value", -ID); df6 %>% rstatix::cohens_d(value ~ type, paired = TRUE) 

mean(df$lone_pre)                  #11.65
median(df$lone_pre)                #11
sd(df$lone_pre)                    # 3.92
range(df$lone_pre, na.rm=TRUE)     # 5-22

mean(df$lone_post, na.rm=TRUE)     #10.86
median(df$lone_post, na.rm=TRUE)   #11
sd(df$lone_post, na.rm=TRUE)       # 3.90
range(df$lone_post, na.rm=TRUE)    # 5-20



# -------------------------------------------------------------------------
# --------------- 4. Prediction of DASS-21 change -------------------------
# -------------------------------------------------------------------------

# Note that this snippet sometimes runs into trouble due to package conflicts; if so, close and reopen R, and only load the packages loaded in this syntax specifically

### Make sum scores we are interested in
df$DASS_change <- df$DASS_post-df$DASS_pre # outcome; + = increases, - reductions in DASS-21 score
df$selfefficacy_pre  <- rowSums(df[,c(59:68)])   
df$PSS_pre  <- rowSums(df[,c(81:90)])   

### Rescore so high values are what we expect to be positive predictors (e.g. female 2 male 1)
df$Nationality2 <- df$Nationality
df$Nationality2 <- as.integer(df$Nationality2 %in% "Dutch") 
df$Nationality2[df$Nationality2==0] <- 2  
df$Nationality2[df$Nationality2==1] <- 0  
df$Nationality2[df$Nationality2==2] <- 1  # Dutch=0 (36), Rest=1 (44)
count(df$Nationality2)    

df$RelationshipStatus2 <- df$RelationshipStatus
df$RelationshipStatus2 <- recode(df$RelationshipStatus2, "3" = 2)
df$RelationshipStatus2[df$RelationshipStatus2==2] <- 0  # Partnered=0 (30), Single=1 (50)
count(df$RelationshipStatus2)    

df$Gender2 <- df$Gender          # 
df$Gender2[df$Gender2==3] <- NA  # code 1 nonbinary participant as NA for this to avoid 3 levels
df$Gender2[df$Gender2==1] <- 0  
df$Gender2[df$Gender2==2] <- 1   # men=0 (19), women=1 (60), 1 NA
count(df$Gender2)                

df$WorkStatus2 <- df$WorkStatus
df$WorkStatus2[df$WorkStatus2==2] <- 0 # 54no, 26yes
count(df$WorkStatus2)            

df$MentalHealth2 <- as.numeric(df$MentalHealth)
df$MentalHealth2[df$MentalHealth2==1] <- 0  
df$MentalHealth2[df$MentalHealth2==2] <- 1   # healthy=0 (59), history=1 (17), 4 NA
count(df$MentalHealth2)

df$selfefficacy_pre2 <- df$selfefficacy_pre*-1 # reverse code


### Combine relevant predictors into data frame
df_reg <- as.data.frame(cbind(
                df$DASS_change,
                df$DASS_pre,
                df$Gender2,
                df$Age, 
                df$Nationality2,
                df$RelationshipStatus2,
                df$WorkStatus2,
                df$MentalHealth2,
                df$selfefficacy_pre2,
                df$PSS_pre,
                df$lone_pre,
                df[,136] #average hours per day social activity
                ))


### Check how distribution of DASS change score (our main outcome) looks like
plot(density(df_reg[,1], na.rm=TRUE), main="Density Plot", ylab="Frequency", 
     sub=paste("Skewness:", round(e1071::skewness(df_reg[,1], na.rm=TRUE), 2))); polygon(density(df_reg[,1], na.rm=TRUE), col="steelblue")


# Add names
names_reg <- colnames(df_reg) <- 
               c("DASS-change",          # positive score=increase          
               "DASS-baseline",
               "Gender",                 # 0=male, 1=female
               "Age",                      
               "International",          # 0=Dutch, 1=international
               "Relationship",           # 0=single, 1=relationship          
               "Working",                # 0=no, 1=yes   
               "Mental Health",          # 0=no prob, 1=yes prob
               "Self-efficacy",          # reverse coded               
               "Perceived stress",                     
               "Loneliness",                      
               "Socially active")        # only variable where I cannot make good prediction

# Turn binary variables into factors for later use
df_reg_f <- df_reg
fact_cols <- c("Gender", "International", "Relationship", "Working", "Mental Health")
df_reg_f[,fact_cols] <- data.frame(apply(df_reg[fact_cols], 2, as.factor))
str(df_reg_f)


### We take a rough look at the correlations first; all make sense (e.g. correlations among mental health problems at baseline)
network_reg <- estimateNetwork(df_reg, default="cor", corMethod="cor", 
                               threshold="none", missing="pairwise")
plot(network_reg, nodeNames=names_reg, details=T, labels=c(1:12), layout="circle", 
     title="Pearson", legend.cex=.4)


### Now we predict DASS-21 change via regression model
regr <- lm(df_reg_f[,1] ~ 
             df_reg_f[,2] +
             df_reg_f[,3] +
             df_reg_f[,4] +
             df_reg_f[,5] +
             df_reg_f[,6] +
             df_reg_f[,7] +
             df_reg_f[,8] +
             df_reg_f[,9] +
             df_reg_f[,10] +
             df_reg_f[,11] +
             df_reg_f[,12],
             data=df_reg_f)
str(df_reg_f)
summary(regr)

