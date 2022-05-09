install.packages("qgraph")
library("qgraph")

install.packages("plyr")
library("plyr")

install.packages("igraph")
library("igraph")
install.packages("mlVAR")
library("mlVAR")

packageDescription("mlVAR")$Version
packageDescription("qgraph")$Version
packageDescription("igraph")$Version

install.packages("foreign")
library("foreign")

Datahigh <- read.spss("highbias data OSF.sav", to.data.frame=TRUE)
head(Datahigh)

vars <- c("INT","JOY","SAD","IRR", "WOR", "POS", "NEG")
idvar <- "short_ID"
reshigh <- mlVAR(Datahigh, vars, idvar, lags = 1,
                 temporal = "orthogonal", contemporaneous = "orthogonal", scale = FALSE, scaleWithin = TRUE)

#temporal networks
g3high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, vsize=5, esize=2, asize=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="highTem", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

#Only significant results
g3bhigh <- plot(reshigh, "temporal",layout="circle", nonsig = "hide", edge.labels = TRUE, vsize=5, esize=2, asize=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="highTemsig", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

Datalow <- read.spss("lowbias data OSF.sav", to.data.frame=TRUE)
head(Datalow)

vars <- c("INT","JOY","SAD","IRR", "WOR", "POS", "NEG")
idvar <- "short_ID"
reslow <- mlVAR(Datalow, vars, idvar, lags = 1,
              temporal = "orthogonal", contemporaneous = "orthogonal", scale = FALSE, scaleWithin = TRUE)


#temporal networks
g4low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, vsize=5, esize=2, asize=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="lowTem", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

#Only significant results
g4blow <- plot(reslow, "temporal",layout="circle", nonsig = "hide", edge.labels = TRUE, vsize=5, esize=2, asize=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="lowTemsig", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

#now with max for a good comparison:
plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, maximum=0.27, vsize=5, esize=2, asize=2, edge.label.cex=0.5, legend.cex=0.5, curveScaleNodeCorrection = TRUE, filetype="jpeg", fade=T, filename="highTemmax", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

plot(reshigh, "temporal",layout="circle", nonsig = "hide", edge.labels = TRUE, maximum=0.27, vsize=5, esize=2, asize=2, edge.label.cex=0.5, curveScaleNodeCorrection = TRUE, legend.cex=0.5, filetype="jpeg", fade=T, filename="highTemsigmax", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, curveScaleNodeCorrection = TRUE, maximum=0.27, vsize=5, esize=2, asize=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="lowTemmax", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

plot(reslow, "temporal",layout="circle", nonsig = "hide", edge.labels = TRUE, curveScaleNodeCorrection = TRUE, maximum=0.27, vsize=5, esize=2, asize=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="lowTemsigmax", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

#now with max and larger edge.width:
#Now for the temporal networks:
plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, maximum=0.28, vsize=5, esize=2, asize=2, edge.width=2, edge.label.cex=0.5, legend.cex=0.5, curveScaleNodeCorrection = TRUE, filetype="jpeg", fade=T, filename="highTemmax2", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

plot(reshigh, "temporal",layout="circle", nonsig = "hide", edge.labels = TRUE, maximum=0.28, vsize=5, esize=2, asize=2, edge.width=2, edge.label.cex=0.5, curveScaleNodeCorrection = TRUE, legend.cex=0.5, filetype="jpeg", fade=T, filename="highTemsigmax2", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, curveScaleNodeCorrection = TRUE, maximum=0.28, vsize=5, esize=2, asize=2, edge.width=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="lowTemmax2", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))

plot(reslow, "temporal",layout="circle", nonsig = "hide", edge.labels = TRUE, curveScaleNodeCorrection = TRUE, maximum=0.28, vsize=5, esize=2, asize=2, edge.width=2, edge.label.cex=0.5, legend.cex=0.5, filetype="jpeg", fade=T, filename="lowTemsigmax2", labels=c("INT", "JOY", "SAD", "IRR", "WOR", "POS", "NEG"), nodeNames=c("Interested", "Joyful", "Sad", "Irritated", "Worried", "Positive event", "Negative event"), mar=c(8,8,8,8))


getNet(reshigh, "temporal",layout="circle", edge.labels = TRUE, nonsig = "show")
getNet(reslow, "temporal",layout="circle", edge.labels = TRUE, nonsig = "show")

#Centrality plots: outstrength and instrength
centralityPlot(list("high happy bias"=g3high,"low happy bias"=g4low),standardized = FALSE)

#save centrality plot manually as pdf

#### plot individual variation in outstrength and instrength: 

#First for low happy bias group:
s1low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=01)
s2low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=02)
s3low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=03)
s4low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=04)
s5low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=05)
s6low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=06)
s7low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=07)
s8low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=08)
s9low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=09)
s10low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=10)
s11low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=11)
s12low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=12)
s13low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=13)
s14low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=14)
s15low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=15)
s16low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=16)
s17low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=17)
s18low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=18)
s19low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=19)
s20low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=20)
s21low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=21)
s22low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=22)
s23low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=23)
s24low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=24)
s25low <- plot(reslow, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=25)

#now for the high bias group:
s1high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=01)
s2high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=02)
s3high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=03)
s4high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=04)
s5high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=05)
s6high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=06)
s7high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=07)
s8high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=08)
s9high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=09)
s10high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=10)
s11high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=11)
s12high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=12)
s13high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=13)
s14high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=14)
s15high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=15)
s16high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=16)
s17high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=17)
s18high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=18)
s19high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=19)
s20high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=20)
s21high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=21)
s22high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=22)
s23high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=23)
s24high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=24)
s25high <- plot(reshigh, "temporal",layout="circle", nonsig = "show", edge.labels = TRUE, subject=25)

#Multiple groups in 1 plot:

library("qgraph")
# Obtain tables:
Table1 <- centralityTable((list("subject 01"=s1low,"subject 02"=s2low,"subject 03"=s3low,"subject 04"=s4low,"subject 05"=s5low,"subject 06"=s6low,"subject 07"=s7low,"subject 08"=s8low,"subject 09"=s9low,"subject 10"=s10low, "subject 11"=s11low, "subject 12"=s12low, "subject 13"=s13low, "subject 14"=s14low, "subject 15"=s15low, "subject 16"=s16low, "subject 17"=s17low, "subject 18"=s18low, "subject 19"=s19low,"subject 20"=s20low, "subject 21"=s21low,"subject 22"=s22low,"subject 23"=s23low,"subject 24"=s24low,"subject 25"=s25low)), standardized = FALSE)
Table2 <- centralityTable((list("subject 01"=s1high,"subject 02"=s2high,"subject 03"=s3high,"subject 04"=s4high,"subject 05"=s5high,"subject 06"=s6high,"subject 07"=s7high,"subject 08"=s8high,"subject 09"=s9high,"subject 10"=s10high, "subject 11"=s11high, "subject 12"=s12high, "subject 13"=s13high, "subject 14"=s14high, "subject 15"=s15high, "subject 16"=s16high, "subject 17"=s17high, "subject 18"=s18high, "subject 19"=s19high, "subject 20"=s20high, "subject 21"=s21high,"subject 22"=s22high, "subject 23"=s23high,"subject 24"=s24high, "subject 25"=s25high)), standardized = FALSE)

# Add identifier:
Table1$id <- "low happy bias"
Table2$id <- "high happy bias"

# Combine:
Table <- rbind(Table1,Table2)
Table$uniqueType <- paste0(Table$type,Table$id)

# Order:
# Ordereing by node name to make nice paths:
library("gtools")
Table <- Table[gtools::mixedorder(Table$node),] 
Table$node <- factor(as.character(Table$node), levels = unique(gtools::mixedsort(as.character(Table$node))))

# Plot
library("ggplot2")
ggplot(Table, aes(x = value, y = node, group = uniqueType, colour = id)) + 
  geom_path() +  xlab("") + ylab("") + geom_point() + 
  facet_grid( ~ measure, scales = "free") + theme_bw()

#Now for anhedonia versus in control in high and low bias groups:

library("qgraph")
# Obtain tables:
Table1low <- centralityTable((list("subject 01"=s1low,"subject 02"=s2low,"subject 03"=s3low,"subject 04"=s4low,"subject 05"=s5low,"subject 07"=s7low,"subject 11"=s11low, "subject 17"=s17low, "subject 22"=s22low,"subject 23"=s23low)), standardized = FALSE)
Table2low <- centralityTable((list("subject 06"=s6low,"subject 08"=s8low,"subject 09"=s9low,"subject 10"=s10low, "subject 13"=s13low, "subject 14"=s14low, "subject 15"=s15low, "subject 16"=s16low, "subject 18"=s18low, "subject 19"=s19low,"subject 20"=s20low, "subject 21"=s21low, "subject 24"=s24low)), standardized = FALSE)

# Add identifier:
Table1low$id <- "low happy bias control"
Table2low$id <- "low happy bias anhedonia"

# Combine:
Tablelow <- rbind(Table1low,Table2low)
Tablelow$uniqueType <- paste0(Tablelow$type,Tablelow$id)

# Order:
# Ordereing by node name to make nice paths:
library("gtools")
Tablelow <- Tablelow[gtools::mixedorder(Tablelow$node),] 
Tablelow$node <- factor(as.character(Tablelow$node), levels = unique(gtools::mixedsort(as.character(Tablelow$node))))

# Plot
library("ggplot2")
ggplot(Tablelow, aes(x = value, y = node, group = uniqueType, colour = id)) + 
  geom_path() +  xlab("") + ylab("") + geom_point() + 
  facet_grid( ~ measure, scales = "free") + theme_bw()

#High bias:
# Obtain tables:
Table1high <- centralityTable((list("subject 01"=s1high,"subject 03"=s3high,"subject 04"=s4high,"subject 05"=s5high,"subject 06"=s6high,"subject 07"=s7high,"subject 10"=s10high, "subject 11"=s11high, "subject 12"=s12high, "subject 13"=s13high, "subject 14"=s14high, "subject 15"=s15high, "subject 18"=s18high, "subject 19"=s19high, "subject 21"=s21high,"subject 23"=s23high,"subject 24"=s24high)), standardized = FALSE)
Table2high <- centralityTable((list("subject 02"=s2high,"subject 08"=s8high,"subject 09"=s9high, "subject 16"=s16high, "subject 17"=s17high, "subject 20"=s20high, "subject 22"=s22high, "subject 25"=s25high)),standardized = FALSE)

# Add identifier:
Table1high$id <- "high happy bias control"
Table2high$id <- "high happy bias anhedonia"

# Combine:
Tablehigh <- rbind(Table1high,Table2high)
Tablehigh$uniqueType <- paste0(Tablehigh$type,Tablehigh$id)

# Order:
# Ordereing by node name to make nice paths:
library("gtools")
Tablehigh <- Tablehigh[gtools::mixedorder(Tablehigh$node),] 
Tablehigh$node <- factor(as.character(Tablehigh$node), levels = unique(gtools::mixedsort(as.character(Tablehigh$node))))

# Plot
library("ggplot2")
ggplot(Tablehigh, aes(x = value, y = node, group = uniqueType, colour = id)) + 
  geom_path() +  xlab("") + ylab("") + geom_point() + 
  facet_grid( ~ measure, scales = "free") + theme_bw()

#Now combine everything:
# Combine:
Tablehighlow <- rbind(Table1high,Table2high, Table1low, Table2low)
Tablehighlow$uniqueType <- paste0(Tablehighlow$type,Tablehighlow$id)

# Order:
# Ordereing by node name to make nice paths:
library("gtools")
Tablehighlow <- Tablehighlow[gtools::mixedorder(Tablehighlow$node),] 
Tablehighlow$node <- factor(as.character(Tablehighlow$node), levels = unique(gtools::mixedsort(as.character(Tablehighlow$node))))

# Plot
library("ggplot2")
ggplot(Tablehighlow, aes(x = value, y = node, group = uniqueType, colour = id)) + 
  geom_path() +  xlab("") + ylab("") + geom_point() + 
  facet_grid( ~ measure, scales = "free") + theme_bw()

##########################################################

##After running the main analyses, we exported the autocorrelations and centrality measures for each individual for the purpose of being able to adjust for anhedoniastatus. 

###################
Tableautohigh <- do.call(rbind, lapply(1:25, function(i)diag(getNet(reshigh,"temporal",layout="circle", edge.labels = TRUE, subject=i))))

write.table(Tableautohigh, "mydataautohigh.txt", sep=",")

Tableautolow <- do.call(rbind, lapply(1:25, function(i)diag(getNet(reslow,"temporal",layout="circle", edge.labels = TRUE, subject=i))))

write.table(Tableautolow, "mydataautolow.txt", sep=",")


#Centrality alles in 1 tabel doorgenummerd:
Tablecenthigh <- centralityTable((list(s1high,s2high,s3high,s4high,s5high,s6high,s7high,s8high,s9high,s10high,s11high,s12high,s13high,s14high,s15high,s16high,s17high,s18high,s19high,s20high,s21high,s22high,s23high,s24high,s25high)), standardized = FALSE)

write.table(Tablecenthigh, "mydatacenthigh.txt", sep=",")

Tablecentlow <- centralityTable((list(s1low,s2low,s3low,s4low,s5low,s6low,s7low,s8low,s9low,s10low,s11low,s12low,s13low,s14low,s15low,s16low,s17low,s18low,s19low,s20low,s21low,s22low,s23low,s24low,s25low)), standardized = FALSE)

write.table(Tablecentlow, "mydatacentlow.txt", sep=",")

##These files were saved and imported in SPSS 25 for further analysis.

