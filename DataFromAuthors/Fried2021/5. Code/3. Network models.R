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
#                     Part III: Detrending and network models                    #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- 1. Loading packages & Data ------------------------------
# -------------------------------------------------------------------------

library("lme4")
library("ggplot2")
library("dplyr")
library("tidyr")
library("qgraph")
library("tidyselect")
library("mlVAR")
library("viridis")
library("summarytools")
library("lm.beta")

figs <- "./figures/" # figure directory
datapath <- "./data/" # data directory


# -------------------------------------------------------------------------
# --------------- 2. Detrending fixed effects -----------------------------
# -------------------------------------------------------------------------

# We do not do multilevel detrending per person

# load 
load(paste0(datapath, "clean_network.RData"))
Data5b <- Data2

# Alpha to detrend:
alpha <- 0.05

# Variables to investigate:
vars <- c("Q1","Q2","Q3","Q4","Q5","Q6","Q7","Q8","Q9","Q10","Q11","Q12","Q13","Q14","Q15","Q16","Q17","Q18")

# Labels:
varLabs <- c("Relax","Irritable","Worry","Nervous","Future","Anhedonia",
             "Tired","Hungry","Alone","Angry","Social_offline","Social_online","Music",
             "Procrastinate","Outdoors","C19_occupied","C19_worry","Home")


names(Data5b)[names(Data5b) %in% vars] <- varLabs

# Remove items:
Data5b <- Data5b %>% select(-Hungry,-Angry,-Music,-Procrastinate)
varLabs <- varLabs[!varLabs %in% c("Hungry","Angry","Music","Procrastinate")]

# Data frame with empty values for fitted effects (all):
fitted_all <- expand.grid(
  beep = seq(min(Data5b$beep),max(Data5b$beep)),
  day = seq(min(Data5b$day),max(Data5b$day))
)

# Data frame with empty values for day trends:
fitted_day <- data.frame(
  day = seq(min(Data5b$day),max(Data5b$day))
)

# Data frame with empty values for beeps:
fitted_beep <- data.frame(
  beep = seq(min(Data5b$beep),max(Data5b$beep))
)

# Data frame to store p-values:
p_values <- data.frame(
  var = c("day", "beep")
)

# Also empty data frame list for test statistics:
testStatistics <- list()
coefficients <- list()
stdcoefficients <- list()

# Make the beep variable factor in dataset:
Data5b$beepFactor <- factor(Data5b$beep, levels = 0:3, labels = c("09:00 - 12:00","12:00 - 15:00","15:00 - 18:00","18:00 - 21:00"))
fitted_all$beepFactor <- factor(fitted_all$beep, levels = 0:3, labels = c("09:00 - 12:00","12:00 - 15:00","15:00 - 18:00","18:00 - 21:00"))
fitted_beep$beepFactor <- factor(fitted_beep$beep, levels = 0:3, labels = c("09:00 - 12:00","12:00 - 15:00","15:00 - 18:00","18:00 - 21:00"))

# Make day variable for dates:
Data5b$date <- as.Date("2020-03-15") + Data5b$day
fitted_all$date <- as.Date("2020-03-15") + fitted_all$day
fitted_day$date <- as.Date("2020-03-15") + fitted_day$day

# Add the midpoints as time variable:
Data5b$midTime <- as.character(factor(Data5b$beep, levels = 0:3, labels = c("10:30","13:30","16:30","19:30")))
Data5b$midTime <- as.POSIXct(paste(Data5b$date,Data5b$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")

fitted_all$midTime <- as.character(factor(fitted_all$beep, levels = 0:3, labels = c("10:30","13:30","16:30","19:30")))
fitted_all$midTime <- as.POSIXct(paste(fitted_all$date,fitted_all$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")

# Data frame to store detrended data:
data_detrended <- Data5b

# Fix curves:
for (v in seq_along(varLabs)){
  formula <- as.formula(paste0(varLabs[v], " ~ 1 + day + factor(beep)"))
  lmRes <- lm(formula, data = Data5b)
  
  # Fixed effects:
  fixed <- coef(lmRes)
  
  # make zero if not significant at alpha:
  p_values[[varLabs[v]]] <- anova(lmRes)[["Pr(>F)"]][1:2]
  if (p_values[[varLabs[v]]][1] > alpha){
    fixed[2] <- 0
  }
  if (p_values[[varLabs[v]]][2] > alpha){
    fixed[3:5] <- 0
  }
  
  # Add to DFs:
  fitted_all[,varLabs[v]] <- fixed[1] + fixed[2] * fitted_all[["day"]]  +  fixed[3] * (fitted_all[["beep"]] == 1)  + 
  fixed[4] * (fitted_all[["beep"]] == 2) + fixed[5] *  (fitted_all[["beep"]] == 3)
  
  fitted_day[,varLabs[v]] <- fixed[1] + fixed[2] * fitted_day[["day"]]
  
  fitted_beep[,varLabs[v]] <- fixed[1] + fixed[2] * median(fitted_day[["day"]]) +  fixed[3] * (fitted_beep[["beep"]] == 1)  + 
    fixed[4] * (fitted_beep[["beep"]] == 2) + fixed[5] *  (fitted_beep[["beep"]] == 3)
  
  # Detrend data:
  data_detrended[,varLabs[v]] <- Data5b[,varLabs[v]] - (fixed[1] + fixed[2] * Data5b[["day"]]  +  fixed[3] * (Data5b[["beep"]] == 1)  + 
    fixed[4] * (Data5b[["beep"]] == 2) + fixed[5] *  (Data5b[["beep"]] == 3))
  
  # Test statistic dataframe:   
  ### TODOSACHA: add cohen's D, for prepost I use 
  ### df6 %>% rstatix::cohens_d(value ~ type, paired = TRUE) 
  ids <- rownames(anova(lmRes))
  testStatistics[[v]] <- cbind(data.frame(var = varLabs[v], effect = ids), anova(lmRes))
  
  coefficients[[v]] <- data.frame(
    var = varLabs[v],
    type = names(coef(lmRes)),
    coef = coef(lmRes),
    std = coef(lm.beta(lmRes))
  )
}

# Make sense of significant trends for descriptives

testStatistics <- do.call(rbind, testStatistics)
coefficients <- do.call(rbind, coefficients)

testStatistics[,7] <- sprintf("%.5f", testStatistics[,7])

testStatistics_sig_day <- subset(testStatistics, effect == "day"); testStatistics_sig_day # this has the p-values for the standardized slopes one line below
coefficients_day <- subset(coefficients, coefficients[,2] == "day"); coefficients_day     # this shows standardized slopes over time; e.g. C19_occupied decreased by 0.18; home increased slightly by 0.03

testStatistics_sig_beep <- subset(testStatistics, testStatistics[,7] < 0.05 & effect == "factor(beep)"); testStatistics_sig_beep
coefficients_beep<- subset(coefficients, coefficients[,2] == "factor(beep)1" |  
                                         coefficients[,2] == "factor(beep)2" | 
                                         coefficients[,2] == "factor(beep)3"); coefficients_beep


# Plot versus averaged:
Data5b_avg <- Data5b %>% group_by(day,beep,conc,beepFactor,date,midTime) %>% summarize_at(varLabs, list(~mean(., na.rm=TRUE)))

# to long:
fitted_long <- fitted_all %>% pivot_longer(cols=all_of(varLabs), values_to = "fitted")
mean_long <- Data5b_avg %>% pivot_longer(cols=all_of(varLabs), values_to = "mean")
longDF <- left_join(fitted_long,mean_long) %>% pivot_longer(cols = c("fitted" , "mean"), names_to = "type")

# Dates:
Sys.setlocale("LC_ALL","C")
breaks <- as.POSIXct(paste0(as.Date("2020-03-15") + 1:14, " 00:01"), format = "%Y-%m-%d %H:%M")


### Plot the trends that are removed:
p1 <- ggplot(longDF, aes(x = midTime, y = value, lty = type, pch = type)) + 
  geom_line() + 
  facet_grid(name ~ ., scales = "free") + 
  scale_x_time(breaks = breaks, labels = format(breaks, "%B %d")) + 
  xlab("") + ylab("") + theme_bw() + 
  scale_linetype_discrete("") + 
  scale_colour_discrete("") + 
  ggtitle("Average time-series (dashed) and fitted fixed-effects (solid)",
          subtitle = "Effects not significant at alpha = 0.05 set to zero in fitted model") + 
  theme(legend.position = "none")

p2 <- ggplot(longDF, aes(x = midTime, y = value, lty = type, pch = type)) + 
  geom_line() + 
  facet_grid(name ~ .) + ylim(range(longDF$value)) + 
  scale_x_time(breaks = breaks, labels = format(breaks, "%B %d")) + 
  xlab("") + ylab("") + theme_bw() + 
  scale_linetype_discrete("") + 
  scale_colour_discrete("") + 
  ggtitle("Average time-series (dashed) and fitted fixed-effects (solid)",subtitle = "Effects not significant at alpha = 0.05 set to zero in fitted model") + 
  theme(legend.position = "none")


# Plot detrended data:
data_detrended_avg <- data_detrended %>% group_by(day,beep,conc,beepFactor,date,midTime) %>% summarize_at(varLabs, list(~mean(., na.rm=TRUE)))
detrended_long <- data_detrended_avg %>% pivot_longer(cols=all_of(varLabs), values_to = "mean")

### Plot the trends that are removed:
p3 <- ggplot(detrended_long, aes(x = midTime, y = mean)) + 
  geom_line() + 
  facet_grid(name ~ ., scales = "free") + 
  scale_x_time(breaks = breaks, labels = format(breaks, "%B %d")) + 
  xlab("") + ylab("") + theme_bw() + 
  scale_linetype_discrete("") + 
  scale_colour_discrete("")  + 
  ggtitle("Detrended data according to fixed-effects model",subtitle = "Effects not significant at alpha = 0.05 set to zero in fitted model")

p4 <- ggplot(detrended_long, aes(x = midTime, y = mean)) + 
  geom_line() + 
  facet_grid(name ~ .) + ylim(range(detrended_long$mean)) + 
  scale_x_time(breaks = breaks, labels = format(breaks, "%B %d")) + 
  xlab("") + ylab("") + theme_bw() + 
  scale_linetype_discrete("") + 
  scale_colour_discrete("")  + 
  ggtitle("Detrended data according to fixed-effects model (fixed y-axis)",subtitle = "Effects not significant at alpha = 0.05 set to zero in fitted model")


# Store:
pdf(paste0(figs,"TS_detrended.pdf"),height=12,width=10)
print(p1)
print(p2)
print(p3)
print(p4)
dev.off()

# Save detrended data for students to work with
# saveRDS(data_detrended, file=paste0(datapath, "data_students_network.RData"))



# -------------------------------------------------------------------------
# --------------- 3. Fit mlVAR network models: orthogonal estimation ------
# -------------------------------------------------------------------------

load(paste0(datapath, "network_orthogonal.RData"))

# Run mlVAR (orthogonal):
# res <- mlVAR(data_detrended,   
#              vars=varLabs, 
#              idvar="id",
#              dayvar="day", 
#              beepvar="beep", 
#              lags = 1,
#              temporal = "orthogonal", 
#              contemporaneous = "orthogonal",
#              nCores = 8)
# save(res, file=paste0(datapath, "network_orthogonal.RData"))

# Plot :
names <- c("Relax","Irritable","Worry","Nervous","Future",
           "Anhedonia","Tired","Alone",
           "Social-offline", "Social-online", "Outdoors",
           "C19-occupied", "C19-worry", "Home")

gr <- list('Stress'=c(1:7), 'Social'=c(8:10), 'COVID-19'=c(11:14))

# Get networks:
cont <- getNet(res, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
bet  <- getNet(res, "between", nonsig = "hide", rule = "and")
temp <- getNet(res, "temporal", nonsig = "hide")



# layout:
L <- averageLayout(cont,bet,temp)

# pdf(paste0(figs, "N_cont_orthogonal.pdf"), width=8, height=6)
# qgraph(cont, layout = L,
#               title="Contemporaneous", theme='colorblind', negDashed=FALSE,
#               groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
# dev.off()
# 
# pdf(paste0(figs, "N_temp_orthogonal.pdf"), width=8, height=6)
# qgraph(temp, layout = L,
#        title="Temporal, Lag-1", theme='colorblind', negDashed=FALSE,
#        groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
# dev.off()
# 
# pdf(paste0(figs, "N_temp_nodiag_orthogonal.pdf"), width=8, height=6)
# qgraph(temp, layout = L,
#        title="Temporal, Lag-1",  theme='colorblind', negDashed=FALSE,
#        groups=gr, legend.cex=0.4, details=TRUE, diag = FALSE, legend=TRUE, nodeNames = names)
# dev.off()
# 
# pdf(paste0(figs, "N_betw_orthogonal.pdf"), width=8, height=6)
# qgraph(bet, layout = L,
#        title="Between-Subjects",theme='colorblind', negDashed=FALSE,
#        groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
# dev.off()
# 
# pdf(paste0(figs, "N_betw_cors_orthogonal.pdf"), width=8, height=6)
# qgraph(cor(data_detrended_avg[,varLabs],use="pairwise.complete.obs"), layout = L, labels = varLabs,
#        title="Between-Subject marginal correlations",  theme='colorblind', negDashed=FALSE,
#        groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
# dev.off()



# -------------------------------------------------------------------------
# --------------- 4. Fit mlVAR network models: correlated estimation ------
# -------------------------------------------------------------------------

res2 <- readRDS(paste0(datapath,"network_correlated.RData"))

# Run mlVAR (correlated):
# res2 <- mlVAR(data_detrended,
#              vars=varLabs,
#              idvar="id",
#              dayvar="day",
#              beepvar="beep",
#              lags = 1,
#              temporal = "correlated",
#              contemporaneous = "correlated",
#              nCores = 8)
# saveRDS(res2, file=paste0(datapath, "network_orthogonal.RData"))

# Plot :
names <- c("Relax","Irritable","Worry","Nervous","Future",
           "Anhedonia","Tired","Alone", 
           "Social-offline", "Social-online", "Outdoors",
           "C19-occupied", "C19-worry", "Home") 

# Get networks:
cont2 <- getNet(res2, "contemporaneous", layout = "spring", nonsig = "hide", rule = "and")
bet2 <- getNet(res2, "between", nonsig = "hide", rule = "and")
temp2 <- getNet(res2, "temporal", nonsig = "hide")


pdf(paste0(figs, "N_cont_correlated.pdf"), width=8, height=6)
qgraph(cont2, layout = L,
       title="Contemporaneous", theme='colorblind', negDashed=FALSE,
       groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
dev.off()

pdf(paste0(figs, "N_temp_correlated.pdf"), width=8, height=6)
qgraph(temp2, layout = L,
       title="Temporal, Lag-1", theme='colorblind', negDashed=FALSE,
       groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
dev.off()

pdf(paste0(figs, "N_temp_nodiag_correlated.pdf"), width=8, height=6)
qgraph(temp2, layout = L,
       title="Temporal, Lag-1",  theme='colorblind', negDashed=FALSE,
       groups=gr, legend.cex=0.4, details=TRUE, diag = FALSE, legend=TRUE, nodeNames = names)
dev.off()

pdf(paste0(figs, "N_betw_correlated.pdf"), width=8, height=6)
qgraph(bet2, layout = L,
       title="Between-Subjects",theme='colorblind', negDashed=FALSE,
       groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
dev.off()

pdf(paste0(figs, "N_betw_cors_correlated.pdf"), width=8, height=6)
qgraph(cor(data_detrended_avg[,varLabs],use="pairwise.complete.obs"), layout = L, labels = varLabs,
       title="Between-Subject marginal correlations",  theme='colorblind', negDashed=FALSE,
       groups=gr, legend.cex=0.4, details=TRUE, legend=TRUE, nodeNames = names)
dev.off()



# -------------------------------------------------------------------------
# --------------- 5. Compare correlated and orthogonal estimation ---------
# -------------------------------------------------------------------------

cor(as.vector(cont),as.vector(cont2))  # 0.993
cor(as.vector(bet),as.vector(bet2))    # 0.983
cor(as.vector(temp),as.vector(temp2))  # 0.972

# fit orthogonal model, "res"
summary(res, show = c("fit"), round = 2)
resAIC <- c(6496.63, 6890.75,5988.36,6233.54,5900.58,6605.80,6630.53,5297.03,6826.54,6348.90,7624.41,6294.97,5693.62,6930.59)
resBIC <- c(6756.84,7150.96,6248.58,6493.75,6160.79,6866.01,6890.74,5557.24,7086.75,6609.11,7884.62,6555.18,5953.83,7190.80)
range(resAIC) # 5297.03 to 7624.41
mean(resAIC)  # 6411.589
range(resBIC) # 5557.24 to 7884.62
mean(resBIC)  # 6671.8

# fit correlated model, "res2
summary(res2, show = c("fit"), round = 2)
res2AIC <- c(6684.38,7053.19,6144.22,6437.75,6037.26,6743.79,6824.25,5409.88,7005.20,6543.92,7799.85,6469.32,5836.57,7081.64)
res2BIC <- c(7566.42,7935.23,7026.26,7319.78,6919.30,7625.83,7706.29,6291.92,7887.24,7425.95,8681.88,7351.36,6718.60,7963.68)
range(res2AIC) # 5409.88 to 7799.85 (worse fit than orthogonal: 5297.03 to 7624.41)
mean(res2AIC)  # 6576.516 (worse than orthogonal: 6411.589)
range(res2BIC) # 6291.92 to 8681.88 (worse than orthogonal: 5557.24 to 7884.62)
mean(res2BIC)  # 7458.553 (worse than orthogonal: 6671.8)

mlVARcompare(res, res2)
# Counts for best model fit over all variables
# lags   temporal temporalModel nAIC nBIC
# 1 correlated           VAR    0    0
# 1 orthogonal           VAR   14   14

layout(t(1:2))
qgraph(cont, layout = L, title="Contemporaneous orthogonal", theme='colorblind', details=TRUE) 
qgraph(cont2, layout = L, title="Contemporaneous correlated", theme='colorblind', details=TRUE) 
qgraph(bet, layout = L, title="Between-Person orthogonal", theme='colorblind', details=TRUE) 
qgraph(bet2, layout = L, title="Between-Person correlated", theme='colorblind', details=TRUE) 
qgraph(temp, layout = L, title="Temporal orthogonal", theme='colorblind', details=TRUE) 
qgraph(temp2, layout = L, title="Temporal correlated", theme='colorblind', details=TRUE) 
dev.off()



# -------------------------------------------------------------------------
# --------------- 6. Fix final plots for paper ----------------------------
# -------------------------------------------------------------------------

# without self loops
pdf(paste0(figs, "FIG_network_nodiag.pdf"), width=6, height=2.5)
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
qgraph(cont, layout = L,
       title="Contemporaneous network", theme='colorblind', negDashed=FALSE,
       groups=gr, legend=FALSE, nodeNames = names, labels=c(1:14),
       vsize=12,color=viridis_pal()(5)[3:5])
qgraph(temp, layout = L,
       title="Temporal network", theme='colorblind', negDashed=FALSE, diag=FALSE,
       groups=gr, legend.cex=0.4, legend=TRUE, nodeNames = names, labels=c(1:14),
       vsize=10,color=viridis_pal()(5)[3:5], asize=6)
dev.off()

# with self loops
pdf(paste0(figs, "FIG_network.pdf"), width=6, height=2.5)
layout(matrix(c(1,1,2,2,2), nc=5, byrow = TRUE)) # 40% vs 60% widths
qgraph(cont, layout = L,
       title="Contemporaneous network", theme='colorblind', negDashed=FALSE,
       groups=gr, legend=FALSE, nodeNames = names, labels=c(1:14),
       vsize=12,color=viridis_pal()(5)[3:5], details=TRUE)
qgraph(temp, layout = L,
       title="Temporal network", theme='colorblind', negDashed=FALSE, 
       groups=gr, legend.cex=0.4, legend=TRUE, nodeNames = names, labels=c(1:14),
       vsize=10,color=viridis_pal()(5)[3:5], asize=6, details=TRUE)
dev.off()

