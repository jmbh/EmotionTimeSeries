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
#                            Part VI: Multilevel trends                          #
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
# --------------- 2. Multilevel regression models -------------------------
# -------------------------------------------------------------------------

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

# Make the beep variable factor in dataset:
Data5b$beepFactor <- factor(Data5b$beep, levels = 0:3, labels = c("09:00 - 12:00","12:00 - 15:00","15:00 - 18:00","18:00 - 21:00"))

# Make day variable for dates:
Data5b$date <- as.Date("2020-03-15") + Data5b$day

# Add the midpoints as time variable:
Data5b$midTime <- as.character(factor(Data5b$beep, levels = 0:3, labels = c("10:30","13:30","16:30","19:30")))
Data5b$midTime <- as.POSIXct(paste(Data5b$date,Data5b$midTime), format = "%Y-%m-%d %H:%M", tz = "Europe/Amsterdam")

# Fit multi level:
library(lme4)
library(lmerTest)

# Standardize effects:
DataSTD <- Data5b %>% ungroup %>%
  mutate(
      day = scale(day)
  ) %>% mutate_at(
    varLabs, list(~scale(.))
  )

DataSTD_lagged <- DataSTD %>%  group_by(id)  %>%
  # Add lagged effect:
  mutate_at(
    varLabs, list(function(x)c(NA,x[-length(x)]))
  )

names(DataSTD_lagged)[names(DataSTD_lagged) %in% varLabs] <- paste0(varLabs,"_lagged")


DataSTD <- DataSTD %>% left_join(DataSTD_lagged)

fits <- list()

# All models to test:
allModels <- expand.grid(
  day = c("zero","fixed","random"),
  beep = c("zero","fixed","random"),
  lag = c("zero","fixed","random")
)


# Fit all models:
for (v in seq_along(varLabs)){
  fits[[v]] <- list()
  names(fits)[v] <- varLabs[v]
  
  # Fit all models:
  for (i in seq_len(nrow(allModels))){
    fixed <- "1"
    random <- "1"
    
    # Fixed:
    if (allModels$day[i] != "zero"){
      fixed <- paste0(fixed, " + day")
    }
    if (allModels$beep[i] != "zero"){
      fixed <- paste0(fixed, " + factor(beep)")
    }
    if (allModels$lag[i] != "zero"){
      fixed <- paste0(fixed, " + ",varLabs[v],"_lagged")
    }
    
    # Random:
    if (allModels$day[i] == "random"){
      random <- paste0(random, " + day")
    }
    if (allModels$beep[i] == "random"){
      random <- paste0(random, " + factor(beep)")
    }
    if (allModels$lag[i] == "random"){
      random <- paste0(random, " + ",varLabs[v],"_lagged")
    }
    
    # Formula:
    form <- paste0(varLabs[v]," ~ ",fixed," + ( ",random," | id)")
    
    # Run lmer:
    fits[[v]][[form]] <- lmer(as.formula(form), data = DataSTD, REML = FALSE)
    
  }
  # 
  # # Fit models for occupied:
  # fits[[v]]$fit_intercept <-  lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 | id)")), data = DataSTD, REML = FALSE)
  # fits[[v]]$fit_intercept_day <- lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 + day | id)")), data = DataSTD, REML = FALSE)
  # fits[[v]]$fit_intercept_beep <- lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 + factor(beep)| id)")), data = DataSTD, REML = FALSE)
  # fits[[v]]$fit_intercept_lag <- lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 + ",varLabs[v],"_lagged  | id)")), data = DataSTD, REML = FALSE)
  # fits[[v]]$fit_intercept_day_beep <- lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 + day + factor(beep)  | id)")), data = DataSTD, REML = FALSE)
  # fits[[v]]$fit_intercept_day_lag <- lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 + day + ",varLabs[v],"_lagged  | id)")), data = DataSTD, REML = FALSE)
  # fits[[v]]$fit_intercept_beep_lag <- lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 +  factor(beep) + ",varLabs[v],"_lagged  | id)")), data = DataSTD, REML = FALSE)
  # fits[[v]]$fit__intercept_day_beep_lag <- lmer(as.formula(paste0(varLabs[v]," ~ 1 + day + factor(beep) + ",varLabs[v],"_lagged + (1 + day + factor(beep) + ",varLabs[v],"_lagged  | id)")), data = DataSTD, REML = FALSE)
}

# Compute BICs:
BICs <- lapply(fits, sapply, BIC)

# Posterior model probability:
library("Rmpfr")
postP <- lapply(BICs,function(x){
  BICs <- mpfr(x, 100)
  BICtrans <- exp(-0.5 * BICs)
  sumBICtrans <- sum(BICtrans)
 as.numeric(BICtrans / sumBICtrans)
})

model <- paste0(
  sapply(allModels$day, switch,
       "zero" = "",
       "fixed" = "day",
       "random" = "day (R)"),
  " + ",
  sapply(allModels$beep, switch,
         "zero" = "",
         "fixed" = "beep",
         "random" = "beep (R)"),
  " + ",
  sapply(allModels$lag, switch,
         "zero" = "",
         "fixed" = "lag",
         "random" = "lag (R)")
)
model <- gsub("^\\s\\+\\s","",model)
model <- gsub("^\\s\\+\\s","",model)
model <- gsub("^\\s\\+\\s","",model)
model <- gsub("\\s\\+\\s\\s\\+\\s"," + ",model)
model <- gsub("\\s\\+\\s$","",model)

# Coefs for day effect:
coefs <- lapply(fits, sapply, function(x){
  coef <- fixef(x)["day"]
  if (is.na(coef)){
    coef <- 0
  }
  coef
})
# SD for day effects:
SDs <- lapply(fits, sapply, function(x){
  SD <- attr( VarCorr(x)$id, "stddev")["day"]
  # if (is.na(SD)){
  #   SD <- 0
  # }
  SD
})



# Heatmap:
df <- do.call(rbind,lapply(seq_along(postP),function(i){
  data.frame(
    var = names(postP)[i],
    model = model,
    BIC = BICs[[i]],
    postP = postP[[i]],
    coefs = coefs[[i]],
    SD = SDs[[i]]
  )
}))

df$raneflabel <- round(df$coefs, 2)
df$raneflabel <- paste0(df$raneflabel,
  ifelse(!is.na(df$SD),  paste0("\n(",round(df$SD,2),")"),
  "")
)

library("ggplot2")
g <- ggplot(df, aes(x=var, y = model, fill = postP,
                    label = raneflabel)) + 
  geom_tile() + 
  scale_fill_gradient("P(model)",low = "white", high = "black",limits = c(0,1)) +
 scale_x_discrete(expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) + 
  geom_text(col="white",cex = 1.75) +
  xlab("") + 
  ylab("") +
  theme_bw() + 
  theme( axis.ticks = element_blank(), axis.text.x = element_text( angle = 270, hjust = 0, vjust = 0.5, colour = "grey50"))+
  geom_hline(yintercept = seq(2, length(unique(df$model)))- 0.5, col = rgb(0.9,0.9,0.9) )+
  geom_vline(xintercept = seq(2, length(unique(df$var)))- 0.5, col = rgb(0.9,0.9,0.9))


pdf("MLtrends.pdf",height = 8, width = 8)
print(g)
dev.off()


# Occupied and worried:
df %>% dplyr::filter(
  var %in% c("C19_worry","C19_occupied")
) %>%
  group_by(var) %>% 
  filter(BIC==min(BIC))

# Occupied:
round(100 * pnorm(0, mean=-0.125,sd=0.131),1)
round(100*pnorm(-0.1, mean=-0.125,sd=0.131),1)

# Worry:
round(100*pnorm(0, mean=-0.102,sd=0.143),1)
round(100*pnorm(-0.1, mean=-0.102,sd=0.143),1)
