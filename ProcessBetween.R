# jonashaslbeck@gmail.com; May 9, 2022

# ------------------------------------------------------
# -------- What is Happening here? ------------------------
# ------------------------------------------------------

# Process between person datasets into a list for use in later analysis

# Code below from Oisin Ryan

# ------------------------------------------------------
# -------- Specify Data Loading ------------------------
# ------------------------------------------------------

baseDir <- "DataClean/"


data_files <- c("data_Rowland2020.RDS", 
                "data_Bringmann2016.RDS", 
                "data_Vrijen2018_between.RDS", 
                "data_Fisher2017_between.RDS", 
                "data_Bringmann2013_between.RDS", 
                "data_Fried2021_between.RDS", 
                "data_Wendt2020_between.RDS")

# Specification of EMOTION variables
l_ind_vars <- list("Rowland2020" = 5:12, # 0-100 scale
                   "Bringmann2016" = 2:7, # 0-100
                   "Vrijen2018" = c(5,6,7,8), # 0-100
                   "Fisher2017" = 2:20, # 0-100
                   "Bringmann2013" = c(4,6:9), # 1-7
                   "Fried2021" = c(2:8,10:11), # 1-5
                   "Wendt2020" = 9:39) # 1-5 scale


datal <- list()
for(v in 1:7){
  datal[[v]] <- readRDS(paste0(baseDir, names(l_ind_vars)[v], "/", data_files[v]))
}

# Make storage for the list file output
data_bet_l <- list()

# ------------------------------------------------------
# ------ Load all datasets, get into same format -------
# ------------------------------------------------------


# ---------------- Rowland 2022 ------------------------


 u_id <- unique(datal[[1]]$subj_id)
 databet <- matrix(NA, nrow = length(u_id), ncol = 2, dimnames = list(NULL, c("subj_id","group")))
for(i in 1:length(u_id)){
  bet <- unique(datal[[1]][datal[[1]][,"subj_id"] == u_id[i],"group"])
  if(length(bet) != 1){ stop(paste0("non-unique between person", i)) 
  }else{
    # recode for dummies
    databet[i,2] <- bet -1
  }
  
  databet[i,1] <- u_id[i]
}

data_bet_l[[1]] <- databet



# -------------- Bringmann 2016 ------------------------


# extremely weird format, see following
databet <- cbind(unique(datal[[2]]$subj_id) ,datal[[2]]$Neuroticism[1:95])
colnames(databet) = c("subj_id","Neuroticism")  
data_bet_l[[2]] <- databet


# ----------- Vrijen et al 2018 ------------------------


u_id <- unique(datal[[3]]$subj_id)
databet <- matrix(NA, nrow = length(u_id), ncol = 2, dimnames = list(NULL, c("subj_id","happybias")))
for(i in 1:length(u_id)){
  d <- as.data.frame(datal[[3]])
  bet <- unique(d[d[,"subj_id"] == u_id[i],"happybias"])
  if(length(bet) != 1){ stop(paste0("non-unique between person", i)) 
  }else{
    # recode for dummies, Low happybias =1 and high happybias = 2. Low happybias is a sign of "unhealthy"
    # so recode such that lowhappybias = 1 and high = 0 (so we always have 0 as healthy)
    bet <- as.matrix(bet)
     databet[i,2] <- ifelse(bet == 2, 0, bet)
  }
  
  databet[i,1] <- u_id[i]
}

data_bet_l[[3]] <- databet


# ----------------- Fisher 2017 ------------------------


colnames(datal[[4]])[1] <- "subj_id"
data_bet_l[[4]] <- datal[[4]]



# -------------- Bringmann 2013 ------------------------


u_id <- unique(datal[[5]]$subj_id)
databet <- matrix(NA, nrow = length(u_id), ncol = 2, dimnames = list(NULL, c("subj_id","Neurotic")))
for(i in 1:length(u_id)){
  bet <- unique(datal[[5]][datal[[5]][,"subj_id"] == u_id[i],"Neurotic"])
  bet <- bet[!is.na(bet)]
  if(length(bet) ==0){ databet[i,2] <- NA}else{
    # recode for dummies
    databet[i,2] <- bet
  }
  
  databet[i,1] <- u_id[i]
}
data_bet_l[[5]] <- databet


# ------------------- Fried 2021------------------------

data_bet_l[[6]] <- datal[[6]]

# ------------------- Wendt 2020------------------------

data_bet_l[[7]] <- datal[[7]]


# ------------------------------------------------------
# -------------------- Save ----------------------------
# ------------------------------------------------------

lapply(data_bet_l, function(l)head(l))
saveRDS(data_bet_l, file = "DataClean/BetweenData.RDS")

