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
#                     Part V: COVID cases in the Netherlands                     #
#                                                                                #
##################################################################################



# -------------------------------------------------------------------------
# --------------- 1. Loading packages & Data ------------------------------
# -------------------------------------------------------------------------

library("ggplot2")
library("viridis")

figs <- "./figures/" # figure directory
datapath <- "./data/" # data directory



# -------------------------------------------------------------------------
# --------------- 2. March 2020 cases, deaths, and transit requests -------
# -------------------------------------------------------------------------


# data for deaths and cases from https://www.worldometers.info/coronavirus/country/netherlands/
# accessed April 12th 2020

# data for mobility requests from https://www.apple.com/covid19/mobility
# accessed April 18th 2020



# period: march 01 to 31
totalNL <- c(10,18,23,38,83,
             128,188,265,321,382,
             503,614,804,959,1135,
             1413,1705,2051,2460,2994,
             3631,4204,4749,5560,6412,
             7431,8603,9762,10866,11750,
             12595)
deathsNL <- c(0,0,0,0,0,
              1,1,8,4,4,
              5,5,10,12,20,
              24,43,58,76,106, 
              136,179,213,276,356, 
              434,546,639,771,864, 
              1038)
transit <- c(102.7,105.5,101.4,102.3,108.8,
             122.0,119.5,101.4,98.01,94.36,
             90.51,77.33,69.44,61.84,43.19,
             32.44,27.85,24.39,22.51,22.32,
             21.25,18.55,19.1,19.48,20.08,
             20.37,20.62,19.38,17.62,20.2,
             20.92)
transit <- transit*100

df_case <- as.data.frame(matrix(NA, nrow=62, ncol=3))
df_case[c(1:31),1]  <- totalNL
df_case[c(32:62),1] <- deathsNL
df_case[c(63:93),1] <- transit
df_case[c(1:31),2]  <- "Cases"
df_case[c(32:62),2] <- "Deaths"
df_case[c(63:93),2] <- "Apple Maps \nrequests (%)"
df_case[c(1:31),3]  <- df_case[c(32:62),3] <- df_case[c(63:93),3] <-c(1:31)

colnames(df_case) <- c("People", "Type", "Time")

pdf(paste0(figs, "FIG_casesdeathsNL.pdf"), width=5.5, height=3.5)
ggplot(df_case, aes(Time, People, color=factor(Type))) + 
  xlab("Days in March 2020") + ylab("") + 
  labs(color="COVID-19") + 
  ylim(-500, 12600) +
  geom_vline(aes(xintercept = 16), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = 29), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_rect(aes(xmin=16,xmax=29,ymin=-Inf,ymax=Inf),
            fill="#eeeeee", alpha=.1, color=NA) +
  annotate("text", x=27.7, y=12000, hjust = 1, size=2.5, color='#444444',
           label="14 days of Ecological  \nMomentary Assessment") +
  annotate("text", x=.5, y=9800, hjust = 0, size=2.5, color=viridis(9)[8],
           label="100%") +
  annotate("text", x=12.6, y=5200, hjust = 0, size=2.5, color=viridis(9)[8],
           label="50%") +
  theme_light() + 
  theme(legend.title = element_blank()) + 
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank()) +
  scale_color_viridis_d(begin=0.3, end=.9, alpha=1, direction=-1) +
  geom_hline(yintercept=c(0,5000,10000),colour="grey70", size=.2) +
  geom_point(alpha=.9, shape=16) +
  geom_line(alpha=0.4)
dev.off()
