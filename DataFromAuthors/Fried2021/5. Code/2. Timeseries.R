##################################################################################
#                                                                                #
#                      COVID-19 student mental health study                      #
#           to be published in Clinical Psychological Science, 2021              #
#                                                                                #
#                         Eiko Fried, eikofried@gmail.com                        #
#              with code from Faidra Papanikolaou & Sacha Epskamp                #
#                                                                                #
#                          Last updated: April 13 2021                           #
#                           (date of paper acceptance)                           #
#                                                                                #
#                              Part II: Time series                              #
#                                                                                #
##################################################################################



### Timeline in the Netherlands: 
# Day1-  16.03.2020: Second day Leiden University is closed, no teaching/assessment this week
# Day2-  17.03.2020: 
# Day3-  18.03.2020: 
# Day4-  19.03.2020: Leiden University: online teaching & assessment until end of academic year
# Day5-  20.03.2020: 
# Day6-  21.03.2020: 
# Day7-  22.03.2020: 
# Day8-  23.03.2020: Dutch Gov: extensive list of rules, e.g. gatherings banned until June 1st, high-contact services until April 6th
# Day9-  24.03.2020: 
# Day10- 25.03.2020: 
# Day11- 26.03.2020: 
# Day12- 27.03.2020: 
# Day13- 28.03.2020: Dutch Gov: measures until April 6th ended to 28 April
# Day14- 29.03.2020: 



# -------------------------------------------------------------------------
# --------------- 1. Loading packages & Data ------------------------------
# -------------------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(mlVAR)
library(qgraph)
library(bootnet)
library(reshape)
library(viridis)

figs <- "./figures/" # figure directory
datapath <- "./data/" # data directory

### I recommend creating the corresponding folders within your core directoy in which you carry out your analyses
### Specifically, you will need to create "/figures" & "/data" to execute this script without changing it


# -------------------------------------------------------------------------
# --------------- 2. Data preparation -------------------------------------
# -------------------------------------------------------------------------

### Time series preprocressing

load(paste0(datapath, "clean_ema.RData"))

Data2 <- as.data.frame(rawdata[,-c(2:5,24)]) 

### Clean data
#recode
colnames(Data2)[1]  <- "id"
colnames(Data2)[20] <- "day"
colnames(Data2)[21] <- "beep"

# beep
Data2$beep <- as.numeric(Data2$beep)
Data2$beep <- Data2$beep-1 # start at 0

# change ID to subsequent numbers, makes looping easier
un <- unique(Data2$id)
id_new <- rep(NA, length(Data2$id))
for(i in 1:length(un)) id_new[Data2$id==un[i]] <- i
Data2$id <- id_new

# change day to subsequent numbers, makes looping easier
Data2$day <- as.numeric(Data2$day)
un2 <- unique(Data2$day)
day_new <- rep(NA, length(Data2$day))
for(i in 1:length(un2)) day_new[Data2$day==un2[i]] <- i
Data2$day <- day_new
# Data2$day <- Data2$day-1  # start at 0

# new time variable taking into account both day and beep
Data2$conc <- as.numeric(interaction(Data2$beep, Data2$day))

# save this dataset for network modeling
# save(Data2, file=paste0(datapath,"clean_network.RData"))



# -------------------------------------------------------------------------
# --------------- 3. Data transformation for ggplot -----------------------
# -------------------------------------------------------------------------


### ggplot2 transformations
# transform all 18 variables to long format for ggplot
# remove day & beep
Data3 <- select(Data2, -day)
Data3 <- select(Data3, -beep)

# create new colums 'variable' & 'values'
Data3 <- melt(Data3, id=c("id","conc")) 
colnames(Data3)[2] <- "time"

# rename variables
Data3[,3] <- as.character(Data3[,3])

Data3[Data3=="Q1"]  <- "Relax"
Data3[Data3=="Q2"]  <- "Irritable"
Data3[Data3=="Q3"]  <- "Worry"
Data3[Data3=="Q4"]  <- "Nervous"
Data3[Data3=="Q5"]  <- "Future"

Data3[Data3=="Q6"]  <- "Anhedonia"
Data3[Data3=="Q7"]  <- "Tired"
Data3[Data3=="Q8"]  <- "Hungry"
Data3[Data3=="Q9"]  <- "Alone"
Data3[Data3=="Q10"] <- "Angry"

Data3[Data3=="Q11"] <- "Social-offline"
Data3[Data3=="Q12"] <- "Social-online"
Data3[Data3=="Q13"] <- "Music"
Data3[Data3=="Q14"] <- "Procrastinate"
Data3[Data3=="Q15"] <- "Outdoors"

Data3[Data3=="Q16"] <- "C19-occupied"
Data3[Data3=="Q17"] <- "C19-worry"
Data3[Data3=="Q18"] <- "Home"

# delete 4 variables of low interest
Data3 <- filter(Data3, Data3$variable!=c("Music"))
Data3 <- filter(Data3, Data3$variable!=c("Procrastinate"))
Data3 <- filter(Data3, Data3$variable!=c("Hungry"))
Data3 <- filter(Data3, Data3$variable!=c("Angry"))

# Check missing values in Data3 for the variables we investigate
length(Data3$value)  # 61208 data points
summary(Data3$value) # 6026 
6026 / 61208         # 9.845% missing data

            
Data4 <- aggregate(Data3,by=list(Data3$time, Data3$variable), FUN=mean,na.rm=T)
# Danger, this changes variable order, ugh

Data4 <- Data4[,-c(3,4,5)]
colnames(Data4) <- c("Time","Variable","Frequency")



# -------------------------------------------------------------------------
# --------------- 4. Plot item means across time --------------------------
# -------------------------------------------------------------------------

# ## All items; too messy to plot.
# pdf(paste0(figs, "TS_all.pdf"), width=6.5, height=4)
# ggplot(Data4, aes(Time, Frequency, colour=Variable)) +
#   #geom_point(alpha=.3) +
#   xlab("Days") +
#   geom_smooth(method = "loess", span = 0.1, se = FALSE, size=.6) +
#   ylim(0.82, 5) +
#   theme_light() +
#   scale_color_viridis_d() +
#   theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
#         panel.grid.major.x = element_blank()) +
#   scale_x_continuous(breaks = c(1,5,9,13,17,21,25,29,33,37,41,45,49,53),
#                      label = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14")) +
#   geom_vline(aes(xintercept = 0  ), color='#777777', alpha=1, linetype=11, size=.3) +
#   geom_vline(aes(xintercept = 4*4 ), color='#777777', alpha=1, linetype=11, size=.3) +
#   geom_vline(aes(xintercept = 8*4 ), color='#777777', alpha=1, linetype=11, size=.3) +
#   geom_vline(aes(xintercept = 13*4), color='#777777', alpha=1, linetype=11, size=.3) +
#   annotate("text", x= .3, y=0.84, hjust = 0, size=2.6, color='#444444',
#            label="University closes, \none week no teaching") +
#   annotate("text", x=16.3, y=0.84, hjust = 0, size=2.6, color='#444444',
#            label="University closed until \nend of semester") +
#   annotate("text", x=32.3, y=0.84, hjust = 0, size=2.6, color='#444444',
#            label="Dutch gov: list of \nCOVID-19 rules") +
#   annotate("text", x=52.3, y=0.84, hjust = 0, size=2.6, color='#444444',
#            label="Extension \nof rules")
# dev.off()


### Mental health items
pdf(paste0(figs, "TS_mental.pdf"), width=6.5, height=4)
ggplot(filter(Data4 %>% filter(Variable == "Relax" |
                               Variable == "Irritable" |
                               Variable == "Worry" |
                               Variable == "Nervous" |
                               Variable == "Future" |
                               Variable == "Anhedonia" |
                               Variable == "Tired")),
       aes(Time, Frequency, colour=Variable)) +
  xlab("Days") +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size=.6) +
  theme_light() +
  scale_color_viridis_d() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank()) +
  geom_vline(aes(xintercept = -1   ), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (4*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (8*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (13*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  annotate("text", x= -.7, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="University closes, \none week no teaching") +
  annotate("text", x=13.3, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="University closed until \nend of semester") +
  annotate("text", x=29.3, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="Dutch gov: list of \nCOVID-19 rules") +
  annotate("text", x=49.3, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="Extension \nof rules") +
  scale_x_continuous(breaks = c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57),
                     label = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")) +
  scale_y_continuous(breaks = c(1,2,3),
                     label = c("1","2","3"), limits=c(0.85,3))
dev.off()

pdf(paste0(figs, "TS_mental_nolegend.pdf"), width=6.5, height=4)
ggplot(filter(Data4 %>% filter(Variable == "Relax" |
                                 Variable == "Irritable" |
                                 Variable == "Worry" |
                                 Variable == "Nervous" |
                                 Variable == "Future" |
                                 Variable == "Anhedonia" |
                                 Variable == "Tired")),
       aes(Time, Frequency, colour=Variable)) +
  xlab("Days") +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size=.6) +
  theme_light() +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank()) +
  geom_vline(aes(xintercept = -1   ), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (4*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (8*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (13*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  annotate("text", x= -.7, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="University closes, \none week no teaching") +
  annotate("text", x=13.3, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="University closed until \nend of semester") +
  annotate("text", x=29.3, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="Dutch gov: list of \nCOVID-19 rules") +
  annotate("text", x=49.3, y=0.88, hjust = 0, size=2.6, color='#444444',
           label="Extension \nof rules") +
  scale_x_continuous(breaks = c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57),
                     label = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")) +
  scale_y_continuous(breaks = c(1,2,3),
                     label = c("1","2","3"), limits=c(0.85,3))
dev.off()


### Social & covid items
pdf(paste0(figs, "TS_social_covid.pdf"), width=6.5, height=4)
ggplot(filter(Data4 %>% filter(Variable == "Alone" |
                               Variable == "Social-offline" |
                               Variable == "Social-online" |
                               Variable == "Outdoors" |
                               Variable == "C19-occupied" |
                               Variable == "C19-worry" |
                               Variable == "Home")), 
       aes(Time, Frequency, colour=Variable)) + 
  xlab("Days") +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size=.6) +
  ylim(0.7, 5) +
  theme_light() +
  scale_color_viridis_d() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank()) +
  scale_x_continuous(breaks = c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57),
                     label = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")) +
  geom_vline(aes(xintercept = -1   ), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (4*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (8*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (13*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  annotate("text", x= -.7, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="University closes, \none week no teaching") +
  annotate("text", x=13.3, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="University closed until \nend of semester") +
  annotate("text", x=29.3, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="Dutch gov: list of \nCOVID-19 rules") +
  annotate("text", x=49.3, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="Extension \nof rules") 
dev.off()  

pdf(paste0(figs, "TS_social_covid_nolegend.pdf"), width=6.5, height=4)
ggplot(filter(Data4 %>% filter(Variable == "Alone" |
                                 Variable == "Social-offline" |
                                 Variable == "Social-online" |
                                 Variable == "Outdoors" |
                                 Variable == "C19-occupied" |
                                 Variable == "C19-worry" |
                                 Variable == "Home")), 
       aes(Time, Frequency, colour=Variable)) + 
  xlab("Days") +
  geom_smooth(method = "loess", span = 0.1, se = FALSE, size=.6) +
  ylim(0.7, 5) +
  theme_light() +
  theme(legend.position = "none") +
  scale_color_viridis_d() +
  theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(), 
        panel.grid.major.x = element_blank()) +
  scale_x_continuous(breaks = c(1,5,9,13,17,21,25,29,33,37,41,45,49,53,57),
                     label = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")) +
  geom_vline(aes(xintercept = -1   ), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (4*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (8*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  geom_vline(aes(xintercept = (13*4)-3), color='#777777', alpha=1, linetype=11, size=.3) +
  annotate("text", x= -.7, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="University closes, \none week no teaching") +
  annotate("text", x=13.3, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="University closed until \nend of semester") +
  annotate("text", x=29.3, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="Dutch gov: list of \nCOVID-19 rules") +
  annotate("text", x=49.3, y=0.75, hjust = 0, size=2.6, color='#444444',
           label="Extension \nof rules") 
dev.off()  
  
  

# -------------------------------------------------------------------------
# --------------- 5. Interindividual variability --------------------------
# -------------------------------------------------------------------------

colnames(Data3) <- c("ID","Time","Variable","Frequency")

### Relative frequencies

pdf(paste0(figs, "TS_items_ordered.pdf"), width=7, height=4.2)
for (i in 1:14) {
    temp <- filter(Data3, Variable==unique(Data3$Variable)[i]) # filter specific variables
    print(ggplot(na.omit(temp), aes(Time, fill = factor(Frequency))) +
    geom_histogram(binwidth = 1, position ="fill") +
    theme_light() + 
    labs(caption = paste("Variable:",temp$Variable[i])) +
    theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
          panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank()) +
    theme(legend.title = element_blank()) + 
    ylab("Relative frequency") +
    xlab("Days") +
    scale_x_continuous(breaks = c(0.5,4.5,9.5,13.5,17.5,21.5,25.5,29.5,33.5,37.5,41.5,45.5,49.5,53.5,57.5),
                       label = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")) +
    scale_fill_viridis_d(option='viridis', begin=0.15, direction=-1) +
    geom_vline(aes(xintercept = -.5   ),     color='#777777', alpha=1, linetype=11, size=.3) +
    geom_vline(aes(xintercept = (4*4)-2.5),  color='#777777', alpha=1, linetype=11, size=.3) +
    geom_vline(aes(xintercept = (8*4)-2.5),  color='#777777', alpha=1, linetype=11, size=.3) +
    geom_vline(aes(xintercept = (13*4)-2.5), color='#777777', alpha=1, linetype=11, size=.3) +
    annotate("text", x= -.2, y=1.04, hjust = 0, size=2.2, color='#444444',
             label="University closes, \none week no teaching") +
    annotate("text", x=13.8, y=1.04, hjust = 0, size=2.2, color='#444444',
             label="University closed until \nend of semester") +
    annotate("text", x=29.8, y=1.04, hjust = 0, size=2.2, color='#444444',
             label="Dutch gov: list of \nCOVID-19 rules") +
    annotate("text", x=49.8, y=1.04, hjust = 0, size=2.2, color='#444444',
             label="Extension \nof rules")
  )
}; dev.off()


pdf(paste0(figs, "TS_items_means.pdf"), width=6.3, height=4.2)
for (i in 1:14) {
  temp <- filter(Data3, Variable==unique(Data3$Variable)[i])
  temp2 <- filter(Data4, Variable==temp[1,3])  # since Data3 > Data4 changed variable order, we need to be creative
  print(ggplot(temp, aes(Time, Frequency)) +  
          geom_vline(aes(xintercept = -.5   ),     color='#777777', alpha=1, linetype=11, size=.3) +
          geom_vline(aes(xintercept = (4*4)-2.5),  color='#777777', alpha=1, linetype=11, size=.3) +
          geom_vline(aes(xintercept = (8*4)-2.5),  color='#777777', alpha=1, linetype=11, size=.3) +
          geom_vline(aes(xintercept = (13*4)-2.5), color='#777777', alpha=1, linetype=11, size=.3) +
          annotate("text", x= -.2, y=5.7, hjust = 0, size=2.2, color='#444444',
                   label="University closes, \none week no teaching") +
          annotate("text", x=13.8, y=5.7, hjust = 0, size=2.2, color='#444444',
                   label="University closed until \nend of semester") +
          annotate("text", x=29.8, y=5.7, hjust = 0, size=2.2, color='#444444',
                   label="Dutch gov: list of \nCOVID-19 rules") +
          annotate("text", x=49.8, y=5.7, hjust = 0, size=2.2, color='#444444',
                   label="Extension \nof rules") +
          #geom_line(position=position_jitter(w=0.4, h=0), alpha=.1) +
          geom_jitter(alpha=.1, height=.2, width=.3) +
          geom_smooth(data=temp2, method = "loess", span = 0.1, se = FALSE, 
                      size=1.7, color=alpha(viridis(11)[2],.8)) +
          ylim(1, 5.7) + 
          xlab("Days") +
          scale_x_continuous(breaks = c(0.5,4.5,9.5,13.5,17.5,21.5,25.5,29.5,33.5,37.5,41.5,45.5,49.5,53.5,57.5),
                             label = c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")) +
          theme_light() +
          labs(caption = paste("Variable:",temp$Variable[i])) +
          theme(panel.grid.minor.y = element_blank(), panel.grid.minor.x = element_blank(),
                panel.grid.major.x = element_blank())
  )
};dev.off() 