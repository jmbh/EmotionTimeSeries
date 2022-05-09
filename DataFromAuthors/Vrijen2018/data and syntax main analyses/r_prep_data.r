### Supplementary code for the paper:
###
###Spread the joy: How high and low bias for happy facial emotions translate into different daily life affect dynamics  
###Charlotte Vrijen, Catharina Hartman, Eeske van Roekel, Peter de Jonge, & Albertine Oldehinkel

### Code for loading and preparing the data.

### Code was originally Written by Wolfgang Viechtbauer for the paper:
###The impact of treatments for depression on the dynamic network structure of mental states: Two randomized controlled trials.
### Evelien Snippe, Wolfgang Viechtbauer, Nicole Geschwind, Peter de Jonge, Marieke Wichers
###
###Code was adapted by Charlotte Vrijen for the present study.

############################################################################

### load data, sort data, and recode/rename variables so that the coding/naming is consistent across trials
### group:  1 = low happy bias, 2 = high happy bias

### outcome names
outnames <- c("ZINT", "ZJOY", "ZSAD", "ZIRR", "ZWOR", "ZPOS", "ZNEG")

if (study == "nfng") {
   dat <- read.table("happybias data for permutation main.csv", sep=",", header=TRUE, na.strings="")
   dat <- dat[order(dat$ID, dat$group, dat$Time),]
   
}

########for the other permutation tests for different group sizes, use "happybias data for permutation 20.csv", "happybias data for permutation 30.csv", "happybias data for permutation 35.csv" and "happybias data for permutation 40.csv"###

############################################################################

### to avoid captured output being split across lines and to avoid scientific notation
options(width=1000, scipen=100)

############################################################################
