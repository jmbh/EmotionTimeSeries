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

########PERMUTATION TEST 3########

############################################################################

rm(list=ls())
library(lme4)
library(parallel)

############################################################################

### select study to analyze
study <- "nfng"

### number of permutation iterations
perms <- 10000

### number of cores to use for (multicore) processing
ncpus <- 1

############################################################################

### load data, sort data, and recode/rename variables so that the coding/naming is consistent across trials
### group:  1 = low happy bias, 2 = high happy bias

source("r_prep_data.r")

############################################################################

### permutation function

permfunc <- function(iter, dat, nobs.per.person, group.per.person, outnames) {
  
  library(nlme)
  
  ### reshuffle group variable (but 1st iteration is always the original data)
  if (iter > 1)
    dat$group <- rep(sample(group.per.person), times=nobs.per.person)
  
  ### matrices for storing the coefficients
  b.G1 <- matrix(NA, nrow=length(outnames), ncol=length(outnames), dimnames=list(outnames,outnames))
  b.G2 <- matrix(NA, nrow=length(outnames), ncol=length(outnames), dimnames=list(outnames,outnames))
  
  for (outname in outnames) {
    
    dat$outcome <- dat[,outname]
    
    res.G1 <- try(lmer(outcome ~ ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 + (ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 || ID), data=dat, subset=group==1), silent=TRUE)
    res.G2 <- try(lmer(outcome ~ ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 + (ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 || ID), data=dat, subset=group==2), silent=TRUE)
    
    
    ### if one of the models doesn't converge, break out of loop
    if (inherits(res.G1, "try-error") | inherits(res.G2, "try-error"))
      break
    
    ### store coefficients
    b.G1[,which(outname == outnames)] <- fixef(res.G1)[2:(length(outnames)+1)]
    b.G2[,which(outname == outnames)] <- fixef(res.G2)[2:(length(outnames)+1)]
  }
  

   ### if one of the coefficients is NA, return NA; otherwise return the difference in the difference in mean connections from positive nodes JOY and POS to negative nodes SAD, IRR WOR and NEG. 
   if (any(is.na(b.G1)) | any(is.na(b.G2))) {
      return(NA)
   } else {
      return((b.G2[2,3]+b.G2[2,4]+b.G2[2,5]+b.G2[2,7]+b.G2[6,3]+b.G2[6,4]+b.G2[6,5]+b.G2[6,7]) - (b.G1[2,3]+b.G1[2,4]+b.G1[2,5]+b.G1[2,7]+b.G1[6,3]+b.G1[6,4]+b.G1[6,5]+b.G1[6,7]))
   }

   
}

############################################################################

### start local cluster for multicore processing
if (ncpus > 1)
   cl <- makePSOCKcluster(ncpus)

### matrices for storing the coefficients
b.G1 <- matrix(NA, nrow=length(outnames), ncol=length(outnames), dimnames=list(outnames,outnames))
b.G2 <- matrix(NA, nrow=length(outnames), ncol=length(outnames), dimnames=list(outnames,outnames))

for (outname in outnames) {

   cat("outcome: ", outname, "\n")

   dat$outcome <- dat[,outname]

  
   res.G1 <- try(lmer(outcome ~ ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 + (ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 || ID), data=dat, subset=group==1), silent=TRUE)
   res.G2 <- try(lmer(outcome ~ ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 + (ZINT_1 + ZJOY_1 + ZSAD_1 + ZIRR_1 + ZWOR_1 + ZPOS_1 + ZNEG_1 || ID), data=dat, subset=group==2), silent=TRUE)
   
   
   
   ### store coefficients
   b.G1[,which(outname == outnames)] <- fixef(res.G1)[2:(length(outnames)+1)]
   b.G2[,which(outname == outnames)] <- fixef(res.G2)[2:(length(outnames)+1)]
   
}

### number of observations per person (pre and post together)
nobs.per.person <- sapply(split(dat$group, dat$ID), length)

### group of each person (either high bias or low bias)
group.per.person <- sapply(split(dat$group, dat$ID), function(x) x[1])

### repeatedly apply permfunc() function
time.start <- proc.time()
if (ncpus == 1) {
   permres <- lapply(1:perms, permfunc, dat=dat, nobs.per.person=nobs.per.person, group.per.person=group.per.person, outnames=outnames)
} else {
   permres <- parLapply(cl, 1:perms, permfunc, dat=dat, nobs.per.person=nobs.per.person, group.per.person=group.per.person, outnames=outnames)
}
time.end <- proc.time()
cat("Minutes:", ((time.end - time.start)/60)[3], "\n")

### turn results into a matrix
permres <- do.call(rbind, permres)

############################################################################

### table with permutation based p-values (two definitions of the p-values)
b.diff.obs <- (b.G2[2,3]+b.G2[2,4]+b.G2[2,5]+b.G2[2,7]+b.G2[6,3]+b.G2[6,4]+b.G2[6,5]+b.G2[6,7]) - (b.G1[2,3]+b.G1[2,4]+b.G1[2,5]+b.G1[2,7]+b.G1[6,3]+b.G1[6,4]+b.G1[6,5]+b.G1[6,7])
p.perm.def1 <- 2*min(mean(permres >= b.diff.obs, na.rm=TRUE), mean(permres <= b.diff.obs, na.rm=TRUE))
p.perm.def2 <- min(1, 2*ifelse(b.diff.obs > 0, mean(permres >= b.diff.obs, na.rm=TRUE), mean(permres <= b.diff.obs, na.rm=TRUE)))

### save results to file
sav <- round(cbind(b.diff.obs, "p-perm.def1"=p.perm.def1, "p-perm.def2"=p.perm.def2, "conv"=sum(!is.na(permres[,1]))), 6)
capture.output(sav, file=paste0("tabletestdiffjoyandposttoneg.txt"))

### stop local cluster for multicore processing
if (ncpus > 1)
   stopCluster(cl)

############################################################################
