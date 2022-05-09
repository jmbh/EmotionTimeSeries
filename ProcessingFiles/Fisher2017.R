# jonashaslbeck@gmail.com; May 5, 2022

# ------------------------------------------------------------------
# ------------- Load data ------------------------------------------
# ------------------------------------------------------------------

# Fisher Reanalysis
folder <- "DataFromAuthors/Fisher2017/fisher2017/R Data/"
files <- list.files(folder)
n_files <- length(files)

datlist <- list()

for (i in seq(length(files))) {
  load(paste0(folder, files[i]))
  datlist[[i]] <- data
}

# Turn this into a single data frame
l_newformat <- list()
for(i in 1:n_files) l_newformat[[i]] <- cbind(i, datlist[[i]][, 3:27])


# ------------------------------------------------------------------
# ------------- Process --------------------------------------------
# ------------------------------------------------------------------

# Collapse
data <- do.call(rbind, l_newformat)
colnames(data) <- c("subj_id", colnames(datlist[[1]][, 3:27]))
head(data)


# ------------------------------------------------------------------
# ------------- Save --------------------------------------------
# ------------------------------------------------------------------

# Within
saveRDS(data, file = "DataClean/Fisher2017/data_Fisher2017.RDS")

# Between
# manually enter table of between-person characteristics into R
# Based on Table 1 of Fisher et al 2017



between <- rbind(
  c( 23, 27 ),
  c( 16,15 ),
  c( 16,33 ),
  c( 13,13 ),
  c( 11,17 ),
  c( 19,15 ),
  c( 17,9 ),
  c( 22,22 ),
  c( 9,13 ),
  c( 14,19 ),
  c( 10,12 ),
  c( 10,10 ),
  c( 15,16 ),
  c( 8,7 ),
  c( 15,14 ),
  c( 8,14 ),
  c( 12,23 ),
  c( 21,41 ),
  c( 14,17 ),
  c( 11,14 ),
  c( 15,13 ),
  c( 12,10 ),
  c( 18,23 ),
  c( 7,14 ),
  c( 18,15 ),
  c( 4,15 ),
  c( 18,19 ),
  c( 12,18 ),
  c( 9,13 ),
  c( 16,15 ),
  c( 14,12 ),
  c( 21,30 ),
  c( 13,11 ),
  c( 16,16 ),
  c( 13,15 ),
  c( 10,11 ),
  c( 18,20 ),
  c( 12,16 ),
  c( 17,23 ),
  c( 17,14 )
)

id <- 1:40

out <- cbind(id, between)
colnames(out) <- c("id", "dep","anx")

saveRDS(out, "DataClean/Fisher2017/data_Fisher2017_between.RDS")




