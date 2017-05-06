library(data.table)
library(epiR)
library(XLConnect)
# dat0 <- data.table(
#   year = 1987:2013,
#   nollAringar = round(rnorm(27, 50000, 300)), 
#   rakit = round(rnorm(27, 400, 30)),
#   barnmissh = round(rnorm(27, 20, 3))
# )
rm(list=ls())
path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
load(file = file.path(path, "Output", "DMP", "DMP_08_per100000.rdata"))
load(file = file.path(path, "Output", "DMP", "DMP_08_nollar.rdata"))
#------------------------- confidence intervals for all data -------------------
# Format data so that we get n total and number of cases for each year
dat0 <- out
dat0$n <- NULL
dat1 <- merge(dat0, noll0, by = "year", all.x=T)
dat1 <- subset(dat1, !is.na(n))

per100k <- function(x){
  out <- get(x)/dat1$n * 100000
  return(out)
}

# chose diagnosis vars
setnames(dat1, names(dat1), paste(names(dat1), "_n", sep = ""))
setnames(dat1, c("n_n", "year_n"), c("n", "year"))

diagnoses <- names(dat1)[!(names(dat1) %in% c("year", "n"))]
#ratevars <- paste("rate_", diagnoses, sep = "")

#best data.table method for assigning new cols using set
alloc.col(dat1,1000)

#store raw data for other cuts
data <- copy(dat1)

for(diag in diagnoses) {
  set(dat1, j=paste(diag,"rate", sep = "_"), value=dat1[[diag]]/dat1[["n"]]*100000)
}

# calculate confidence interval
#dat1[,lapply(.SD, function(x) paste(x, n)), by = c("n","year")]
dat2 <- dat1[,lapply(.SD, 
             function(x) paste(round(epi.conf(t(matrix(c(x, n))), ctype = "prop.single")[c("lower","upper")]*100000), 
                               collapse="-")),
             by = c("n","year")
     ]

dat2 <- dat2[,grep("rate", names(dat2), invert= TRUE),with=F]
setnames(dat2, names(dat2), paste(names(dat2), "_xCI95", sep = ""))
setnames(dat2, c("n_xCI95", "year_xCI95"), c("n", "year"))
dat2$n <- NULL

setkey(dat1, year)
setkey(dat2, year)


dat3 <- merge(dat1,dat2)
setnames(dat3, names(dat3), gsub("_n_", "_", names(dat3)))
setcolorder(dat3, c("year", "n", sort(grep("year|^n$", names(dat3), value=TRUE, invert = TRUE))))
#-------------------------------------------------------------------------------
writeWorksheetToFile(dat3, file = file.path(path,"Output","DMP","DMP_10_per100000.xlsx"), sheet= "summaries")

#--------------------------- conf int for moving averages ----------------------
# glidande medelvärden?
library(zoo)

datMA0 <- data #alla har ändelse _n
alloc.col(datMA0, 1000)

rlSum <- function(x){
  out <- rollsum(x, 3, align="center", fill = c(0,0,0))#/
    #rollsum(nn, 3, align="center", fill = c(0,0,0))
  #out <- paste(x, nn, sep = "/")
  return(out)
}

#diagvars <- c("rakit", "barnmisshandel")
diagnoses
#meanvars <- paste("mean", diagvars, sep="_")

#calculate sums per year creating the same kind of data as in step 1.
for(diag in c("n", diagnoses)){
  set(datMA0, j=paste(diag,"sum", sep = "_"), value=rlSum(datMA0[[diag]]))#, datMA0[["n"]]))
}

# calculate rates
for(diag in paste(diagnoses, "sum", sep = "_")) {
  set(datMA0, j=paste(gsub("_sum", "", diag),"rate", sep = "_"), value=datMA0[[diag]]/datMA0[["n_sum"]]*100000)
}

# calculate confidence interval
# This should be done for the sums calculatedin the last step
# hence we should only keep variables with suffix "sum" for datMA1
datMA1 <- datMA0[,lapply(.SD, 
                     function(x) paste(round(epi.conf(t(matrix(c(x, n_sum))), ctype = "prop.single")[c("lower","upper")]*100000), 
                                       collapse="-")),
             by = c("n","year")
             ]

datMA1 <- datMA1[,grep("_sum|year", names(datMA1), value=TRUE),with=F]
setnames(datMA1, names(datMA1), paste(gsub("_n_sum|_sum", "", names(datMA1)), "_xCI95", sep = ""))
setnames(datMA1, c("n_xCI95", "year_xCI95"), c("n", "year"))
datMA1$n <- NULL

setkey(datMA0, year)
setkey(datMA1, year)

datMA2 <- merge(datMA0,datMA1)
setnames(datMA2, names(datMA2), gsub("_n_", "_", names(datMA2)))
setcolorder(datMA2, c("year", "n", sort(grep("year|^n$", names(datMA2), value=TRUE, invert = TRUE))))

writeWorksheetToFile(datMA2, file = file.path(path,"Output","DMP","DMP_10_Glidandeper100000.xlsx"), sheet= "summaries")
#---------------------------- conf int för grupper -----------------------------

datGRP0 <- data


ll <- split(datGRP0$year, rep(0:8,each=3))
levels <- sapply(1:9, function(x) paste(ll[[x]][c(1,3)], collapse="-"))

datGRP0$group <- rep(levels, each=3)
datGRP0$year <- NULL

datGRP0 <- datGRP0[,lapply(.SD, sum),by = "group"]

alloc.col(datGRP0, 1000)
for(diag in diagnoses) {
  set(datGRP0, j=paste(diag,"rate", sep = "_"), value=datGRP0[[diag]]/datGRP0[["n"]]*100000)
}

# calculate confidence interval
#datGRP0[,lapply(.SD, function(x) paste(x, n)), by = c("n","year")]
datGRP1 <- datGRP0[,lapply(.SD, 
                     function(x) paste(round(epi.conf(t(matrix(c(x, n))), ctype = "prop.single")[c("lower","upper")]*100000), 
                                       collapse="-")),
             by = c("n","group")
             ]

datGRP1 <- datGRP1[,grep("rate", names(datGRP1), invert= TRUE),with=F]
setnames(datGRP1, names(datGRP1), paste(names(datGRP1), "_xCI95", sep = ""))
setnames(datGRP1, c("n_xCI95", "group_xCI95"), c("n", "group"))
datGRP1$n <- NULL

setkey(datGRP0, group)
setkey(datGRP1, group)


datGRP2 <- merge(datGRP0,datGRP1)
setnames(datGRP2, names(datGRP2), gsub("_n_", "_", names(datGRP2)))
setcolorder(datGRP2, c("group", "n", sort(grep("group|^n$", names(datGRP2), value=TRUE, invert = TRUE))))
writeWorksheetToFile(datGRP2, file = file.path(path,"Output","DMP","DMP_10_3årsgrupperper100000.xlsx"), sheet= "summaries")

#------------------------------ conf int för minigrupper -----------------------

datGrpMin0 <- data
datGrpMin0 <- subset(datGrpMin0, year %in% 1997:2013)
datGrpMin0$group <- ifelse(datGrpMin0$year %in% 1997:2007,"1997-2007","2008-2013")
datGrpMin0$year <- NULL

datGrpMin0 <- datGrpMin0[,lapply(.SD, sum),by = "group"]

alloc.col(datGrpMin0, 1000)
for(diag in diagnoses) {
  set(datGrpMin0, j=paste(diag,"rate", sep = "_"), value=datGrpMin0[[diag]]/datGrpMin0[["n"]]*100000)
}

# calculate confidence interval
#datGrpMin0[,lapply(.SD, function(x) paste(x, n)), by = c("n","year")]
datGrpMin1 <- datGrpMin0[,lapply(.SD, 
                           function(x) paste(round(epi.conf(t(matrix(c(x, n))), ctype = "prop.single")[c("lower","upper")]*100000), 
                                             collapse="-")),
                   by = c("n","group")
                   ]

datGrpMin1 <- datGrpMin1[,grep("rate", names(datGrpMin1), invert= TRUE),with=F]
setnames(datGrpMin1, names(datGrpMin1), paste(names(datGrpMin1), "_xCI95", sep = ""))
setnames(datGrpMin1, c("n_xCI95", "group_xCI95"), c("n", "group"))
datGrpMin1$n <- NULL

setkey(datGrpMin0, group)
setkey(datGrpMin1, group)

datGrpMin2 <- merge(datGrpMin0,datGrpMin1)
setnames(datGrpMin2, names(datGrpMin2), gsub("_n_", "_", names(datGrpMin2)))
setcolorder(datGrpMin2, c("group", "n", sort(grep("group|^n$", names(datGrpMin2), value=TRUE, invert = TRUE))))
writeWorksheetToFile(datGrpMin2, file = file.path(path,"Output","DMP","DMP_10_2grupperper100000.xlsx"), sheet= "summaries")
