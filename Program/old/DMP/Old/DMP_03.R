# SETUP #########################################################
# NAME      : UH
#
# R-version : 3.0.2
#              
# PRODUCES  : multivariat analys
# 
# STATUS    : DRAFT
#
# AUTHOR    : Per Wikman
# CREATED   : 2015-04-08
# NOTES     : 
# 
rm(list=objects())

# Todays date
datet <- format(Sys.time(), "%x")
fileDate <- gsub("-", "", datet)

# Rversion
Rversion <- paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor, sep=".")

rm(list=ls())
library(data.table)
library(ggplot2)
library(scales)
library(rtf)

path = "K:/Academy/UU/UU__4423-2 Högberg Morbarn"
setwd(path)

#Source
#-------------------------------------------------------------------------------
#K:\Academy\UU\UU__4423-2 Högberg Morbarn\Indata\Ny databas 2015
folders = file.path(path,"Indata","Ny databas 2015")

#get all filenames
sources.files  <- list.files(path=folders,
                             recursive=T,
                             pattern="\\.CSV",
                             full.names=T)

#get filenames per folder
gdata = list.files(path=file.path(folders,"Grunddata"),
           recursive=T,
           pattern="\\.CSV",
           full.names=T)

mfr1 = list.files(path=file.path(folders,"mfr_1"),
                   recursive=T,
                   pattern="\\.CSV",
                   full.names=T)

#mfr2 = list.files(path=file.path(folders,"mfr_2"),
#                  recursive=T,
#                  pattern="\\.CSV",
#                  full.names=T)

#the following are big
mor = list.files(path=file.path(folders,"mor"),
                  recursive=T,
                  pattern="\\.CSV",
                  full.names=T)

sos_barn = list.files(path=file.path(folders,"SOS_BARN"),
                  recursive=T,
                  pattern="\\.CSV",
                  full.names=T)

#-------------------------------- read in --------------------------------------

#mfr easy 
#on top of this both are the same
mfr_ett = read.csv(mfr1,stringsAsFactors=F)
#mfr_tva = read.csv(mfr2,stringsAsFactors=F)
#identical(mfr_ett,mfr_tva)

#-------------------------------- grunddata ------------------------------------

srcer = function(x){ y = data.table(read.csv(x, stringsAsFactors=F)); y$srcfile = gsub("\\.CSV","",gsub(" ","",gsub("^.*/","",x)));return(y)}


descFunction = function(x){
   #prints how many unique values and how many NAs in any numeric vector "x"
   #and returns a dataframe
   
   percentage_na = sum(is.na(x))/length(x)
   unique_values = sum(table(unique(x)))
   
   return(data.frame("unique_values"=unique_values,"precentage_na"=percentage_na)) 
}



#----------------------------------- barn ---------------------------------------
ll_barn = lapply(sos_barn,srcer)

#print some infor for orientation
sos_barn
lapply(ll_barn, function(x) dim(x))
lapply(ll_barn, function(x) nrow(x)/sum(table(unique(x$lpnr))))

library(plyr)

#bind together all causes of death
dod_barn = rbind.fill(ll_barn[1:2])

#rbind lakemedel
lakemedel_barn = rbind.fill(ll_barn[5:15])

#bind care datasets
vard_barn = rbind.fill(ll_barn[16:18])

#read in final dataset containing additional id info
inf_barn = ll_barn[[3]]

#change format of lakemedel column 
#one na that is ok for now
lakemedel_barn$lpnr = as.numeric(as.character(lakemedel_barn$lpnr))

#stack files
prt1_barn = rbind.fill(dod_barn,lakemedel_barn)
prt2_barn = rbind.fill(prt1_barn,vard_barn)


#prt3_barn = merge(prt2_barn,inf_barn, by="lpnr", all.y=T)

#check how many ids
sum(table(unique(prt2_barn$lpnr)))


save(prt2_barn,file=file.path(path,"Output","DMP","Databas 2015","sos_barn.Rdata"))

#descriptives
llbarn = lapply(prt2_barn[,], descFunction)

outbarn = do.call("rbind",llbarn)
save(outbarn,file=file.path(path,"Output","DMP","Databas 2015","barn.Rdata"))

#-------------------------------------------------------------------------------



