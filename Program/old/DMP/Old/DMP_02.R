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

path = "K:/Academy/UU/UU__5185 Barnmisshandel"
setwd(path)

#Source
#-------------------------------------------------------------------------------
#K:\Academy\UU\UU__4423-2 Högberg Morbarn\Indata\Ny databas 2015
folders = file.path(path,"Indata")

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

#corece list elements to datframes and add sourcetag
ll_gdata = lapply(gdata,srcer)

#print fraction of unique ids per file
lapply(ll_gdata, function(x) nrow(x)/sum(table(unique(x$lpnr))))

#Slå ihop sluten öppen och dagkirurgi
prt1 = rbind(ll_gdata[[4]],ll_gdata[[5]],fill=T)
prt2 = rbind(prt1, ll_gdata[[6]],fill=T)

#all these are unique
dod = ll_gdata[[1]]

#add all 
prt3 = merge(prt2,dod, by="lpnr", all=T)

#reformat variable names according to rule:
#RULE: names with suffix x are originals 
#names with suffix y comes from DOD_6113 and should be named as such
names(prt3) = gsub("\\.x$","",names(prt3))
names(prt3) = gsub("\\.y$","\\.DOD_6113",names(prt3))

#get info file and remove overlapping var
info = ll_gdata[[2]]
info$srcfile = NULL

#full merge retaining all row
prt4 = merge(prt3, info, by="lpnr", all.y=T)

#read in final file
mfr = ll_gdata[[3]]

mfr$lpnr_BARN
#Run merge on children only
prt5 = merge(data.frame(prt4), data.frame(mfr), by.x="lpnr",by.y="lpnr_BARN", all.x=F)#Ändrat till false

#reformat variable names according to rule:
#RULE: names with suffix x are originals 
#names with suffix y comes from MFR and should be named as such
names(prt5) = gsub("\\.x$","",names(prt5))
names(prt5) = gsub("\\.y$","\\.MFR",names(prt5))

#check how many ids
sum(table(unique(prt5[prt5$barn==1,"lpnr"])))
sum(table(unique(prt5[prt5$mor==1,"lpnr"])))

#Display descriptives for error check
descFunction = function(x){
   #prints how many unique values and how many NAs in any numeric vector "x"
   #and returns a dataframe
   
   percentage_na = sum(is.na(x))/length(x)
   unique_values = sum(table(unique(x)))
   
   return(data.frame("unique_values"=unique_values,"precentage_na"=percentage_na)) 
}

llgdata = lapply(prt5[,], descFunction)

outgrunddata = do.call("rbind",llgdata)

save(outgrunddata,file=file.path(path,"Output","DMP","grund.Rdata"))
save(prt5,file=file.path(path,"Output","DMP","DMP_02_grunddata.Rdata"))

#-------------------------------------------------------------------------------

#----------------------------------- Mor ---------------------------------------
ll_mor = lapply(mor,srcer)

#print some infor for orientation
mor
lapply(ll_mor, function(x) dim(x))
lapply(ll_mor, function(x) nrow(x)/sum(table(unique(x$lpnr))))

library(plyr)

#bind together all causes of death
dod_mor = rbind.fill(ll_mor[1:2])

#rbind lakemedel
lakemedel_mor = rbind.fill(ll_mor[5:15])

#bind care datasets
vard_mor = rbind.fill(ll_mor[16:18])

#read in final dataset containing additional id info
inf_mor = ll_mor[[3]]

#change format of lakemedel column 
#one na that is ok for now
lakemedel_mor$lpnr = as.numeric(as.character(lakemedel_mor$lpnr))

#stack files
prt1_mor = rbind.fill(dod_mor,lakemedel_mor)
prt2_mor = rbind.fill(prt1_mor,vard_mor)


prt3_mor = merge(prt2_mor,inf_mor, by="lpnr", all.y=T)

#check how many ids
sum(table(unique(prt3_mor$lpnr)))

#
llmor = lapply(prt3_mor[,], descFunction)

outmor = do.call("rbind",llmor)

save(prt3_mor,file=file.path(path,"Output","DMP","DMP_02_mor.Rdata"))
save(outmor,file=file.path(path,"Output","DMP","mor.Rdata"))

#-------------------------------------------------------------------------------




