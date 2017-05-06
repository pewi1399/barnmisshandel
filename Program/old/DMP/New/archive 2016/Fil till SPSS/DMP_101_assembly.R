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
#library(XLConnect)
library(scales)
library(reshape2)
#library(rtf)
library(plyr)
library(haven)

path = "/home/per/Polycarp/KBH/P101_barnmisshandel"
setwd(path)
#-------------------------------------------------------------------------------

load(file.path(path, "Output","DMP","DMP_01_keyfile.Rdata")) #keyfile
load(file = file.path(path,"Output","DMP","DMP_100_medicine_parents.Rdata")) #lak7
load(file = file.path(path,"Output","DMP","DMP_100_diagnoser_parents.Rdata")) #pdia7
load(file = file.path(path,"Output","DMP","DMP_100_perinatala_mfr.Rdata")) #mfr3
load(file = file.path(path,"Output","DMP","DMP_100_barndiagnoser.Rdata")) #dia5

mfr_1_0 = read.csv(file.path(path,"Indata","mfr_1","MFR                             .CSV"), stringsAsFactors=F)

#add kompletteringsdata
mfr_kompl = read.csv(file.path(path,"Indata","Komplettering","mor","MFR                             .CSV"),stringsAsFactors=F)
mfr_kompl$kompletterat_fall = "Aug"

mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2a$kompletterat_fall = "Oct"
mfr_kompl2b$kompletterat_fall = "Oct"

mfr_1_0 = rbind.fill(mfr_1_0,mfr_kompl,mfr_kompl2a,mfr_kompl2b)

mfr_1_1 = mfr_1_0[!duplicated(mfr_1_0$lpnr_BARN),]

invars = read.csv2("/home/per/Polycarp/KBH/P101_barnmisshandel/Transfer/Sticka levererad 2015-06-04/variabler till SPSS 150603.csv")
excl = names(mfr_1_1)[names(mfr_1_1) %in% invars$vars]

mfr_1_2 = mfr_1_1[,c("lpnr_BARN",excl,"kompletterat_fall")]
mfr_1_2$lpnr = mfr_1_2$lpnr_BARN
mfr_1_2$lpnr_BARN = NULL
#-------------------------------------------------------------------------------

#--------------------- keyfile contains all relevant data ----------------------

#-------------------------- merge on all kid variables -------------------------

mfr3$lpnr = mfr3$lpnr_BARN
mfr3$lpnr_BARN = NULL
setkey(mfr3,"lpnr")
setkey(dia5,"lpnr")


dat0a = merge(dia5,mfr3,all=T)
sum(table(unique(dat0a$lpnr))) == nrow(dat0a)
#sum(table(unique(dat0$lpnr)))
#table(dat0$lpnr %in% keyfile$lpnr_BARN)
#-------------------------------------------------------------------------------

#------------------------------ join parent diagnoses --------------------------

#one lopnr is strange
#which(is.na(as.numeric(as.character(lak7$lpnr))))
#lak7[13859,]
#diagnoses per kid are merged on  parent diagnoses by lpnr for kids
lak7$lpnr = as.numeric(lak7$lpnr)

lak7 = subset(lak7, !is.na(lak7$lpnr))

setkey(lak7,"lpnr")

#pdia7$lpnr = pdia7$lpnr_BARN 
#pdia7$lpnr_BARN = NULL

setkey(pdia7,"lpnr")


dat0b = merge(pdia7,lak7, all=T)
sum(table(unique(dat0b$lpnr))) == nrow(dat0b)
#-------------------------------------------------------------------------------

#merge kids with keyfile 

keyfile = data.table(keyfile)

setnames(keyfile,names(keyfile),c("lpnr_b","lpnr_m"))
setnames(dat0a,"lpnr","lpnr_b")
#setnames(dat0b,"lpnr","lpnr_m")

setkey(dat0a,"lpnr_b")
setkey(keyfile,"lpnr_b")

dat1 = dat0a[keyfile]

setkey(dat1, "lpnr_b")
setkey(dat0b,"lpnr")

dat2 = dat0b[dat1]
#-------------------------------------------------------------------------------
#---------------------------------- Add mfr variables --------------------------
mfr_1_2 = data.table(mfr_1_2)
setkey(mfr_1_2,"lpnr")
setkey(dat2,"lpnr")
#mfr_1_2$lpnr

table(dat2$lpnr %in% mfr_1_2$lpnr)
sum(table(unique(dat2$lpnr)))
sum(table(unique(mfr_1_2$lpnr)))

dat3 = dat2[mfr_1_2]

#dat3$lpnr = dat3$lpnr_b
names(dat3)
dat3$lpnr_mor = NULL
#dat3$lpnr_b = NULL
dat3$lpnr_BARN = NULL

dat4 = dat3[,c("lpnr","lpnr_m",names(dat3)[!grepl("lpnr",names(dat3))]),with=F]

countries = structure(c(
   "Sverige",
   rep("Skandinavien",3),
   rep("Nordeuropa",2),
   rep("Östeuropa",3),
   rep("Sydeuropa",6), 
   rep("Latinamerika",6),
   rep("Nordafrika",2),
   rep("Subsahariska afrika",5),
   rep("Västasien",5),
   rep("Sydasien",4),
   rep("Asien",5)
   ), 
   names=
      c("SVERIGE",
   "DANMARK","FINLAND","NORGE",
   "STORBRITANNIEN OCH NORDIRLAND","TYSKLAND",
   "POLEN","RUMÄNIEN","LITAUEN",
   "JUGOSLAVIEN","KOSOVO","KROATIEN","SERBIEN","MONTENEGRO","SERBIEN OCH MONTENEGRO",
   "ARGENTINA","BOLIVIA","CHILE","GUYANA","JAMAICA","KUBA",
   "MAROCKO","TUNISIEN",
   "SOMALIA","GHANA", "KONGO","NIGERIA","UGANDA",
   "IRAK","IRAN","SYRIEN","TURKIET","VÄSTBANKEN",
   "INDIEN","SRI LANKA","THAILAND","VIETNAM",
   "AFGHANISTAN","TURKMENISTAN","FILIPPINERNA","KINA","PAKISTAN")
   )

dat4$MFODLAND_REGION = countries[dat4$MFODLAND]

#-------------------------------------------------------------------------------
#----------------------------------- Steg 2 ------------------------------------
#-------------------------------------------------------------------------------

load(file = file.path(path,"Output","DMP","DMP_100_dodBarn.Rdata"))#dodBarn0
load(file = file.path(path,"Output","DMP","DMP_100_snq.Rdata"))#DMP_100_snq
load(file = file.path(path,"Output","DMP","DMP_100_suicidMord_far_mor.Rdata" ))#fDod7
load(file = file.path(path,"Output","DMP","DMP_100_barndiagnoserMisshandel.Rdata" ))#barnDiag8

#----------------------------------- add dodbarn -------------------------------

table(names(dat4) %in% names(dodBarn0))
names(dat4) = gsub("\xe4","ä",names(dat4))
names(dat4) = gsub("\xf6","ö",names(dat4))
names(dat4) = gsub("\U3e35653c","å",names(dat4))
names(dat4) = gsub("\xd6","Ö",names(dat4))

dat4 = data.frame(dat4)

#ok
#sum(table(unique(dodBarn0$lpnr)))

dat5 = merge(dat4,dodBarn0, by="lpnr", all.x=T)
#-------------------------------------------------------------------------------

#---------------------- add child diagnoses conditioned on misshdel ------------
names(dat5)[names(dat5) %in% names(barnDiag8)]
barnDiag8$lan_text = NULL

#ok
#sum(table(unique(barnDiag8$lpnr)))
dat6 = merge(dat5,barnDiag8, by="lpnr", all.x=T)
#-------------------------------------------------------------------------------

#---------------------------- Add parent suicid mord ---------------------------
names(dat6)[names(dat6) %in% names(fDod7)]

dat7 = merge(dat6,fDod7, by.x="lpnr", by.y = "lpnr_BARN", all.x=T)

#---------------------------------- add snq variables --------------------------

dat8 = merge(dat7,DMP_100_snq, by="lpnr", all.x=T)

sum(table(unique(dat4$lpnr)))
#sum(table(unique(snq1$lpnr_barn)))
#table(dat7$lpnr_m == dat7$lpnr_mor)
dat8$lpnr_mor = dat8$lpnr_m 
dat8$lpnr_m  = NULL

#----------------- derive age in months variable at barnmisshandel -------------


#dat8$BFODDAT
#dat8$INDATUM
#table(is.na(dat8$DatumFörstaBarnMisshandel))
#names(dat8)
#as.Date(as.character(dat8$BFODDAT),"%Y%m%d")
#class(as.characterdat8$BFODDAT)
#class(dat8$INDATUM)

dat8$ålderVidMisshandelMånader = round(difftime(dat8$INDATUM,as.Date(as.character(dat8$BFODDAT),"%Y%m%d"), units = "days")/(365.24/12),1)


#-------------------------- Add fathers birthdate ------------------------------
far_lpnr = read.csv2("/home/per/Polycarp/KBH/P101_barnmisshandel/Indata/far/SOS/info_lpnr.csv")
#far_lpnr = far_lpnr[!duplicated(far_lpnr$lpnr),]

far_lpnr$lpnr_farX = far_lpnr$lpnr

far_lpnr$FARFODDAG = far_lpnr$fod_man_dag 

far_lpnr$lpnr = far_lpnr$LopNrBarn


dat9 = merge(dat8,far_lpnr[,c("lpnr","lpnr_farX","FARFODDAG")], by = "lpnr", all.x = T)

#table(is.na(dat9$FARFODDAG))
#table(dat9$lpnr_far==dat9$lpnr_farX)
dat9$lpnr_far = ifelse(is.na(dat9$lpnr_far),dat9$lpnr_farX,dat9$lpnr_farX)
dat9$lpnr_farX=NULL
#table(is.na(dat8$ålderVidMisshandel))

tmp = dat9[dat9$lpnr ==2,c("lpnr","lpnr_far","lpnr_mor","FARFODDAG")]
#-------------------------------------------------------------------------------

#----------- create indicator for late registrated data Oct 2015 ---------------

quantile(dat9$lpnr)

#-------------------------------------------------------------------------------

#-------------------------------- Push to csv ----------------------------------
save(dat9,file =  file.path(path,"Output","DMP","DMP_101_assembly2015.Rdata"))
write.csv2(dat9, file.path(path,"Output","DMP","DMP_101_assembly2015.csv"),na="",row.names=F)

names(dat9) = gsub("Å","A",names(dat9))
names(dat9) = gsub("å","a",names(dat9))
names(dat9) = gsub("Ä","A",names(dat9))
names(dat9) = gsub("ä","a",names(dat9))
names(dat9) = gsub("Ö","O",names(dat9))
names(dat9) = gsub("ö","o",names(dat9))

recoder = function(x){
   
   if(class(x)=="factor" | class(x)=="Date" ){
      
      y = as.character(x)
      
   }else if(class(x)=="integer"| class(x)=="logical"){
      
      y = as.numeric(x)
      
   }else{
     
     y = x
     
   }
   
   return(y)
   
}

#dat9[is.na(dat9)] = 999

names(dat9) = gsub("\xe4","a",names(dat9))
names(dat9) = gsub("\xf6","o",names(dat9))
names(dat9) = gsub("\U3e35653c","a",names(dat9))
names(dat9) = gsub("\xd6","O",names(dat9))
names(dat9) = gsub("\xc5r","A",names(dat9))

out <- dat9

rm(dat1);rm(dat2);rm(dat3);rm(dat4);rm(dat5);rm(dat6);rm(dat7);rm(dat8);gc()

out[,] = lapply(out[,], recoder)


write_dta(out, file.path(path,"Output","DMP","Analysdata20151221.dta"))
saveRDS(out, file.path(path,"Output","DMP","Analysdata.rds"))
#write_sav(dat9, file.path(path,"Output","DMP","Analysdata20151102.sav"))
#-------------------------------------------------------------------------------
out1 <- out[,grep("diagnosInnan|BFODDAT", names(out), value=T)]
out1$year <- substr(out1$i.BFODDAT,1,4)
out1$BFODDAT <- NULL
out1$i.BFODDAT <- NULL

save(out1, file = file.path(path,"Output","DMP","diagnosinnan.rdata"))
table(dat9$barnMisshandel_d1)

