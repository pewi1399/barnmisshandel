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
# CREATED   : 2015-06-01
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
source("K:/Academy/UU/UU__5185 Barnmisshandel/Program/DMP/functions/func.R", encoding="utf-8")

#grunddata
load(file.path(path,"Output","DMP","DMP_02_grunddata.Rdata"))
#load(file.path(path,"Output","DMP","Databas 2015","sos_barn.Rdata"))

#prt5 från DMP02 är grunddata
grund = prt5
#-------------------------------------------------------------------------------

#------------------------------ Tabellera -------------------------------------- 
tabvars = names(grund)[!(names(grund) %in% namesstart)]
names(grund)
grund0 = data.table(grund[,c("lpnr","ALDER","mor","ageClassWeeks","barn",tabvars,"INDATUM","fod_man_dag")])

#grund1 = subset(grund,!is.na(ALDER))
grund1 = data.table(subset(grund0,barn ==1))

#-------------------------------- Tabell 3 -------------------------------------

grund1$fdag = as.Date(as.character(grund1$fod_man_dag), "%Y%m%d")
grund1$diagnosdag = as.Date(grund1$INDATUM)


#grund1 = subset(grund1,is.na(INDATUM))
grund1$diagnoseAge = difftime(grund1$diagnosdag,grund1$fdag, units = "days")

#age classes for table 1
grund1$ageClass = ifelse(grund1$diagnoseAge<30.5*3,"< 1 månad",
                         ifelse(grund1$diagnoseAge<30.5*9,"1-3 månader","> 3 månader"))

#birthyear
grund1$birthYear = as.numeric(format(grund1$fdag, "%Y"))

#birthclass
grund1$birthClass = ifelse(grund1$birthYear<1980,"<1980",
                           ifelse(grund1$birthYear<=1989,"1980-1989",
                                  ifelse(grund1$birthYear<=1999,"1990-1999",
                                         ifelse(grund1$birthYear>=2000,"2000 - 2013+",NA))))

#order factors
grund1$ageClass = as.factor(grund1$ageClass)
grund1$ageClass = factor(grund1$ageClass,levels(grund1$ageClass)[c(1,3,2)])

grund1$birthClass = as.factor(grund1$birthClass)
#grund1$birthClass = factor(grund1$birthClass,levels(grund1$birthClass)[c(1,3,2)])


#recreate table 3
t3 = grund1[,list(
   rakit = ifelse(sum(aktivRakit)>0,1,0),
   barnmisshandel = ifelse(sum(misstankeOmBarnMisshdel)>0,1,0),
   ageClass = unique(ageClass),
   birthClass = unique(birthClass)
),
by=c("lpnr")]


tab3 = t3[,list(
   n = sum(table(unique(lpnr))),
   rakit = sum(rakit),
   barnmisshandel = sum(barnmisshandel)
),
by=c("ageClass","birthClass")]


tab3[,n:= sum(n),by = "birthClass"]
tab3 = na.omit(tab3)

#order table
setkey(tab3,birthClass,ageClass)

#calculate incidence
tab3[,rakit_per_100k:=rakit*(100000/n),]
tab3[,misshandel_per_100k:=barnmisshandel*(100000/n),]
#-------------------------------------------------------------------------------

#-------------------------------- Controls ------------------------------------- 

#Se tetsprogram
#subRakit = subset(tst, rakit==max(tst$rakit))
#subMissh = subset(tst, barnmisshandel==max(tst$barnmisshandel))

#-------------------------------------------------------------------------------

#---------------------------- calculate frequencies ----------------------------
#this part takes care of stacked diagnoses per lpnr See DMP02
for(i in 1:length(tabvars)) {
   print(i)
   grund1[ ,
          paste(tabvars[i],"gg",sep="") := ifelse(sum(get(tabvars[i]),na.rm=T)>0,1,0),
          by="lpnr"]
}

#since table vars are set for each id now we may safely drop dupes
grund2 = grund1[!duplicated(lpnr),,]


grund3 = as.data.frame(grund2)


#change to factor for plotting
grund3[,tabvars] = lapply(grund3[,tabvars],function(x) as.factor(x))

#relevel = function(x){
#   levels(x) = c(levels(x), "1")[!duplicated(c(levels(x), "1"))]
#}

#grund3[,tabvars] = lapply(grund3[,tabvars],relevel)


table(grund2[,tabvars[1],with=F])

#str(grund3[,tabvars])

rtffile= RTF(file.path(path,"Output","TLP","Tabell1_DRAFT.doc"))


tab2(grund3,vars = tabvars,printcsv=F,groupvar="birthClass",print=T)
done(rtffile)

rtffile= RTF(file.path(path,"Output","TLP","Tabell3_DRAFT.doc"))


tab3$rakit_per_100k = round(tab3$rakit_per_100k,1)
tab3$misshandel_per_100k = round(tab3$misshandel_per_100k,1)

addTable(rtffile,tab3, row.names=F)

done(rtffile)
#-------------------------------------------------------------------------------



#----------------------------- PAR tabell --------------------------------------
tmp = grund1 

vars = names(grund1)[grepl("^P.*d1$",names(grund1))]

#for(i in 1:length(vars)){

#   tmp[,vars=list(lapply(),by=c("lpnr","ageClassWeeks")]

#}

t4 = tmp[,list(
   Pclavikel = ifelse(sum(Pclavikel_d1)>0,1,0),
   Plårben = ifelse(sum(Plårben_d1)>0,1,0),
   Pöverarm = ifelse(sum(Pöverarm_d1)>0,1,0),
   Pskallfraktur = ifelse(sum(Pskallfraktur_d1)>0,1,0),
   Pövrigfraktur = ifelse(sum(PövrigFraktur_d1)>0,1,0),
   Psubdural = ifelse(sum(Psubdural_d1)>0,1,0),
   Pintrakraniellskada = ifelse(sum(Pintrakraniellskada_d1)>0,1,0),
   PintrakraniellskadaIckeTrauma = ifelse(sum(PintrakraniellIckeTrauma_d1)>0,1,0)
),
by=c("lpnr","ageClassWeeks")]


t5 = t4[,list(
   Pclavikel = sum(Pclavikel),
   Plårben = sum(Plårben),
   Pöverarm = sum(Pöverarm),
   Pskallfraktur = sum(Pskallfraktur),
   Pövrigfraktur = sum(Pövrigfraktur),
   Psubdural = sum(Psubdural),
   Pintrakraniellskada = sum(Pintrakraniellskada),
   PintrakraniellskadaIckeTrauma = sum(PintrakraniellskadaIckeTrauma)
),
by=c("ageClassWeeks")]

rtffile= RTF(file.path(path,"Output","DMP","Tabell3_diagnoser_PAR_DRAFT.doc"))
addTable(rtffile,t5)
done(rtffile)



#table(grund3$kalciumRubbningarICD10) 

