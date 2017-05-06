rm(list=ls())
path = "K:/Academy/UU/UU__5185 Barnmisshandel"
setwd(path)

library(data.table)
library(haven)

load(file =  file.path(path,"Output","DMP","DMP_101_assembly2015.Rdata"))

names(dat9) = gsub("Å","A",names(dat9))
names(dat9) = gsub("å","a",names(dat9))
names(dat9) = gsub("Ä","A",names(dat9))
names(dat9) = gsub("ä","a",names(dat9))
names(dat9) = gsub("Ö","O",names(dat9))
names(dat9) = gsub("ö","o",names(dat9))

dat9$alderVidMisshandelManaderClass = cut(as.numeric(dat9$alderVidMisshandelManader),breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))
#table(dat9$alderVidMisshandelManader,dat9$alderVidMisshandelManaderClass)

dat9$alderVidMisshandelManaderClass = as.character(dat9$alderVidMisshandelManaderClass)
dat9$alderVidMisshandelManaderClass[is.na(dat9$alderVidMisshandelManaderClass)] = "Ej misshandel"

dat9$alderVidMisshandelManaderClass = factor(dat9$alderVidMisshandelManaderClass,
                                              levels = sort(unique(dat9$alderVidMisshandelManaderClass))[c(1,2,5,6,7,8,9,10,11,12,3,4,13)])

#--------------------------------- Tabell 1 ------------------------------------
tab1Vars = c("barnMisshandel_d1", 
          "UtrObsMissh_d1",
          "DiagnosMissh_d1",
          "frakturRevben_d1",
          "frakturLarben_d1",
          "frakturUnderben_d1",
          "ovrigSkallskada_d1",
          "barnmisshandelssyndromUNS",
          "syndromBarnMisshdel",
          "frakturOverUnderArm_d1",
          "skallFraktur_d1",
          "retinalBlodning_d1",
          "subduralIT_d1",
          "subduralTraum_d1",
          "svt_d1",
          "ogi_d1",
          "spadbarnskolik_d1",
          "frakturRorben_d2",
          "subduralBlodning_d2",
          "frakturLandkotpelare",
          "nyckelbenfraktur",
          "ickeSpecSymtomNervMuskoskelSys",
          "blattOga",
          "ytligSkadaHuvudBrost",
          "kontusionRyggBackenBukvaggLarUnderben",
          "brannskada",
          "anoxiskHjarnskada",
          "skadorPaHuvudetUtanFraktur",
          "andningsuppehall",
          "epilepsi.1",
          "ytligSarskada",
          "fallskadaKlamskada",
          "summationVariabler_d1"
          )

dat10 = dat9[,c("alderVidMisshandelManaderClass",tab1Vars)]

setDT(dat10, key = "alderVidMisshandelManaderClass")

dat10$n = 1

#tab1a = dat10[,lapply(.SD, function(x) sum(x,na.rm=T)),by=alderVidMisshandelManaderClass ]
tab1a = dat10[,lapply(.SD, function(x) paste(sum(x,na.rm=T), "  (", round(sum(x,na.rm=T)/sum(.N)*100),"%)", sep="")),
                      by=alderVidMisshandelManaderClass ]

tab1b = t(tab1a)
colnames(tab1b) = tab1b[1,]

tab1b = as.matrix(tab1b[-1,])


write.csv2(tab1a,file.path(path,"Output","TLP","TLP_016_tab1a.csv"),row.names = F)
write.csv2(tab1b,file.path(path,"Output","TLP","TLP_016_tab1b.csv"),row.names = F)
#-------------------------------------------------------------------------------

#----------------------------- Tabell 2 ----------------------------------------
tab2Vars = c("rakit_d1",
             "frakturRevben_d1",
             "frakturLarben_d1",
             "frakturUnderben_d1",
             "ovrigSkallskada_d1",
             "frakturOverUnderArm_d1",
             "skallFraktur_d1",
             "retinalBlodning_d1",
             "subduralIT_d1",
             "subduralTraum_d1",
             "svt_d1",
             "ogi_d1",
             "spadbarnskolik_d1",
             "frakturRorben_d2",
             "subduralBlodning_d2",
             "frakturLandkotpelare",
             "nyckelbenfraktur",
             "ickeSpecSymtomNervMuskoskelSys",
             "blattOga",
             "ytligSkadaHuvudBrost",
             "kontusionRyggBackenBukvaggLarUnderben",
             "brannskada",
             "anoxiskHjarnskada",
             "skadorPaHuvudetUtanFraktur",
             "andningsuppehall",
             "epilepsi",
             "ytligSarskada",
             "fallskadaKlamskada",
             "summationVariabler_d1"
             )

x = tab2Vars[1]
   
tabulator = function(x){
   #x = "rakit_d1"
   tmp = subset(dat9, dat9$barnMisshandel_d1==0)
   
   print(x)
   y = grep(paste("alderDiagnosManad_",x,sep=""),names(tmp),value=T)
   
   tmp$timeClass = cut(as.numeric(tmp[,y]),breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12))
   
   tmp$timeClass = as.character(tmp$timeClass)
   tmp$timeClass[is.na(tmp$timeClass)] = "Diagnos 12+ months"
   
   #tmp$timeClass = factor(tmp$timeClass,
   #                      levels = sort(unique(tmp$timeClass))[c(1,2,5,6,7,8,9,10,11,12,3,4,13)])
   
   setDT(tmp, key = "timeClass")
   
   n = sum(tmp[,x,with=F],na.rm=T)
   
   out = tmp[, paste(sum(get(x),na.rm=T),"  (", round(sum(get(x),na.rm=T)/n*100),"%)", sep=""), by="timeClass"]
   setnames(out,"V1",x)
   
   return(out)
}

ll = lapply(tab2Vars, tabulator)

data <- Reduce(function(x, y) merge(x, y, all=T, 
                                    by="timeClass"), ll, accumulate=F)

tab2a = data.frame(data[c(1,2,5,6,7,8,9,10,11,12,3,4,13),])

tab2a[is.na(tab2a)] = "0  (0%)"
tab2a[tab2a=="0  (NaN%)"]= "0  (0%)"


tab2b = t(tab2a)
colnames(tab2b) = tab2b[1,]

tab2b = tab2b[-1,]
write.csv2(tab2a,file.path(path,"Output","TLP","TLP_016_tab2a.csv"),row.names = F)
write.csv2(tab2b,file.path(path,"Output","TLP","TLP_016_tab2b.csv"),row.names = F)
#--------------------------------------------------------------------------------
