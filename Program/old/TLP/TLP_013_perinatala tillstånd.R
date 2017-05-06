
#--------------------- extract parents diagnoses -------------------------------
mfr0 = DMP_01_mfr

#remove subscores
mfr1 = mfr0[,names(mfr0)[!grepl("ICD",names(mfr0))]]

mfr1 = data.table(mfr0)


#create timeclassification 
mfr1$year = as.numeric(substr(mfr1$BFODDAT,1,4))

mfr1$yearClass = cut(mfr1$year,c(1973,1979,1989,1999,2004,2009,2013), include.lowest = T)


#beräkna födelsedag och diagnosdag
#grund$fdag = as.Date(as.character(grund$fod_man_dag), "%Y%m%d")
#grund$diagnosdag = as.Date(grund$INDATUM)

#grund1 = subset(grund1,is.na(INDATUM))
#grund$diagnoseAgeWeeks = difftime(grund$diagnosdag,grund$fdag, units = "weeks")

#grund$ageClassWeeks = ifelse(grund$diagnoseAgeWeeks<28,"<=28",
#                             ifelse(grund$diagnoseAgeWeeks<=32,"29-32",
##                                    ifelse(grund$diagnoseAgeWeeks<=36,"33-36",
#                                           ifelse(grund$diagnoseAgeWeeks>=37,"37+",NA))))

#create table of diagnoses per timeclass and lopnr
mfr2 = mfr1[,list(Pclavikel_d1 = sum(Pclavikel_d1),
                  Plårben_d1 = sum(Plårben_d1),
                  Pöverarm_d1 = sum(Pöverarm_d1),
                  Prörben_d1 = sum(Prörben_d1),
                  Pskallfraktur_d1 = sum(Pskallfraktur_d1),
                  PandraSkelettSkador_d1 = sum(PandraSkelettSkador_d1),
                  PandraSpecificeradeSkadorSkelett_d1 = sum(PandraSpecificeradeSkadorSkelett_d1),
                  PövrigFraktur_d1 = sum(PövrigFraktur_d1),
                  Psubdural_d1 = sum(Psubdural_d1),
                  Pintrakraniellskada_d1 = sum(Pintrakraniellskada_d1),
                  PintrakraniellIckeTrauma_d1 = sum(PintrakraniellIckeTrauma_d1),
                  PintrakraniellSkadaBlödning_d1 = sum(PintrakraniellSkadaBlödning_d1)
),
by=c("lpnr_BARN","yearClass")]

mfr3 = data.frame(mfr2)

tabvars = names(mfr3)[grepl("_d",names(mfr3))]

mfr3[,tabvars] = lapply(mfr3[,tabvars],function(x){ifelse(x>0,1,0)})

mfr3 = data.table(mfr3)

#automatic sum does not work fro some stupid reason, fix this!
mfr3$n = 1

#Create final table 
mfr4 = mfr3[,list(Pclavikel_d1 = sum(Pclavikel_d1),
                  Plårben_d1 = sum(Plårben_d1),
                  Pöverarm_d1 = sum(Pöverarm_d1),
                  Prörben_d1 = sum(Prörben_d1),
                  Pskallfraktur_d1 = sum(Pskallfraktur_d1),
                  PandraSkelettSkador_d1 = sum(PandraSkelettSkador_d1),
                  PandraSpecificeradeSkadorSkelett_d1 = sum(PandraSpecificeradeSkadorSkelett_d1),
                  PövrigFraktur_d1 = sum(PövrigFraktur_d1),
                  Psubdural_d1 = sum(Psubdural_d1),
                  Pintrakraniellskada_d1 = sum(Pintrakraniellskada_d1),
                  PintrakraniellIckeTrauma_d1 = sum(PintrakraniellIckeTrauma_d1),
                  PintrakraniellSkadaBlödning_d1 = sum(PintrakraniellSkadaBlödning_d1),
                  n = sum(n)
),
by=c("yearClass")]

mfr5 = melt(mfr4, id=c("yearClass","n"))

#
rowAdder = function(sub){
   #appends empty row at end of dataframe
   rw = sub[1,]
   rw[,] = ""
   
   out = rbind(sub,rw)
   return(out)
} 

setnames(mfr5,old =names(mfr5) , new = c("Period","n","Diagnos","Antal"))

mfr5 = mfr5[,c("Diagnos","Period","Antal","n"),with=F]

mfr6 = ddply(mfr5, .(Diagnos),rowAdder)
#-------------------------------------------------------------------------------

