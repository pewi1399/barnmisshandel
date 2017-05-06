
#--------------------- extract parents diagnoses -------------------------------
parentDiag0 = DMP_03_föräldradiagnoser

#remove subscores
dia0 = parentDiag0[,names(parentDiag0)[!grepl("ICD",names(parentDiag0))]]

dia1 = data.table(dia0)

#create table of diagnoses per timeclass and lopnr
dia2 = dia1[,list(psykdiagnos = sum(psykdiagnos),
                  bipolärSjukdom = sum(bipolärSjukdom),
                  depression = sum(depression),
                  neuroticism = sum(neuroticism),
                  postpartumDepr = sum(postpartumDepr),
                  ADHD = sum(ADHD),
                  autismSpektrum = sum(autismSpektrum),
                  postpartumPsykos = sum(postpartumPsykos),
                  alkohol = sum(alkohol),
                  drogProblem = sum(drogProblem),
                  PersonlighetsStörning = sum(PersonlighetsStörning)
),
by=c("lpnr")]

dia3 = data.frame(dia2)

tabvars = names(dia3)[!grepl("lpnr",names(dia3))]

dia3[,tabvars] = lapply(dia3[,tabvars],function(x){ifelse(x>0,1,0)})

dia3 = data.table(dia3)

#Create final table 
dia4 = dia3[,list(psykdiagnos = sum(psykdiagnos),
                  bipolärSjukdom = sum(bipolärSjukdom),
                  depression = sum(depression),
                  neuroticism = sum(neuroticism),
                  postpartumDepr = sum(postpartumDepr),
                  ADHD = sum(ADHD),
                  autismSpektrum = sum(autismSpektrum),
                  postpartumPsykos = sum(postpartumPsykos),
                  alkohol = sum(alkohol),
                  drogProblem = sum(drogProblem),
                  PersonlighetsStörning = sum(PersonlighetsStörning),
                  n = sum(.N)
),]

dia5 = melt(dia4, id="n")

#
rowAdder = function(sub){
   #appends empty row at end of dataframe
   rw = sub[1,]
   rw[,] = ""
   
   out = rbind(sub,rw)
   return(out)
} 

setnames(dia5,old =names(dia5) , new = c("n","Diagnos","Antal"))

dia5 = dia5[,c("Diagnos","Antal","n"),with=F]

dia6_all = ddply(dia5, .(Diagnos),rowAdder)
#-------------------------------------------------------------------------------


#---------------------- extract parents medicine -------------------------------
lak0 = data.frame(DMP_05_föräldramedicin)


lak1 = data.table(lak0)

#create table of Mednoses per timeclass and lopnr
lak2 = lak1[,list(neruoleptika = sum(neruoleptika),
                  bensoSläkt = sum(bensoSläkt),
                  lugnandeAtaraktika = sum(lugnandeAtaraktika),
                  sömnOchLugnandeMedel = sum(sömnOchLugnandeMedel),
                  bensoDerivat = sum(bensoDerivat),
                  N05AlugnandeOchSömn = sum(N05AlugnandeOchSömn),
                  N06AAntidepressiva = sum(N06AAntidepressiva),
                  psykostimulantiaADHDAutism = sum(psykostimulantiaADHDAutism),
                  antabus = sum(antabus),
                  antacida = sum(antacida),
                  magsår = sum(magsår)
),
by=c("lpnr")]

lak3 = data.frame(lak2)

tabvars = names(lak3)[!grepl("lpnr",names(lak3))]

lak3[,tabvars] = lapply(lak3[,tabvars],function(x){ifelse(x>0,1,0)})

lak3 = data.table(lak3)

#Create final table 
lak4 = lak3[,list(neruoleptika = sum(neruoleptika),
                  bensoSläkt = sum(bensoSläkt),
                  lugnandeAtaraktika = sum(lugnandeAtaraktika),
                  sömnOchLugnandeMedel = sum(sömnOchLugnandeMedel),
                  bensoDerivat = sum(bensoDerivat),
                  N05AlugnandeOchSömn = sum(N05AlugnandeOchSömn),
                  N06AAntidepressiva = sum(N06AAntidepressiva),
                  psykostimulantiaADHDAutism = sum(psykostimulantiaADHDAutism),
                  antabus = sum(antabus),
                  antacida = sum(antacida),
                  magsår = sum(magsår),
                  n = sum(.N)
),]

lak5 = melt(lak4, id="n")

setnames(lak5,old =names(lak5) , new = c("n","Läkemedel","Antal"))

lak5 = lak5[,c("Läkemedel","Antal","n"),with=F]

lak6_all = ddply(lak5, .(Läkemedel),rowAdder)

#-------------------------------------------------------------------------------
