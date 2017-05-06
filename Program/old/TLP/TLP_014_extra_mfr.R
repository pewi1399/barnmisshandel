

mfr_new0 = DMP_01_mfr

#remove subscores
mfr_new1 = mfr_new0[,names(mfr_new0)[!grepl("ICD",names(mfr_new0))]]

mfr_new1 = data.table(mfr_new0)


#create timeclassification 
mfr_new1$year = as.numeric(substr(mfr_new1$BFODDAT,1,4))

mfr_new1$yearClass = cut(mfr_new1$year,c(1973,1979,1989,1999,2004,2009,2013), include.lowest = T)


#create table of diagnoses per timeclass and lopnr
mfr_new2 = mfr_new1[,list(
  # preeklampsi = sum(preeklampsi),
   placentaavlossning = sum(placentaavlossning),
   kroniskHT = sum(kroniskHT),
   #gravididtetsDiab = sum(gravididtetsDiab), 
   pregestDiab = sum(pregestDiab),
   oligohydramn = sum(oligohydramn),
   värkrubbningar = sum(värkrubbningar),
   värkrubbningarSvaghet = sum(värkrubbningarSvaghet), 
   värkrubbningarUtdragenFörloss = sum(värkrubbningarUtdragenFörloss),
   hotandeAsfyxi = sum(hotandeAsfyxi),
   navelsträngsKompl = sum(navelsträngsKompl),
   feberUnderfFörloss = sum(feberUnderfFörloss),
   sätesextraktion = sum(sätesextraktion),
   påverkanFosterPgaKejsarsnitt = sum(påverkanFosterPgaKejsarsnitt),
   förlossningsskadaCnsBlödning = sum(förlossningsskadaCnsBlödning),
   kefalhematom = sum(kefalhematom),
   frakturer = sum(frakturer),
   nervskada = sum(nervskada),
   andraFörlossSkador = sum(andraFörlossSkador),
   intrauterinHypoxi = sum(intrauterinHypoxi),
   HIE = sum(HIE),
   respDistress = sum(respDistress),
   pneumoni = sum(pneumoni),
   sepsisNyfödd = sum(sepsisNyfödd),
   blödningFoster = sum(blödningFoster),
   kärnikterus = sum(kärnikterus),
   kramper = sum(kramper),
   missbildningar = sum(missbildningar)
   ),
by=c("lpnr_BARN","yearClass")]

mfr_new3 = data.frame(mfr_new2)

tabvars = names(mfr_new3)[grepl("_d",names(mfr_new3))]

mfr_new3[,tabvars] = lapply(mfr_new3[,tabvars],function(x){ifelse(x>0,1,0)})

mfr_new3 = data.table(mfr_new3)

#automatic sum does not work fro some stupid reason, fix this!
mfr_new3$n = 1

#Create final table 
mfr_new4 = mfr_new3[,list(
   #preeklampsi = sum(preeklampsi),
   placentaavlossning = sum(placentaavlossning),
   kroniskHT = sum(kroniskHT),
   #gravididtetsDiab = sum(gravididtetsDiab), 
   pregestDiab = sum(pregestDiab),
   oligohydramn = sum(oligohydramn),
   värkrubbningar = sum(värkrubbningar),
   värkrubbningarSvaghet = sum(värkrubbningarSvaghet), 
   värkrubbningarUtdragenFörloss = sum(värkrubbningarUtdragenFörloss),
   hotandeAsfyxi = sum(hotandeAsfyxi),
   navelsträngsKompl = sum(navelsträngsKompl),
   feberUnderfFörloss = sum(feberUnderfFörloss),
   sätesextraktion = sum(sätesextraktion),
   påverkanFosterPgaKejsarsnitt = sum(påverkanFosterPgaKejsarsnitt),
   förlossningsskadaCnsBlödning = sum(förlossningsskadaCnsBlödning),
   kefalhematom = sum(kefalhematom),
   frakturer = sum(frakturer),
   nervskada = sum(nervskada),
   andraFörlossSkador = sum(andraFörlossSkador),
   intrauterinHypoxi = sum(intrauterinHypoxi),
   HIE = sum(HIE),
   respDistress = sum(respDistress),
   pneumoni = sum(pneumoni),
   sepsisNyfödd = sum(sepsisNyfödd),
   blödningFoster = sum(blödningFoster),
   kärnikterus = sum(kärnikterus),
   kramper = sum(kramper),
   missbildningar = sum(missbildningar),
   n = sum(n)
),
by=c("yearClass")]

mfr_new5 = melt(mfr_new4, id=c("yearClass","n"))

#
rowAdder = function(sub){
   #appends empty row at end of dataframe
   rw = sub[1,]
   rw[,] = ""
   
   out = rbind(sub,rw)
   return(out)
} 

setnames(mfr_new5,old =names(mfr_new5) , new = c("Period","n","Diagnos","Antal"))

mfr_new5 = mfr_new5[,c("Diagnos","Period","Antal","n"),with=F]

mfr_new6 = ddply(mfr_new5, .(Diagnos),rowAdder)
#-------------------------------------------------------------------------------