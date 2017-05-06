load(file.path(path,"Output","DMP","DMP_02_mor.Rdata"))

lak = data.table(prt3_mor)



#------------------------------ Läkemedel ATC ----------------------------------
lak$neruoleptika = ifelse(grepl("N05A",lak$atc),1,0)

lak$bensoSläkt = ifelse(grepl("N05CF",lak$atc),1,0)

lak$lugnandeAtaraktika = ifelse(grepl("N05B",lak$atc),1,0)

lak$sömnOchLugnandeMedel = ifelse(grepl("N05C",lak$atc),1,0)

lak$bensoDerivat = ifelse(grepl("N05CD",lak$atc),1,0)

#lugnandeOchSömn
lak$lugnandeOchSömn = ifelse(grepl("N05",lak$atc),1,0)

#lak$antiDepressiva = ifelse(grepl(" N06A A ",lak$atc),1,0)
#lak$antiDepressiva = ifelse(grepl(" N06A A ",lak$atc),1,0)

#samla alla N06A preparat
lak$N06AAntidepressiva = ifelse(grepl("N06AA|N06AB|N06AG|N06AX",lak$atc),1,0)


lak$psykostimulantiaADHDAutism = ifelse(grepl("N06B",lak$atc),1,0)

lak$antabus = ifelse(grepl("N07BB01",lak$atc),1,0)

lak$antacida = ifelse(grepl("A02A[A-C]|A02B ",lak$atc),1,0)

lak$magsår = ifelse(grepl("A02B[A-C]",lak$atc),1,0)

#Barn
#grund$minifom = ifelse(grepl("A03AX13",grund$atc),1,0)
#-------------------------------------------------------------------------------

#------------------------------ Tabellera --------------------------------------

tabLak = lak[,list(
   neruoleptika = ifelse(sum(neruoleptika)>0,1,0),
   bensoSläkt = ifelse(sum(bensoSläkt)>0,1,0),
   lugnandeAtaraktika = ifelse(sum(lugnandeAtaraktika)>0,1,0),
   sömnOchLugnandeMedel = ifelse(sum(sömnOchLugnandeMedel)>0,1,0),
   bensoDerivat = ifelse(sum(bensoDerivat)>0,1,0),
   psykostimulantiaADHDAutism = ifelse(sum(psykostimulantiaADHDAutism)>0,1,0),
   antabus = ifelse(sum(antabus)>0,1,0),
   antacida = ifelse(sum(antacida)>0,1,0)
),by="lpnr"]

tabLak1 = tabLak[,list(
   neruoleptika = sum(neruoleptika),
   bensoSläkt = sum(bensoSläkt),
   lugnandeAtaraktika = sum(lugnandeAtaraktika),
   sömnOchLugnandeMedel = sum(sömnOchLugnandeMedel),
   bensoDerivat = sum(bensoDerivat),
   psykostimulantiaADHDAutism = sum(psykostimulantiaADHDAutism),
   antabus = sum(antabus),
   antacida = sum(antacida)
),]


rtffile= RTF(file.path(path,"Output","DMP","Tabell4_läkemedel_DRAFT.doc"))
addTable(rtffile,tabLak1)
done(rtffile)



