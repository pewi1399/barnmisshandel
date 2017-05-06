
#-------------------------------- Part 1 ---------------------------------------
load(file.path(path,"Output","DMP","TMPfile.Rdata"))


#extract neccessary vars for creating time class
variables = c("lpnr","INDATUM","barn","mor","lpnr_mor","barnMisshandel_d1")

#subset on variables
tmp0 = grund[,variables]

#only children are applicable and only 
tmp1 = subset(tmp0, barn == 1 &  barnMisshandel_d1 == 1) 
#-------------------------------------------------------------------------------

#-------------------------------- Part 2 ---------------------------------------
load(file.path(path,"Output","DMP","DMP_02_mor.Rdata"))

lak = data.table(prt3_mor)
#-------------------------------------------------------------------------------



#------------------------------ Läkemedel ATC ----------------------------------
lak$neruoleptika = ifelse(grepl("N05A",lak$atc),1,0)

lak_variables = c("lpnr", "FDATUM", "neruoleptika", "atc","mor")

lak1 = lak[,lak_variables,with=F]

#subset those without prescription
lak2 = subset(lak1, !is.na(atc))
#-------------------------------------------------------------------------------





