rm(list=objects())

#Source
#source("Program/DMP/New/functions/deriveMFR.R", encoding="utf-8")

#read in mfr
mfr <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_MFR_28574_2015.txt",
                  sep = "\t",
                  header = TRUE
                  )
#-------------------------------------------------------------------------------

#Check how many ids
#table(mfr_grund0$lpnr_BARN %in% mfr_1_0$lpnr_BARN)

#There is one non unique id exclude it
#mfr_1_1 = mfr_1_0[!duplicated(mfr_1_0$lpnr_BARN),]


#Create diagnosis variable
childDiagnoses = c(names(mfr)[grep("BDIA",names(mfr))])

mfr$BDIAG <- apply(mfr[ ,childDiagnoses] , 1 , paste , collapse = " ")
mfr$BDIAG = paste(" ",mfr$BDIAG," ", sep="") 

#create mother diagnosis
motherDiagnoses = c(names(mfr)[grep("MDIA",names(mfr))])

mfr$MDIAG <- apply(mfr[ ,motherDiagnoses] , 1 , paste , collapse = " ")
mfr$MDIAG = paste(" ",mfr$MDIAG," ", sep="") 

#create flop
motherFlop = c(names(mfr)[grep("^FLOP",names(mfr))])

mfr$MFLOP <- apply(mfr[ ,motherFlop] , 1 , paste , collapse = " ")
mfr$MFLOP = paste(" ",mfr$MFLOP," ", sep="") 

mfr$BFLOP <- paste(" ",mfr$BFLOP," ", sep="") 

#at this point all that is needed is MDIAG BDIAG date and lpnr
outvars = c("BLOPNR", 
"Mlopnr", 
"AR", 
"SJUKHUS_S", 
"INDATMHV", 
"MVIKT", 
"MLANGD", 
"ROK0", 
"SNUS0", 
"ROK1", 
"SNUS1", 
"FAMSIT", 
"OFRIBARN", 
"OFRIIATG",
"OFRIABEF", 
"OFRISTIM", 
"OFRIKIRU", 
"OFRIICSI", 
"OFRIANN", 
"TIDDODF",
"TIDLEVF",
"TID7DOD",
"TIDSDOD",
"URINVINF",
"NJURSJUK", 
"DIABETES",
"EPILEPSI",
"ULCOLIT",
"SLE",
"HYPERTON",
"ROK2", 
"SNUS2",
"BJUDNING",
"FLSPONT",
"FLINDUKT",
"SECFORE",
"ELEKAKUT", 
"VAGINAL",
"SUGKLOCK",
"TANG",
"SECAVSL",
"TANGMARK",
"SUGMARK", 
"SECMARK",
"SJUKHUS",
"BORDF2",
"KON",
"GRVFV",
"GRDFV", 
"GRVBS", 
"GRDBS",
"BVIKT",
"BLANGDF2",
"HOMF",
"APGAR1",
"APGAR5", 
"APGAR10", 
"VENT",
"INTUB",
"HJMASS",
"ACIDOS",
"BFLOP",
"PARITET",
"PARITET_F",
"MALDER", 
"MISSB",
"MSGA",
"MLGA",
"CIVIL",
"Cfnat",
"Cmnat", 
"CMFODLAND", 
"LAN",
"BDIAG",
"MDIAG")



#-------------------------------------------------------------------------------


#save 

#keyfile
#
#mfr2$vårdtidFödsel
#<- difft


#keyfile = mfr[,c("lpnr_BARN","lpnr_mor")]
#saveRDS(keyfile, "Output/DMP/DMP_01_keyfile.rds")

out <- mfr[,outvars]

#--------------------------- prepare for print ---------------------------------
saveRDS(out, "Output/1_mfr.rds")

#EOF