rm(list=ls())
library(data.table)
#Source
#source("/home/per/Polycarp/KBH/P101_barnmisshandel/Program/DMP/New/functions/deriveParentDiagnoses.R", encoding="utf-8")
#load(file.path(path, "Output","DMP","DMP_01_keyfile.Rdata"))
#-------------------------------------------------------------------------------

#------------------------------- föräldradata ----------------------------------
par_foralder <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_PAR_F_28574_2015.txt",
                       sep = "\t",
                       encoding = "utf-8",
                       header = TRUE,
                       stringsAsFactors = FALSE,
                       nrow = 114074
)

# row 114075
# gg <- par_foralder[114073:114074,]

#-------------------------------------------------------------------------------

par_far$DIAGNOS = paste(" ",par_far$DIAGNOS," ", sep="") 

#new eko
cols = c(names(par_far)[grep("EKO",names(par_far))])

par_far$EKO <- apply(par_far[ , cols ] , 1 , paste , collapse = " ")
par_far$EKO = paste(" ",par_far$EKO," ", sep="") 

#something is strange with this one 
par_far$INDATUM = gsub(" ","",as.character(par_far$INDATUM))

par_far[par_far$INDATUM=="","INDATUM"] = NA

par_far1 = par_far[!is.na(par_far$INDATUM),c("lpnr","INDATUM","DIAGNOSER","EKO",
                                             "MDC","INDATUM", "UTDATUM","sjukhusnamn",
                                             "lan_text","klinik", "SOURCE")]

# #extract kids from parent file
# par_far2 = subset(par_far1,!(par_far1$lpnr %in% key$lpnr_BARN))
# par_barn = subset(par_far1,par_far1$lpnr %in% key$lpnr_BARN)
# #nrow(par_far1)
# #nrow(par_far2)+ nrow(par_barn)
# 
# par_far2 = par_far2[,c("lpnr","INDATUM","DIAGNOSER","EKO")]
# #create diagnoses
# par_far3 = deriveParentDiagnoses(par_far2)
# 
# 
# 
# par_far3$övergreppMisshMordDråpVård = par_far3$övergreppMisshMordDråp
# 
# #outs = c("självmordsförsökICD10", "självmordsförsök", "suicidICD10", "suicid",
# #"övergreppMisshMordDråpICD10", "övergreppMisshMordDråp","mordDråpICD10","mordDråp")
# 
# #par_far4 = par_far3[,!c(names(par_far3) %in% outs)]
# #table(par_mor3$övergreppMisshMordDråpVård)
# #-------------------------------------------------------------------------------
# 
# #--------------------------- prepare for print ---------------------------------
# DMP_03_tillagg_barn = par_barn
# DMP_03_föräldradiagnoser_far = par_far3
# 
# save(key, file=file.path(path, "Output","DMP","DMP_03_keyfile2.Rdata"))
# save(DMP_03_tillagg_barn, file=file.path(path, "Output","DMP","DMP_03_tillagg_barn.Rdata")) #added because kids and fathers was mixed in this file
# 
# 
# write.csv2(DMP_03_föräldradiagnoser_far[,c("lpnr","INDATUM","DIAGNOSER","EKO","självmordsförsök")], file=file.path(path, "Output","DMP","DMP_03_foraldradiagnoser_far.csv"),na="",row.names=F)
# DMP_03_föräldradiagnoser_far$EKO = NULL 
# save(DMP_03_föräldradiagnoser_far, file=file.path(path, "Output","DMP","DMP_03_föräldradiagnoser_far.Rdata"))
#EOF