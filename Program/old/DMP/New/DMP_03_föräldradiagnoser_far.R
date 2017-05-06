rm(list=ls())
library(data.table)
library(haven)
#Source
#source("/home/per/Polycarp/KBH/P101_barnmisshandel/Program/DMP/New/functions/deriveParentDiagnoses.R", encoding="utf-8")
#load(file.path(path, "Output","DMP","DMP_01_keyfile.Rdata"))
#-------------------------------------------------------------------------------

#------------------------------- föräldradata ----------------------------------
if(TRUE){
  #par_foralder <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/UT_PAR_F_28574_2015.txt",
  #                       sep = "\t",
  #                       encoding = "utf-8",
  #                       header = TRUE,
  #                       stringsAsFactors = FALSE,
  #                       nrow = 114074
  #)
  
  par_foralder <- read_sas("Indata/Sos_20170407/SoS/Data/UT_28574_2015/ut_par_f_28574_2015.sas7bdat")
  par_foralder <- data.frame(par_foralder)
  
  # row 114075
  # gg <- par_foralder[114073:114074,]
  
  #-------------------------------------------------------------------------------
  
  par_foralder$DIAGNOS = paste(" ",par_foralder$DIAGNOS," ", sep="") 
  
  #new eko
  cols = c(names(par_foralder)[grep("EKO",names(par_foralder))])
  
  par_foralder$EKO <- apply(par_foralder[ , cols ] , 1 , paste , collapse = " ")
  par_foralder$EKO = paste(" ",par_foralder$EKO," ", sep="") 
  
  #something is strange with this one 
  par_foralder$INDATUM = gsub(" ","",as.character(par_foralder$INDATUM))
  
  #par_foralder[par_foralder$INDATUM=="","INDATUM"] = NA
  
  par_foralder = par_foralder[,c("lopnr","INDATUM","DIAGNOS","EKO",
                                               "OP","INDATUM", "UTDATUM",
                                               "source", "Cnation")]
  saveRDS(par_foralder, "Output/03_par_foralder.rds")
}else{
  par_foralder <- readRDS("Output/03_par_foralder.rds")
}

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