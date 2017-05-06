# quantile(keyfile$lpnr_b)
# quantile(keyfile$lpnr_m,na.rm=T)
# quantile(lak7$lpnr,na.rm=T)
# quantile(pdia7$lpnr,na.rm=T)
# quantile(dia3$lpnr,na.rm=T)
# 
# quantile(fDod7$lpnr_BARN,na.rm=T)
# quantile(dodBarn0$lpnr,na.rm=T)
# quantile(DMP_100_snq$lpnr,na.rm=T)
# quantile(barnDiag8$lpnr,na.rm=T)
# 
# 
# quantile(dat1$lpnr_b)
# quantile(dat2$lpnr,na.rm=T)
# quantile(dat3$lpnr,na.rm=T)
# 
# 
# bb = barnDiag8[barnDiag8$lpnr>7000000,]
#-------------------------------------------------------------------------------
#read in data from komplettering nr 2 october 2015
sluten_barnOct1 = read.csv(file.path(path,"Indata","Komplettering_2","sos","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F)
oppen_barnOct1 = read.csv(file.path(path,"Indata","Komplettering_2","sos","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)

sluten_barnOct2 = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","PATIENT_SLUTEN                  .CSV"),stringsAsFactors=F)
oppen_barnOct2 = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","PATIENT_OPPEN                   .CSV"),stringsAsFactors=F)

komplettOct = rbind.fill(sluten_barnOct1,oppen_barnOct1,sluten_barnOct2,oppen_barnOct2)

#split kids and mothers by mfr data
mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

par = subset(komplettOct, komplettOct$lpnr %in% c(mfr_kompl2b$lpnr_BARN,mfr_kompl2a$lpnr_BARN))
#-------------------------------------------------------------------------------

#create diagnosis vars
cols = c(names(par)[grep("bdia|hdia",names(par))])

par$DIAGNOSER <- apply(par[ , cols ] , 1 , paste , collapse = " ")
par$DIAGNOSER = paste(" ",par$DIAGNOSER," ", sep="") 

#new eko
cols = c(names(par)[grep("EKO",names(par))])

par$EKO <- apply(par[ , cols ] , 1 , paste , collapse = " ")
par$EKO = paste(" ",par$EKO," ", sep="") 
#new MDC
#no need it is already one var

#merge EKO and diagnosis
par$DIAGNOSER = paste(par$DIAGNOSER,par$EKO,sep="")

par$misstankeOmBarnMisshdelICD10 = ifelse(grepl(" Z038K | Z045| Y071A | T741[012389] | T741 | 96[0-9]",par$DIAGNOSER),1,0)
par$misstankeOmBarnMisshdelICD9 = ifelse(grepl(" 995F | 995.81 | 96[0-9] ",par$DIAGNOSER),1,0)
par$misstankeOmBarnMisshdelICD8 = ifelse(grepl(" 968,9 | 968 | 960,9 ",par$DIAGNOSER),1,0)


par$misstankeOmBarnMisshdel = ifelse(rowSums(par[,names(par)[grep("misstankeOmBarnMisshdel",names(par))]],na.rm=T)>0,1,0)
#par0 = par[,c("lpnr","INDATUM","DIAGNOSER","misstankeOmBarnMisshdelICD10")]
sum(par$misstankeOmBarnMisshdel)



gg = par[,c("lpnr","DIAGNOSER","misstankeOmBarnMisshdelICD10","misstankeOmBarnMisshdel")]

#-------------------------------- titta i mfr ----------------------------------
mfr_kompl2a = read.csv(file.path(path,"Indata","Komplettering_2","sos","MFR                             .CSV"),stringsAsFactors=F) #added october
mfr_kompl2b = read.csv(file.path(path,"Indata","Komplettering_2","sos_2","MFR                             .CSV"),stringsAsFactors=F) #added october

#-------------------------------------------------------------------------------
mfr0 = rbind.fill(mfr_kompl2a,mfr_kompl2a)

#calculate diagnosis vars

#Create diagnosis variable
childDiagnoses = c(names(mfr0)[grep("BDIA",names(mfr0))])

mfr0$BDIAG <- apply(mfr0[ ,childDiagnoses] , 1 , paste , collapse = " ")
mfr0$BDIAG = paste(" ",mfr0$BDIAG," ", sep="") 


mfr0$misstankeOmBarnMisshdelICD10 = ifelse(grepl(" Z038K | Z045| Y071A | T741[012389] | T741 | 96[0-9]",mfr0$BDIAG),1,0)
mfr0$misstankeOmBarnMisshdelICD9 = ifelse(grepl(" 995F | 995.81 | 96[0-9] ",mfr0$BDIAG),1,0)
mfr0$misstankeOmBarnMisshdelICD8 = ifelse(grepl(" 968,9 | 968 | 960,9 ",mfr0$BDIAG),1,0)


mfr0$misstankeOmBarnMisshdel = ifelse(rowSums(mfr0[,names(mfr0)[grep("misstankeOmBarnMisshdel",names(mfr0))]],na.rm=T)>0,1,0)
sum(mfr0$misstankeOmBarnMisshdel)

