# SETUP #########################################################
# NAME      : UH
#
# R-version : 3.0.2
#              
# PRODUCES  : Kontroller
# 
# STATUS    : DRAFT
#
# AUTHOR    : Per Wikman
# CREATED   : 2015-04-08
# NOTES     : 
# 
rm(list=objects())

# Todays date
datet <- format(Sys.time(), "%x")
fileDate <- gsub("-", "", datet)

# Rversion
Rversion <- paste(sessionInfo()$R.version$major, sessionInfo()$R.version$minor, sep=".")

rm(list=ls())
library(data.table)
library(ggplot2)
library(XLConnect)


path = "K:/Academy/UU/UU__5185 Barnmisshandel"
setwd(path)
load(file =  file.path(path,"Output","DMP","DMP_101_assembly2015.Rdata"))#dat9
dat0 = dat9
#-------------------------------------------------------------------------------

#kontrollera N05aLugnande och sömn
vars = grep("lpnr|N05AlugnandeOchSömn_..._Year",names(dat0),value=T)
n05 = dat0[,sort(vars)]

n05$filter = ifelse(rowSums(n05[,grep("N05AlugnandeOchSömn_..._Year",names(dat0),value=T)],na.rm=T),1,0)

n05 = subset(n05,filter==1)
n05$filter = NULL

#kontrollera N06aAntidepressiva
vars = grep("lpnr|N06AAntidepressiva_..._Year",names(dat0),value=T)
n06 = dat0[,sort(vars)]

n06$filter = ifelse(rowSums(n06[,grep("N06AAntidepressiva_..._Year",names(dat0),value=T)],na.rm=T),1,0)

n06 = subset(n06,filter==1)
n06$filter = NULL

#kontrollera självmordsförsök
vars = grep("lpnr|själv",names(dat0),value=T)
sja = dat0[,sort(vars)]

sja$filter = ifelse(rowSums(sja[,grep("själv",names(dat0),value=T)],na.rm=T),1,0)

sja = subset(sja,filter==1)
sja$filter = NULL

#-------------------------------------------------------------------------------

#------------------------ skriv till  exceldokument ----------------------------
wb = loadWorkbook(file.path(path,"Output","DMP","DMP99kontroller.xlsx"),create=T)
createSheet(wb,name = "N05a")
createSheet(wb,name = "N06a")
createSheet(wb,name = "SuicidForsok")

writeWorksheet(wb,n05,sheet= "N05a")
writeWorksheet(wb,n06,sheet="N06a")
writeWorksheet(wb,sja,sheet="SuicidForsok")

saveWorkbook(wb)
#-------------------------------------------------------------------------------
#EOF
