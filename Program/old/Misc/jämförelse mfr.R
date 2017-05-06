
#read files
gmfr = data.table(read.csv("K:/Academy/UU/UU__5185 Barnmisshandel/Indata/Grunddata/grunddata/MFR                             .CSV"))
mfr1 = data.table(read.csv("K:/Academy/UU/UU__5185 Barnmisshandel/Indata/mfr_1/MFR                             .CSV"))

info = data.table(read.csv("K:/Academy/UU/UU__5185 Barnmisshandel/Indata/Grunddata/grunddata/INFO_LPNR                       .CSV"))

infoB = subset(info,barn==1)
infoM = subset(info,mor==1)

#infoBM = subset(info,mor==1 & barn==1)
#nrow(info)-nrow(infoB)-nrow(infoM)


vars = c("lan_text", "AR", "MFODDAT", "BFODDAT", "INDATMHV","lpnr_BARN", "lpnr_mor") 

#check if vars are same (sample)

gdat = data.frame(gmfr[,vars,with=F])

dat1 = data.frame(mfr1[,vars,with=F])













