# 
# #------------------------ prepare case ids and dates ---------------------------
# barnDiag0 = DMP_02_barndiagnoser
# keyfile0 = keyfile
# 
# #for tabulation only misshdel cases are neccesary
# barnDiag1 = subset(barnDiag0,barnMisshandel_d1==1)
# 
# #sum(table(unique(barnDiag1$lpnr)))
# #481 st looking quite good
# 
# #Include only lpnr, diagnosis and date of diagnosis 
# barnDiag2 = barnDiag1[,c("lpnr","barnMisshandel_d1","INDATUM")]
# 
# #merge on keyfile
# barnDiag3 = merge(barnDiag2,keyfile0,by.x="lpnr",by.y="lpnr_BARN")
# 
# #set as date
# barnDiag3$misshandelsDatum = as.Date(barnDiag3$INDATUM,format="%Y-%m-%d")
# barnDiag3$INDATUM = NULL
# 
# barnDiag3$lpnr_barn = barnDiag3$lpnr
# barnDiag3$lpnr = NULL
# 
# 
# #calculate first case per id
# barnDiag4 = data.table(barnDiag3)
# barnDiag4 = barnDiag4[,list(lpnr_mor = unique(lpnr_mor),
#                             misshandelsDatum= min(misshandelsDatum)),
#                       by="lpnr_barn"]
# barnDiag4 = data.frame(barnDiag4)
# #-------------------------------------------------------------------------------
# 
# #--------------------- extract parents diagnoses -------------------------------
 barnDod0 = DMP_06_dodBarn
 nrow(barnDod0)
# #-------------------------------------------------------------------------------

forDod0 = DMP_07_dodfor

#remove subscores
fDod0 = forDod0[,names(forDod0)[!grepl("ICD",names(forDod0))]]

#dia0 = merge(parentDiag1,barnDiag4,by.x="lpnr",by.y="lpnr_mor")

#dia0$INDATUM= as.Date(dia0$INDATUM,format="%Y-%m-%d")

#calculate time difference as date minus event date negative values indicate
#diagnosis was present at time of event
#dia0$timeDiff = difftime(dia0$INDATUM,dia0$misshandelsDatum,units="days")

#dia0$timeClass = ifelse(dia0$timeDiff< -365.24,"Before event",
#                        ifelse(dia0$timeDiff>-365.24 & dia0$timeDiff<=0,"Year before event","After event"))
fDod1 = data.table(fDod0)

#create table of fDodgnoses per timeclass and lopnr
fDod2 = fDod1[,list(självmordsförsök = sum(självmordsförsök),
                  suicid = sum(suicid),
                  övergreppMisshMordDråp = sum(övergreppMisshMordDråp),
                  mordDråp = sum(mordDråp)
),]


fDod4 = fDod2

#fDod2$timeClass = as.factor(fDod2$timeClass)

#fDod2$timeClass = factor(fDod2$timeClass,levels(fDod2$timeClass)[c(2,3,1)])

#setkey(fDod2,timeClass)

#remove dupes we are only interested in first fDodgnosis
#fDod3 = data.frame(fDod2[!duplicated(fDod2[,c("lpnr"),with=F]),,])

#tabvars = names(fDod3)[!grepl("lpnr|timeClass",names(fDod3))]

#fDod3[,tabvars] = lapply(fDod3[,tabvars],function(x){ifelse(x>0,1,0)})

#fDod3 = data.table(fDod3)
# 
# #Create final table 
# fDod4 = fDod3[,list(psykfDodgnos = sum(psykfDodgnos),
#                   bipolärSjukdom = sum(bipolärSjukdom),
#                   depression = sum(depression),
#                   neuroticism = sum(neuroticism),
#                   postpartumDepr = sum(postpartumDepr),
#                   ADHD = sum(ADHD),
#                   autismSpektrum = sum(autismSpektrum),
#                   postpartumPsykos = sum(postpartumPsykos),
#                   alkohol = sum(alkohol),
#                   drogProblem = sum(drogProblem),
#                   PersonlighetsStörning = sum(PersonlighetsStörning)
# ),
# by=c("timeClass")]

fDod4$avlidnaBarn = nrow(barnDod0)

fDod5 = melt(fDod4)

#
rowAdder = function(sub){
   #appends empty row at end of dataframe
   rw = sub[1,]
   rw[,] = ""
   
   out = rbind(sub,rw)
   return(out)
} 

setnames(fDod5,old =names(fDod5) , new = c("Diagnos","Antal"))

fDod5 = fDod5[,c("Diagnos","Antal"),with=F]

fDod6 = ddply(fDod5, .(Diagnos),rowAdder)
#-------------------------------------------------------------------------------

