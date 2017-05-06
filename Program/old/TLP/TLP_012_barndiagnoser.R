
#--------------------- extract child diagnoses ---------------------------------
barnDiag0 = DMP_02_barndiagnoser

#remove subscores
dia0 = barnDiag0[,names(barnDiag0)[grepl("lpnr|_d",names(barnDiag0))]]

dia1 = data.table(dia0)

#create table of diagnoses per timeclass and lopnr
dia2 = dia1[,list(rakit_d1 = sum(rakit_d1),
                  frakturRevben_d1 = sum(frakturRevben_d1),
                  frakturLårben_d1 = sum(frakturLårben_d1),
                  frakturUnderben_d1 = sum(frakturUnderben_d1),
                  övrigSkallskada_d1 = sum(övrigSkallskada_d1),
                  barnMisshandel_d1 = sum(barnMisshandel_d1),
                  frakturÖverUnderArm_d1 = sum(frakturÖverUnderArm_d1),
                  skallFraktur_d1 = sum(skallFraktur_d1),
                  retinalBlödning_d1 = sum(retinalBlödning_d1),
                  subduralIT_d1 = sum(subduralIT_d1),
                  subduralTraum_d1 = sum(subduralTraum_d1),
                  svt_d1 = sum(svt_d1),
                  ogi_d1 = sum(ogi_d1),
                  spädbarnskolik_d1 = sum(spädbarnskolik_d1),
                  frakturRörben_d2 = sum(frakturRörben_d2),
                  subduralBlödning_d2 = sum(subduralBlödning_d2)
),
by=c("lpnr")]

dia3 = data.frame(dia2)

tabvars = names(dia3)[!grepl("lpnr",names(dia3))]

dia3[,tabvars] = lapply(dia3[,tabvars],function(x){ifelse(x>0,1,0)})

dia3 = data.table(dia3)

#Create final table 
dia4 = dia3[,list(rakit_d1 = sum(rakit_d1),
                  frakturRevben_d1 = sum(frakturRevben_d1),
                  frakturLårben_d1 = sum(frakturLårben_d1),
                  frakturUnderben_d1 = sum(frakturUnderben_d1),
                  övrigSkallskada_d1 = sum(övrigSkallskada_d1),
                  barnMisshandel_d1 = sum(barnMisshandel_d1),
                  frakturÖverUnderArm_d1 = sum(frakturÖverUnderArm_d1),
                  skallFraktur_d1 = sum(skallFraktur_d1),
                  retinalBlödning_d1 = sum(retinalBlödning_d1),
                  subduralIT_d1 = sum(subduralIT_d1),
                  subduralTraum_d1 = sum(subduralTraum_d1),
                  svt_d1 = sum(svt_d1),
                  ogi_d1 = sum(ogi_d1),
                  spädbarnskolik_d1 = sum(spädbarnskolik_d1),
                  frakturRörben_d2 = sum(frakturRörben_d2),
                  subduralBlödning_d2 = sum(subduralBlödning_d2),
                  n = sum(.N)
),
]

dia5 = melt(dia4, id="n")

#
rowAdder = function(sub){
   #appends empty row at end of dataframe
   rw = sub[1,]
   rw[,] = ""
   
   out = rbind(sub,rw)
   return(out)
} 

setnames(dia5,old =names(dia5) , new = c("n","Diagnos","Antal"))

dia5 = dia5[,c("Diagnos","Antal","n"),with=F]

dia6_barn = ddply(dia5, .(Diagnos),rowAdder)
#-------------------------------------------------------------------------------

