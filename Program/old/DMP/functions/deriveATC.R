

deriveATC = function(lak){
   #give dataset
   #needs to contain variable "atc" containing prescription drugs 
   
   lak = data.table(lak)   
   
   #------------------------------ Läkemedel ATC ----------------------------------
   lak$neruoleptika = ifelse(grepl("N05A",lak$atc),1,0)
   
   lak$bensoSläkt = ifelse(grepl("N05CF",lak$atc),1,0)
   
   lak$lugnandeAtaraktika = ifelse(grepl("N05B",lak$atc),1,0)
   
   lak$sömnOchLugnandeMedel = ifelse(grepl("N05C",lak$atc),1,0)
   
   lak$bensoDerivat = ifelse(grepl("N05CD",lak$atc),1,0)
   
   #lugnandeOchSömn
   lak$N05AlugnandeOchSömn = ifelse(grepl("N05",lak$atc),1,0)
   
   #lak$antiDepressiva = ifelse(grepl(" N06A A ",lak$atc),1,0)
   #lak$antiDepressiva = ifelse(grepl(" N06A A ",lak$atc),1,0)
   
   #samla alla N06A preparat
   lak$N06AAntidepressiva = ifelse(grepl("N06AA|N06AB|N06AG|N06AX",lak$atc),1,0)
   
   
   lak$psykostimulantiaADHDAutism = ifelse(grepl("N06B",lak$atc),1,0)
   
   lak$antabus = ifelse(grepl("N07BB01",lak$atc),1,0)
   
   lak$antacida = ifelse(grepl("A02A[A-C]|A02B ",lak$atc),1,0)
   
   lak$magsår = ifelse(grepl("A02B[A-C]",lak$atc),1,0)
   
   out = lak 
   
   return(out)
}   



