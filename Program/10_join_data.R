rm(list=ls())
library(dplyr)

analysdata <- readRDS("Output/6_analysdata.rds")
scb <- readRDS("Output/4_SCB.rds")
bu <- readRDS("Output/5_BU.rds")
atc <- readRDS("Output/7_ATC.rds")
dod <- readRDS("Output/8_dod.rds")
tidsdiagnoser <- readRDS("Output/9_tidsdiagnoser.rds")
names(tidsdiagnoser) <- gsub(" ", "", names(tidsdiagnoser))



sum(table(unique(scb$LopNr))) == nrow(scb) # föräldrars utbildning
sum(table(unique(bu$LopNr))) == nrow(bu) # barns insatser
sum(table(unique(atc$LopNrBarn))) == nrow(atc) # barns föräldramedicinering

bu <- 
  bu %>% 
  rename("LopNrBarn" = LopNr)

bu_atc <- merge(bu, atc, by = "LopNrBarn", all = TRUE)

bu_atc_vars <- names(bu_atc)

analysdata <- 
analysdata %>% 
  rename("LopNrBarn" = BLOPNR)


analysdata_bu_atc <-merge(analysdata, bu_atc, by = "LopNrBarn", all.x = TRUE)


# add parents 
kopplingBio <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/U_HOGBERG_LEV_BIOFORAL.txt",
                          sep = "\t",
                          header = TRUE,
                          stringsAsFactors = FALSE
)

kopplingAdoptiv <- read.table("Indata/Sos_20170407/SoS/Data/UT_28574_2015/U_HOGBERG_LEV_ADOPTIVF.txt",
                              sep = "\t",
                              header = TRUE,
                              stringsAsFactors = FALSE
)

# order of names is important
names(kopplingAdoptiv) <- names(kopplingBio)
koppling <- rbind(kopplingBio, kopplingAdoptiv)

koppling <-
  koppling %>% 
  rename("LopNrBarn" = LopNr) %>% 
  filter(!duplicated(LopNrBarn))
  
analysdata_bu_atc <- merge(analysdata_bu_atc, koppling, by = "LopNrBarn", all.x = TRUE) 


names(scb) <- c("LopNrMor", "MOR_Utbildningsniva")
analysdata_bu_atc <- merge(analysdata_bu_atc, scb, by = "LopNrMor", all.x = TRUE)  

names(scb) <- c("LopNrFar", "FAR_Utbildningsniva")
analysdata_bu_atc_scb <- merge(analysdata_bu_atc, scb, by = "LopNrFar", all.x = TRUE) 


# join in death data
analysdata_bu_atc_scb_dod <- merge(analysdata_bu_atc_scb, dod, by = "LopNrBarn", all.x = TRUE) 

# join in time dependent diags
analysdata_bu_atc_scb_dod_tidsdiagnoser <- merge(analysdata_bu_atc_scb_dod, tidsdiagnoser, by = "LopNrBarn", all.x = TRUE) 


# fyll ut med nollor 
zero_vars <- c(
"LVUantalinsatser", 
"LVUvarddagartotalt", 
"FARADHD", 
"MORADHD",
grep("LopNrBarn",names(tidsdiagnoser), invert = TRUE, value = TRUE),
grep("LopNrBarn",names(atc), invert = TRUE, value = TRUE)
)


rm_vars <- c(
"n_NeoSDH7",
"n_NEOSDH28"#,
#"n_SDH_IT_1_11",
#"n_SDH_T_1_11"
)

zero_vars <- zero_vars[!duplicated(zero_vars)]

keep_vars <- names(analysdata_bu_atc_scb_dod_tidsdiagnoser)[!(names(analysdata_bu_atc_scb_dod_tidsdiagnoser) %in% rm_vars)]

analysdata_bu_atc_scb_dod_tidsdiagnoser <- data.frame(analysdata_bu_atc_scb_dod_tidsdiagnoser)

analysdata_bu_atc_scb_dod_tidsdiagnoser[,zero_vars] <- lapply(analysdata_bu_atc_scb_dod_tidsdiagnoser[,zero_vars], function(x) {ifelse(is.na(x), 0, x)})

out <- analysdata_bu_atc_scb_dod_tidsdiagnoser[, keep_vars]
out$Longboneinteskaft	<- ifelse(out$n_markerLongbone == 1 & out$n_frakturskaftlongbone == 0, 1, 0)
#out$Frakturutolycka		<- ifelse(out$n_a Anyfracture = 1 &  n_Fallolycka = 0 Fracturenoacc=1. EXECUTE.

out <- 
out %>% 
  select(-Mlopnr.x, -Mlopnr.y, -BFLOP, -BDIAG, -MDIAG)

out$CMFODLAND <- as.character(out$CMFODLAND) 
out$Cfnat <- as.character(out$Cfnat)
out$Cmnat <- as.character(out$Cmnat)


classes <- sapply(out, class)

out_numeric <- out[,classes == "numeric" | classes == "integer"]

out_numeric[,] <- lapply(out_numeric[,], as.numeric) 


if(FALSE){
  #system.time({
  #  write.table(out, "Output/8_analysdata_short.txt",
  #              sep = "\t",
  #              row.names = FALSE,
  #              na = "") 
  #})
  if(pathanalys){
    
   path_vars <- c("n_maltreatmentSyndrome",
    "n_assault",
    "n_undeterminedCauses",
    "n_adverseSocial",
    "n_dottBarn",
    "n_dottBarnMaltreatmsyndrome",
    "n_markerLongbone",
    "n_markerIntracranial",
    "n_retinalBlodning",
    "n_subduralBlodningIckeTrauma",
    "n_subduralBlodningTrauma",
    "n_subduralBlodning",
    "n_sinusventrombos",
    "n_skallfraktur",
    "n_hjarnskakning",
    "n_hjarnodem",
    "n_huvudskadaEjFraktur",
    "n_intrakraniellskada",
    "n_frakturRevben",
    "n_frakturRevbernMultipla",
    "n_RevbenAlla",
    "n_nyckelbenFraktur",
    "n_frakturLarben",
    "n_frakturLarbenskaft",
    "n_frakturskaftlongbone",
    "n_farkturLarbenMultipla",
    "n_frakturHumerusskaft",
    "n_frakturUnderben",
    "n_underbensfrakturer",
    "n_frakturSluten",
    "n_frakturOverUnderarm",
    "n_aktivRakit",
    "n_rubbningarKalciumMineral",
    "n_RakitKalcium",
    "n_osteogenesisImperfecta",
    "n_ALTE",
    "n_andningsuppehall",
    "n_ALTE_plus_andningsuppehall",
    "n_spadbarnskolik",
    "n_feberkramp",
    "n_kramper",
    "n_krampanfallUNS",
    "n_kramperAlla",
    "n_epilepsi",
    "n_anoxiskHjarnskada",
    "n_maltreatmentRelatedInjury")
   
   
    path_vars[!(path_vars %in% names(out))]
    
    
    out$motherSwedish <- ifelse(out$CMFODLAND == "SVERIGE", 1, 0)
    out$new_preterm <- ifelse(out$GRDBS/7 < 37, 1, 0)
    out$new_small <- out$MSGA
    out$new_twins <- ifelse(out$BORDF2 == 2, 1, 0)
    out$new_southern <- ifelse(out$LAN %in% c(7, 10 , 12, 13), 1, 0)
    out$new_southEast<- ifelse(out$LAN %in% c(5, 6, 8), 1, 0)
    out$new_western <- ifelse(out$LAN %in% c(14), 1, 0)
    out$new_uppsalaOrebro <- ifelse(out$LAN %in% c(3, 4, 17, 18 ,19 ,20, 21), 1, 0)
    out$new_stockholm <- ifelse(out$LAN %in% c(1, 9), 1, 0)
    out$new_northern <- ifelse(out$LAN %in% c(22, 23, 24, 25), 1, 0)
    
    
    out$n_ALTE_plus_andningsuppehall <- ifelse(out$n_ALTE + out$n_andningsuppehall >0, 1, 0)

    out$n_maltreatmentRelatedInjury <- ifelse(out$n_maltreatmentSyndrome + 
                                              out$n_assault + 
                                              out$n_undeterminedCauses + 
                                              out$n_adverseSocial >0, 1, 0)
    
    out$period <- ifelse(out$FODAR<1997,"<1997",
                         ifelse(out$FODAR <2008, "1997_2007",
                                ifelse(out$FODAR <2015, "2008_2014", ">2014")))
    
    # save most complete set of data
    saveRDS(out, "Output/10_analysdata.rds")
    
    path_vars <- c(path_vars, 
                   grep("^period$|FODAR|TYPE|new_|motherSwed|SDH", names(out), value = TRUE), 
                   "n_ALTE_plus_andningsuppehall",
                   "n_maltreatmentRelatedInjury")
    
    pathdata <- out[, path_vars]
    
    pathdata <- pathdata %>% 
      filter(FODAR >1996 )
    
    #openxlsx::write.xlsx(pathdata, "Output/pathData20170717.xlsx")
    write.csv2(pathdata, "Output/pathData20170717.csv",
               na = "",
               row.names = FALSE)
    
  }

  
  system.time({
    write.table(out_numeric, "Output/10_analysdata_numeric.txt",
                sep = "\t",
                row.names = FALSE,
                na = "") 
  })
}




varlist <- function(x, dataset){
  
  out <- data.frame(table(dataset[,x]))
  out$variable <- x
  out <- out[,c("variable", "Var1", "Freq")]
  
  names(out) <- c("variable", "Värde", "Antal")
  return(out)
}

ll <- lapply(c("SJUKHUS_S", "CMFODLAND", "Cfnat", "Cmnat", grep("^n_",names(out), value = TRUE),"FARLMefter", 
               "FARLMinnan", "FARSSRIefter", "FARSSRIinnan", "MORLMefter", "MORLMinnan", 
               "MORSSRIefter", "MORSSRIinnan", "FARADHD", "MORADHD", "MOR_Utbildningsniva", 
               "FAR_Utbildningsniva", "Longboneinteskaft"), varlist, dataset = out)

excellist <- do.call("rbind", ll)
excellist$label <- ""

openxlsx::write.xlsx(excellist, "Output/labels.xlsx")
