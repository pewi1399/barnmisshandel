library(dplyr)

analysdata <- readRDS("Output/6_analysdata.rds")
scb <- readRDS("Output/4_SCB.rds")
bu <- readRDS("Output/5_BU.rds")
atc <- readRDS("Output/7_ATC.rds")

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

# fyll ut med nollor 
zero_vars <- c(
"LVUantalinsatser", 
"LVUvarddagartotalt", 
"FARLMefter", 
"FARLMinnan", 
"FARSSRIefter", 
"FARSSRIinnan", 
"MORLMefter", 
"MORLMinnan", 
"MORSSRIefter", 
"MORSSRIinnan", 
"FARADHD", 
"MORADHD")


rm_vars <- c(
"n_NeoSDH7",
"n_NEOSDH28",
"n_SDH_IT_1_11",
"n_SDH_T_1_11"
)

keep_vars <- names(analysdata_bu_atc_scb)[!(names(analysdata_bu_atc_scb) %in% rm_vars)]

analysdata_bu_atc_scb  <- data.frame(analysdata_bu_atc_scb)
analysdata_bu_atc_scb[,zero_vars] <- lapply(analysdata_bu_atc_scb[,zero_vars], function(x) {ifelse(is.na(x), 0, x)})


out <- analysdata_bu_atc_scb[, keep_vars]
out$Longboneinteskaft	<- ifelse(out$n_markerLongbone == 1 & out$n_frakturskaftlongbone == 0, 1, 0)
#out$Frakturutolycka		<- ifelse(out$n_a Anyfracture = 1 &  n_Fallolycka = 0 Fracturenoacc=1. EXECUTE.

out <- 
out %>% 
  select(-MFLOP,-BFLOP,-Mlopnr, -BDIAG, -MDIAG)

out$CMFODLAND <- as.character(out$CMFODLAND) 
out$Cfnat <- as.character(out$Cfnat)
out$Cmnat <- as.character(out$Cmnat)


classes <- sapply(out, class)

out_numeric <- out[,classes == "numeric" | classes == "integer"]

out_numeric[,] <- lapply(out_numeric[,], as.numeric) 


if(TRUE){
  #system.time({
  #  write.table(out, "Output/8_analysdata_short.txt",
  #              sep = "\t",
  #              row.names = FALSE,
  #              na = "") 
  #})
  
  system.time({
    write.table(out_numeric, "Output/8_analysdata_numeric.txt",
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
