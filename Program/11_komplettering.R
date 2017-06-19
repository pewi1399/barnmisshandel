library(dplyr)
new <- read.table("Output/10_analysdata_numeric.txt",
                  header = TRUE,
            sep = "\t", stringsAsFactors = FALSE)

old <- read.table("Output/8_analysdata_numeric_old.txt",
                  header= TRUE,
           sep = "\t", stringsAsFactors = FALSE)

mfr <- readRDS("Output/1_mfr.rds")

mfr<-
mfr %>% 
  rename("LopNrBarn" = BLOPNR) %>% 
  filter(!duplicated(LopNrBarn))


neww <- merge(new, mfr, by = "LopNrBarn", all.x = TRUE)
  
newVars <- names(neww)[!(names(neww) %in% names(old))]

komplettering  <- c("LopNrBarn", newVars)
komplettering <- komplettering[!grepl("SJUKHUS.*|Mlopnr", komplettering)]

out <- neww[, komplettering]


out_integers <- names(out)[sapply(out, class)=="integer"]
out[,out_integers] <- lapply(out[, out_integers], as.numeric)

out_factors <- names(out)[sapply(out, class)=="factor"]
out[,out_factors] <- lapply(out[, out_factors], as.character)

system.time({
  write.table(out, "Output/11_komplettering_numeric.txt",
              sep = "\t",
              row.names = FALSE,
              na = "") 
})

openxlsx::write.xlsx(names(out), "Output/varlist.xlsx")


#--------------------- komplettering ändrade diagnoser -------------------------
analysdata <- readRDS("Output/6_analysdata.rds")
tmp <- analysdata[,c("BLOPNR","n_subduralBlodning",  "n_Alla_SDH_1_11")]

new <- new[,c("LopNrBarn","n_subduralBlodning",  "n_Alla_SDH_1_11")]


names(new) <- paste0(names(new), "_old")
new <- 
  new %>% 
  rename("BLOPNR" = LopNrBarn_old)


out <- merge(tmp, new, by = "BLOPNR")

system.time({
  write.table(tmp, "Output/11_komplettering_numeric.txt",
              sep = "\t",
              row.names = FALSE,
              na = "") 
})



