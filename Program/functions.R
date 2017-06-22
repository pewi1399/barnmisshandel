if(TRUE){ # metadata and diagnosis functions
  # diagnoses
   metadata <- openxlsx::read.xlsx("Indata/dataDictionary_170620.xlsx", sheet = "barnmissh") 
   
   metadata$variable <- paste0("n_", metadata$variable)
   # collapse all code variables
   diags <- grep("kod", names(metadata), value = TRUE)
   
   #insert leading spaces
   metadata[, diags] <- lapply(metadata[, diags], function(x) paste(" ",x, sep = ""))
   
   # assemble search phrases
   metadata$search <- do.call(paste, c(metadata[,grep("kod", names(metadata))], sep="|"))
   
   derivVars <- subset(metadata, derived == "ja")
   metadata <- subset(metadata, derived == "nej")
   
   # searches should start with an exact match but may end on any string 
  # metadata$search <- gsub("\\|\\ NA.*$", "", metadata$search)
  
   # apply search phrase on these
   applySearch <- function(variable, phrase){
     ## variable: variable containing ICD diag
     ## phrase: a string containing string or regex to be used for matching
     out<- ifelse(grepl(phrase, variable), 1,0)  
     return(out)
   }
}

#------------------------------- parallelisation -------------------------------

parallelDiagnoses <- function(metadata_tmp, dataset= NULL, suffix , type = NULL, diagvar, byvariable = "lopnr"){
  library(data.table)
  setDT(dataset)
  
  dataset[,(paste0(metadata_tmp$variable, suffix)):=lapply(metadata_tmp$search, applySearch, variable = dataset[,diagvar, with = FALSE][[1]]),]
  
  if(type == "par"){
    dataset <- dataset[,lapply(.SD, function(x){ifelse(sum(x, na.rm = TRUE)>0,1,0)}), by = byvariable, .SDcols = paste0(metadata_tmp$variable, suffix)]
    dataset <- data.frame(dataset)
    
    out <- dataset[,c(byvariable, paste0(metadata_tmp$variable, suffix))]
  }else if (type == "mfr"){
    dataset <- data.frame(dataset)
    
    out <- dataset[,c("BLOPNR","Mlopnr", paste0(metadata_tmp$variable, suffix))] 
  }
  
  return(out)
}