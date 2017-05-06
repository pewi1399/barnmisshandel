if(TRUE){ # metadata and diagnosis functions
  # diagnoses
   metadata <- openxlsx::read.xlsx("Indata/dataDictionary_170404.xlsx", sheet = "barnmissh") 
   
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