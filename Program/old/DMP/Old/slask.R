

descFunction = function(x){
   #prints how many unique values and how many NAs in any numeric vector "x"
   #and returns a dataframe
   
   percentage_na = sum(is.na(x))/length(x)
   unique_values = sum(table(unique(x)))
   
   return(data.frame("unique_values"=unique_values,"precentage_na"=percentage_na)) 
}



path = "K:/Academy/UU/UU__4423-2 Högberg Morbarn"
setwd(path)

load(file=file.path(path,"Output","DMP","Databas 2015","grund.Rdata"))
load(file=file.path(path,"Output","DMP","Databas 2015","mor.Rdata"))
load(file=file.path(path,"Output","DMP","Databas 2015","outbarn.Rdata"))


library(XLConnect)
#K:\Academy\UU\UU__4423-2 Högberg Morbarn\Output\DMP\Databas 2015
wb = loadWorkbook(file.path(path,"Output","DMP","Databas 2015","DMP_01.xlsx"),create=T)

createSheet(wb, name="Grunddata")
createSheet(wb, name="Mor")
createSheet(wb, name="Barn")

outgrunddata$var = rownames(outgrunddata)
outmor$var = rownames(outmor)
outbarn$var = rownames(outbarn)


outgrunddata = outgrunddata[,c("var","unique_values","precentage_na")]
outbarn = outbarn[,c("var","unique_values","precentage_na")]
outmor = outmor[,c("var","unique_values","precentage_na")]

writeWorksheet(wb,outgrunddata, sheet="Grunddata")
writeWorksheet(wb,outmor, sheet="Mor")
writeWorksheet(wb,outbarn, sheet="Barn")

saveWorkbook(wb)
#llbarn = lapply(prt2_barn[,], descFunction)

#outbarn = do.call("rbind",llbarn)

#save(outbarn,file=file.path(path,"Output","DMP","Databas 2015","outbarn.Rdata"))
