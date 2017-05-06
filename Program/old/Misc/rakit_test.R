library(data.table)
library(tidyr)
load("/home/per/Polycarp/KBH/P101_barnmisshandel/Output/DMP/DMP_02_barndiagnoser.Rdata")
rak <- DMP_02_barndiagnoser[,c("lpnr", "INDATUM", "rakit_d1", "SOURCE")]
load("/home/per/Polycarp/KBH/P101_barnmisshandel/Output/DMP/DMP_101_assembly2015.Rdata")# dat9
old <- dat9[,c("lpnr", "rakit_d1")]
load("/home/per/Polycarp/KBH/P101_barnmisshandel/Output/DMP/DMP_100_barndiagnoser.Rdata")#dia5
dia <- dia5[,c("lpnr", "rakit_d1", "BFODDAT", "alderDiagnosManad_rakit_d1"), with=F]
#-------------------------------------------------------------------------------

# create true date format
rak$INDATUM <- as.Date(rak$INDATUM, "%Y-%m-%d")

# order and remove dupes
setDT(rak)
rak <- rak[order(lpnr,-rakit_d1, INDATUM)]
rak1 <- rak[!duplicated(rak$lpnr),]

sum(rak1$rakit_d1)
sum(old$rakit_d1, na.rm=T)

# 
rak3 <- subset(rak1, lpnr %in% old$lpnr)
sum(rak3$rakit_d1)

rakit <- subset(rak3, rakit_d1 == 1)
lnr <- 54158

rakR <- subset(rak, lpnr ==  lnr)
oldR <- subset(old, lpnr ==  lnr)
diaR <- subset(dia, lpnr ==  lnr)


#write.csv2(rak3, "rakitIndividuellaFall.csv")
rak3$year <- as.numeric(substr(rak3$INDATUM, 1,4))
rak4 <- rak3[,list(rakit = sum(rakit_d1, na.rm= T)
           ),by = c("year", "SOURCE")]

setkey(rak4, year)

rak5 <- rak4 %>% 
  spread(SOURCE, rakit)

rak5 <- data.frame(rak5)

rak5[is.na(rak5)] = 0



ggplot(rak4)+
  geom_line(aes(x = year, y = rakit))+
  scale_x_continuous(breaks = 1970:2014)+
  #scale_y_discrete(breaks = 0:10)+
  theme_bw()

write.csv2(rak5, "/home/per/Polycarp/KBH/P101_barnmisshandel/Output/DMP/rakitSourceYear.csv")


