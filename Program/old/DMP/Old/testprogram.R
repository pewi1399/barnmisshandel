#----------------------------- Kontrollera id ----------------------------------
tst = grund1[,list(
   n = sum(table(unique(lpnr))),
   rakit = sum(aktivRakit),
   barnmisshandel = sum(misstankeOmBarnMisshdel)
),
by=c("lpnr")]

subRakit = subset(tst, rakit==max(tst$rakit))
subMissh = subset(tst, rakit==max(tst$barnmisshandel))
#42232

sub = subset(grund1,lpnr == 42232)

#-------------------------------------------------------------------------------

#------------------------- testa uttag av diagnoser ----------------------------
grund$DIAGNOSER[grepl("823,",grund$DIAGNOSER)]


grund$tst = ifelse(grepl(" Z038K | Y071A | T741 ",grund$DIAGNOSER),1,0)


sub = subset(grund,tst==1)

View(sub[,c("lpnr","DIAGNOSER")])
#grund$misstankeOmBarnMisshdelICD9 = ifelse(grepl(" 995F | 995.81 ",grund$DIAGNOSER),1,0)




#--------------------------- Skriv ut variabellista ----------------------------
vars = data.frame(vars = names(grund))

vars$type = sapply(grund[,],class)


write.csv2(vars,file.path(path,"variabellista.csv"))

