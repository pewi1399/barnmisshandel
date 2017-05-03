deriveChildDiagnoses = function(grund){
      
   #--------------------------------- Rakit ---------------------------------------
   grund$aktivRakitICD10 = ifelse(grepl(" E55",grund$DIAGNOSER),1,0)
   grund$aktivRakitICD9 = ifelse(grepl(" 268A ",grund$DIAGNOSER),1,0)
   grund$aktivRakitICD8 = ifelse(grepl(" 265,00 ",grund$DIAGNOSER),1,0)
   
   grund$aktivRakit = ifelse(rowSums(grund[,names(grund)[grep("aktivRakit",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD10)
   #-------------------------------------------------------------------------------
   
   #------------------------------ Osteomalaci ------------------------------------
   grund$OsteomalaciICD8 = ifelse(grepl(" 265,20 ",grund$DIAGNOSER),1,0)
   
   grund$Osteomalaci = grund$OsteomalaciICD8
   #-------------------------------------------------------------------------------
   
   #----------------------------- kalciumRubbningar -------------------------------
   grund$kalciumRubbningarICD10 = ifelse(grepl(" E835 | E835X | E388 ",grund$DIAGNOSER),1,0)
   grund$kalciumRubbningarICD9 = ifelse(grepl(" 269D ",grund$DIAGNOSER),1,0)
   grund$kalciumRubbningarICD8 = ifelse(grepl(" 269D ",grund$DIAGNOSER),1,0)
   
   grund$kalciumRubbningar = ifelse(rowSums(grund[,names(grund)[grep("kalciumRubbningar",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- misstankeOmBarnMisshdel -------------------------
   grund$misstankeOmBarnMisshdelICD10 = ifelse(grepl(" Z038K | Z045| Y071A | T741[012389] | T741 | 96[0-9]",grund$DIAGNOSER),1,0)
   grund$misstankeOmBarnMisshdelICD9 = ifelse(grepl(" 995F | 995.81 | 96[0-9] ",grund$DIAGNOSER),1,0)
   grund$misstankeOmBarnMisshdelICD8 = ifelse(grepl(" 968,9 | 968 | 960,9 ",grund$DIAGNOSER),1,0)
   
   
   grund$misstankeOmBarnMisshdel = ifelse(rowSums(grund[,names(grund)[grep("misstankeOmBarnMisshdel",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #------------------- Special UtrObsMissh tillagd 2015-11-03  -------------------
   grund$UtrObsMisshICD10 = ifelse(grepl(" Z038K | Y071A | Y07 | Z045 ",grund$SPEC),1,0)
   #grund$UtrObsMisshICD9 = ifelse(grepl(" 995F | 995.81 | 96[0-9] ",grund$SPEC),1,0)
   #grund$UtrObsMisshICD8 = ifelse(grepl(" 968,9 | 968 | 960,9 ",grund$SPEC),1,0)
   
   
   grund$UtrObsMissh = grund$UtrObsMisshICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- barnmisshandelssyndromUNS -----------------------
   grund$barnmisshandelssyndromUNSICD10 = ifelse(grepl(" T741 | T749 ",grund$DIAGNOSER),1,0)
   grund$barnmisshandelssyndromUNSICD9 = ifelse(grepl(" 995F ",grund$DIAGNOSER),1,0)
   
   grund$barnmisshandelssyndromUNS  = ifelse(rowSums(grund[,names(grund)[grep("barnmisshandelssyndromUNS",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- syndromBarnMisshdel -------------------------
   grund$syndromBarnMisshdelICD10 = ifelse(grepl(" T741 ",grund$DIAGNOSER),1,0)
   grund$syndromBarnMisshdelICD9 = ifelse(grepl(" 967",grund$DIAGNOSER),1,0)
   
   
   grund$syndromBarnMisshdel = ifelse(rowSums(grund[,names(grund)[grep("syndromBarnMisshdel",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   
   #----------------------------- frakturRevben -----------------------------------
   grund$frakturRevbenICD10 = ifelse(grepl(" S223 | S224 ",grund$DIAGNOSER),1,0)
   grund$frakturRevbenICD9 = ifelse(grepl(" 807A ",grund$DIAGNOSER),1,0)
   grund$frakturRevbenICD8 = ifelse(grepl(" 807,10",grund$DIAGNOSER),1,0)
   
   grund$frakturRevben = ifelse(rowSums(grund[,names(grund)[grep("frakturRevben",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- multiplaFrakturerRevben -------------------------
   grund$multiplaFrakturerRevbenICD10 = ifelse(grepl(" S224 ",grund$DIAGNOSER),1,0)
   grund$multiplaFrakturerRevbenICD9 = ifelse(grepl(" 819A ",grund$DIAGNOSER),1,0)

   grund$multiplaFrakturerRevben = ifelse(rowSums(grund[,names(grund)[grep("multiplaFrakturerRevben",names(grund))]],na.rm=T)>0,1,0)
   #-------------------------------------------------------------------------------
   
   
   #------------------------------- frakturLårben ---------------------------------
   grund$frakturLårbenICD10 = ifelse(grepl(" S729 | S72 | S723 | S7230 ",grund$DIAGNOSER),1,0)
   grund$frakturLårbenICD9 = ifelse(grepl(" 821 | 821(A|C) | 823(A|C) ",grund$DIAGNOSER),1,0)
   grund$frakturLårbenICD8 = ifelse(grepl(" 821 ",grund$DIAGNOSER),1,0)  ##ÄNDRAS EFTER MÖTE 20150515!!
   
   grund$frakturLårben = ifelse(rowSums(grund[,names(grund)[grep("frakturLårben",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- multiplaLårbensFrakturer ------------------------
   grund$multiplaFrakturerLårben = ifelse(grepl(" S727",grund$DIAGNOSER),1,0)
   
   #-------------------------------------------------------------------------------
   
   #----------------------------- frakturUnderben -------------------------------
   grund$frakturUnderbenICD10 = ifelse(grepl(" S821 | S822 | S823 | S827 | S82.90 ", grund$DIAGNOSER),1,0)
   grund$frakturUnderbenICD9 = ifelse(grepl(" 823 | 823A | 823C | 824W | 828A | 828A ",grund$DIAGNOSER),1,0)
   grund$frakturUnderbenICD8 = ifelse(grepl(" 823",grund$DIAGNOSER),1,0)
   
   grund$frakturUnderben = ifelse(rowSums(grund[,names(grund)[grep("frakturUnderben",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- underbensFrakturer ------------------------------
   grund$underbensFrakturer = ifelse(grepl(" S8270 | S823 | S825 ",grund$DIAGNOSER),1,0)
   
   #-------------------------------------------------------------------------------
   
   #----------------------------- frakturSluten -----------------------------------
   grund$frakturSluten = ifelse(grepl(" S7230 ",grund$DIAGNOSER),1,0)
   
   #-------------------------------------------------------------------------------
   
   #----------------------------- frakturÖverUnderArm -------------------------------
   grund$frakturÖverUnderArmICD10 = ifelse(grepl(" S420 | S422 | S424 | S423 | S52 | S520 | S525 | S528 |S529 ",grund$DIAGNOSER),1,0)
   grund$frakturÖverUnderArmICD9 = ifelse(grepl(" 812 | 812[ACE] | 813[AC] ",grund$DIAGNOSER),1,0)
   grund$frakturÖverUnderArmICD8 = ifelse(grepl(" 812 ",grund$DIAGNOSER),1,0)##ÄNDRAS EFTER MÖTE 20150515!!
   
   grund$frakturÖverUnderArm = ifelse(rowSums(grund[,names(grund)[grep("frakturÖverUnderArm",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- skallFraktur ------------------------------------
   grund$skallFrakturICD10 = ifelse(grepl(" S020 | S021 | S027 | S028 | S0209 ",grund$DIAGNOSER),1,0)
   grund$skallFrakturICD9 = ifelse(grepl(" 800A | 803A ",grund$DIAGNOSER),1,0)
   
   
   grund$skallFraktur = ifelse(rowSums(grund[,names(grund)[grep("skallFraktur",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- skadaSynnerven ----------------------------------
   grund$skadaSynnervenICD10 = ifelse(grepl(" S04\\.0 ",grund$DIAGNOSER),1,0)  #FINNS INGEN
   
   grund$skadaSynnerven = grund$skadaSynnervenICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- intrakraniellSkada ------------------------------
   grund$intrakraniellSkadaICD10 = ifelse(grepl(" S068 | S069 ",grund$DIAGNOSER),1,0)
   grund$intrakraniellSkadaICD9 = ifelse(grepl(" 800B | 801B | 803A ",grund$DIAGNOSER),1,0)
   
   grund$intrakraniellSkada = ifelse(rowSums(grund[,names(grund)[grep("intrakraniellSkada",names(grund))]],na.rm=T)>0,1,0)
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- klämskadaHuvud ----------------------------------
   grund$klämskadaHuvudICD10 = ifelse(grepl(" S078 | S079 ",grund$DIAGNOSER),1,0)
   
   grund$klämskadaHuvud = grund$klämskadaHuvudICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- ickeSpecHuvudskada ------------------------------
   grund$ickeSpecHuvudskadaICD10 = ifelse(grepl(" S097 | S098 ",grund$DIAGNOSER),1,0)
   
   grund$ickeSpecHuvudskada = grund$ickeSpecHuvudskadaICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #---------------------------- senaBesvärSkallfraktur ---------------------------
   grund$senaBesvärSkallfrakturICD10 = ifelse(grepl(" T902 ",grund$DIAGNOSER),1,0)
   
   grund$senaBesvärSkallfraktur = grund$senaBesvärSkallfrakturICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #------------------------ senaBesvärAvAndraSkallskador -------------------------
   grund$senaBesvärAvAndraSkallskadorICD10 = ifelse(grepl(" T908 | T909 ",grund$DIAGNOSER),1,0)
   
   grund$senaBesvärAvAndraSkallskador = grund$senaBesvärAvAndraSkallskadorICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- retinalBlödning ----------------------------------
   grund$retinalBlödningICD10 = ifelse(grepl(" H356 | 362W ",grund$DIAGNOSER),1,0)
   
   grund$retinalBlödning = grund$retinalBlödningICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #---------------------- subduralBlödningIckeTraumatisk -------------------------
   grund$subduralBlödningIckeTraumatiskICD10 = ifelse(grepl(" I620 | I629 ",grund$DIAGNOSER),1,0)
   grund$subduralBlödningIckeTraumatiskICD9 = ifelse(grepl(" 432B ",grund$DIAGNOSER),1,0)
   grund$subduralBlödningIckeTraumatiskICD8 = ifelse(grepl(" 431,91 ",grund$DIAGNOSER),1,0)
   
   grund$subduralBlödningIckeTraumatisk = ifelse(rowSums(grund[,names(grund)[grep("subduralBlödningIckeTraumatisk",names(grund))]],na.rm=T)>0,1,0)
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------- subduralBlödningTraumatisk ----------------------------
   #grund$subduralBlödningTraumatiskICD10 = ifelse(grepl(" S065 ",grund$DIAGNOSER),1,0)
   grund$subduralBlödningTraumatiskICD10 = ifelse(grepl(" S065 | S0650 ",grund$DIAGNOSER),1,0)
   grund$subduralBlödningTraumatiskICD9 = ifelse(grepl(" 767A | 852A | 852B | 853B ",grund$DIAGNOSER),1,0)
   grund$subduralBlödningTraumatiskICD8 = ifelse(grepl(" 772,02 ",grund$DIAGNOSER),1,0)
   
   grund$subduralBlödningTraumatisk = ifelse(rowSums(grund[,names(grund)[grep("subduralBlödningTraumatisk",names(grund))]],na.rm=T)>0,1,0)
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- Sinusventrombos ----------------------------------
   grund$SinusventrombosICD10 = ifelse(grepl(" I636 ",grund$DIAGNOSER),1,0)
   
   grund$Sinusventrombos = grund$SinusventrombosICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #------------------------- ostogenesisImperfecta -------------------------------
   grund$osteogenesisImperfectaICD10 = ifelse(grepl(" Q780 ",grund$DIAGNOSER),1,0)
   grund$osteogenesisImperfectaICD9 = ifelse(grepl(" 756F ",grund$DIAGNOSER),1,0)
   grund$osteogenesisImperfectaICD8 = ifelse(grepl(" 756,50 ",grund$DIAGNOSER),1,0)
   
   grund$osteogenesisImperfecta = ifelse(rowSums(grund[,names(grund)[grep("osteogenesisImperfecta",names(grund))]],na.rm=T)>0,1,0)
   
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #------------------------- ostogenesisImperfecta -------------------------------
   grund$spädbarnskolikICD10 = ifelse(grepl(" R104A ",grund$DIAGNOSER),1,0)
   
   grund$spädbarnskolik = grund$spädbarnskolikICD10
   #sum(grund$AktivRakit)
   #sum(grund$aktivRakitICD8)
   #-------------------------------------------------------------------------------
   
   #----------------------------- våldtäkt -----------------------------
   grund$våldtäktICD10 = ifelse(grepl(" Y05,99 | T742 | Y059A ",grund$DIAGNOSER),1,0)
   grund$våldtäktICD9 = ifelse(grepl(" V71F | 960 | 878X | 939 ",grund$DIAGNOSER),1,0)
   grund$våldtäktICD8 = ifelse(grepl(" 960,9 ",grund$DIAGNOSER),1,0)
   
   grund$våldtäkt = ifelse(rowSums(grund[,names(grund)[grep("våldtäkt",names(grund))]],na.rm=T)>0,1,0)
   #grund$våldtäkt = grund$våldtäktICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- glaskroppsblödning -----------------------------
   grund$glaskroppsblödningICD10 = ifelse(grepl(" H431 ",grund$DIAGNOSER),1,0)
   grund$glaskroppsblödningICD9 = ifelse(grepl(" 379C ",grund$DIAGNOSER),1,0)

   grund$glaskroppsblödning = ifelse(rowSums(grund[,names(grund)[grep("glaskroppsblödning",names(grund))]],na.rm=T)>0,1,0)
   #grund$glaskroppsblödning = grund$glaskroppsblödningICD10
   #-------------------------------------------------------------------------------
 
   #----------------------------- subkonjunktivalBlödning -----------------------------
   grund$subkonjunktivalBlödningICD10 = ifelse(grepl(" H113 ",grund$DIAGNOSER),1,0)
   
   grund$subkonjunktivalBlödning = grund$subkonjunktivalBlödningICD10
   #grund$subkonjunktivalBlödning = grund$subkonjunktivalBlödningICD10
   #-------------------------------------------------------------------------------  
   
   #----------------------------- hjärnskakning -----------------------------
   grund$hjärnskakningICD10 = ifelse(grepl(" S060 ",grund$DIAGNOSER),1,0)
   
   grund$hjärnskakning = grund$hjärnskakningICD10
   #grund$hjärnskakning = grund$hjärnskakningICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- frakturLändkotpelare -----------------------------
   grund$frakturLändkotpelareICD10 = ifelse(grepl(" 805E ",grund$DIAGNOSER),1,0)
   
   grund$frakturLändkotpelare = grund$frakturLändkotpelareICD10
   #grund$frakturLändkotpelare = grund$frakturLändkotpelareICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- nyckelbenfraktur -----------------------------
   grund$nyckelbenfrakturICD10 = ifelse(grepl(" S420 ",grund$DIAGNOSER),1,0)
   grund$nyckelbenfrakturICD9 = ifelse(grepl(" 810A | 810B ",grund$DIAGNOSER),1,0)
   
   grund$nyckelbenfraktur = ifelse(rowSums(grund[,names(grund)[grep("nyckelbenfraktur",names(grund))]],na.rm=T)>0,1,0)
   #grund$nyckelbenfraktur = grund$nyckelbenfrakturICD10
   #----------------------------------------------------------------------------
   
   #----------------------------- kräkningar -----------------------------
   grund$kräkningarICD10 = ifelse(grepl(" R119B ",grund$DIAGNOSER),1,0)
   
   grund$kräkningar = grund$kräkningarICD10
   #grund$kräkningar = grund$kräkningarICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- infektion -----------------------------
   grund$infektionICD10 = ifelse(grepl(" B956 | B349 | J050 | J210 | J219 | J209 | H6660 ",grund$DIAGNOSER),1,0)
   grund$infektionICD9 = ifelse(grepl(" 008E | 008W | 009A | 00381A | 320A | 464A | 466A ",grund$DIAGNOSER),1,0)

   
   grund$infektion = ifelse(rowSums(grund[,names(grund)[grep("infektion",names(grund))]],na.rm=T)>0,1,0)
   #grund$infektion = grund$infektionICD10
   #-------------------------------------------------------------------------------
   
   
   #----------------------------- neutropeni -----------------------------
   grund$neutropeniICD10 = ifelse(grepl(" D709C ",grund$DIAGNOSER),1,0)
   
   grund$neutropeni = grund$neutropeniICD10
   #grund$neutropeni = grund$neutropeniICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- anemi -----------------------------
   grund$anemiICD10 = ifelse(grepl(" D649 ",grund$DIAGNOSER),1,0)
   
   grund$anemi = grund$anemiICD10
   #grund$anemi = grund$anemiICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- blöjderytemDermatit -----------------------------
   grund$blöjderytemDermatitICD10 = ifelse(grepl(" L229A ",grund$DIAGNOSER),1,0)
   
   grund$blöjderytemDermatit = grund$blöjderytemDermatitICD10
   #grund$blöjderytemDermatit = grund$blöjderytemDermatitICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- kraniosunostos -----------------------------
   grund$kraniosunostosICD10 = ifelse(grepl(" Q750 ",grund$DIAGNOSER),1,0)
   
   grund$kraniosunostos = grund$kraniosunostosICD10
   #grund$kraniosunostos = grund$kraniosunostosICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- missbildningar -----------------------------
   grund$missbildningarICD10 = ifelse(grepl(" Q213 | Q900 | Q211 ",grund$DIAGNOSER),1,0)
   
   grund$missbildningar = grund$missbildningarICD10
   #grund$missbildningar = grund$missbildningarICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- obstipation -----------------------------
   grund$obstipationICD10 = ifelse(grepl(" K590 | K566 ",grund$DIAGNOSER),1,0)
   
   grund$obstipation = grund$obstipationICD10
   #grund$obstipation = grund$obstipationICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- gulsot -----------------------------
   grund$gulsotICD10 = ifelse(grepl(" R179 ",grund$DIAGNOSER),1,0)
   
   grund$gulsot = grund$gulsotICD10
   #grund$gulsot = grund$gulsotICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- failureToThrive -----------------------------
   grund$failureToThriveICD10 = ifelse(grepl(" R628A ",grund$DIAGNOSER),1,0)
   
   grund$failureToThrive = grund$failureToThriveICD10
   #grund$failureToThrive = grund$failureToThriveICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- försenatKroppsligtUtvecklingsstadium -----------------------------
   grund$försenatKroppsligtUtvecklingsstadiumICD10 = ifelse(grepl(" R620 ",grund$DIAGNOSER),1,0)
   
   grund$försenatKroppsligtUtvecklingsstadium = grund$försenatKroppsligtUtvecklingsstadiumICD10
   #grund$försenatKroppsligtUtvecklingsstadium = grund$försenatKroppsligtUtvecklingsstadiumICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- ickeSpecSymtomNervMuskoskelSys -----------------------------
   grund$ickeSpecSymtomNervMuskoskelSysICD10 = ifelse(grepl(" R298 ",grund$DIAGNOSER),1,0)
   
   grund$ickeSpecSymtomNervMuskoskelSys = grund$ickeSpecSymtomNervMuskoskelSysICD10
   #grund$ickeSpecSymtomNervMuskoskelSys = grund$ickeSpecSymtomNervMuskoskelSysICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- blåttÖga -----------------------------
   grund$blåttÖgaICD10 = ifelse(grepl(" S001 ",grund$DIAGNOSER),1,0)
   
   grund$blåttÖga = grund$blåttÖgaICD10
   #grund$blåttÖga = grund$blåttÖgaICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- ytligSkadaHuvudBröst -----------------------------
   grund$ytligSkadaHuvudBröstICD10 = ifelse(grepl(" S009 | S208 ",grund$DIAGNOSER),1,0)
   grund$ytligSkadaHuvudBröstICD9 = ifelse(grepl(" 873E | 920X | 921X | 995A ",grund$DIAGNOSER),1,0)
   
   grund$ytligSkadaHuvudBröst = ifelse(rowSums(grund[,names(grund)[grep("ytligSkadaHuvudBröst",names(grund))]],na.rm=T)>0,1,0)
   #grund$ytligSkadaHuvudBröst = grund$ytligSkadaHuvudBröstICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- kontusionRyggBäckenBukväggLårUnderben -----------------------------
   grund$kontusionRyggBäckenBukväggLårUnderbenICD10 = ifelse(grepl(" S202 | S300 | S301 | S701 | S817 | S208 ",grund$DIAGNOSER),1,0)
   grund$kontusionRyggBäckenBukväggLårUnderbenICD9 = ifelse(grepl(" 911X | 922B | 923A | 924A ",grund$DIAGNOSER),1,0)
   
   
   grund$kontusionRyggBäckenBukväggLårUnderben = ifelse(rowSums(grund[,names(grund)[grep("kontusionRyggBäckenBukväggLårUnderben",names(grund))]],na.rm=T)>0,1,0)
   #grund$kontusionRyggBäckenBukväggLårUnderben = grund$kontusionRyggBäckenBukväggLårUnderbenICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- brännskada -----------------------------
   grund$brännskadaICD10 = ifelse(grepl(" T250 ",grund$DIAGNOSER),1,0)
   
   grund$brännskada = grund$brännskadaICD10
   #grund$brännskada = grund$brännskadaICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- anoxiskHjärnskada -----------------------------
   grund$anoxiskHjärnskadaICD10 = ifelse(grepl(" G93\\.1 ",grund$DIAGNOSER),1,0)
   grund$anoxiskHjärnskadaICD9 = ifelse(grepl(" 348B ",grund$DIAGNOSER),1,0)
   

   grund$anoxiskHjärnskada = ifelse(rowSums(grund[,names(grund)[grep("anoxiskHjärnskada",names(grund))]],na.rm=T)>0,1,0)
   #grund$brännskada = grund$brännskadaICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- hjärnödem -----------------------------
   grund$hjärnödemICD10 = ifelse(grepl(" G93\\.6 | S06\\.1 ",grund$DIAGNOSER),1,0)
   
   grund$hjärnödem = grund$hjärnödemICD10
   #grund$hjärnödem = grund$hjärnödemICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- skadorPåHuvudetUtanFraktur -----------------------------
   grund$skadorPåHuvudetUtanFrakturICD9 = ifelse(grepl(" 850X ",grund$DIAGNOSER),1,0)
   
   grund$skadorPåHuvudetUtanFraktur = grund$skadorPåHuvudetUtanFrakturICD9
   #grund$skadorPåHuvudetUtanFraktur = grund$skadorPåHuvudetUtanFrakturICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- andningsuppehåll -----------------------------
   grund$andningsuppehållICD10 = ifelse(grepl(" R092 ",grund$DIAGNOSER),1,0)
   
   grund$andningsuppehåll = grund$andningsuppehållICD10
   #grund$andningsuppehåll = grund$andningsuppehållICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- epilepsi -----------------------------
   grund$epilepsiICD10 = ifelse(grepl(" G40 | G41 ",grund$DIAGNOSER),1,0)
   grund$epilepsiICD9 = ifelse(grepl(" 345 | 345K ",grund$DIAGNOSER),1,0)
   
   grund$epilepsi = ifelse(rowSums(grund[,names(grund)[grep("epilepsi",names(grund))]],na.rm=T)>0,1,0)
   #grund$epilepsi = grund$epilepsiICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- ytligSårskada -----------------------------
   grund$ytligSårskadaICD10 = ifelse(grepl(" T140 ",grund$DIAGNOSER),1,0)
   grund$ytligSårskadaICD9 = ifelse(grepl(" 879C | 880A | 887X | 891A | 900X | 910W | 911X ",grund$DIAGNOSER),1,0)
   
   grund$ytligSårskada = ifelse(rowSums(grund[,names(grund)[grep("ytligSårskada",names(grund))]],na.rm=T)>0,1,0)
   #grund$ytligSårskada = grund$ytligSårskadaICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- fallskadaKlämskada -----------------------------
   grund$fallskadaKlämskadaICD10 = ifelse(grepl(" W0199 | W0601 | W0609 | W0801 | W1009 | W1900 | W2308 | W5201 ",grund$DIAGNOSER),1,0)
   grund$fallskadaKlämskadaICD9 = ifelse(grepl(" 925X ",grund$DIAGNOSER),1,0)
   
   grund$fallskadaKlämskada = ifelse(rowSums(grund[,names(grund)[grep("fallskadaKlämskada",names(grund))]],na.rm=T)>0,1,0)
   #grund$fallskadaKlämskada = grund$fallskadaKlämskadaICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- kontaktHetDryckFöda -----------------------------
   grund$kontaktHetDryckFödaICD10 = ifelse(grepl(" X1001 ",grund$DIAGNOSER),1,0)

   grund$kontaktHetDryckFöda =    grund$kontaktHetDryckFödaICD10
   #grund$kontaktHetDryckFöda = grund$kontaktHetDryckFödaICD10
   #-------------------------------------------------------------------------------
   
   
   #----------------------------- misshandelUtövadFörälder -----------------------------
   grund$misshandelUtövadFörälderICD10 = ifelse(grepl(" Y0711 | Y0710 | Y0718 | Y0719 ",grund$DIAGNOSER),1,0)
   
   grund$misshandelUtövadFörälder =    grund$misshandelUtövadFörälderICD10
   #grund$misshandelUtövadFörälder = grund$misshandelUtövadFörälderICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- misshandelUtövadIckeSpecPerson -----------------------------
   grund$misshandelUtövadIckeSpecPersonICD10 = ifelse(grepl(" Y0790 | Y0799 ",grund$DIAGNOSER),1,0)
   
   grund$misshandelUtövadIckeSpecPerson =    grund$misshandelUtövadIckeSpecPersonICD10
   #grund$misshandelUtövadIckeSpecPerson = grund$misshandelUtövadIckeSpecPersonICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- mordDråpförsök -----------------------------
   grund$mordDråpförsökICD10 = ifelse(grepl(" Y0999 ",grund$DIAGNOSER),1,0)
   
   grund$mordDråpförsök =  grund$mordDråpförsökICD10
   #grund$mordDråpförsök = grund$mordDråpförsökICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- olycka -----------------------------
   grund$olyckaICD10 = ifelse(grepl(" X5988 | X5999 ",grund$DIAGNOSER),1,0)
   
   grund$olycka =  grund$olyckaICD10
   #grund$olycka = grund$olyckaICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- övergrepp -----------------------------
   grund$övergreppICD10 = ifelse(grepl(" Y0400 ",grund$DIAGNOSER),1,0)
   
   grund$övergrepp =  grund$övergreppICD10
   #grund$övergrepp = grund$övergreppICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- andraSkadorOklarAvsikt -----------------------------
   grund$andraSkadorOklarAvsiktICD10 = ifelse(grepl(" Y3301 | Y3399 | Y3409 | Y3499 ",grund$DIAGNOSER),1,0)
   
   grund$andraSkadorOklarAvsikt =  grund$andraSkadorOklarAvsiktICD10
   #grund$andraSkadorOklarAvsikt = grund$andraSkadorOklarAvsiktICD10
   #-------------------------------------------------------------------------------
   
   #----------------------------- psykiskStörningRubbningIFamilj -----------------------------
   grund$psykiskStörningRubbningIFamiljICD10 = ifelse(grepl(" Z818 ",grund$DIAGNOSER),1,0)
   
   grund$psykiskStörningRubbningIFamilj =  grund$psykiskStörningRubbningIFamiljICD10
   #grund$psykiskStörningRubbningIFamilj = grund$psykiskStörningRubbningIFamiljICD10
   #-------------------------------------------------------------------------------
   
   #------------------------- nya diagnoser dec 2015 ---------------------------
   
   #----------------------------- maltreatmentSyndrome -------------------------
   grund$maltreatmentSyndromeICD10 = ifelse(grepl(" Z038K | Y071A | Y07| T741 | Y06",grund$DIAGNOSER),1,0)
   grund$maltreatmentSyndromeICD9 = ifelse(grepl(" 995F | E967 | 9955 | 9942 | 9943 ",grund$DIAGNOSER),1,0)
   
   grund$maltreatmentSyndrome = ifelse(rowSums(grund[,names(grund)[grep("maltreatmentSyndrome",names(grund))]],na.rm=T)>0,1,0)
   #grund$maltreatmentSyndrome = grund$maltreatmentSyndromeICD10
   #----------------------------------------------------------------------------
   
   #---------------------------------- assault ---------------------------------
   grund$assaultICD10 = ifelse(grepl(" X85| Y06| Y09| Z040 | Z045 | Z048 ",grund$DIAGNOSER),1,0)
   grund$assaultICD9 = ifelse(grepl(" E596[0-6] | E969 ",grund$DIAGNOSER),1,0)
   
   grund$assault = ifelse(rowSums(grund[,names(grund)[grep("assault",names(grund))]],na.rm=T)>0,1,0)
   #grund$assault = grund$assaultICD10
   #----------------------------------------------------------------------------
   
   #----------------------------- undeterminedCauses ---------------------------
   grund$undeterminedCausesICD10 = ifelse(grepl(" Y[1-2][0-9]| Y3[0-4]| Z040",grund$DIAGNOSER),1,0)
   grund$undeterminedCausesICD9 = ifelse(grepl(" E98[0-9] | V68A | V68X | V682 | V70E | V704 | V71G | V71F | V714 | V715 | V716 | V7181 ",grund$DIAGNOSER),1,0)
   
   grund$undeterminedCauses = ifelse(rowSums(grund[,names(grund)[grep("undeterminedCauses",names(grund))]],na.rm=T)>0,1,0)
   #grund$undeterminedCauses = grund$undeterminedCausesICD10
   #----------------------------------------------------------------------------
   
   #----------------------------- adverseSocialCircumstances -------------------
   grund$adverseSocialCircumstancesICD10 = ifelse(grepl(" Z60| Z61| Z62| Z72| Z74| Z76 | Z76[12]| Z7612 | Z81| Z86 | Z865 | Z91 | Z916 | Z918 ",grund$DIAGNOSER),1,0)
   grund$adverseSocialCircumstancesICD9 = ifelse(grepl(" V15[459] | V6[01] ",grund$DIAGNOSER),1,0)
   
   grund$adverseSocialCircumstances = ifelse(rowSums(grund[,names(grund)[grep("adverseSocialCircumstances",names(grund))]],na.rm=T)>0,1,0)
   #grund$adverseSocialCircumstances = grund$adverseSocialCircumstancesICD10
   #----------------------------------------------------------------------------
   
   #----------------------------------- dottBarn -------------------------------
   grund$dottBarnICD10 = ifelse(grepl(" X85| Y09",grund$DIAGNOSER),1,0)
   grund$dottBarnICD9 = ifelse(grepl(" E595[0-9] ",grund$DIAGNOSER),1,0)
   
   grund$dottBarn = ifelse(rowSums(grund[,names(grund)[grep("dottBarn",names(grund))]],na.rm=T)>0,1,0)
   #grund$dottBarn = grund$dottBarnICD10
   #----------------------------------------------------------------------------
   
   #----------------------------- maltreatmentRelatedInjury --------------------
   grund$maltreatmentRelatedInjury = ifelse(rowSums(grund[,names(grund)[grep("maltreatmentSyndrome|assault|undeterminedCauses|adverseSocialCircumstances",names(grund))]],na.rm=T)>0,1,0)
   #grund$maltreatmentRelatedInjury = grund$maltreatmentRelatedInjuryICD10
   #----------------------------------------------------------------------------
   
   #----------------------------- markerlongbone -------------------------------
   grund$markerlongboneICD10 = ifelse(grepl(" S42[23478]| S52| S72| S82| T10| T12",grund$DIAGNOSER),1,0)
   grund$markerlongboneICD9 = ifelse(grepl(" 812| 813| 819| 820| 821| 822| 823| 824| 827| 828",grund$DIAGNOSER),1,0)
   
   grund$markerlongbone = ifelse(rowSums(grund[,names(grund)[grep("markerlongbone",names(grund))]],na.rm=T)>0,1,0)
   #grund$markerlongbone = grund$markerlongboneICD10
   #----------------------------------------------------------------------------
   
   #----------------------------- markerintracranical -------------------------
   grund$markerintracranicalICD10 = ifelse(grepl(" S06",grund$DIAGNOSER),1,0)
   grund$markerintracranicalICD9 = ifelse(grepl(" 85[0-4]",grund$DIAGNOSER),1,0)
   
   grund$markerintracranical = ifelse(rowSums(grund[,names(grund)[grep("markerintracranical",names(grund))]],na.rm=T)>0,1,0)
   #grund$markerintracranical = grund$markerintracranicalICD10
   #----------------------------------------------------------------------------
   
   #----------------------------- frakturHumerusskaftet -------------------------
   grund$frakturHumerusskaftetICD10 = ifelse(grepl(" S423",grund$DIAGNOSER),1,0)
   grund$frakturHumerusskaftetICD9 = ifelse(grepl(" 812C",grund$DIAGNOSER),1,0)
   
   grund$frakturHumerusskaftet = ifelse(rowSums(grund[,names(grund)[grep("frakturHumerusskaftet",names(grund))]],na.rm=T)>0,1,0)
   #grund$frakturHumerusskaftet = grund$frakturHumerusskaftetICD10
   #----------------------------------------------------------------------------
   
   
   #------------------------------- Variabler nivå 2 ------------------------------
   
   #Rakit
   grund$rakit_d1 = ifelse(rowSums(grund[,names(grund)[grep("kalciumRubbningar|aktivRakit|Osteomalaci",names(grund))]],na.rm=T)>0,1,0)
   
   #Fraktur revben
   grund$frakturRevben_d1 = ifelse(rowSums(grund[,names(grund)[grep("multiplaFrakturerRevben|frakturRevben",names(grund))]],na.rm=T)>0,1,0)
   
   #Fraktur lårben
   grund$frakturLårben_d1 = ifelse(rowSums(grund[,names(grund)[grep("frakturLårben|multiplaFrakturerLårben",names(grund))]],na.rm=T)>0,1,0)
   
   #Fraktur underben
   grund$frakturUnderben_d1 = ifelse(rowSums(grund[,names(grund)[grep("underbensFrakturer|frakturUnderben",names(grund))]],na.rm=T)>0,1,0)
   
   #Övrig Skallskada
   grund$övrigSkallskada_d1 = ifelse(rowSums(grund[,names(grund)[grep("intrakraniellSkada|klämskadaHuvud|ickeSpecHuvudskad",names(grund))]],na.rm=T)>0,1,0)
   
   #variabler som redan finns
   
   #Barnisshandel
   grund$barnMisshandel_d1 = ifelse(rowSums(grund[,names(grund)[grep("BarnMisshdel",names(grund))]],na.rm=T)>0,1,0)
   
   #barnmisshandel delmängd
   grund$barnmisshandelssyndromUNS_u1 = ifelse(rowSums(grund[,names(grund)[grep("barnmisshandelssyndromUNS",names(grund))]],na.rm=T)>0,1,0)

   
   #armfraktur
   grund$frakturÖverUnderArm_d1 = grund$frakturÖverUnderArm
   
   #skallfraktur
   grund$skallFraktur_d1 = grund$skallFraktur
   
   #retinalblödning
   grund$retinalBlödning_d1 = grund$retinalBlödning
   
   #subduralblödning icje traumatisk
   grund$subduralIT_d1 =  grund$subduralBlödningIckeTraumatisk
   
   #subduralblödning traumatisk
   grund$subduralTraum_d1 =  grund$subduralBlödningTraumatisk
   
   #sinusventrombos
   grund$svt_d1 = grund$Sinusventrombos
   
   #Osteogenesis
   grund$ogi_d1 = grund$osteogenesisImperfecta
   
   #Spädbarnskolik
   grund$spädbarnskolik_d1 = grund$spädbarnskolik
   
   #ny barnmissh
   grund$UtrObsMissh_d1 = grund$UtrObsMissh
   grund$DiagnosMissh_d1 = grund$barnMisshandel_d1 - grund$UtrObsMissh_d1
   
   #-------------------------------------------------------------------------------
   
   #------------------------------- Variabler nivå 3 ------------------------------
   
   
   #fraktur rörben
   grund$frakturRörben_d2 = ifelse(rowSums(grund[,names(grund)[grep("frakturLårben_d1|frakturUnderben_d1|frakturÖverUnderArm_d1",names(grund))]],na.rm=T)>0,1,0)
   
   #Subdural blödning
   grund$subduralBlödning_d2 = ifelse(rowSums(grund[,names(grund)[grep("subduralTraum_d1|subduralIT_d1",names(grund))]],na.rm=T)>0,1,0)
   
   
   #nya diagnoser kompletterat material
   grund$ALTE = ifelse(grepl(" P284| J96",grund$DIAGNOSER),1,0)
   grund$spädbarnskolik = ifelse(grepl(" R104| 789A",grund$DIAGNOSER),1,0)
   grund$kramper = ifelse(grepl(" R56| 780D",grund$DIAGNOSER),1,0)
   grund$feberkramp = ifelse(grepl(" R560| 780D",grund$DIAGNOSER),1,0)
   grund$krampanfallUNS = ifelse(grepl(" R568",grund$DIAGNOSER),1,0)
   grund$epilepsi = ifelse(grepl(" G40| G41| 345 ",grund$DIAGNOSER),1,0)
   grund$frakturFemurskaftet = ifelse(grepl(" 821A| S723",grund$DIAGNOSER),1,0)
   
   #förekomst urval av variabler d1 2015-11-03
   grund$summationVariabler_d1 = ifelse(rowSums(grund[,names(grund)[grep("feberkramp|hjärnskakning|anoxiskHjärnskada|krampanfallUNS|epilepsi|kramper",names(grund))]],na.rm=T)>0,1,0)
   

   
   #-------------------------------------------------------------------------------
   out = grund
return(out)
}
