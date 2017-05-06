#########################################################
#Program:  Standardtabell_3.2.R	 
#Project:	Programbank		                         
#Catalogue: G:\Programbank\R\Tabellmakro\ 
#Author:	Ingrid L?nnstedt					  
#Task:	Funktioner f?r standardtabeller 
#New:		Denna version hanterar visit, dvs tabell by visit
#Changes:	J?mf?rt med version 3.1 har Chitv?test lagts till
#		och utskriften anpassats tiill Windows 7.  
#########################################################

#########################################################
#Info
#########################################################
cat('tab skapar en tabell liknande Statisticon standard f?r kontinuerliga\n
 (integer och numeric) och kategoriska (character, factor och ordered factor)\n 
 variabler.\n\n

 tab <- function(data, groupvar=NULL, visitvar=NULL, vars, varlabs=vars,\n
 \t idvar=NULL, dec=2,\n
 \t print=NULL, prefix.out=paste(getwd(),"/",sep=""),\n 
 \t totals=TRUE, extrarow=FALSE,\n
 \t p.values=FALSE, tests=list(continuous="parametric", categorial="fisher"),\n
 \t stat.continuous=c("Mean","SD","Median","Range","n"),\n
 \t stat.categorial=c("n","Percent"))\n\n

Argument:\n

data\t dataset from which to tabulate variables\n
groupvar\t name (character) of variable in data by which variables are\n
	 \t tabulated. Defaults to NULL which does not give any tabulation \n
	\t by group.\n
visitvar\t name (character) of variable in data which specifies which visit\n
	\t the measurments comes from.\n
vars\t character vector of names of variables in data to be tabulated.\n
varlabs\t character vector of labels of variables to be tabulated\n
	\t Defaults to vars.
idvar\t name (character) of variable in data which specifies subject identity.\n
dec\t numeric vector of number ov decimal places in table. Recycled\n
print\t name (character) of file for table to be printed to. Defaults to\n
	\t NULL which gives no output to file.\n
prefix.out\t directory of output file.\n
totals\t if TRUE (default) a column of totals is added to the table.\n
extrarow\t if TRUE a blanc row is added under each variable specific subtable.\n
p.values\t if TRUE a column of p-values is added to the table.\n
tests\t a list or character vector of tests to apply if p.values=TRUE.\n
	\t If tests is a list the tests specified by the tests$continuous\n
	\t character vector are used for continuous variables and tests\n
	\t specified by the character vector tests$categorial are used for\n
	\t categorial variables. All character vectors of tests are recycled.\n
	\t Tests currently supported are "parametric" (gives t- or anova test),\n
	\t "non-parametric" (gives Mann Whitney U or Kruskal Wallis test), "t",\n
	\t "anova", "mannwhitney", "kruskal", "wilcox.exact", "jonckheere",\n
	\t "chisq" and "fisher".\n 
stat.continuous\t Statistics to tabulate for continuous variables.\n
stat.categorial\t Statistics to tabulate for categorial variables.\n\n

Value\n
The resulting table.\n\n

Examples\n
Run in separate R session!\n\n

load("G:/Programbank/R/Tabellmakro/Testdata_3.RData")\n
vars<-names(indata[,c(2,3,4,25,30,36)])\n

tab(data=indata, groupvar="Treat", vars=vars)\n
tab(data=indata, groupvar="Treat", vars=vars, extrarow=T)\n
tab(data=indata, groupvar="Treat", vars=vars, totals=F)\n
tab(data=indata, vars=vars)\n
tab(data=indata, vars=vars, totals=F)\n
tab(data=indata, groupvar="Treat", vars=vars,\n 
	\t stat.continuous=c("Median","Range"), stat.categorial="n")\n
tab(data=indata, groupvar="Treat", vars=vars,print="Table_1")\n
tab(data=indata, groupvar="Treat", vars=vars, totals=F,\n
	\t p.values=T)\n
tab(data=indata, groupvar="Treat", vars=vars, totals=F,\n
	\t p.values=T, tests=list(continuous=c("kruskal","anova","kruskal"),\n
	\t categorial="fisher"))\n
#Or equivalently:\n
tab(data=indata, groupvar="Treat", vars=vars, totals=F,\n
	\t p.values=T, tests=c("kruskal","anova","kruskal","fisher","fisher",\n
	\t "fisher"))\n
#Data ?r f?r glesa f?r Chitv?-test (ger varningar): \n
tab(data=indata2, groupvar="Treat", vars=vars, \n
	\t  extrarow=T, p.values=T, tests=c("chisq"))\n
\n')

###Konstruktion av data med bes?ksvariabeln Visit
#indata2 <- indata
#indata2$Visit <- "Baseline"
#visnames <- c('6 weeks','3 months')
#template <- indata2
#for (i in 1:2){
#	tmp <- template
#	for (j in (1:ncol(indata))[!(names(indata) %in% 
#		c('patno','TreatShort','Age','Gender','Treat','Visit'))]){
#		tmp[,j] <- sample(template[,j],nrow(tmp), replace=T)
#		tmp$Visit <- visnames[i]
#	}
#	indata2 <- rbind(indata2, tmp)
#}
#indata2$Visit <- factor(indata2$Visit, ordered=T,
#	levels=c('Baseline', '6 weeks', '3 months'))
#str(indata2)


cat('#Tables by visit\n      

tab(data=indata2, groupvar="Treat", visitvar="Visit", vars=vars, \n
	\t extrarow=T, p.values=T)\n

#Bara kategoriska variabler:\n
vars <- names(indata[,c(25,30,36)]) \n
tab(data=indata2, groupvar="Treat", visitvar="Visit", vars=vars, \n
	\t  extrarow=T, p.values=T) \n
')

#########################################################
#Underfunktioner
#########################################################


type.func <- function(vari){
  if(any(class(vari) %in% c('integer','numeric'))) return("continuous")
  "categorial"
}

###Standard statistics functions
sdx<-function(x){(var(x,use="pairwise.complete.obs")^0.5)}

stats <- function(vari, statistics, dec){
    res <- NULL

    ####Continuous variable
    if (any(class(vari) %in% c('integer','numeric'))){

	if ("Mean" %in% statistics) {
		res <- format(round(mean(vari, na.rm=T), dec), nsmall=dec, trim=T)
		names(res) <- "Mean"
	}
	if ("SD" %in% statistics) {
		tmp <- format(round(sdx(vari), dec), nsmall=dec, trim=T)
		if ("Mean" %in% names(res)){
			res["Mean"] <- paste(res["Mean"]," (",tmp,")",sep='')
			names(res)[names(res)=="Mean"] <- "Mean (SD)"
		} else {
			res <- c(res, tmp)
			names(res)[length(res)] <- "SD"
		}
	}
	if ("CI" %in% statistics){
      
		#bootmed = apply(matrix(sample(vari, rep=TRUE, 10^4*length(vari)), nrow=10^4), 1, mean, na.rm=T)
		#lwr<-quantile(bootmed, c(.025))
      #upr<-quantile(bootmed, c(.975))
		
      lwr<-tryCatch(t.test(vari)$conf.int[1], error=function(e) NA)
		upr<-tryCatch(t.test(vari)$conf.int[2], error=function(e) NA)
      
		res <- c(res, paste(round(lwr,dec),'to',round(upr,dec)))
		names(res)[length(res)] <- "95 % CI"
	}
	if ("Median" %in% statistics){
		res <- c(res,round(median(vari, na.rm=T), dec))
		names(res)[length(res)] <- "Median"
	}
	if ("Range" %in% statistics){
		tmp1 <- as.character(round(min(vari, na.rm=T),dec))
		tmp2 <- as.character(round(max(vari, na.rm=T),dec))
		res <- c(res, paste(tmp1,'to',tmp2))
		names(res)[length(res)] <- "Range"
	}
	if ("n" %in% statistics & (is.numeric(vari) | is.integer(vari))){
		res <- c(res, as.character(length(na.omit(vari))))
		names(res)[length(res)] <- "n"
	}
	return(res)
    }

    ####Categorial variable
    if (any(class(vari) %in% c('ordered','factor','character'))){
	if ("n" %in% statistics) {
		res <- table(vari)
	}
	if ("Percent" %in% statistics) {
		tmp <- format(round(prop.table(table(vari))*100,dec),nsmall=dec,trim=T)
		tmp <-ifelse(tmp=="NaN",format(0,nsmall=dec, trim=T),tmp)
		if ("n" %in% statistics){
			res <- paste(res," (",tmp,")",sep='')
		} else  res <- tmp
		names(res) <- names(tmp)
	}
	return(res)
    }

    ####
    cat(paste('Data of class',class(vari),'is not supported.'))  
}

#########################################################
#Huvudfunktion "tab"
#########################################################


tab2 <- function(data,printcsv=TRUE, namevec=NULL,title=NULL, groupvar=NULL, visitvar=NULL, vars, varlabs=vars, 
	idvar=NULL, dec=2, print=NULL, prefix.out=paste(getwd(),"/",sep=''), 
	totals=TRUE, extrarow=FALSE,
	p.values=FALSE, tests=list(continuous="parametric", categorial="fisher"),
	stat.continuous=c("Mean","SD","Median","CI","Range","n"),
	stat.categorial=c("n","Percent")){

  if (is.null(groupvar) & !totals) {
	cat("groupvar must be submitted unless totals=T\n")
	return()
  }

  if(is.null(groupvar) & p.values){
	pvalues <- FALSE
	cat("P-values cannot be calculated unless groupvar is supplied\n")
  }

  if(is.null(idvar)){
	data$ID <- as.character(1:nrow(data))
	idvar <- "ID"
	cat("idvar is not supplied and it will be assumed that all rows of\n
	 data represent different subjects.\n")
  }

  #misc
  if (length(dec)<length(vars)) dec <- rep(dec, length=length(vars))
  types <- NULL
  for (i in 1:length(vars)) types <- c(types, type.func(data[,vars[i]]))
  if (is.null(visitvar)) visit <- rep(1,nrow(data)) else visit <- data[,visitvar]

  #G?ra om formatet p? "tests"
  if(p.values){
    if (!is.list(tests)) {
	if (length(tests)<length(vars)) tests <- 
		rep(tests, length=length(vars)) 
    } else {
		tests2 <- numeric(length(vars))
		tests2[types=="continuous"] <- tests$continuous
		tests2[types=="categorial"] <- tests$categorial
		tests <- tests2
    }
    tests[tests %in% c('t','parametric')] <- "anova"
    tests[tests %in% c('mannwhitney.test', 'non-parametric')] <- "kruskal"
  }

  ###Header
  tabell <- data.frame(rowlabs=c('',''), empty=c('',''),
	stringsAsFactors=F)
  if (!is.null(groupvar)){
	if (is.factor(data[,groupvar])) {
	  oldlevels <- levels(data[,groupvar])
	  newlevels <- oldlevels[oldlevels %in% unique(data[,groupvar])]
	  options(warn=-1)
	  data[,groupvar] <- factor(data[,groupvar], ordered=T, levels=newlevels)
	  options(warn=0)
  	}
  	data[,groupvar]<-as.factor(data[,groupvar])
	groups <- levels(data[,groupvar])
	tmp <- unique(subset(data, select=c(idvar,groupvar)))
	Ns <- paste("(N=",table(tmp[,groupvar]),")",sep="")
	for (i in 1:length(groups)) tabell[[paste("group",i,sep='')]] <- 
		c(groups[i], Ns[i]) 
  }
  if (totals) tabell$total=c('Total', 
	paste("(N=",length(unique(data[,idvar])),")",sep=""))
  if (p.values) tabell$p.value=c('','P-value')

  ###Table header if categorial variables only.
  cat.chr <- ifelse("n" %in% stat.categorial & "Percent" %in% stat.categorial,"n (%)",
	stat.categorial)
  onlycat <- length((setdiff(types, "categorial")))==0
  if (onlycat) tabell <- tabell[,names(tabell) != "empty"]
  tabrow <- matrix("", ncol=ncol(tabell), nrow=1)
  colnames(tabrow) <- names(tabell)
  if (onlycat) {
	tabell <- rbind(tabell, tabrow)
	tabell[3,substr(names(tabell),1,5) %in% c('group','total')] <- cat.chr
  }
	

  ####Deltabeller
  for (i in 1:length(vars)){
	
	tabell <- rbind(tabell, tabrow)	
	tabell[nrow(tabell),"rowlabs"] <- varlabs[i]
	if (!onlycat & types[i]=="categorial") tabell[nrow(tabell),"empty"] <- cat.chr 

	###Standard statistics
	cont <- types[i] == "continuous"
	statistics <- stat.continuous
	if (!cont) statistics <- stat.categorial

	####By visit
	for (v in 1:length(unique(visit))){
	  temptabell <- tabrow
	  vari <- data[visit==unique(visit)[v],vars[i]]
	  grupp <- data[visit==unique(visit)[v],groupvar]

	  if (!is.null(groupvar)){
		res <- tapply(vari, grupp, 
			stats,statistics=statistics, dec=dec[i])
		if (!cont & !is.list(res) & length(intersect(unique(na.omit(vari)),
			names(res)))==0) {
			res <- as.list(res)
			for (j in 1:length(res)) names(res[[j]]) <- 
				as.character(unique(na.omit(vari)))
		}
		if (!is.list(res)) res <- list(v1=res) 
		temptabell <- rbind(temptabell, matrix(rep(tabrow,
			length(res[[1]])),ncol=ncol(tabell)))
		temptabell[2:(1+length(res[[1]])),substr(names(tabell),1,5)=="group"] <- 
			unlist(res)
	  }

	  if (totals){
		res <- stats(vari, statistics=statistics, dec=dec[i])
		if (nrow(temptabell)==1) temptabell <- rbind(temptabell, 
			matrix(rep(tabrow,length(res)),ncol=ncol(tabell)))
		temptabell[2:(1+length(res)),names(tabell)=="total"] <- res
		res <- list(v1=res)
	  }

	  ###Information i v?nsterkolumnerna:
	  if (cont){
	    temptabell[2:((length(res[[1]])+1)),"empty"] <- names(res[[1]])
	    if (!is.null(visitvar)) temptabell[2,"rowlabs"] <- 
			paste("'''''",unique(visit)[v],sep='')
	    temptabell <- temptabell[-1,]
	  } else {
	    if (is.null(visitvar)){
		temptabell[2:((length(res[[1]])+1)),"rowlabs"] <- 
			paste("'''''",names(res[[1]]),sep='')
		temptabell <- temptabell[-1,]
	    } else {
		if (onlycat){
			temptabell[1,"rowlabs"] <- 
				paste("'''''",unique(visit)[v],sep='')
			temptabell[2:((length(res[[1]])+1)),"rowlabs"] <- 
				paste("''''''''''",names(res[[1]]),sep='')
		} else {
			temptabell[2:((length(res[[1]])+1)),"empty"] <- names(res[[1]])
			temptabell[2,"rowlabs"] <- paste("'''''",unique(visit)[v],sep='')
			temptabell <- temptabell[-1,]
		}
	    }
	  }	  
	  if (is.null(dim(temptabell))) temptabell <- matrix(temptabell, nrow=1)

	  ###P-v?rden
	  if (p.values){
	    switch(tests[i], 
		anova={
			pval <- anova(lm(vari~grupp))[1,"Pr(>F)"]	
		}, 
		fisher={
			pval <- ifelse(length(res[[1]])<10 & length(res[[1]])>1, 
				fisher.test(table(vari,grupp))$p.value,"-")
		}, 
		chisq={
			pval <- ifelse(length(res[[1]])>1, 
				chisq.test(table(vari,grupp))$p.value,"-")
		}, 
		kruskal={
			pval <- tryCatch(kruskal.test(vari~grupp)$p.value, error=function(e) NA)
		}, 
		wilcox.exact={
			require("coin")
			pval<-pvalue(wilcox_test(vari~factor(grupp,ordered=F),
				distribution="exact"))
			"The wilcoxon test requires subjects to be on the same order within each treatment group\n"
		}, 
		jonckheere={
			require(clinfun)
			pval<-jonckheere.test(vari, grupp, alternative = "two.sided")$p.value
		},
		cat(paste("Test",tests[i], "not implemented\n"))
	    )
	    if (is.numeric(pval)) pval <- format(round(pval,3),nsmall=3)
	    pval <- ifelse(pval=="0.000","<0.001",pval)
	    temptabell[1,ncol(temptabell)] <- pval
	  }

	  temptabell <- as.data.frame(temptabell,stringsAsFactors=F)
	  names(temptabell) <- names(tabell)
      
   
	  tabell <- rbind(tabell, temptabell)
      
	
	} 	##End of by-visit loop.

	###Other
	if (extrarow) tabell <- rbind(tabell, tabrow)
  }
  
  if(p.values){
     tabell[,ncol(tabell)]<-ifelse(tabell[,ncol(tabell)]=="","'''''",tabell[,ncol(tabell)])
     #tabell[,ncol(tabell)]<-"GGG"
     #tabell[,ncol(tabell)-3]<-"HHH"
  }

  ###Utskrift
  if(!is.null(print)){
     if(printcsv==T){
	write.table(tabell,
		paste(prefix.out,print,'_',format(Sys.time(), "%x"),
		'.csv',sep=''),sep=';',	
		row.names=F,col.names=F, quote=F)
     }else{    
        
        
        names(tabell)<-tabell[1,]
        tabell[1,]<-tabell[2,]
        
        names(tabell)[1:2]<-" "
        tabell[1,c(1,2)]<-" "
        
        tabell<-tabell[-2,]
        #tabell[1,]<-rep("",nrow(tabell))
        
        #setwd("C:/Users/perwik/Desktop/Tabelltest")
        #rtffile <- RTF("rtf.doc")  # this can be an .rtf or a .doc
        addParagraph(rtffile, title)
        addTable(rtffile, tabell)
        
       return(rtffile)
     }
  }
  
  
  
  
  rownames(tabell) <- 1:nrow(tabell)
  tabell
}

#prompt(name="tab")

###############################################################

