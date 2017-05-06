*Conversion macro;





*LIBNAME Test "K:\Academy\UU\UU__4423-2 Högberg Morbarn\Indata\Ny databas 2015\test\";  
    %MACRO Convert2CSV(Libname,path); 
    DATA MEMBERS;   
    SET SASHELP.VMEMBER(WHERE=(LIBNAME = "&Libname"));   
     RETAIN OBS 0;  
     OBS = OBS+1;  
     KEEP MEMNAME OBS;  
    RUN;  
    PROC SQL;  
    SELECT MIN(OBS) INTO :MIN  
    FROM MEMBERS;  
    QUIT;  
    PROC SQL;  
    SELECT MAX(OBS) INTO :MAX  
    FROM MEMBERS;  
    QUIT;  
    %Local D;  
     %DO D = &MIN %TO &MAX;  
      PROC SQL;  
      SELECT COMPRESS(MEMNAME) INTO: Table  
       FROM MEMBERS  
       WHERE OBS=&D;  
      QUIT;  
      PROC EXPORT DBMS=CSV DATA=&Libname..&Table  
      OUTFILE="&path.&Table..CSV";  
      RUN;  
    %END;  
    %MEND;  
    *%Convert2CSV(TEST,path=K:\Academy\UU\UU__4423-2 Högberg Morbarn\Indata\Ny databas 2015\test\);  



/* Added LIBREF */
libname BARN  "K:\Academy\UU\UU__5185 Barnmisshandel\Transfer\Skiva levererad 2015-08-26\BARN\";
libname MOR  "K:\Academy\UU\UU__5185 Barnmisshandel\Transfer\Skiva levererad 2015-08-26\mor\";



%Convert2CSV(BARN,path=K:\Academy\UU\UU__5185 Barnmisshandel\Transfer\Skiva levererad 2015-08-26\BARN\);  
%Convert2CSV(MOR,path=K:\Academy\UU\UU__5185 Barnmisshandel\Transfer\Skiva levererad 2015-08-26\mor\); 	
