20000   dim ls_field_name$*128,ls_field_value$*256
20020   for x=1 to 3
20040 ! pr '.';
20060     open #ls_h+=1: 'Name=test.dat,Version=1,KFName=C:\users\john\appdata\roaming\acs\temp\ls.idx,Use,RecL=384,KPs=1,KLn=128,Shr',internal,outin,keyed 
20080     ls_field_name$=rpad$('011.cursys',128) : ls_field_value$='UB'
20090     read #ls_h,using 'form pos 1,c 128,c 256',key=ls_field_name$: ls_field_name$,ls_field_value$ nokey LS_REWR
20100 LS_REWR: ! 
20110     rewrite #ls_h,using 'form pos 1,c 128,c 256',key=ls_field_name$: ls_field_name$,ls_field_value$ nokey LS_WRITE
20120     goto LS_SAVE_XIT
20140 LS_WRITE: ! 
20160     write #ls_h,using 'form pos 1,c 128,c 256': ls_field_name$,ls_field_value$
20180 LS_SAVE_XIT: ! 
20200     for y=1 to 3
20220       read #ls_h,using 'form pos 1,c 128,c 256',key=ls_field_name$: ls_field_name$,ls_field_value$
20240       rewrite #ls_h,using 'form pos 1,c 128,c 256',key=ls_field_name$: ls_field_name$,ls_field_value$ nokey LS_WRITE
20260     next y
20280 ! close #ls_h:
20300   next x
