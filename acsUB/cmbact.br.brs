10000 ! Replace S:\acsUB\CmbAct.br
10020 ! creates a screen ace combobox for "&env$('Q')&"\UBmstr accounts
10040   def library fncmbact(myline,mypos; addall,container,indexfile$*256)
10080     library 'S:\Core\Library': fncombof
10140     dim if$*256
20020     if addall<>1 then addall=0
20060     if addall=0 then 
20080       let fen$="CAct"
20100     else 
20120       let fen$="CActALL"
20140     end if 
20160     if indexfile$="" then 
20180       if$=env$('Q')&"\UBmstr\ubIndex.h"&env$('cno') ! str$(cno)
20200     else 
20220       if$=indexfile$
20240     end if 
20260     fncmbact=fncombof(fen$,myline,mypos,43,env$('Q')&"\UBmstr\Customer.h"&env$('cno'),1,10,41,30,if$,1+addall,1,"Select from the list of accounts, to add an account go to the Customer File.",container)
30140  fnend 
