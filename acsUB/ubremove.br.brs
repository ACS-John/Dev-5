20000 ! Replace S:\acsUB\ubRemove
20020 ! remove old transactions
20040   library 'S:\Core\Library': fnacs,fnlbl,fntxt,fnwait,fntos,fncno,fnerror,fnxit,fndate_mmddyy_to_ccyymmdd,fncmdset,fntop,fnfra
20060   on error goto ERTN
20080 ! ______________________________________________________________________
20100   dim cap$*128
20120 ! ______________________________________________________________________
20140   fntop("S:\acsUB\ubRemove",cap$="Remove Old Transactions")
20160   cancel=5 : left=0 : right=1 : center=2
20180   fncno(cno)
20200 SCREEN1: ! 
20220   fntos(sn$="ubRemove")
20240   frac=0
20260   fnfra(1,1,10,fraonewidth=70,'') ! cap$)
20280   fraone=frac+=1 : flc=0
20300   fraonewidth-=2
20320   fnlbl(flc+=1,1,"Warning",fraonewidth,center,3,fraone)
20340   fnlbl(flc+=2,1,"This program will erase old transactions ",fraonewidth,center,0,fraone)
20360   fnlbl(flc+=1,1,"from the Customer Transaction History File.  ",fraonewidth,center,0,fraone)
20380   fnlbl(flc+=1,1,"Enter the oldest date to be retained (mm/dd/yy):",55,right,0,fraone)
20400   fntxt(flc,58,8,8,1,"1",0,"",fraone)
20420   resp$(1)=""
20440   fnlbl(flc+=2,1,"Transactions older than this date will be deleted!",fraonewidth,center,2,fraone)
20460   fncmdset(2)
20480   fnacs(sn$,0,mat resp$,ck)
20500   if ck=cancel then goto XIT
20520   rd1=val(resp$(1))
20540   rd1=fndate_mmddyy_to_ccyymmdd(rd1)
20560   open #h_trans:=2: "Name="&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&",Shr",internal,outin,keyed 
20580 READ_TRANS: ! 
20600   read #h_trans,using 'Form POS 1,C 10,N 8,N 1,PD 4.2': p$,tdate,tcode,tamount eof END1
20620   if tdate<rd1 then delete #h_trans: 
20640   goto READ_TRANS
20660 END1: ! 
20680   close #h_trans: 
20700   execute "Index "&env$('Q')&"\UBmstr\UBTransVB.h"&str$(cno)&' '&env$('Q')&"\UBmstr\UBTrIndx.h"&str$(cno)&" 1 19 Replace DupKeys -n"
20720   goto XIT
20740 ! ______________________________________________________________________
20760 XIT: fnxit
20780 ! ______________________________________________________________________
20800 ! <Updateable Region: ERTN>
20820 ERTN: fnerror(program$,err,line,act$,"xit")
20840   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
20860   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
20880   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
20900 ERTN_EXEC_ACT: execute act$ : goto ERTN
20920 ! /region
20940 ! ______________________________________________________________________
