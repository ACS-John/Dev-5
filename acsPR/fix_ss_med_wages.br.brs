00020   library 'S:\Core\Library': fntop,fnxit, fnwait, fnerror,fncno, fnopenprn,fncloseprn,fnprocess,fntos,fnlbl,fnqgl,fnrgl$,fncmdkey,fnacs,fnagl$,fnbutton,fntxt,fnss_employee,fnss_employer
00030   dim tdc(10),tcp(32)
00090   dim cap$*128
00104   fncno(cno)
00110   ssrate1=fnss_employee*.01
00120   ssrate2=fnss_employer*.01
00130   fntop(program$,cap$="Fix WH Wages in Check History")
00150 ! 
00160   gosub READ_COMPANY_INFO
00162   mcr=mcr*.01
00170   open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&str$(cno)&",Shr",internal,outin,keyed 
00180 RD_HIST: read #4,using F_HIST: heno,tdn,prdate,ckno,mat tdc,mat tcp eof END_HIST
00190 F_HIST: form pos 1,n 8,n 3,pd 6,n 7,5*pd 3.2,37*pd 5.2
00192   if tcp(2)=round(tcp(31)*ssrate1,2) then tdc(7)=tcp(31) else tdc(7)=round(tcp(2)/ssrate1,2)
00210   if tcp(3)=round(tcp(31)*mcr,2) then tdc(8)=tcp(31) else tdc(8)=round(tcp(3)/mcr,2) ! calculate medicare wages
00220   rewrite #4,using F_HIST: heno,tdn,prdate,ckno,mat tdc,mat tcp
00230   goto RD_HIST
00240 ERTN: fnerror(program$,err,line,act$,"xit")
00250   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00260   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00270   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00280 ERTN_EXEC_ACT: execute act$ : goto ERTN
00290 READ_COMPANY_INFO: ! 
00300   dim a$(3)*40
00310   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno),internal,outin,relative 
00320   read #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2',rec=1: mat a$,fid$,mcr
00330   close #1: 
00340   return 
00350 END_HIST: stop 
00360 XIT: fnxit
