10020   library "S:\Core\Library.br": fncno,fnxit
10040   dim z$*10,a$*10,srch$*10,ru(12),charge(10),bd(10),tg(10)
10060   fncno(cno)
10100   open #(h_trans:=11): "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&str$(cno)&",SHR",internal,outin,keyed 
10140 F_TRANS: form c 10,n 8,n 1,12*pd 4.2,2*pd 5,pos 98,pd 4.2,n 1
10160   err_count=0
10180   do 
10220     read #h_trans,using F_TRANS: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode eof FINIS ioerr ERROR_ON_IT
10240   loop 
10260 ERROR_ON_IT: ! 
10280   err_count+=1
10282   pr rec(h_trans)
10290   delete #h_trans: 
10320   continue 
10340 FINIS: ! 
10360   pr 'total errors encountered: ';err_count
10380   end 
