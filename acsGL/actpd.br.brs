00010 ! Replace S:\acsGL\ActPd.br
00020 ! accounting periond from company information file
00030 ! ______________________________________________________________________
00040   def library fnactpd(;actpd)
00050     library 'S:\Core\Library': fngethandle,fncno
00060     let get=1 : let put=2
00070     if actpd=0 then let get_or_put=get else let get_or_put=put
00080     let fncno(cno)
00090     open #tmp=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative 
00100     if get_or_put=get then !:
            read #tmp,using "Form POS 268,N 2",rec=1: actpd norec CLOSE_TMP
00110     if get_or_put=put then !:
            rewrite #tmp,using "Form POS 268,N 2",rec=1: actpd
00120 CLOSE_TMP: close #tmp: 
00130     let fnactpd=actpd
00140 XIT: ! 
00150   fnend 
