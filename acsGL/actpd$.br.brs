00010 ! Replace S:\acsGL\actpd$.br
00020 ! ______________________________________________________________________
00030   def library fnactpd$(;actpd$)
00040     library 'S:\Core\Library': fncno,fngethandle
00050     let get=1 : let put=2
00060     if trim$(actpd$)="" then let get_or_put=1 else let get_or_put=2
00070     let fncno(cno)
00080     open #tmp=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,outin,relative 
00090     if get_or_put=get then !:
            read #tmp,using "Form POS 270,C 6",rec=1: actpd$ norec CLOSE_TMP
00100     if get_or_put=put then !:
            rewrite #tmp,using "Form POS 270,C 6",rec=1: actpd$
00110 CLOSE_TMP: close #tmp: 
00120     let fnactpd$=actpd$
00130 XIT: ! 
00140   fnend 
