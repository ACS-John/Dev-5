00010 ! Replace S:\acsGL\fnActPd$
33000 def library fnactpd$(;actpd$)
33020   library 'S:\Core\Library': fncno,fngethandle
33040   let get=1 : let put=2
33060   if trim$(actpd$)="" then let get_or_put=1 else let get_or_put=2
33080   open #tmp=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
33100   if get_or_put=get then 
33120     read #tmp,using "Form POS 270,C 6",rec=1: actpd$ norec CLOSE_TMP
33140   else if get_or_put=put then 
33160     rewrite #tmp,using "Form POS 270,C 6",rec=1: actpd$
33180   end if
33200   CLOSE_TMP: !
33220   close #tmp: 
33240   let fnactpd$=actpd$
33260   XIT: ! 
33280 fnend 
