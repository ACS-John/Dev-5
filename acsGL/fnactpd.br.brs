00010 ! Replace S:\acsGL\fnActPd
00020 ! accounting periond from company information file
00030 ! ______________________________________________________________________
20000 def library fnactpd(;actpd)
20020   library 'S:\Core\Library': fngethandle,fncno
20040   let get=1 : let put=2
20060   if actpd=0 then let get_or_put=get else let get_or_put=put
20080   open #tmp=fngethandle: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
20100   if get_or_put=get then 
20120     read #tmp,using "Form POS 268,N 2",rec=1: actpd norec CLOSE_TMP
20140   else if get_or_put=put then 
20160     rewrite #tmp,using "Form POS 268,N 2",rec=1: actpd
20180   end if
20200   CLOSE_TMP: !
20220   close #tmp: 
20240   fnactpd=actpd
20260 fnend 
