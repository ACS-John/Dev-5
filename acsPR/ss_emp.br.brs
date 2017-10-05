10000   def library fnss_employee
10002     cno=val(env$('cno'))
10010     if ss_empee_setup<>cno then 
10020       library 'S:\Core\Library': fngethandle
10030       open #h_company:=fngethandle: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno'),internal,input 
10040       read #h_company,using 'form pos 236,pd 3.3': ficarate
10050       close #h_company: 
10060       if ficarate=0 then ficarate=6.2
10070     end if  ! ss_empee_setup<>cno
10080     fnss_employee=ficarate
10090   fnend  ! fnss_employee
10100   def library fnss_employer
10102     cno=val(env$('cno'))
10110     if ss_empee_setup<>cno then 
10120       library 'S:\Core\Library': fngethandle
10130       open #h_company:=fngethandle: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno'),internal,input 
10140       read #h_company,using 'form pos 236,pd 3.3': ficarate
10150       close #h_company: 
10160       if ficarate=0 then ficarate=6.2
10170     end if  ! ss_empee_setup<>cno
10180     fnss_employer=ficarate
10190   fnend  ! fnss_employee
