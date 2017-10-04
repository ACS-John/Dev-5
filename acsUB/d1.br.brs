10000 ! Replace S:\acsUB\d1.br
10020 ! returns the last billing date
10040 ! ______________________________________________________________________
10060   def library fnd1(&d1; get_or_put)
10080 ! ______________________________________________________________________
10100     library 'S:\Core\Library': fnerror,fnpause,fncreg_read,fncreg_write,fnreg_read
10120     on error goto ERTN
10140 ! ______________________________________________________________________
10160 ! get_or_put = 0 = Get
10180 !            = 1 = Put
10200 ! ______________________________________________________________________
10220     if get_or_put<>0 and get_or_put<>1 then 
10240       pr "Get_or_Put =0=Get  =1=Put   anything else - not allowed."
10260       fnpause
10280     end if 
10320     if get_or_put=0 then 
10340       fncreg_read('Current Billing Date',d1$)
10350       if d1$='' then let fnreg_read('UB.Current Billing Date.Company '&env$('cno'),d1$)
10360       let d1=val(d1$)
10380       if d1=0 or env$('ACSDeveloper')<>'' then 
10400         open #20: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,input,relative 
10420         read #20,using "Form POS 121,N 6",rec=1: d1
10440         close #20: 
10460         fncreg_write('Current Billing Date',str$(d1))
10480       end if 
10500     else if get_or_put=1 then 
10520       fncreg_write('Current Billing Date',str$(d1))
10532       ! open #20: "Name="&env$('Q')&"\UBmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
10534       ! rewrite #20,using "Form POS 121,N 6",rec=1: d1
10536       ! close #20: 
10540     end if 
10560     goto XIT
10580 ! ______________________________________________________________________
10600 ! <Updateable Region: ERTN>
10620 ERTN: let fnerror(program$,err,line,act$,"xit")
10640     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
10660     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
10680     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
10700 ERTN_EXEC_ACT: execute act$ : goto ERTN
10720 ! /region
10740 ! ______________________________________________________________________
10760 XIT: fnend 
10780 ! ______________________________________________________________________
