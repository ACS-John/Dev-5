10000 ! Replace S:\acsUB\d1.br
10020 ! returns the last billing date
10060   def library fnLastBillingDate(&d1; get_or_put)
10100   library 'S:\Core\Library': fncreg_read,fncreg_write,fnreg_read
10320   if get_or_put=0 then ! 0 = Get
10340     fncreg_read('Current Billing Date',d1$)
10350     if d1$='' then let fnreg_read('UB.Current Billing Date.Company [cno]',d1$)
10360     d1=val(d1$)
10380     if d1=0 then 
10400       open #20: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input,relative 
10420       read #20,using "Form POS 121,N 6",rec=1: d1
10440       close #20: 
10460       fncreg_write('Current Billing Date',str$(d1))
10480     end if 
10500   else if get_or_put=1 then ! 1 = Put
10520     fncreg_write('Current Billing Date',str$(d1))
10540   end if 
10760 fnend 
