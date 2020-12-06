! Replace S:\acsUB\d1.br
! returns the last billing date
def library fnLastBillingDate(; &d1,get_or_put)
	autoLibrary
	if get_or_put=0 then ! 0 = Get
		fncreg_read('Current Billing Date',d1$)
		if d1$='' then let fnreg_read('UB.Current Billing Date.Company [cno]',d1$)
		d1=val(d1$)
		if d1=0 and exists('[Q]\UBmstr\Company.h[cno]') then 
			open #20: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input,relative 
			read #20,using "Form POS 121,N 6",rec=1: d1
			close #20: 
			fncreg_write('Current Billing Date',str$(d1))
		end if 
	else if get_or_put=1 then ! 1 = Put
		fncreg_write('Current Billing Date',str$(d1))
	end if
	fnLastBillingDate=d1
fnend 
