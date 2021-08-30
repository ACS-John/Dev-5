! Replace S:\acsUB\d1.br
! returns the last billing date
def library fnLastBillingDate(; &d1,put,___,d1$)
	autoLibrary
	if put then
		fncreg_write('Current Billing Date',str$(d1))
	else ! Get
		d1=fncreg_read('Current Billing Date',d1$)
		if ~d1 then
			d1=fnreg_read('UB.Current Billing Date.Company [cno]',d1$)
			if ~d1 and exists('[Q]\UBmstr\Company.h[cno]') then 
				open #hTmp=fnH: "Name=[Q]\UBmstr\Company.h[cno],Shr",i,i,r 
				read #hTmp,using "Form POS 121,N 6",rec=1: d1
				close #hTmp: 
			end if 
			fncreg_write('Current Billing Date',str$(d1))
		end if
	end if
	fnLastBillingDate=d1
fnend 
