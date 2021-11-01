! this is GetEmail.br to contain def library fnCustomerHasEbilling(clientId$) for returnN records
pr "running as a test only"
autoLibrary
testAcct$='1438' : pr 'fnCustomerHasEbilling('&testAcct$&') returns ';fnCustomerHasEbilling(testAcct$)
testAcct$='3045' : pr 'fnCustomerHasEbilling('&testAcct$&') returns ';fnCustomerHasEbilling(testAcct$)
testAcct$='1438' : pr 'fnCustomerHasEbilling('&testAcct$&') returns ';fnCustomerHasEbilling(testAcct$)
testAcct$='3045' : pr 'fnCustomerHasEbilling('&testAcct$&') returns ';fnCustomerHasEbilling(testAcct$)
end
def library fnCustomerHasEbilling(clientId$;___,hContact,returnN)
	! this returns the emailbilling status of a client if returnN is selected
	dim contact$(0)*256
	dim contactN(0)
	hContact=fn_openFio('Client Billing Contact',mat contact$,mat contactN, 1) ! the 1 means opened for input only
	clientId$=rpad$(trim$(clientId$),kln(hContact))
	restore #hContact,key=>clientId$: nokey CheFinis
	do
		read #hContact,using form$(hContact): mat contact$,mat contactN eof CheFinis
		if contact$(con_clientId)=clientId$ and contactN(con_emailbilling)=1 then
			returnN+=1
		end if
	loop while contact$(con_clientId)=clientId$
	CheFinis: !
	close #hContact:
	fnCustomerHasEbilling=returnN
fnend 
include: fn_open
