! this is GetEmail.br to contain def library fnCustomerHasEmail(clientId$) for returnN records
pr "running as a test only"
library : fnCustomerHasEmail
let answer=fnCustomerHasEmail("1438")
pr answer 
stop 
def library fnCustomerHasEmail(clientId$;___,hContact,returnN)
	! this returns the emailbilling status of a client if returnN is selected
	! open Contact file
	library 'S:\Core\Library': fngethandle
	dim contact$(0)*256
	dim contactN(0)
	hContact=fn_open(table$,mat contact$,mat contactN,mat form$, 1) ! the 1 means opened for input only
	! open #hContact:=fngethandle: "Name=S:\Core\Data\acsllc\Contact.h[cno],KFName=S:\Core\Data\acsllc\Contact-idx.h[cno],Shr",internal,input,keyed
	! read by clientId$ key
	! ReadContact: form x 260,n 1
	clientId$=rpad$(trim$(clientId$),kln(hContact))
	restore #hContact,key=clientId$: 
	do
		! read #hContact,using ReadContact,key=rpad$(trim$(clientId$),5," "): returnN nokey ignore eof ignore
		read #hContact,using form$(hContact): mat contact$,mat contactN eof CheFinis
		if contact$(con_clientId)=clientId$ then
			returnN+=1
		end if
	loop while contact$(con_clientId)=clientId$
	read #hContact,using ReadContact,key=rpad$(trim$(clientId$),5," "): returnN nokey ignore eof ignore
	! close file
	CheFinis: !
	close #hContact:
	let fnCustomerHasEmail=returnN
fnend 
include: fn_open
