! this is GetEmail.br to contain def library fnCustomerHasEbilling(Client_id$) for ebilling records
pr "running as a test only"
library : fnCustomerHasEbilling
let answer=fnCustomerHasEbilling("1438")
pr answer 
stop 
def library fnCustomerHasEbilling(Client_id$;___,h_contact,ebilling)
	! this returns the emailbilling status of a client if ebilling is selected
	! open Contact file
	library 'S:\Core\Library': fngethandle
	open #h_contact:=fngethandle: "Name=S:\Core\Data\acsllc\Contact.h[cno],KFName=S:\Core\Data\acsllc\Contact-idx.h[cno],Shr",internal,input,keyed
	! read by client_id$ key
	readcontact: form x 260,n 1
	read #h_contact,using readcontact,key=rpad$(trim$(client_id$),5," "): ebilling nokey ignore eof ignore
	! close file
	close #h_contact:
	let fnCustomerHasEbilling=ebilling
fnend 