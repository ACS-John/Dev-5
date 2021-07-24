on error goto Ertn
autoLibrary
fnTop(program$)
fnStatus('Merging transactions...')
open #hClient=fnH: 'Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr',internal,outIn,keyed
FclientBal: form pos 283,pd 5.2
open #hArTrans=fnH: 'Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr',internal,outIn,relative
Ftrans: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
open #hAddr=fnH: 'Name=[Temp]\Addr.[session]',internal,outIn,relative
Faddr: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
do ! r: main loop
	ReadNext: !
	dim clientId$*5
	dim inv$*12
	dim tr(6)
	dim id$*20
	read #hAddr,using Faddr: clientId$,inv$,mat tr,id$ eof Finis,noRec Finis
	if tr(6)<>9 then 
		if trim$(clientId$)<>'-1' then 
			read #hClient,using FclientBal,key=rpad$(trim$(clientId$),5): am6 nokey NokeyClient
		end if
		inv$=lpad$(rtrm$(inv$),12)
		
		if tr(5)<3 or tr(5)=5 then 
			am6+-tr(3) 
		else 
			am6-=tr(3)
		end if
		if tr(5)=3 then am6-=tr(2)
		
		tr2=tr(2)
		if tr(5)=3 then tr(3)=tr(3)+tr2
		tr(2)=tr(3)
		if tr(5)=4 then tr(5)=6
		if tr(5)=3 then tr(5)=4
		if tr(5)=2 then tr(5)=5
		
		if trim$(clientId$)<>'-1' then 
			write #hArTrans,using Ftrans,reserve: rpad$(trim$(clientId$),5),inv$,mat tr,id$,0
			rewrite #hClient,using FclientBal,key=rpad$(trim$(clientId$),5): am6 nokey NokeyClient
		end if
		if tr(5)=4 then tr(2)=tr2
		if tr(5)=4 then tr(3)=tr(3)-tr2
		rewrite #hAddr,using 'form pos 37,n 1': 9
	end if
loop ! /r

NokeyClient: ! r:
	if trim$(clientId$)<>'' and trim$(clientId$)<>'0' then
		clientNokeyCount+=1
		fnopenprn
		pr #255: 'Cannot locate account '&clientId$
		fnStatus('Cannot locate account '&clientId$)
		pause
	end if
goto ReadNext ! /r

Finis: ! r:
close #hClient:
close #hArTrans:
close #hAddr:

if clientNokeyCount then 
	clientNokeyCount=0
	pr #255: 
	pr #255: 'Set up the '&str$(clientNokeyCount)&' A/R accounts as'
	pr #255: 'indicated on the print-out.  then reenter any '
	pr #255: 'transactions that were rejected.'
	pr #255: 
	fncloseprn
end if
fnStatusClose
goto Xit ! /r

Xit: fnXit
include: ertn

