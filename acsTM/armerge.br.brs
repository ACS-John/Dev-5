on error goto Ertn
autoLibrary
fnTop(program$,"AR Merge")
fnStatus("A/R Merge Transactions In Process")
open #hClient=fnH: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed
open #hArTrans=fnH: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,outIn,relative
open #hAddr=fnH: "Name=[Temp]\Addr.[session]",internal,outIn,relative
LOOP_TOP: !
	dim p$*5,iv$*12,tr(6),id$*20
	dim ga(10)
	read #hAddr,using F_addr: p$,iv$,mat tr,id$ eof Finis,noRec Finis
	F_addr: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
	if tr(6)=9 then goto LOOP_TOP
	if trim$(p$)<>"-1" then 
		read #hClient,using F_client,key=rpad$(trim$(p$),5): am6 nokey CLSMSTR_NOKEY
		F_client: form pos 283,pd 5.2
	end if
	iv$=lpad$(rtrm$(iv$),12)
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
	if trim$(p$)<>"-1" then 
		write #hArTrans,using L360,reserve: rpad$(trim$(p$),5),iv$,mat tr,id$,0
		L360: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
		rewrite #hClient,using F_client,key=rpad$(trim$(p$),5): am6 nokey CLSMSTR_NOKEY
	end if
	if tr(5)=4 then tr(2)=tr2
	if tr(5)=4 then tr(3)=tr(3)-tr2
	rewrite #hAddr,using 'form pos 37,n 1': 9
goto LOOP_TOP

CLSMSTR_NOKEY: !
	if trim$(p$)<>"" and trim$(p$)<>"0" then
		prtcode=1
		fnopenprn
		pr #255: ,"Cannot locate account number ";p$
	end if
goto LOOP_TOP

Finis: !
close #hClient:
close #hArTrans:
close #hAddr:

if prtcode then 
	pr #255: 
	pr #255: "BE SURE TO SET UP THE A/R ACCOUNTS AS"
	pr #255: "INDICATED ON THE PRINT-OUT.  THEN REENTER ANY "
	pr #255: "TRANSACTIONS THAT WERE REJECTED."
	pr #255: 
	fncloseprn
	prtcode=0
end if
fnStatusClose
Xit: fnXit
include: ertn

