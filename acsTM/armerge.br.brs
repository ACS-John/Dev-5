on error goto Ertn
dim gl$(10)*12,ga(10),pgl$*12
autoLibrary
fnTop(program$,cap$="AR Merge")
dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),hd$(2)*50
pr newpage
pr f "10,10,c 50,H,N": "A/R Merge Transactions In Process"
open #hClient=1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,outIn,keyed
open #hArTrans=2: "Name=S:\Core\Data\acsllc\ARTrans.h[cno],Shr",internal,outIn,relative
open #hAddr=fnH: "Name=[Temp]\Addr."&session$,internal,outIn,relative
! open #h_armotran:=4: "Name=S:\Core\Data\acsllc\ARMoTran.h[cno],Shr",internal,output
LOOP_TOP: !
	read #hAddr,using F_addr: p$,iv$,mat tr,id$,pgl$,gl$(1),ga(1),gl$(2),ga(2),gl$(3),ga(3),gl$(4),ga(4),gl$(5),ga(5),gl$(6),ga(6),gl$(7),ga(7),gl$(8),ga(8),gl$(9),ga(9),gl$(10),ga(10) eof Finis,noRec Finis
	F_addr: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,c 12,10*(c 12,pd 5.2)
	if tr(6)=9 then goto LOOP_TOP
	if ltrm$(p$)<>"-1" then 
		read #hClient,using F_client,key=p$: am6 nokey CLSMSTR_NOKEY
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
	if ltrm$(p$)="-1" then goto L420
	L340: !
	write #hArTrans,using L360,reserve: p$,iv$,mat tr,id$,0 duprec L340
	L360: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20,pd 3
	rewrite #hClient,using F_client,key=p$: am6 nokey CLSMSTR_NOKEY
	L420: if tr(5)=4 then tr(2)=tr2
	if tr(5)=4 then tr(3)=tr(3)-tr2
	rewrite #hAddr,using 'form pos 37,n 1': 9
goto LOOP_TOP

CLSMSTR_NOKEY: !
	if rtrm$(p$)="" or trim$(p$)="0" then goto LOOP_TOP
	prtcode=1
	fnopenprn
	pr #255: ,"Cannot locate account number ";p$
goto LOOP_TOP

Finis: !
close #hClient:
close #hArTrans:
close #hAddr:
! close #h_armotran:
if prtcode then 
	pr #255: 
	pr #255: "BE SURE TO SET UP THE A/R ACCOUNTS AS"
	pr #255: "INDICATED ON THE PRINT-OUT.  THEN REENTER ANY "
	pr #255: "TRANSACTIONS THAT WERE REJECTED."
	pr #255: 
	fncloseprn
end if
Xit: fnXit
include: ertn

