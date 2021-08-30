	autoLibrary
	fnTop(program$)
	dim fl1$(7),flo1$(11),sc3$(5),pt(6),f3$*255,flo3$(6),name$*25
	dim p$*5,iv$*12,tr(6),id$*20,sc1$(5),sc2$(9),hd$(2)*50
	dim flo4$(5),sc4$(5),ot4$(5),fli4$(5),q(3),gln1(3),gln2(3),otgl$(3)
	dim gl(10,4),fli1$(49),ot1$(49),pgl(3)
	open #h_company:=1: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,input ioerr Ertn
	read #h_company,using 'form pos 161,3*n 1,pos 178,n 3,n 6,n 3,n 3,n 6,n 3': i3,i4,i5,mat gln1,mat gln2 ioerr Ertn
	! i3=1 ! ENTER G/L #'S
	close #h_company:
	namtab=66-int(len(rtrm$(env$('cnam')))/2)
	otgl$(1)="9,30,pic(zzz)"
	otgl$(2)="9,34,pic(zzzzzz)"
	otgl$(3)="9,41,pic(zzz)"
	if i3=0 then ! NO GL TO BE ENTERED
		sz=6
	else if i4=1 and i5=1 then ! YES DEPT   YES SUBACCOUNT
		sz=2
		gx=4
		gpx=2
	else if i4=0 and i5=1 then ! NO DEPT    YES SUBACCOUNT
		sz=3
		gx=3
		mat gl(10,3)=(0)
		mat pgl(2)=(0)
		gpx=1
	else if i4=1 and i5=0 then ! YES DEPT    NO SUB ACCOUNT
		sz=4
		gx=3
		mat gl(10,3)=(0)
		mat pgl(2)=(0)
		gpx=2
	else ! NO DEPT    NO SUBACCOUNT
		sz=5
		gx=2
		mat gl(10,2)=(0)
		mat pgl(1)=(0)
		gpx=1
	end if

	open #h_addr:=fnH: "Name=[Temp]\Addr.[session],Replace,RecL=239",i,outi,r ioerr Ertn

	open #1: "Name=S:\acsTM\TMSCRN.CL,Shr",i,i,r ioerr Ertn
	read #1,using L560,rec=sz: f3$,mat fl1$,mat sc1$,mat sc2$,mat fli1$,mat ot1$,mat flo1$,mat flo3$,mat sc3$ ioerr Ertn
	L560: form pos 1,c 255,142*c 18
	close #1:

	open #hCl1=fnH: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",i,i,k ioerr Ertn
	open #hCl2=fnH: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",i,i,k ioerr Ertn  ! alpha index on clients
	L590: !
	hd$(1)="A/R Input Selection Menu"
	hd$(2)="ENTER SELECTION"
	L610: !
	pr newpage
	pr f mat fl1$: mat sc1$,mat hd$
	L630: !
	input fields "13,29,n 1,eu,n": tr5 conv L630
	if tr5=0 then vf=1 : goto ScreenTotals
	if tr5<1 or tr5>4 then goto L630
	hd$(1)="A/R INPUT "&sc1$(tr5+1)(5:18)
	hd$(2)="Client Number as 0 to stop"
	L680: !
	if tr5=4 or tr5=3 then sc2$(7)="G/L # TO CREDIT" else sc2$(7)="G/L # TO Debit"
	if tr5=3 then sc2$(6)="DISCOUNT AMOUNT" else sc2$(6)=""
	if gx=0 then sc2$(7)=" "
	L710: !
	pr newpage
	pr f mat flo1$: mat sc2$,mat hd$
	ps1=0
	if vf=0 then goto ScreenSomething
	if gx><0 then goto L780
	L760: !
	pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2)
	goto ScreenSomething
	L780: !
	pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
	ScreenSomething: !
	pr f "5,30,pic(zzzzzz)": tr(1)
	pr f "24,20,C 50,N": "F1 Continue   F2 verify name    F4 Search"
	if gx><0 then goto L910
	L820: !
	input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2) conv L870
	if cmdkey=4 then gosub TMSRCH : goto L760
	p$=rpad$(trim$(p$),5)
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=0
	goto L1280
	L870: !
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=cnt+1
	fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
	goto L820
	L910: !
	if ps1=1 or vf=1 then goto L1060
	L920: !
	rinput fields "3,30,C 5,EU,n": p$ conv L920
	if cmdkey=4 then gosub TMSRCH : goto L920
	p$=rpad$(trim$(p$),5)
	if ltrm$(p$)="-1" then pr f mat otgl$: mat gln1 else pr f mat otgl$: mat gln2
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto L590
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
	if ltrm$(p$)="-1" then name$="CASH SALE" else goto Read9
	goto L1050
	Read9: !
	read #hCl1,using FclientName,key=rpad$(trim$(p$),kln(hCl1)),release: name$ nokey L1020 ioerr Ertn
	FclientName: form pos 6,c 25
	goto L1050
	L1020: !
	! pr 'nokey on read '&file$(hCl1)
	name$="INVALID CLIENT NUMBER"
	pr f "3,40,C 25,R,N": name$
	goto L920
	L1050: !
	pr f "3,40,C 25,N": name$
	L1060: !
	fli1$(4)="6,30,n 11.2,ut,n"
	if r1>0 then goto L1170
	if tr5=3 then fli1$(4)="6,30,n 11.2,ue,n"
	input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
	if cmdkey=2 then goto L920
	if tr5<>3 then goto L1200
	fli1$(4)="6,30,n 11.2,ut,n"
	if sz=4 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=tr(3)
	if sz=3 then gl(1,1)=gln1(2): gl(1,2)=gln1(3): gl(1,3)=tr(3)
	if sz=2 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=gln1(3): gl(1,4)=tr(3)
	if sz=5 then gl(1,1)=gln1(2): gl(1,2)=tr(3)
	L1170: !
	pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
	L1180: !
	input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl conv L1240
	if cmdkey=2 then goto L920
	L1200: !
	p$=rpad$(trim$(p$),5)
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=0
	goto L1280
	L1240: !
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,"RC","U")
	ce=cnt+1
	fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,"U","RC")
	if cnt<=4 then goto L1060 else goto L1180
	L1280: !
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto L590
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto L1630
	ps1=1
	if tr(1)<10100 or tr(1)>123199 then
		pr f "5,48,c 20": "Invalid Date"
		goto ScreenSomething
	end if
	L1340: !
	if tr(3) then
		if gx=0 then goto L1520
		if pgl(gpx)>0 then goto L1410
		pr f "9,45,c 30": "G/L # REQUIRED"
	else
		pr f "6,48,c 20": "NO AMOUNT ENTERED"
	end if
	goto ScreenSomething
	L1410: !
	gla=0
	for j=1 to 10
		if gl(j,gx)=0 then goto L1460
		gla=gla+gl(j,gx)
	next j
	L1460: !
	if tr5=3 then gla=gla-tr(2)
	if gla=tr(3) then goto L1520
	pr f "11,2,c 75,h,n": " G/L allocations do not agree with total amount.  Press enter to continue."
	input fields "11,78,c 1,EU,n": pause$
	pr f "11,2,c 75,n,n": " "
	goto ScreenSomething
	L1520: !
	if ltrm$(p$)="-1" then goto L1540
	pt(1)=pt(1)+val(p$) conv ignore
	L1540: !
	pt(tr5+1)=pt(tr5+1)+tr(3)
	if tr5=3 then tdt=tdt+tr(2)
	if ltrm$(p$)="-1" then pt(6)=pt(6)+tr(3)
	if vf=1 then goto L1670
	r3=r3+1
	tr(5)=tr5
	write #h_addr,using f3$,rec=r3: p$,iv$,mat tr,id$,mat pgl,mat gl
	p$=""
	q2=0
	goto L710
	L1630: !
	iv$=" "
	mat tr=(0)
	id$=" "
	mat gl=(0)
	L1670: !
	rewrite #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl
	p$=""
goto L2060
ScreenTotals: ! r:
	pr newpage
	hd$(1)="A/R Input Proof Totals"
	hd$(2)=""
	pr f mat fl1$: mat sc3$,mat hd$
	pr f "11,5,C 20": "TOTAL CASH SALES"
	pr f "12,5,C 22": "TOTAL DISCOUNTS TAKEN"
	pr f mat flo3$: mat pt
	pr f "12,26,n 11.2": tdt
	pr f "18,1,C 70,H,N": "ENTER 1 TO MERGE; 2 FOR CORRECTIONS: 5 STOP WITHOUT POSTING"
	L1790: !
	input fields "18,61,n 1,eu,n": j conv L1790
	if j=5 then goto Xit
on j goto ChainArMerge,L1810 none L1790 ! /r
L1810: ! r:
	pr newpage
	pr f "10,5,c 60": "ENTER 1 FOR A LISTING OF ENTRIES; ELSE ENTER 2"
	L1830: !
	input fields "10,60,n 1,eu,n": j conv L1830
on j goto L1850,L2060 none L1830 ! /r
L1850: !
	r=0
	fnopenprn
	pr newpage
	on fkey 5 goto L2040
	pr newpage
	pr #255,using L1910: date$,env$('cnam'),time$,"INPUT EDIT LIST"
	L1910: form pos 1,c 8,pos namtab,c 50,skip 1,pos 1,c 8,pos 58,c 50,skip 1
	pr f "10,20,c 40,n": "INPUT EDIT LISTING IN PROCESS"
	pr f "23,2,C 30,N": "Press F5 to stop"
	pr #255: "REF #  CL #  INVOICE #";
	pr #255: tab(34);"Date     Amount             Description           Discount          Tr Code"
	L1960: !
	r+=1
	read #h_addr,using L2110,rec=r: p$,iv$,mat tr,id$ eof L2040,noRec L2040 ioerr Ertn
	if ltrm$(p$)="0" or ltrm$(p$)="" then goto L1960
	name$=""
	read #hCl1,using FclientName,key=rpad$(trim$(p$),kln(hCl1)),release: name$ nokey L2010
	L2010: !
	pr #255,using L2020: r,p$,iv$,tr(1),tr(3),tr(4),name$(1:22),tr(2),tr(5)
	L2020: form pos 1,n 4,x 2,c 5,x 2,c 18,n 6,n 11.2,pic(zzzzzz),x 7,c 22,n 12.2,n 12,skip 1
	goto L1960
	L2040: !
	fncloseprn
	on fkey 5 ignore
	L2060: !
	pr newpage
	pr f "10,10,c 60": "ENTER REF # TO CORRECT; ENTER 0 WHEN COMPLETED"
	L2080: !
	input fields "10,61,n 4,eu,n": r1 conv L2080
	if r1=0 then goto L2220
	read #h_addr,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl noRec L2060 ioerr Ertn
	L2110: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
	if ltrm$(p$)="0" or ltrm$(p$)="" then goto L2060
	tr5=tr(5)
	if p><-1 then pt(1)=pt(1)-val(p$) conv L2150
	L2150: !
	pt(tr5+1)=pt(tr5+1)-tr(3)
	if ltrm$(p$)="-1" then pt(6)=pt(6)-tr(3)
	if tr5=3 then tdt=tdt-tr(2)
	hd$(1)="A/R CORRECT "&sc1$(tr5+1)(5:18)
	hd$(2)="ENTER CLIENT # AS 0 TO DELETE THIS ENTRY"
	vf=1
	goto L680
	L2220: !
	pr newpage
	vf=0
	pr f "10,10,c 50": "ENTER 1 TO MAKE ADDITIONAL ENTRIES; ELSE ENTER 2"
	L2250: !
	input fields "10,61,N 1,EU,N": j conv L2250
	on j goto L590,ScreenTotals none L2250
	ChainArMerge: !
fnChain('S:\Time Management\Merge Transactions')
Xit: pr newpage: fnXit
 
TMSRCH: ! r: search for customer #
	! uses hCl2
	dim selection$*70
	fnsearch(hCl2,"form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2",'pic($$$,$$$.##)',selection$,5)
	p$=selection$ ! pull key from first field in search line
	ano=0
	ano=val(selection$) conv ignore
return ! /r
include: ertn
