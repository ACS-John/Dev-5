autoLibrary
fnTop(program$)

! r: setup
dim pt(6)
dim name$*25
dim p$*5
dim iv$*12
dim tr(6)
dim flo4$(5)
dim sc4$(5)
dim ot4$(5)
dim fli4$(5)
dim q(3)
dim gln1(3)
dim gln2(3)
dim pgl(3)
open #h_company=fnH: "Name=S:\Core\Data\acsllc\Company.h[cno],Shr",internal,input
read #h_company,using 'form pos 161,3*n 1,pos 178,n 3,n 6,n 3,n 3,n 6,n 3': i3,i4,i5,mat gln1,mat gln2
! i3=1 ! ENTER G/L #'S
close #h_company:

		! if i3=0 then ! NO GL TO BE ENTERED
		! 	sz=6
		! else if i4=1 and i5=1 then ! YES DEPT   YES SUBACCOUNT
		! 	sz=2
		! 	gx=4
		! 	gpx=2
		! else if i4=0 and i5=1 then ! NO DEPT    YES SUBACCOUNT
		! 	sz=3
		! 	gx=3
		! 	mat gl(10,3)=(0)
		! 	mat pgl(2)=(0)
		! 	gpx=1
		! else if i4=1 and i5=0 then ! YES DEPT    NO SUB ACCOUNT
		! 	sz=4
		! 	gx=3
		! 	mat gl(10,3)=(0)
		! 	mat pgl(2)=(0)
		! 	gpx=2
		! else ! NO DEPT    NO SUBACCOUNT
			sz=5
			gx=2
			dim gl(10,4)
			mat gl(10,2)=(0)
			mat pgl(1)=(0)
			gpx=1
		! end if

		dim f3$*255
		f3$='FORM POS 1,C 5,C 12,N 6,2*PD 5.2,PD 2,2*N 1,C 20,x 3,n 6,x 3,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2'
		dim sc1$(5)
		sc1$(1)='0 = Completed'
		sc1$(2)='1 = Invoices'
		sc1$(3)='2 = Debit Memos'
		sc1$(4)='3 = Collections'
		sc1$(5)='4 = Credit Memos'
		dim sc2$(9)

		sc2$(1)='Client'
		sc2$(2)='Invoice'
		sc2$(3)='Date'
		sc2$(4)='Amount'
		sc2$(5)='Description'
		sc2$(6)='Cost of Goods'
		sc2$(7)=''
		sc2$(8)='G/L Account'
		sc2$(9)='Amount'

		dim fli1$(49)
		fli1$(1 )='3,30,C 5,ut,n'
		fli1$(2 )='4,30,c 12,cu,n'
		fli1$(3 )='5,30,n 6,ut,n'
		fli1$(4 )='6,30,n 11.2,ut,n'
		fli1$(5 )='7,30,c 20,ut,n'
		fli1$(6 )='8,30,n 11.2,ut,n'
		fli1$(7 )='9,34,n 6,ut,n'
		fli1$(8 )='12,24,n 6,ut,n'
		fli1$(9 )='12,40,n 11.2,ut,n'
		fli1$(10)='13,24,n 6,ut,n'
		fli1$(11)='13,40,n 11.2,ut,n'
		fli1$(12)='14,24,n 6,ut,n'
		fli1$(13)='14,40,n 11.2,ut,n'
		fli1$(14)='15,24,n 6,ut,n'
		fli1$(15)='15,40,n 11.2,ut,n'
		fli1$(16)='16,24,n 6,ut,n'
		fli1$(17)='16,40,n 11.2,ut,n'
		fli1$(18)='17,24,n 6,ut,n'
		fli1$(19)='17,40,n 11.2,ut,n'
		fli1$(20)='18,24,n 6,ut,n'
		fli1$(21)='18,40,n 11.2,ut,n'
		fli1$(22)='19,24,n 6,ut,n'
		fli1$(23)='19,40,n 11.2,ut,n'
		fli1$(24)='20,24,n 6,ut,n'
		fli1$(25)='20,40,n 11.2,ut,n'
		fli1$(26)='21,24,n 6,ut,n'
		fli1$(27)='21,40,n 11.2,ut,n'

		dim ot1$(49)
		ot1$(1 )='3,30,C 5,ut,n'
		ot1$(2 )='4,30,c 12,ut,n'
		ot1$(3 )='5,30,n 6,ut,n'
		ot1$(4 )='6,30,n 11.2,ut,n'
		ot1$(5 )='7,30,c 20,ut,n'
		ot1$(6 )='8,30,n 11.2,ut,n'
		ot1$(7 )='9,34,n 6,ut,n'
		ot1$(8 )='12,24,n 6,ut,n'
		ot1$(9 )='12,40,n 11.2,ut,n'
		ot1$(10)='13,24,n 6,ut,n'
		ot1$(11)='13,40,n 11.2,ut,n'
		ot1$(12)='14,24,n 6,ut,n'
		ot1$(13)='14,40,n 11.2,ut,n'
		ot1$(14)='15,24,n 6,ut,n'
		ot1$(15)='15,40,n 11.2,ut,n'
		ot1$(16)='16,24,n 6,ut,n'
		ot1$(17)='16,40,n 11.2,ut,n'
		ot1$(18)='17,24,n 6,ut,n'
		ot1$(19)='17,40,n 11.2,ut,n'
		ot1$(20)='18,24,n 6,ut,n'
		ot1$(21)='18,40,n 11.2,ut,n'
		ot1$(22)='19,24,n 6,ut,n'
		ot1$(23)='19,40,n 11.2,ut,n'
		ot1$(24)='20,24,n 6,ut,n'
		ot1$(25)='20,40,n 11.2,ut,n'
		ot1$(26)='21,24,n 6,ut,n'
		ot1$(27)='21,40,n 11.2,ut,n'
		dim flo1$(11)
		flo1$(1 )='3,5,c 20'
		flo1$(2 )='4,5,c 20'
		flo1$(3 )='5,5,c 20'
		flo1$(4 )='6,5,c 20'
		flo1$(5 )='7,5,c 20'
		flo1$(6 )='8,5,c 20'
		flo1$(7 )='9,5,c 20'
		flo1$(8 )='11,20,c 20'
		flo1$(9 )='11,40,c 20'
		flo1$(10)='1,15,c 40'
		flo1$(11)='2,5,c 45'

		! open #1: "Name=S:\Client Billing\Legacy\TMSCRN.CL,Shr",i,i,r
		! read #1,using L560,rec=sz: ...
		! L560: form pos 1,c 255,142*c 18
		! close #1:

dim otgl$(3)
otgl$(1)="9,30,pic(zzz)"
otgl$(2)="9,34,pic(zzzzzz)"
otgl$(3)="9,41,pic(zzz)"


open #hTransBatch:=fnH: "Name=[Temp]\transBatch.[session],Replace,RecL=239",i,outi,r
open #hCl1=fnH: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr",i,i,k
FclientName: form pos 6,c 25
open #hCl2=fnH: "Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",i,i,k  ! alpha index on clients
! /r

ScreenAddMore: ! r:
	if samOpt$(1)='' then mat samOpt$=('False') : samOpt$(3)='True'
	fntos : rc=0
	fnlbl(1,1,'Add:',8,1)
	fnopt(1,10,'1 - Invoices')      	: resp$(rc+=1)=samOpt$(1)
	fnopt(2,10,'2 - Debit Memos')   	: resp$(rc+=1)=samOpt$(2)
	fnopt(3,10,'3 - Collections')   	: resp$(rc+=1)=samOpt$(3)
	fnopt(4,10,'4 - Credit Memos')  	: resp$(rc+=1)=samOpt$(4)
	fncmdset(11)
	ckey=fnacs(mat resp$)
	if ckey=5 then
		tr5=0
		vf=1
		goto ScreenTotals
	else
		tr5=srch(mat resp$,'True')
	end if
	! pr 'ckey=';ckey
	! pr mat resp$
	! pause

	! pr newpage
	! pr f ' 6,5,c 20': '0 = COMPLETED'
	! pr f ' 7,5,c 20': '1 = INVOICES'
	! pr f ' 8,5,c 20': '2 = DEBIT MEMOS'
	! pr f ' 9,5,c 20': '3 = COLLECTIONS'
	! pr f '10,5,c 20': '4 = CREDIT MEMOS'
	! pr f ' 3,10,c 50': 'A/R Input Selection Menu'
	! pr f '13,10,c 50':'Selection'
	! L630: !
	! pause
	! input fields "13,29,n 1,eu,n": tr5 conv L630
	! if tr5=0 then vf=1 : goto ScreenTotals
	! if tr5<1 or tr5>4 then goto L630
	dim hd$(2)*50
	hd$(1)="A/R INPUT "&sc1$(tr5+1)(5:18)
	hd$(2)="Client Number as 0 to stop"
goto ScreenSomething1 ! /r

ScreenSomething1: ! r:
	if tr5=4 or tr5=3 then sc2$(7)="G/L # to Credit" else sc2$(7)="G/L # to Debit"
	if tr5=3 then sc2$(6)="Discount Amount" else sc2$(6)=""
	if gx=0 then sc2$(7)=" "
	L710: !
	pr newpage
	pr f mat flo1$: mat sc2$,mat hd$
	ps1=0
	if vf then
		dim id$*20
		if gx><0 then
			pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2),mat pgl,mat gl
		else
			ScreenSomething1b: !
			pr f mat ot1$: p$,iv$,tr(1),tr(3),id$,tr(2)
		end if
	end if
goto ScreenSomething2 ! /r

ScreenSomething2: ! r:
	pr f "5,30,pic(zzzzzz)": tr(1)
	pr f "24,20,C 50,N": "F1 Continue   F2 verify name    F4 Search"
	if gx><0 then goto L910
	L820: !
	input fields mat fli1$: p$,iv$,tr(1),tr(3),id$,tr(2) conv L870
	if cmdkey=4 then gosub TMSRCH : goto ScreenSomething1b
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
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto ScreenAddMore
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto RewrTransBlank
	if ltrm$(p$)="-1" then name$="CASH SALE" else goto Read9
	goto L1050
	Read9: !
	read #hCl1,using FclientName,key=rpad$(trim$(p$),kln(hCl1)),release: name$ nokey L1020
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
	! if sz=4 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=tr(3)
	! if sz=3 then gl(1,1)=gln1(2): gl(1,2)=gln1(3): gl(1,3)=tr(3)
	! if sz=2 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=gln1(3): gl(1,4)=tr(3)
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
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=0 then goto ScreenAddMore
	if ltrm$(p$)="0" or ltrm$(p$)="" and vf=1 then goto RewrTransBlank
	ps1=1
	if tr(1)<10100 or tr(1)>123199 then
		pr f "5,48,c 20": "Invalid Date"
		goto ScreenSomething2
	end if

	if tr(3) then
		if gx=0 then goto L1520
		if pgl(gpx)>0 then goto L1410
		pr f "9,45,c 30": "G/L # REQUIRED"
	else
		pr f "6,48,c 20": "NO AMOUNT ENTERED"
	end if
	goto ScreenSomething2
	
	L1410: !
	gla=0
	for j=1 to 10
		if gl(j,gx)=0 then goto L1460
		gla=gla+gl(j,gx)
	next j
	L1460: !
	
	if tr5=3 then gla=gla-tr(2)
	if gla<>tr(3) then
		pr f "11,2,c 75": " G/L allocations do not agree with total amount.  Press enter to continue."
		input fields "11,78,c 1,EU,n": pause$
		pr f "11,2,c 75,n,n": " "
		goto ScreenSomething2
	end if
	
	L1520: !
	if ltrm$(p$)<>"-1" then
		pt(1)+=val(p$) conv ignore
	end if
	pt(tr5+1)=pt(tr5+1)+tr(3)
	if tr5=3 then tdt=tdt+tr(2)
	if ltrm$(p$)="-1" then pt(6)=pt(6)+tr(3)
	if vf=1 then goto RewrTransNow
	r3=r3+1
	tr(5)=tr5
	write #hTransBatch,using f3$,rec=r3: p$,iv$,mat tr,id$,mat pgl,mat gl
	p$=""
	q2=0
goto L710 ! /r

RewrTransBlank: ! r:  rewrite hTransBatch
	iv$=id$=" "
	mat tr=(0)
	mat gl=(0)
	RewrTransNow: !
	rewrite #hTransBatch,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl
	p$=""
goto AskMakeCorrection ! /r
ScreenTotals: ! r:
	do
		pr newpage
		pr f ' 3,10,cc 50':"A/R Input Proof Totals"
		pr f ' 6,5,cr 20': "Total Account #s:"
		pr f ' 6,26,n 11.2': pt(1)
		pr f ' 7,5,cr 20': "Total Invoices:"
		pr f ' 7,26,n 11.2': pt(2)
		pr f ' 8,5,cr 20': "Total Debit Memos:"
		pr f ' 8,26,n 11.2': pt(3)
		pr f ' 9,5,cr 20': "Total Collections:"
		pr f ' 9,26,n 11.2': pt(4)
		pr f '10,5,cr 20': "Total Credit Memos:"
		pr f '10,26,n 11.2': pt(5)

		pr f "11,5,C 20": "Total Cash Sales"
		pr f '11,26,n 11.2': pt(6)
		pr f "12,5,C 22": "Total Discounts Taken"
		pr f "12,26,n 11.2": tdt
		pr f '13,10,cc 50':""
		pr f '18,1,C 70': "1 to Merge; 2 for Corrections: 5 Stop Without Posting"
		pr f "19,25,30/Cc 30,,B3": "[F3] Print Entry Listing"
		L1790: !
		input fields "18,61,n 1,eu,n": j conv L1790
		
		if fkey=5 or fkey=99 or j=5 then
			goto Xit
		end if

		if fkey=3 then
			gosub PrintEntryList
		else if j=1 then
			goto ChainArMerge
		else if j=2 then
			goto AskMakeCorrection
		end if
	loop
! /r

PrintEntryList: ! r: requires:hTransBatch, localOnly: r,p$,iv$,tr(1),tr(3),tr(4),name$(1:22),tr(2),tr(5)
	r=0
	! pr newpage
	! pr f "10,20,c 40,n": "Input Edit Listing In Process"
	! pr f "23,2,C 30,N": "Press F5 to stop"
	! on fkey 5 goto Pel_finis
	fnopenprn
	pr #255,using 'form pos 1,c 8,pos 21,Cc 50': date$,env$('cnam')
	pr #255,using 'pos 1,c 8,pos 58,c 15': time$,"Input Edit List"
	pr #255: "RefNo  Clnt  InvoiceNo";
	pr #255: tab(34);"Date     Amount             Description           Discount          Tr Code"
	do
		r+=1
		read #hTransBatch,using Faddr,rec=r: p$,iv$,mat tr,id$ eof Pel_finis,noRec Pel_finis
		if ltrm$(p$)<>"0" and ltrm$(p$)<>"" then
			name$=""
			read #hCl1,using FclientName,key=rpad$(trim$(p$),kln(hCl1)),release: name$ nokey ignore
			pr #255,using FprEntryLine: r,p$,iv$,tr(1),tr(3),tr(4),name$(1:22),tr(2),tr(5)
			FprEntryLine: form pos 1,n 4,x 2,c 5,x 2,c 18,n 6,n 11.2,pic(zzzzzz),x 7,c 22,n 12.2,n 12
		end if

	loop
	Pel_finis: !
	fncloseprn
	! on fkey 5 ignore
return ! /r

AskMakeCorrection: ! r:
	pr newpage
	pr f "10,10,c 60": "Enter Ref # to Correct; Enter 0 when Completed"
	pr f "12,25,30/Cc 30,,B3": "[F3] Print Entry Listing"
	! 'lyne,ps,width/CC len,,B#'
	L2080: !
	input fields "10,61,n 4,eu,n": r1 conv L2080
	if ckey=3 then gosub PrintEntryList : goto AskMakeCorrection
	if r1=0 then
		goto AskAddMore
	else
		read #hTransBatch,using f3$,rec=r1: p$,iv$,mat tr,id$,mat pgl,mat gl noRec AskMakeCorrection
		Faddr: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
		if ltrm$(p$)="0" or ltrm$(p$)="" then goto AskMakeCorrection
		tr5=tr(5)
		if p><-1 then pt(1)=pt(1)-val(p$) conv ignore
		pt(tr5+1)=pt(tr5+1)-tr(3)
		if ltrm$(p$)="-1" then pt(6)=pt(6)-tr(3)
		if tr5=3 then tdt=tdt-tr(2)
		hd$(1)="A/R Correct "&sc1$(tr5+1)(5:18)
		hd$(2)="Enter Client as 0 to Delete this entry"
		vf=1
		goto ScreenSomething1
	end if
! /r
AskAddMore: ! r:
	pr newpage
	vf=0
	pr f "10,10,c 50": "Enter 1 to make additional entries; else enter 0"
	L2250: !
	input fields "10,61,N 1,EU,N": j conv L2250
	if j=1 then goto ScreenAddMore
goto ScreenTotals ! /r

ChainArMerge: !
fnChain('S:\Client Billing\Merge Transactions')
Xit: pr newpage: fnXit

TMSRCH: ! r: search for customer #
	! uses hCl2
	dim selection$*70
	fnSearch(hCl2,"form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2",'pic($$$,$$$.##)',selection$,5)
	p$=selection$ ! pull key from first field in search line
	ano=0
	ano=val(selection$) conv ignore
return ! /r
include: ertn
