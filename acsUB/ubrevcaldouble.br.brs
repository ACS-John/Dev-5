! Replace S:\acsUB\UBRevCaldouble
 
	autoLibrary
	on error goto Ertn
 
	dim x$*10,p$*10,reqz$*10,reqz12$*2,sr$*1,gb(10)
	dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
	dim cap$*128,txt$*200,tg(11),key$*19
	dim bt1(14,2),badr(2),resp$(5)*60
 
	fnTop("S:\acsUB\UBRevCal",cap$="Reverse Calculation")
	fnLastBillingDate(d1)
	if d1=0 then d1=val(date$(4:5)&date$(7:8)&date$(1:2))
 
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #11: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",internal,outIn,keyed
	open #12: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx3.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed
	gosub BUD1
ASK1: !
	x=6
	close #111: ioerr L250
L250: fnTos(sn$="ubrevcal") : _
	respc=0
	fnLbl(1,1,"You may limit the customers to reverse by changing the options below.",73,2)
	fnLbl(2,1,"You may only reverse calculations for the most recent Billing Date!",73,2)
	fnLbl(4,1,"Account:",27,1)
	fncmbact(4,29,1) : _
	resp$(respc+=1)=""
	fnLbl(5,1,"Current Billing Date:",27,1)
	fnTxt(5,29,8,0,0,"1") : _
	resp$(respc+=1)=str$(d1)
	fnLbl(6,1,"Previous Billing Date:",27,1)
	fnTxt(6,29,8,0,0,"1") : _
	resp$(respc+=1)=""
	fnLbl(7,1,"Route Number:",27,1)
	fncmbrt2(7,29) : _
	resp$(respc+=1)="1"
	fnChk(8,29,"Print Status Report") : _
	resp$(respc+=1)="True"
	fnCmdSet(2) : _
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(1)="[All]" then resp$(1)=""
	if resp$(4)="[All]" then resp$(4)=""
	if trim$(reqz$)="" and trim$(holdreqz$)<>"" then goto Xit : _
		! if they ever select a customer and then accidently take f1 to : _
		! continue, it will stop instead of reversing everyone else in file
	reqz$=lpad$(rtrm$(resp$(1)(1:10)),10) : _
	reqf=val(resp$(2)) : _
	olddat=val(resp$(3)) : _
	reqz12$=resp$(4)
	if uprc$(resp$(5))=uprc$("True") then sr$="Y" else sr$="N"
	if sr$="Y" then let fnopenprn
	if sr$="Y" and secondpass<>1 then gosub SRHDR
	secondpass=1
L470: form pos 5,c 10,x 5,pic(zz/zz/zz)
L480: if rtrm$(reqz$)<> "" then : _
		read #1,using L830,key=reqz$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 nokey ASK1 else : _
		goto L510
	if f<>reqf then goto ASK1 : _
		! must have current billing date
	goto L600
L510: read #1,using L830: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 eof Xit
	if rtrm$(reqz12$)<>"" and str$(route)<>lpad$(rtrm$(reqz12$),2) then : _
		goto L510
! If TRIM$(Z$)="210008.02" Then Pause
! If REQF<>0 AND F<>REQF Then Goto 510
	x=fndate_mmddyy_to_ccyymmdd(reqf)
	key$=z$&cnvrt$("n 8",x)&"1"
	wr=wu=er=eu=gr=gu=0 ! set all previous readings to zero
	read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L510 ! read history to pull new mat g
	for j=1 to 11: g(j)=tg(j): next j ! set array g in customer to array tg in matching transaction
L600: if sr$="Y" then : _
		pr #255,using L470: z$,f pageoflow SRPGOF
	for j=1 to 9 : gb(j)=gb(j)-g(j) : next j : _
	! subtract out current bill from breakdown except for penalty
	bal=bal-g(11)
	x=fndate_mmddyy_to_ccyymmdd(olddat)
	key$=z$&cnvrt$("n 8",x)&"1"
	wr=wu=er=eu=gr=gu=0 ! set all previous readings to zero
	read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L670 ! read previous months history to pull old readings and usages
L670: d(1)=d(2) ! set current water reading to last month
	d(2)=wr ! set prior reading to month before last
	d(4)=d(4)-d(3) ! subtract out current usage from year to date
	d(3)=wu ! set usage to amount in history
	d(5)=d(6) ! set current electric reading to last month
	d(6)=er ! set prior reading to month before last
	d(8)=d(8)-d(7) ! subtract out current usage from year to date
	d(7)=eu ! set usage to amount in history
	d(9)=d(10) ! set current gas reading to last month
	d(10)=gr ! set prior reading to month before last
	d(12)=d(12)-d(11) ! subtract out current usage from year to date
	d(11)=gu ! set usage to amount in history
	f=0 ! set billing date to zero
	extra3=extra4: extra4=0
	mat g=(0) ! SET ALL LAST TIME BILL TO ZERO
	rewrite #1,using L830: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4
L830: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 354,c 7,2*c 12,pd 3,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6
	x=fndate_mmddyy_to_ccyymmdd(reqf)
	key$=z$&cnvrt$("n 8",x)&"1"
	read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L480
L870: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	delete #2:
	x=fndate_mmddyy_to_ccyymmdd(reqf)
	key$=z$&cnvrt$("n 8",0)&" "
L910: read #2,using L870,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L480
	if p$<>z$ then goto L950
	holdtbal=tbal
	read #2,using L870: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L480
	goto L910
L950: rewrite #1,using L960,key=z$: holdtbal
L960: form pos 292,pd 4.2
	goto L510
	form pos 15,pd 4
	if bud1=1 then gosub BUD2
	if rtrm$(reqz$)<>"" then : _
		holdreqz$=reqz$ : _
		reqz$="" : _
		goto ASK1 else : _
		goto L510
 
Xit: !
	if sr$<>"Y" then goto L1050
	fncloseprn
L1050: fnXit
 
SRHDR: !
	pg+=1
	pr #255: "Reverse Calculation Status Report"
	pr #255: "Page "&str$(pg)
	pr #255: ""
	pr #255: "All accounts listed have been reversed."
	pr #255: ""
	pr #255: "Account           Billing Date"
	pr #255: "_______________   ____________"
return
 
SRPGOF: !
	pr #255: newpage
	gosub SRHDR
	continue
BUD1: bud1=0
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L1260
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative
	bud1=1
L1260: return
 
BUD2: !
	bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
	if bud1=0 then goto L1400
	read #81,using L1320,key=z$: x$,mat ba,mat badr nokey L1400
L1320: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	ta1=badr(1)
L1340: if ta1=0 then goto L1400
	read #82,using L1360,rec=ta1: x$,mat bt1,nba noRec L1400
L1360: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	if bt1(1,1)=n then : _
		mat bt1=(0) : _
		rewrite #82,using L1380,rec=ta1: mat bt1
L1380: form pos 11,2*pd 4,24*pd 5.2,2*pd 4
	ta1=nba: goto L1340
L1400: return
 
include: Ertn No
