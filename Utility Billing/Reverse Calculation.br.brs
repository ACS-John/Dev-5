! Replace S:\acsUB\UBRevCal

autoLibrary
on error goto Ertn

dim x$*10,p$*10,reqz$*10,reqz12$*5,sr$*1,gb(10)
dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
dim tg(11),key$*19
dim bt1(14,2),badr(2),resp$(5)*60

fnTop(program$)

goto ALLOW_PROGRAM ! if env$('client')="Ash Grove" then goto ALLOW_PROGRAM
	dim _msg$(4)*80
	_msg$(1)="This program has been removed due to heavy misuse."
	_msg$(2)="To recalculate bills simply use Enter Readings and Charges"
	_msg$(3)="with the same billing date as used previously."
	_msg$(4)="If you feel you need to use this legacy program, please contact ACS."
	fnmsgbox(mat _msg$,resp$(1),'',16)
goto Xit
ALLOW_PROGRAM: !

	fnLastBillingDate(d1)
	if d1=0 then d1=val(date$(4:5)&date$(7:8)&date$(1:2))

ASK1: !
	fnTos
	respc=0
	fnLbl(1,1,"You may limit the customers to reverse by changing the options below.",73,2)
	fnLbl(2,1,"You may only reverse calculations for the most recent Billing Date!",73,2)
	fnLbl(4,1,"Account:",27,1)
	fncmbact(4,29,1)
	resp$(respc+=1)="[All]"
	fnLbl(5,1,"Current Billing Date:",27,1)
	fnTxt(5,29,8,0,0,"1")
	resp$(respc+=1)=str$(d1)
	fnLbl(6,1,"Previous Billing Date:",27,1)
	fnTxt(6,29,8,0,0,"1")
	resp$(respc+=1)=""
	fnLbl(7,1,"Route Number:",27,1)
	fncmbrt2(7,29)
	resp$(respc+=1)="[All]"
	fnChk(8,29,"Print Status Report")
	resp$(respc+=1)='True'
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit

	fnAutomatedSavePoint('before')

	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	open #11: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",internal,outIn,keyed
	open #12: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx3.h[cno],Shr",internal,outIn,keyed
	open #2: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed
	open #htrans2=fnH: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",internal,outIn,keyed
	fn_bud1

	if trim$(reqz$)="" and trim$(holdreqz$)<>"" then goto Xit ! if they ever select a customer and then accidently take f1 to continue, it will stop instead of reversing everyone else in file
	reqz$=lpad$(rtrm$(resp$(1)(1:10)),10)
	if trim$(reqz$)='[All]' then reqz$=''
	reqf=val(resp$(2))
	olddat=val(resp$(3))
	reqz12$=resp$(4)
	if reqz12$="[All]" then reqz12$=""
	if uprc$(resp$(5))=uprc$('True') then sr$="Y" else sr$="N"
	if sr$="Y" then let fnopenprn
	if sr$="Y" and secondpass<>1 then let fn_srhdr
	secondpass=1
	L480: !
	if rtrm$(reqz$)<> "" then
		read #1,using L770,key=reqz$: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 nokey ASK1
	else
		goto CUSTOMER_READ
	end if
	if f<>reqf then goto ASK1 ! must have current billing date
goto L550
CUSTOMER_READ: !
	read #1,using L770: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4 eof FINIS
	if trim$(reqz12$)<>"" and route<>val(reqz12$) then goto CUSTOMER_READ
	! If TRIM$(Z$)="210008.02" Then Pause
	if reqf<>0 and f<>reqf then goto CUSTOMER_READ
	L550: !
	if sr$="Y" then
		pr #255,using L470: z$,f pageoflow SRPGOF
		L470: form pos 5,c 10,x 5,pic(zz/zz/zz)
	end if
	for j=1 to 9 : gb(j)=gb(j)-g(j): bal=bal-g(j): next j ! subtract out current bill from breakdown
	! bal=bal-g(11)  moved above 06/01/12
	key$=z$&cnvrt$("n 8",fndate_mmddyy_to_ccyymmdd(olddat))&"1"
	wr=wu=er=eu=gr=gu=0 ! set all previous readings to zero
	read #2,using L810,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L620 ! read previous months history to pull old readings and usages
	L620: !
	d(1)=d(2) ! set current water reading to last month
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
	f=0 !  f=olddat   ! set billing date to zero
	extra3=extra4 : extra4=0
	mat g=(0) ! SET ALL LAST TIME BILL TO ZERO
	rewrite #1,using L770: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),bra,mat gb,route,extra3,extra4
	L770: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 354,c 7,2*c 12,pd 3,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6
	key$=z$&cnvrt$("n 8",fndate_mmddyy_to_ccyymmdd(reqf))&"1"
	read #2,using L810,key=key$: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode nokey L480
	L810: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	delete #2:
	goto L480
	if bud1=1 then let fn_bud2
	if rtrm$(reqz$)<>"" then
		holdreqz$=reqz$
		reqz$=""
		goto ASK1
	else
		goto L480
	end if

FINIS: ! r:
	if sr$="Y" then
		fncloseprn
	end if
goto Xit ! /r
Xit: fnXit

def fn_srhdr
	pg+=1
	pr #255: "Reverse Calculation Status Report"
	pr #255: "Page "&str$(pg)
	pr #255: ""
	pr #255: "All accounts listed have been reversed."
	pr #255: ""
	pr #255: "Account           Billing Date"
	pr #255: "_______________   ____________"
fnend

SRPGOF: ! r:
	pr #255: newpage
	fn_srhdr
continue ! /r
def fn_bud1
	bud1=0
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L1120
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",i,outi,r
	bud1=1
	L1120: !
fnend
def fn_bud2
	bd1=0 : mat bd1(5) : mat bd1=(0) : mat bd2=(0) : mat bd3=(0)
	if bud1=0 then goto L1260
	read #81,using L1180,key=z$: x$,mat ba,mat badr nokey L1260
	L1180: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	ta1=badr(1)
	L1200: !
	if ta1=0 then goto L1260
	read #82,using L1220,rec=ta1: x$,mat bt1,nba noRec L1260
	L1220: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	if bt1(1,1)=n then
		mat bt1=(0)
		rewrite #82,using L1240,rec=ta1: mat bt1
		L1240: form pos 11,2*pd 4,24*pd 5.2,2*pd 4
	end if
	ta1=nba: goto L1200
	L1260: !
fnend

include: ertn No
