! Replace S:\acsUB\ubCorAmt
! -- Enter Beginning Balances and Readings
 
	autoLibrary
	on error goto Ertn
 
	dim z$*10,d(15),adr(2),p$*10,txt$*80,resp$(20)*80,txt$(6)*80
	dim o(2),srv$(10)*20,in1(19),gb(10),e$*30,tg(11),g(12)
 
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],NoShr",internal,outIn,keyed
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr,USE,RecL=102,KPs=1,KLn=19",internal,outIn,keyed
	open #hTrans2=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr,Use,KPs=11/1,KLn=8/10",internal,outIn,keyed
	fnGetServices(mat srv$)
	for j=1 to udim(srv$)
		srv$(j)=trim$(srv$(j))
		if srv$(j)<>"" then srv$(j)=srv$(j)&":" : services+=1
	next j
	fnTop(program$)
	goto MENU1
 
MENU1: !
	fnTos
	mylen=48
	mypos=mylen+2
	fnLbl(1,1,"Starting Account ([All] for first run):",mylen,1)
	fncmbact(1,mypos,1)
	resp$(1)="[All]"
	fnLbl(2,1,"Summary Transaction Date (mmddyy):",mylen,1)
	fnTxt(2,mypos,8,0,0,"1")
	resp$(2)=str$(n)
	fnLbl(4,1,"This program requires exclusive use of the Customer File.",90,2)
	fnLbl(5,1,"Please make sure no one else is in Utility Billing",90,2)
	fnLbl(6,1,"while you use this program.",90,2)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	n=val(resp$(2))
	hz$=z$=lpad$(trim$(resp$(1)(1:10)),10)
 
	if trim$(uprc$(hz$))=uprc$("[All]") then
		goto READ_CUSTOMER 
	else
		read #1,using L380,key=z$: z$,e$,mat d,bal,mat gb nokey MENU1
		goto SKIP_READ_CUSTOMER
	end if
 
READ_CUSTOMER: !
	read #1,using L380: z$,e$,mat d,bal,mat gb eof DONE
L380: form pos 1,c 10,pos 41,c 30,pos 217,15*pd 5,pos 292,pd 4.2,pos 388,10*pd 5.2
SKIP_READ_CUSTOMER: !
	goto SCREEN2
 
SCREEN2: !
	fnTos
	mylen=15
	for j=1 to 10 ! UDIM(SRV$)
		mylen=max(len(trim$(srv$(j))),mylen)
	next j
	mypos=mylen+2
	mypos2=mypos+12 ! kj
	fnLbl(1,1,'Account:',mylen,1)
	fnTxt(1,mypos,10,0,1,"",1)
	resp$(1)=z$
	fnLbl(2,1,"Balance:",mylen,1)
	fnTxt(2,mypos,9,0,1,"10")
	resp$(2)=str$(bal)
	fnLbl(4,mypos,"Balance",10,2,1)
	fnLbl(4,mypos2,"Reading",10,2,1)
	respc=2
	resp_line=4
	for j=1 to 10 ! UDIM(SRV$) kj
		if trim$(srv$(j))<>"" then
			respc+=1
			resp_line+=1
			fnLbl(resp_line,1,srv$(j),mylen,1)
			fnTxt(resp_line,mypos,9,0,1,"10")
			resp$(respc)=str$(gb(j))
		end if
		if trim$(srv$(j))<>"" and (j=1) then
			fnTxt(resp_line,mypos2,11,0,1,"20")
			respc+=1 : resp$(respc)=str$(d(1))
		end if
		if (trim$(srv$(j))="Electric:" or trim$(srv$(j))="Lawn Meter:") and (j=3) then
			fnTxt(resp_line,mypos2,11,0,1,"20")
			respc+=1 : resp$(respc)=str$(d(5))
		end if
		if trim$(srv$(j))="Gas:" and (j=4) then
			fnTxt(resp_line,mypos2,11,0,1,"20")
			respc+=1 : resp$(respc)=str$(d(9))
		end if
	next j
	respc+=1 : resp$(respc)=e$
	fnTxt(1,40,30,0,0,"",1)
	fnCmdKey("&Next",1,1,0,"Save and display next account in account order." )
	fnCmdKey("&Select Account",2,0,0,"Select another account." )
	fnCmdKey("E&Xit",5,0,1,"Returns to menu")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	bal=val(resp$(2))
	respc=2
	for j=1 to 10 ! udIM(SRV$)  kj
		if trim$(srv$(j))<>"" then
			respc+=1 : gb(j)=val(resp$(respc))
			tg(j)=val(resp$(respc)) conv L670
		end if
		! If BAL<0 Then tG(J)=-VAL(RESP$(RESPC)) Else tG(J)=VAL(RESP$(RESPC))
		if trim$(srv$(j))="Water:" and (j=1) then
			respc+=1 : d(1)=val(resp$(respc)): wr=d(1)
		end if
		if trim$(srv$(j))="Electric:" and (j=3) then
			respc+=1 : d(5)=val(resp$(respc)): er=d(5)
		end if
		if trim$(srv$(j))="Gas:" and (j=4) then
			respc+=1 : d(9)=val(resp$(respc)): gr=d(9)
		end if
		L670: !
	next j
	! set T1 to total allocations
	t1=0
	for j=1 to udim(gb)
		t1+=gb(j)
	next j
	if t1<>bal then
		gosub T1_NOT_EQUAL_BAL 
		goto SCREEN2 
	else
		goto WRITE_TRANS
	end if
 
T1_NOT_EQUAL_BAL: !
	txt$(1)="Total Allocations do not equal Current Balance"
	txt$(2)=""
	txt$(3)=cnvrt$("pic(--------#.##)",bal)&"  Current Balance  "
	txt$(4)=cnvrt$("pic(--------#.##)",t1)&"  Total Allocations"
	txt$(5)="------------"
	txt$(6)=cnvrt$("pic(--------#.##)",bal-t1)&"  Difference"
	fnmsgbox(mat txt$,resp$,'',48)
return
 
WRITE_TRANS: !
	gosub DEL_HIST
	if bal<0 then tcode=4 else tcode=5 ! SHOW AS CREDIT MEMO IF NEGATIVE BAL OWED, ELSE DEBIT MEMO
	tdate=n
	tdate=fndate_mmddyy_to_ccyymmdd(tdate)
	tg(11)=sum(tg)
	if bal=0 then goto L860
	write #2,using L840: z$,tdate,tcode,abs(bal),mat tg,wr,wu,er,eu,gr,gu,bal,pcode
L840: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	mat g=(0)
L860: rewrite #1,using L870: mat d,bal,mat g,mat adr,mat gb
L870: form pos 217,15*pd 5,pos 292,pd 4.2,pos 300,12*pd 4.2,pos 348,2*pd 3,pos 388,10*pd 5.2
	if ckey=2 then goto MENU1 ! enter next Account
	goto READ_CUSTOMER
 
DONE: !
	close #1:
	close #2:
	close #hTrans2:
goto Xit

Xit: fnXit
 
DEL_HIST: ! Delete History
	restore #2,key>=z$&"         ": nokey L1140
	do
		read #2,using L1100: p$,tdate eof L1140
		L1100: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
		if p$<>z$ then goto L1140 ! not same account
		if p$=z$ then delete #2:
	loop
	L1140: !
return
include: ertn
