! Replace S:\acsUB\ubprtbl1_Bethany
! pr bills for Village of Bethany
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,datafile$*256
 
	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input
	read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
	close #21:
	at$(1)=env$('cnam')
	z=21
	at$(1)=trim$(at$(1))(1:z)
	x=len(at$(1)) : y=z-x
	at$(1)=rpt$(" ",int(y/2))&at$(1)
	z=26
	for j=2 to udim(at$)
		at$(j)=trim$(at$(j))(1:z)
		x=len(at$(j)) : y=z-x
		at$(j)=rpt$(" ",int(y/2))&at$(j)
	next j
	linelength=62
 
	fnTop("S:\acsUB\ubprtbl1",cap$="Print Bills")
! Gosub BULKSORT ! want printed in alphabetic order
! Open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",Internal,Input,Keyed  ! open in Account order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
	open #8: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
 
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1")
	pf=33 : ll=30
	respc=0
	fnLbl(3,1,"Penalty Due Date:",ll,1)
	fnTxt(3,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(4,1,"Message on Bill:",ll,1)
	fnTxt(4,pf,30,30)
	resp$(respc+=1)=mg$(1)
	fnTxt(5,pf,30,30)
	resp$(respc+=1)=mg$(2)
	fnTxt(6,pf,30,30)
	resp$(respc+=1)=mg$(3)
	fnLbl(7,1,"Date of Billing:",ll,1)
	fnTxt(7,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(8,1,"Starting Account:",ll,1)
	fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]" ,1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
	resp$(respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf)
	resp$(respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print:",1)
	resp$(respc+=1)="False"
	fnLbl(11,1,"Date Meter Read:",ll,1)
	fnTxt(11,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",newd3)
	fnLbl(12,1,"Previous Reading Date:",ll,1)
	fnTxt(12,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",newd2)
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto ENDSCR
	d1 = val(resp$(5))
	d4 = val(resp$(1))
	mg$(1) = resp$(2)
	mg$(2) = resp$(3)
	mg$(3) = resp$(4)
	if resp$(6)="[All]" then
		a$=""
	else
		a$ = lpad$(trim$(resp$(6)(1:9)),9)
	end if
	if resp$(7)="[All]" then
		prtbkno=0
	else
		prtbkno = val(resp$(7))
	end if
	if resp$(8)="True" then sl1=1 : z$="" else sl1=0
	if trim$(a$)<>"" then
		read #2,using L500,key=a$: z$,route,sequence nokey SCREEN1
		holdz$=z$: begin=1
		st1=1
	end if
	newd3=val(resp$(9))
	newd2=val(resp$(10))
L500: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed
	gosub BUD1
	gosub VBOPENPRINT
 
	on fkey 5 goto RELEASE_PRINT
L600: if sl1=1 then goto SCREEN3
L610: ! Read #6,Using 640: Z$ Eof 910
	if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L610 ! start with
	begin=0 ! cancel starting account
	form pos 22,c 10
! Read #1,Using 680,Key=Z$: Z$,MAT E$,F$,A3,MAT B,FINAL,MAT D,BAL,F,MAT G,BRA,MAT GB,ROUTE,D3,D2,BULK$,EXTRA1$,ESTIMATEDATE Nokey 610
	read #1,using L680: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate eof RELEASE_PRINT
	if newd3<>0 then d3=newd3
	if newd2<>0 then d2=newd2
L680: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
	if prtbkno=0 then goto L710
	if prtbkno><route then goto RELEASE_PRINT
L710: if f><d1 then goto L600
	if st1=0 then goto READALTADR
! If ST1$=Z$ Then sT1=0 Else Goto 560
READALTADR: ! r:
! read alternate billing address
	read #3,using L770,key=z$: mat ba$ nokey L840
	L770: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then
			e1=e1+1 : pe$(e1)=ba$(j)
		end if
	next j
	goto L1000
 
	L840: !
	e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then
			e1=e1+1 : pe$(e1)=e$(j)
		end if
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
goto L1000 ! /r
 
L1000: ! r:
	if bud1=1 then gosub BUD2
	pb=bal-g(11)
!	 If BAL<=0 Then g(10)=0 ! don't show penalty if balance 0 or less
	! print bill routine
	gosub VBPRINT
	! end of pr routine
	bct(2)=bct(2)+1 ! accumulate totals
goto L600 ! /r
 
	
RELEASE_PRINT: ! r:
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
goto ENDSCR ! /r
 
 
SCREEN3: ! r:
	sn$ = "UBPrtBl1-2"
	fnTos(sn$)
	fnLbl(1,1,"Account (blank to stop)",31,1)
! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
	if trim$(z$)<>"" then
		fnLbl(3,1,"Last Account entered was "&z$ ,44,1)
	end if
	fncmbact(1,17)
	resp$(1)=a$
	fnCmdKey("&Next",1,1,0,"Accept this record for printing")
	fnCmdKey("&Complete",5,0,1,"Print all selected records")
	fnAcs(mat resp$,ckey)
	a$ = lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #8,using L680,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
	if d3=0 then d3=newd3
	if d2=0 then d2=newd2
goto READALTADR ! /r
 
SORT1: ! r: SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1510
	open #6: "Name=[Temp]\Temp."&session$&",Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then
		routekey$=""
	else ! key off first record in route (route # no longer part of customer #)
		routekey$=cnvrt$("N 2",prtbkno)&"       "
	end if
	restore #2,search>=routekey$:
	L1310: !
	read #2,using L1320: z$,f,route eof END5
	L1320: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1350
	if prtbkno><route then goto END5
	L1350: !
	if f><d1 then goto L1310
	zip5$=cr$=""
	read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1380
	L1380: !
	write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
	goto L1310
	END5: !
	close #6:
	open #9: "Name=[Temp]\Control."&session$&",Size=0,RecL=128,Replace",internal,output
	L1430: form pos 1,c 128
	write #9,using L1430: "File [Temp]\Temp."&session$&",,,[Temp]\Addr."&session$&",,,,,A,N"
	write #9,using L1430: "Mask 1,19,C,A"
	close #9:
	execute "Free [Temp]\Addr."&session$&" -n" ioerr ignore
	execute "Sort [Temp]\Control."&session$&" -n"
	open #6: "Name=[Temp]\Temp."&session$,internal,input,relative
	open #7: "Name=[Temp]\Addr."&session$,internal,input,relative
	L1510: !
return ! /r
 
ENDSCR: ! r: pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos(sn$="Bills-Total")
	mylen=23 : mypos=mylen+2
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",0)
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
	fnCmdSet(52)
	fnAcs(mat resp$,ckey)
goto Xit ! /r
Xit: fnXit
 
VBOPENPRINT: ! r:
	fnPa_open("Landscape")
	lyne=3
return ! /r
 
VBPRINT: ! r:
	! -- Standard 4 Per Page Even Perferated Card Stock Bills
	checkcounter+=1
	if checkcounter=1 then xmargin=0 : ymargin=0
	if checkcounter=2 then xmargin=139 : ymargin=0
	if checkcounter=3 then xmargin=0 : ymargin=108
	if checkcounter=4 then
		xmargin=139 : ymargin=108
		checkcounter=0
	end if
 
	pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
	pr #20: "Call Print.MyFontBold(True)"
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.MyFont("Courier New")'
	fnpa_txt(at$(1),xmargin+8,lyne*1-1+ymargin) ! pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
	pr #20: 'Call Print.MyFont("Lucida Console")'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.MyFontBold(False)'
	fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2) ! pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
	fnpa_txt(at$(3),xmargin+6,lyne*3+1+ymargin) ! pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
	pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
	fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin) ! pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
	pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
	pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
 
	PRINTGRID: !
	meter=14
	pr #20: 'Call Print.MyFontSize(8)'
	if g(1) then
		pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(2) then
		pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(3) or d(7) then
		pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(4) then
		pr #20: 'Call Print.AddText("GAS",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(5) then
		pr #20: 'Call Print.AddText("SL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(6) then
		pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(7) then
		pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(8) then
		pr #20: 'Call Print.AddText("Recycle",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if g(9) then
		pr #20: 'Call Print.AddText("GAS TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	end if
	if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
	if pb><0 then
		pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin+2)&')'
	end if
	! If BUDGET>0 Then pB=PBUD ! owe old budget payment
	if pb><0 then ! error when line numbers were removed
		pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin+2)&')'
	end if
	pr #20: 'Call Print.MyFontSize(10)'
 
	if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin+10)&',63,0)'
	! pr #20: 'Call Print.AddText("Budget Payment",'&STR$(XMARGIN+68)&','&STR$(LYNE*12+YMARGIN)&')'
	! If BUDGET>0 Then bAL=BUDGET+PBUD ! IF BUDGET MAKE NET DUE = BUDGET PLUS ANY OLD BUDGET PAYMENTS NOT MADE
	if budget>0 then goto L2350 else goto L2400
	L2350: !
	pr #20: 'Call Print.AddText("Actual Balance",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin+10)&')'
	pr #20: 'Call Print.AddText("Budget Amount",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin+10)&')'
	goto L2440
	L2400: !
	pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin+10)&')'
	pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal+round(bal*.10,2),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*25+ymargin+10)&')'
	L2440: !
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin+10)&',63,0)'
	! pr #20: 'Call Print.AddText("Phone: 217-665-3351",'&STR$(XMARGIN+1)&','&STR$(LYNE*27+YMARGIN)&')'
	! pr #20: 'Call Print.AddText("Re-connect fee $??.00",'&STR$(XMARGIN+1)&','&STR$(LYNE*28+YMARGIN)&')'
 
	special=28
 
	pr #20: 'Call Print.MyFontSize(7)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
	! pr #20: 'Call Print.AddText("   Pre-Sorted",'&STR$(XMARGIN+100)&','&STR$(LYNE*1-1+YMARGIN)&')'
	pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
	pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  Permit No 1",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(9)'
	! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
	pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("'&env$('cnam')&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(10)'
	if budget>0 then
		pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
	else
		pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
		pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal+round(bal*.10,2),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
	end if
	pr #20: 'Call Print.MyFontSize(9)'
	addy=14
	fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
	addy+=1
	pr #20: 'Call Print.MyFontSize(10)'
	if df$="Y" then
		pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
	end if
	if c4>0 then
		pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)&')'
	end if
	pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
	if pe$(1)<>"" then
		fnpa_txt(trim$(pe$(1)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(2)<>"" then
		fnpa_txt(trim$(pe$(2)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(3)<>"" then
		fnpa_txt(trim$(pe$(3)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if pe$(4)<>"" then
		fnpa_txt(trim$(pe$(4)),xmargin+68,lyne*(addy+=1)+ymargin)
	end if
	if checkcounter=1 then checkx=1.375 : checky=3.6875
	if checkcounter=2 then checkx=6.75 : checky=3.6875
	if checkcounter=3 then checkx=1.375 : checky=7.9375
	if checkcounter=0 then
		checkx=6.75 : checky=7.9375
		fnpa_newpage
	end if
return ! /r
 
BULKSORT: ! r: bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #6: "Name=[Temp]\Temp."&session$&",Replace,RecL=31",internal,output
	do
		read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3070
		write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	loop
	L3070: !
	close #1: ioerr ignore
	close #6: ioerr ignore
	L3090: execute "Index [Temp]\Temp."&session$&" [Temp]\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L3110
	open #6: "Name=[Temp]\Temp."&session$&",KFName=[Temp]\TempIdx."&session$,internal,input,keyed
	L3110: !
return ! /r
BUD1: ! r:
	bud1=0
	dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L3200
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative
	bud1=1
	for j=1 to 5
		bd$(j)=str$(j+10)&",20,PIC(##/##/##),U,N"
	next j
	L3200: !
return ! /r
BUD2: ! r:
	budget=pbud=bd1=0
	mat bd1(5)
	mat bd1=(0)
	mat bd2=(0)
	if bud1=0 then goto L3360
	read #81,using L3280,key=z$: z$,mat ba,mat badr nokey L3360
	L3280: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	ta1=badr(1)
	L3300: !
	if ta1=0 then goto L3360
	read #82,using L3320,rec=ta1: z$,mat bt1,nba noRec L3360
	L3320: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	if bt1(1,1)=d1 then budget=budget+bt1(12,1): goto L3350 ! budget for current month
	if bt1(14,1)=0 then pbud=pbud+bt1(12,1): goto L3350 ! budget for any previous months not paid
	L3350: !
	ta1=nba : goto L3300
	L3360: !
return ! /r
include: ertn
