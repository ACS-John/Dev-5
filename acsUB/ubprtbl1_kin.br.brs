! Replace S:\acsUB\ubprtbl1_kin
! pr bills for Village of Kincaid  (4 part plain paper)

	autoLibrary
	on error goto Ertn

	dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256

	fncno(cno,cnam$) : _
	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input  : _
	read #21,using "form pos 41,2*C 40": at$(2),at$(3) : _
	close #21:
	at$(1)=cnam$ : _
	z=21 : _
	at$(1)=trim$(at$(1))(1:z) : _
	x=len(at$(1)) : y=z-x : _
	at$(1)=rpt$(" ",int(y/2))&at$(1)
	z=26 : _
	for j=2 to udim(at$) : _
		at$(j)=trim$(at$(j))(1:z) : _
		x=len(at$(j)) : y=z-x : _
		at$(j)=rpt$(" ",int(y/2))&at$(j) : _
	next j
	linelength=62

	fnTop("S:\acsUB\ubprtbl1",cap$="Print Bills")
	gosub BULKSORT
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k  ! open in route-sequence #

SCREEN1: !
	a$="" : prtbkno=0
	mg$(1)='Temp Return Service Requested'
	fnTos(sn$="UBPrtBl1-1") : _
	pf=26 : ll=24 : _
	respc=0
	fnLbl(3,1,"Penalty Due Date:",ll,1)
	fnTxt(3,pf,8,8,1,"1",0,tt$) : _
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(4,1,"Message on Bill:",ll,1)
	fnTxt(4,pf,30,30) : _
	resp$(respc+=1)=mg$(1)
	fnTxt(5,pf,30,30) : _
	resp$(respc+=1)=mg$(2)
	fnTxt(6,pf,30,30) : _
	resp$(respc+=1)=mg$(3)
	fnLbl(7,1,"Date of Billing:",ll,1)
	fnTxt(7,pf,8,8,1,"1") : _
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(8,1,"Starting Account:",ll,1)
	fe$="ubm-act-nam" : _
	datafile$="[Q]\UBmstr\Customer.h[cno]" : _
	indexfile$="[Q]\UBmstr\ubindx5.h[cno]" : _
	kp=1741: kl=9 : dp=41 : dl=30 : _
	fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) : _
	resp$(respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf) : _
	resp$(respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print",1) : _
	resp$(respc+=1)='False'
	fnCmdSet(3) : _
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ENDSCR
	d1 = val(resp$(5)) : _
	d4 = val(resp$(1)) : _
	mg$(1) = resp$(2) : _
	mg$(2) = resp$(3) : _
	mg$(3) = resp$(4)
	if resp$(6)="[All]" then : _
		a$="" else : _
		a$ = lpad$(trim$(resp$(6)(1:9)),9)
	if resp$(7)="[All]" then : _
		prtbkno=0 else : _
		prtbkno = val(resp$(7))
	if resp$(8)='True' then sl1=1: z$="" else sl1=0
	if trim$(a$)<>"" then read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1 : _
		holdz$=z$: begin=1 : _
		st1=1
L460: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
! If TRIM$(A$)="" AND PRTBKNO>0 Then Restore #2,Key>=CNVRT$("pic(zz)",PRTBKNO)&"       ": ! selected a route and no beginning Account

	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",i,i,k
	gosub VBOPENPRINT

	on fkey 5 goto RELEASE_PRINT
L550: if sl1=1 then goto SCREEN3
L560: read #6,using L590: z$ eof RELEASE_PRINT
	if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
	begin=0 ! cancel starting account
L590: form pos 22,c 10
	read #1,using L620,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L560
	if prtbkno>0 and route<>prtbkno then goto L560
L620: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
	if prtbkno=0 then goto L650
	if prtbkno><route then goto L560
L650: if f><d1 then goto L550
	if st1=0 then goto READALTADR
! If ST1$=Z$ Then sT1=0 Else Goto 560
READALTADR: !
! read alternate billing address
	read #3,using L710,key=z$: mat ba$ nokey L800
L710: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=ba$(j)
	next j
	if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
	if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
	goto L950

L800: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=e$(j)
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto L950

RELEASE_PRINT: !
	close #1: ioerr L890
L890: close #3: ioerr L900
L900: fnpa_finis
	goto ENDSCR

L950: !
	pb=bal-g(11)
	if bal<=0 then g(10)=0 ! don't show penalty if balance 0 or less
! print bill routine
	gosub VBPRINT
! end of pr routine
	bct(2)=bct(2)+1 : _
	! accumulate totals
	goto L550

SCREEN3: !
	sn$ = "UBPrtBl1-2" : _
	fnTos(sn$)
	txt$="Account (blank to stop)" : _
	fnLbl(1,1,txt$,31,1)
! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
	if trim$(z$)<>"" then : _
		txt$="Last Account entered was "&z$ : _
		fnLbl(3,1,txt$,44,1) else : _
		txt$="" : _
		fnLbl(3,1,txt$,44,1)
	fncmbact(1,17) ! : _
	resp$(1)=a$
	fnCmdSet(3): ckey=fnAcs(mat resp$)
	a$ = lpad$(trim$(resp$(1)(1:10)),10) : _
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #1,using L620,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
	goto READALTADR

SORT1: ! SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",i,i,k ioerr L1420
	open #6: "Name=[Temp]\Temp.[Session],Replace,RecL=19",internal,output
	s5=1
	routekey$="" ! If PRTBKNO=0 Then rOUTEKEY$="" Else : _
	! rOUTEKEY$=CNVRT$("N 2",PRTBKNO)&"       " : _
	! key off first record in route (route # no longer part of customer #)
	restore #2,search>=routekey$:
L1220: read #2,using L1230: z$,f,route eof END5
L1230: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1260
	if prtbkno><route then goto L1220
L1260: if f><d1 then goto L1220
	zip5$=cr$=""
	read #5,using "form pos 96,C 5,pos 108,C 4",key=z$: zip5$,cr$ nokey L1290
L1290: write #6,using "form pos 1,C 5,C 4,C 10": zip5$,cr$,z$
	goto L1220

END5: close #6:
	open #9: "Name=[Temp]\Control.[Session],Size=0,RecL=128,Replace",internal,output
L1340: form pos 1,c 128
	write #9,using L1340: "File [Temp]\Temp.[Session],,,[Temp]\Addr.[Session],,,,,A,N"
	write #9,using L1340: "Mask 1,19,C,A"
	close #9:
	execute "Free [Temp]\Addr.[Session] -n" ioerr L1390
L1390: execute "Sort [Temp]\Control.[Session] -n"
	open #6: "Name=[Temp]\Temp."&session$,i,i,r
	open #7: "Name=[Temp]\Addr."&session$,i,i,r
L1420: return

ENDSCR: ! pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos(sn$="Bills-Total") : _
	mylen=23 : mypos=mylen+2 : _
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1) : _
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
! fnLbl(2,1,"Total  Bills  Coded:",MYLEN,1)
! fnTxt(2,MYPOS,8,0,1,"",1) : _
	! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
! fnLbl(3,1,"Total Bills Not Coded:",MYLEN,1)
! fnTxt(3,MYPOS,8,0,1,"",1) : _
	! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
! fnLbl(4,1,"Percent of Bills Coded:",MYLEN,1)
! fnTxt(4,MYPOS,8,0,1,"",1) : _
	! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
	fnCmdSet(52) : _
	ckey=fnAcs(mat resp$)
Xit: fnXit


VBOPENPRINT: !
		fnPa_open("Landscape")
		lyne=3
return

VBPRINT: !
! -- Standard 4 Per Page Even Perferated Card Stock Bills
	checkcounter+=1
	if checkcounter=1 then xmargin=0 : ymargin=0
	if checkcounter=2 then xmargin=139 : ymargin=0
	if checkcounter=3 then xmargin=0 : ymargin=108
	if checkcounter=4 then xmargin=139 : ymargin=108 : _
		checkcounter=0

	pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
	pr #20: "Call Print.MyFontBold(True)"
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.MyFont("Courier New")'
	pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
	pr #20: 'Call Print.MyFont("Lucida Console")'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.MyFontBold(False)'
	pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
	pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
	pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
	pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
	pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3)&'",'&str$(xmargin+2)&','&str$(lyne*7+1+ymargin)&')'
	pr #20: 'Call Print.AddText("                       ",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
	pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'

PRINTGRID: meter=14 : _
	pr #20: 'Call Print.MyFontSize(8)'
	if g(1)=0 then goto L2050 else : _
		pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2050: if g(2)=0 then goto L2060 else : _
		pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2060: if g(3)=0 and d(7)=0 then goto L2080 else : _
		pr #20: 'Call Print.AddText("LM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
!  pr #20: 'Call Print.AddText("'&FNFORMNUMB$(G(3),2,9)&'",'&STR$(XMARGIN+45)&','&STR$(LYNE*METER+YMARGIN)&')'
L2080: if a4=1 then gcode$="RSGS" else : _
		if a4=2 then gcode$="CMGS" else : _
			if a4=3 then gcode$="INGS" else : _
				gcode$="GAS"
	if g(4)=0 then goto L2100 else : _
		pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2100: if g(5)=0 then goto L2110 else : _
		pr #20: 'Call Print.AddText("WSur",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2110: if g(6)=0 then goto L2120 else : _
		pr #20: 'Call Print.AddText("SSur",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2120: if g(7)=0 then goto L2130 else : _
		pr #20: 'Call Print.AddText("FUEL ADJ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2130: if g(8)=0 then goto L2140 else : _
		pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2140: if g(9)=0 then goto L2150 else : _
		pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2150: if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
	if pb><0 then pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
	if pb=0 then goto L2180 else : _
		pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')' : _
		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
L2180: pr #20: 'Call Print.MyFontSize(10)'

	if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*20+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*22+1+ymargin)&',63,0)'
	pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*23+ymargin)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+42)&','&str$(lyne*23+ymargin)&')'
	pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+42)&','&str$(lyne*24+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*25+1+ymargin)&',63,0)'
	pr #20: 'Call Print.AddText("Phone: 217-237-4713",'&str$(xmargin+1)&','&str$(lyne*26+ymargin)&')'
	pr #20: "Call Print.MyFontBold(True)"
	pr #20: 'Call Print.AddText("Shut off notices will",'&str$(xmargin+8)&','&str$(lyne*27+ymargin)&')'
	pr #20: 'Call Print.AddText("no longer be mailed.",'&str$(xmargin+10)&','&str$(lyne*28+ymargin)&')'
	pr #20: 'Call Print.AddText("Last Tuesday of the month",'&str$(xmargin+8)&','&str$(lyne*29+ymargin)&')'
	pr #20: 'Call Print.AddText("at 9 AM water will be",'&str$(xmargin+8)&','&str$(lyne*30+ymargin)&')'
	pr #20: 'Call Print.AddText("disconnected if unpaid.",'&str$(xmargin+8)&','&str$(lyne*31+ymargin)&')'
	pr #20: 'Call Print.MyFontBold(False)'

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
	pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
	pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
	pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  Permit No 13",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(9)'
! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
	pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(bal+g(10),2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(9)'
	addy=14
	pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
	pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
	pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
	addy+=1
	pr #20: 'Call Print.MyFontSize(10)'
	if df$="Y" then : _
		pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
	if c4>0 then : _
		pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
	pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
	if pe$(1)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=2)+ymargin)&')'
	if pe$(2)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
	if pe$(3)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
	if pe$(4)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68)&','&str$(lyne*(addy+=1)+ymargin)&')'
	if checkcounter=1 then checkx=1.375 : checky=3.6875
	if checkcounter=2 then checkx=6.75 : checky=3.6875
	if checkcounter=3 then checkx=1.375 : checky=7.9375
	if checkcounter=0 then checkx=6.75 : checky=7.9375
! bc$=""
! if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
	if checkcounter=0 then : _
		fnpa_newpage
return

BULKSORT: ! bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
	open #6: "Name=[Temp]\Temp.[Session],Replace,RecL=31",internal,output
L2810: read #1,using "form pos 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2840
	write #6,using "form pos 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	goto L2810
L2840: close #1: ioerr L2850
L2850: close #6: ioerr L2860
L2860: execute "Index [Temp]\Temp.[Session] [Temp]\TempIdx.[Session] 1,19,Replace,DupKeys -n" ioerr L2880
	open #6: "Name=[Temp]\Temp.[Session],KFName=[Temp]\TempIdx."&session$,i,i,k
L2880: return
include: ertn
