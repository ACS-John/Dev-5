! Replace S:\acsUB\ubprtbl1_chatom ! based on ubprtbl1_mow
! pr bills for Chatom

	autoLibrary
	on error goto Ertn

	dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256

	fnLastBillingDate(d1)
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",i,i
	read #21,using "form pos 41,2*C 40": at$(2),at$(3)
	close #21:
	cnam$='Chatom Utilities'
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
	fn_bulksort
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",i,i,k  ! open in route-sequence #

SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1")
	pf=26 : ll=24
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
	fe$="ubm-act-nam"
	datafile$="[Q]\UBmstr\Customer.h[cno]"
	indexfile$="[Q]\UBmstr\ubindx5.h[cno]"
	kp=1741: kl=9 : dp=41 : dl=30
	fnComboF(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2)
	resp$(respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf)
	resp$(respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print",1)
	resp$(respc+=1)='False'
	fnCmdSet(3)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto ENDSCR
	d1=val(resp$(5))
	d4=val(resp$(1))
	mg$(1)=resp$(2)
	mg$(2)=resp$(3)
	mg$(3)=resp$(4)
	if resp$(6)="[All]" then a$="" else a$=lpad$(trim$(resp$(6)(1:9)),9)
	if resp$(7)="[All]" then prtbkno=0 else prtbkno=val(resp$(7))
	if resp$(8)='True' then sl1=1: z$="" else sl1=0
	if trim$(a$)<>"" then
		read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1
		st1=1
		st1$=z$
	end if
L460: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account

	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",i,i,k ioerr FAIL_OPEN_3
FAIL_OPEN_3: !
	fnPa_open("Landscape")
	lyne=3
	character=1.5

	on fkey 5 goto RELEASE_PRINT
L550: if sl1=1 then goto SCREEN3
L560: read #6,using L570: z$ eof RELEASE_PRINT
L570: form pos 22,c 10
	read #1,using L590,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey L560
L590: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1743,n 7
	if prtbkno=0 then goto L620
	if prtbkno><route then goto RELEASE_PRINT
L620: if f><d1 then goto L550
	if st1=0 then goto READALTADR
	if st1$=z$ then st1=0 else goto L550
READALTADR: !
! read alternate billing address
	read #3,using L680,key=z$: mat ba$ nokey L750 ioerr L750
L680: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then e1=e1+1 : pe$(e1)=ba$(j)
	next j
	goto L900

L750: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then e1=e1+1 : pe$(e1)=e$(j)
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto L900

RELEASE_PRINT: !
	close #1: ioerr L840
L840: close #3: ioerr L850
L850: fnpa_finis
	goto ENDSCR

L900: !
	pb=bal-g(11)
	if bal<=0 then g(10)=0 ! don't show penalty if balance 0 or less
! print bill routine
	fn_vbprint
! end of pr routine
	bct(2)=bct(2)+1 ! accumulate totals
	goto L550

SCREEN3: !
	sn$="UBPrtBl1-2"
	fnTos(sn$)
	txt$="Account (blank to stop)"
	fnLbl(1,1,txt$,31,1)
! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
	if trim$(z$)<>"" then
		txt$="Last Account entered was "&z$
		fnLbl(3,1,txt$,44,1)
	else
		txt$=""
		fnLbl(3,1,txt$,44,1)
	end if
	fncmbact(1,17) !
	resp$(1)=a$
	fnCmdSet(3): ckey=fnAcs(mat resp$)
	a$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #1,using L590,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,sequence nokey SCREEN3
	goto READALTADR

SORT1: ! SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",i,i,k ioerr L1370
	open #6: "Name=[Temp]\Temp.[Session],Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
	restore #2,search>=routekey$:
L1170: read #2,using L1180: z$,f,route eof END5
L1180: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1210
	if prtbkno><route then goto END5
L1210: if f><d1 then goto L1170
	zip5$=cr$=""
	read #5,using "form pos 96,C 5,pos 108,C 4",key=z$: zip5$,cr$ nokey L1240
L1240: write #6,using "form pos 1,C 5,C 4,C 10": zip5$,cr$,z$
	goto L1170

END5: close #6:
	open #9: "Name=[Temp]\Control.[Session],Size=0,RecL=128,Replace",internal,output
L1290: form pos 1,c 128
	write #9,using L1290: "File [Temp]\Temp.[Session],,,[Temp]\Addr.[Session],,,,,A,N"
	write #9,using L1290: "Mask 1,19,C,A"
	close #9:
	execute "Free [Temp]\Addr."&session$ ioerr L1340
L1340: execute "Sort [Temp]\Control."&session$
	open #6: "Name=[Temp]\Temp."&session$,i,i,r
	open #7: "Name=[Temp]\Addr."&session$,i,i,r
L1370: return

ENDSCR: ! pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos(sn$="Bills-Total")
	mylen=23 : mypos=mylen+2
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1)
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
! fnLbl(2,1,"Total  Bills  Coded:",MYLEN,1)
! fnTxt(2,MYPOS,8,0,1,"",1)
! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
! fnLbl(3,1,"Total Bills Not Coded:",MYLEN,1)
! fnTxt(3,MYPOS,8,0,1,"",1)
! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
! fnLbl(4,1,"Percent of Bills Coded:",MYLEN,1)
! fnTxt(4,MYPOS,8,0,1,"",1)
! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
	fnCmdSet(52)
	ckey=fnAcs(mat resp$)
Xit: fnXit

ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto L1570
	execute "List -"&str$(line) : pause : goto L1570
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L1570: execute act$
	goto ERTN

def fn_vbprint
! -- Standard 4 Per Page Even Perferated Card Stock Bills
		checkcounter+=1
		if checkcounter=1 then xmargin=0 : ymargin=0
		if checkcounter=2 then xmargin=140 : ymargin=0
		if checkcounter=3 then xmargin=0 : ymargin=108
		if checkcounter=4 then xmargin=140 : ymargin=108 : checkcounter=0

		pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',55,'&str$(lyne*3+3)&',True)'
		pr #20: "Call Print.MyFontBold(True)"
		pr #20: 'Call Print.MyFontSize(10)'
		pr #20: 'Call Print.MyFont("Courier New")'
		fnpa_txt(cnam$,xmargin+16,lyne*1-1+ymargin)
		pr #20: 'Call Print.MyFont("Lucida Console")'
		pr #20: 'Call Print.MyFontSize(10)'
		pr #20: 'Call Print.MyFontBold(False)'
		fnpa_txt(at$(2),xmargin+6,lyne*2+1+ymargin-.2)
		fnpa_txt(at$(3),xmargin+6 ,lyne*3+1+ymargin)
		fnpa_txt("#"&trim$(z$)&'  '&bulk$,xmargin+4,lyne*5+ymargin)
		fnpa_txt(e$(1),xmargin+4,lyne*6+ymargin)
! fnpa_txt("From: "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d3),xmargin+2,lyne*7+ymargin)
		pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
		pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
		pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
		pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
		pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
		pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'

PRINTGRID: meter=14
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
		if g(3) then
			pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
L2020: !
		if a4=1 then
			gcode$="RSGS"
		else if a4=2 then
			gcode$="CMGS"
		else if a4=3 then
			gcode$="INGS"
		else
			gcode$="GAS"
		end if
		if g(4) then
			fnpa_txt(gcode$,xmargin+1,lyne*(meter+=1)+ymargin)
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(5) then
			pr #20: 'Call Print.AddText("SAN",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(6) then
			pr #20: 'Call Print.AddText("FP",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(7) then
			pr #20: 'Call Print.AddText("FUEL ADJ",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(8) then
			pr #20: 'Call Print.AddText("Misc",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(9) then
			pr #20: 'Call Print.AddText("Tax",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if pb then
			pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		pr #20: 'Call Print.MyFontSize(10)'

		pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin)&',63,0)'
		pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin)&')'
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+40,lyne*24+ymargin)
		pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin)&')'
		fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+40,lyne*25+ymargin)
		pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*26+1+ymargin)&',63,0)'
		fnpa_txt("Phone: 251-847-2858",xmargin+1,lyne*27+ymargin)
		pr #20: 'Call Print.MyFontSize(8)'
		fnpa_txt("Service will be discontinued after ",xmargin+1,lyne*28+ymargin)
		fnpa_txt("due date without further notice.",xmargin+1,lyne*29+ymargin)
		pr #20: 'Call Print.MyFontSize(10)'

		special=28

! pr #20: 'Call Print.MyFontSize(7)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
! pr #20: 'Call Print.AddText("   Pre-Sorted",'&str$(xmargin+100)&','&str$(lyne*1-1+ymargin)&')'
! pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
! pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
! pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
! pr #20: 'Call Print.AddText("  Permit No 38",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
		pr #20: 'Call Print.MyFontSize(9)'
! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
		fnpa_txt("Please return this",xmargin+68,lyne*11+ymargin)
		fnpa_txt("side with payment to:",xmargin+68,lyne*12+ymargin)
		fnpa_txt(cnam$,xmargin+68,lyne*13+ymargin)
		pr #20: 'Call Print.MyFontSize(10)'
		fnpa_txt("Pay By "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+68,lyne*25+ymargin)
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*25+ymargin)
		fnpa_txt('After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+68,lyne*29+ymargin)
		fnpa_txt(fnformnumb$(bal+g(10),2,9),xmargin+106,lyne*29+ymargin)
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
			pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
		end if
		if d(10)=1 then
			pr #20: 'Call Print.AddText("Bill Estimated",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
		end if
		fnpa_txt("#"&trim$(z$)&' '&bulk$,xmargin+68,lyne*(addy+=1)+ymargin)
		fnpa_txt(pe$(1),xmargin+68,lyne*(addy+=1)+ymargin)
		fnpa_txt(pe$(2),xmargin+68,lyne*(addy+=1)+ymargin)
		fnpa_txt(pe$(3),xmargin+68,lyne*(addy+=1)+ymargin)
		fnpa_txt(pe$(4),xmargin+68,lyne*(addy+=1)+ymargin)
		if checkcounter=1 then checkx=1.375 : checky=3.6875
		if checkcounter=2 then checkx=6.75 : checky=3.6875
		if checkcounter=3 then checkx=1.375 : checky=7.9375
		if checkcounter=0 then checkx=6.75 : checky=7.9375
		if checkcounter=0 then fnpa_newpage
fnend

def fn_bulksort ! bulk sort order
		open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k  ! open in Account order
		open #6: "Name=[Temp]\Temp.[Session],Replace,RecL=31",internal,output
L2730: read #1,using "form pos 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2760
		write #6,using "form pos 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
		goto L2730
L2760: close #1: ioerr ignore
		close #6: ioerr ignore
		execute "Index [Temp]\Temp.[Session] [Temp]\Tempidx."&wsid$&" 1,19,Replace,DupKeys -n" ! ioerr L2800
		open #6: "Name=[Temp]\Temp.[Session],KFName=[Temp]\Tempidx."&wsid$,i,i,k
! L2800: !
fnend
