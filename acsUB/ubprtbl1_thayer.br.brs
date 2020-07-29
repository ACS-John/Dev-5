! Replace S:\acsUB\ubprtbl1_thayer
! pr bills for Village of Thayer  (new 4 per page 10/24/06)
!
	autoLibrary
	on error goto Ertn
!
	dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,datafile$*256,indexfile$*256
!
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
!
!
	fnTop("S:\acsUB\ubprtbl1",cap$="Print Bills")
	fn_bulksort
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
!
!
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1T")
	pf=32 : ll=30
	respc=0
	fnLbl(2,1,"Penalty Due Date:",ll,1)
	fnTxt(2,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(4,1,"Message on Bill:",ll,1)
	fnTxt(4,pf,30,30)
	resp$(respc+=1)=mg$(1)
	fnTxt(5,pf,30,30)
	resp$(respc+=1)=mg$(2)
	fnTxt(6,pf,30,30)
	resp$(respc+=1)=mg$(3)
	fnLbl(8,1,"Date of Billing:",ll,1)
	fnTxt(8,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(10,1,"Prior Reading Date Override:",ll,1)
	fnTxt(10,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",reading_date_prior_s1)
	fnLbl(11,1,"Current Reading Date Override:",ll,1)
	fnTxt(11,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",reading_date_cur_s1)
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto ENDSCR
	d4=val(resp$(1))
	mg$(1)=resp$(2)
	mg$(2)=resp$(3)
	mg$(3)=resp$(4)
	d1=val(resp$(5))
	reading_date_prior_s1=val(resp$(6))
	reading_date_cur_s1=val(resp$(7))
	a$=""
	prtbkno=0
	sl1=0
	if trim$(a$)<>"" then
		read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1
		holdz$=z$: begin=1
		st1=1
	end if
L460: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
!
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed
	gosub VBOPENPRINT ! Open #20: "Name=[Q]\UBmstr\Bill"&WSID$&".txt,Replace,RecL=5000",Display,Output
! .   ! fnOPENPRN
!
	on fkey 5 goto RELEASE_PRINT
L550: if sl1=1 then goto SCREEN3
L560: read #6,using L590: z$ eof RELEASE_PRINT
	if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
	begin=0 ! cancel starting account
L590: form pos 22,c 10
	read #1,using L610,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,escrow nokey L560
L610: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1859,pd 5.2
	if prtbkno=0 then goto L640
	if prtbkno><route then goto RELEASE_PRINT
L640: if f><d1 then goto L550
	if st1=0 then goto READALTADR
! If ST1$=Z$ Then sT1=0 Else Goto 560
READALTADR: !
! read alternate billing address
	read #3,using L700,key=z$: mat ba$ nokey L770
L700: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then
			e1=e1+1
			pe$(e1)=ba$(j)
		end if
	next j
	goto L920
!
L770: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then
			e1=e1+1
			pe$(e1)=e$(j)
		end if
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto L920
!
RELEASE_PRINT: !
	close #1: ioerr L860
L860: close #3: ioerr L870
L870: !
!
	fnpa_finis
	goto ENDSCR
!
L920: !
	pb=bal-g(11)
	if bal<=0 then g(5)=g(6)=g(7)=0 ! don't show penalty if balance 0 or less
! print bill routine
	fn_vbprint
! end of pr routine
	bct(2)=bct(2)+1
! .   ! accumulate totals
	goto L550
!
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
	fnCmdSet(3): fnAcs(mat resp$,ckey)
	a$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #1,using L610,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,escrow nokey SCREEN3
	goto READALTADR
!
SORT1: ! SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1390
	open #6: "Name=[Temp]\Temp."&session$&",Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then
		routekey$=""
	else
		routekey$=cnvrt$("N 2",prtbkno)&"       "
! .    ! key off first record in route (route # no longer part of customer #)
	end if
	restore #2,search>=routekey$:
L1190: read #2,using L1200: z$,f,route eof END5
L1200: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1230
	if prtbkno><route then goto END5
L1230: if f><d1 then goto L1190
	zip5$=cr$=""
	read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1260
L1260: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
	goto L1190
!
END5: close #6:
	open #9: "Name=[Temp]\Control."&session$&",Size=0,RecL=128,Replace",internal,output
L1310: form pos 1,c 128
	write #9,using L1310: "File [Temp]\Temp."&session$&",,,[Temp]\Addr."&session$&",,,,,A,N"
	write #9,using L1310: "Mask 1,19,C,A"
	close #9:
	execute "Free [Temp]\Addr."&session$&" -n" ioerr L1360
L1360: execute "Sort [Temp]\Control."&session$&" -n"
	open #6: "Name=[Temp]\Temp."&session$,internal,input,relative
	open #7: "Name=[Temp]\Addr."&session$,internal,input,relative
L1390: return
!
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
! .   ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(2))
! fnLbl(3,1,"Total Bills Not Coded:",MYLEN,1)
! fnTxt(3,MYPOS,8,0,1,"",1)
! .   ! rESP$(RESPC+=1)=CNVRT$("N 8",BCT(1))
! fnLbl(4,1,"Percent of Bills Coded:",MYLEN,1)
! fnTxt(4,MYPOS,8,0,1,"",1)
! .   ! rESP$(RESPC+=1)=CNVRT$("N 8.2",PCT)
	fnCmdSet(52)
	fnAcs(mat resp$,ckey)
Xit: fnXit
!
VBOPENPRINT: !
		fnpa_open("Landscape")
		lyne=3
	return
!
	def fn_vbprint
! -- Standard 4 Per Page Even Perferated Card Stock Bills
		billcounter+=1
		if billcounter=1 then xmargin=4 : ymargin=5
		if billcounter=2 then xmargin=143 : ymargin=5
		if billcounter=3 then xmargin=4 : ymargin=113
		if billcounter=4 then xmargin=143 : ymargin=113 : billcounter=0
!
! pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
		if reading_date_cur_s1=0 then reading_date_cur=d3 else reading_date_cur=reading_date_cur_s1
		if reading_date_prior_s1=0 then reading_date_prior=d2 else reading_date_prior=reading_date_prior_s1
		fnpa_line(xmargin+5,ymargin+2,57,lyne*3+3, 1)
		pr #20: "Call Print.MyFontBold(True)"
		pr #20: 'Call Print.MyFontSize(12)'
		pr #20: 'Call Print.MyFont("Courier New")'
		pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
		pr #20: 'Call Print.MyFont("Lucida Console")'
		pr #20: 'Call Print.MyFontSize(10)'
		pr #20: 'Call Print.MyFontBold(False)'
		pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
		pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
		fnpa_txt('#'&trim$(z$),xmargin+4,lyne*5+ymargin)
		pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
		pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_prior/100))&'  To: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_cur/100))&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
		pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
		pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
! pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
		fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
		pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
		pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
		pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
!
PRINTGRID: !
		meter=14
		pr #20: 'Call Print.MyFontSize(8)'
		if g(1)<>0 then
			pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(2)<>0 then
			pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(3)<>0 then
			pr #20: 'Call Print.AddText("Admin",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if a4=1 then
			gcode$="RSGS"
		else if a4=2 then
			gcode$="CMGS"
		else if a4=3 then
			gcode$="INGS"
		else
			gcode$="GAS"
		end if
		if g(4)<>0 then
			pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(5)<>0 or g(6)<>0 or g(7)<>0 then
			pr #20: 'Call Print.AddText("PEN",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(5)+g(6)+g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
! if g(6)<>0 then
!  pr #20: 'Call Print.AddText("FUR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
!  pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
! . end if
		if g(10)<>0 then
			pr #20: 'Call Print.AddText("Penalty",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(8)<>0 then
			pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if g(9)<>0 then
			pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
		if pb><0 then
			pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(10)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
		if pb<>0 then
			pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
			pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
		end if
! If ESCROW>0 Then
! pr #20: 'Call Print.AddText("Escrow CR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(escrow,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'\
! end if
		pr #20: 'Call Print.MyFontSize(10)'
!
		if estimatedate=d1 then let fnpa_txt("Bill estimated!",xmargin+1,lyne*27+ymargin)
		fnpa_line(xmargin+1,lyne*23+1+ymargin,63,0)
		fnpa_txt("Pay By    "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+1,lyne*24+ymargin)
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*24+ymargin)
		fnpa_txt("Pay After "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+1,lyne*25+ymargin)
		if bal>0 then
			fnpa_txt(fnformnumb$(round(bal*1.1,2),2,9),xmargin+42,lyne*25+ymargin)
		else
			fnpa_txt(fnformnumb$(bal,2,9),xmargin+42,lyne*25+ymargin)
		end if
		fnpa_line(xmargin+1,lyne*26+1+ymargin,63,0)
! fnpa_txt("  Office 217-628-3416",xmargin+1,lyne*28.5+ymargin)
		fnpa_txt("Address Correction Requested",xmargin+1,lyne*27.9+ymargin)
		fnpa_txt(mg$(1),xmargin+1,29*lyne+ymargin)
		fnpa_txt(mg$(2),xmargin+1,32*lyne+ymargin)
		fnpa_txt(mg$(3),xmargin+1,33*lyne+ymargin)
!
		special=28
!
		pr #20: 'Call Print.MyFontSize(7)'
		pr #20: 'Call Print.AddLine('&str$(xmargin+97)&','&str$(ymargin+0)&',29,'&str$(lyne*5+2)&',TRUE)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+0)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+2.8)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+5.6)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+8.4)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+11.2)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+14)&',7,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+90)&','&str$(ymargin+17)&',7,0)'
		fnpa_txt("FIRST CLASS MAIL",xmargin+100,lyne*1-1+ymargin)
		fnpa_txt("U.S. POSTAGE PAID",xmargin+100,lyne*2-1+ymargin)
		fnpa_txt("   ONE OUNCE",xmargin+100,lyne*3-1+ymargin)
		fnpa_txt("THAYER, IL 62689",xmargin+100,lyne*4-1+ymargin)
		fnpa_txt("  PERMIT NO. 1",xmargin+100,lyne*5-1+ymargin)
		pr #20: 'Call Print.MyFontSize(9)'
		fnpa_txt("Please return this",xmargin+68,lyne*7+ymargin)
		fnpa_txt("side with payment to",xmargin+68,lyne*8+ymargin)
		fnpa_txt("Thayer Water/Sewer Dept.",xmargin+68,lyne*9+ymargin)
		pr #20: 'Call Print.MyFontSize(10)'
		fnpa_txt("Pay By "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+68,lyne*11+ymargin)
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*11+ymargin)
		fnpa_txt("After  "&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&":",xmargin+68,lyne*12+ymargin)
		if bal>0 then
! fnpa_txt('3Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',csp-2,factor+line_height*11)
			fnpa_txt(fnformnumb$(round(bal*1.1,2),2,9),xmargin+106,lyne*12+ymargin)
		else
			fnpa_txt(fnformnumb$(bal,2,9),xmargin+106,lyne*12+ymargin)
		end if
		pr #20: 'Call Print.MyFontSize(9)'
		addy=18 ! 14
! fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
! fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
! fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
! addy+=1
		pr #20: 'Call Print.MyFontSize(10)'
		if df$="Y" then
			fnpa_txt("Drafted",xmargin+1,lyne*(addy+=1)+ymargin)
		end if
		if final>0 then let fnpa_txt("Final Bill",xmargin+1,lyne*(addy+5)+ymargin)
		pr #20: 'Call Print.MyFontSize(12)'
		addy+=.5
		fnpa_txt('#'&trim$(z$),xmargin+75,lyne*(addy+=1.1)+ymargin)
		if pe$(1)<>"" then let fnpa_txt(trim$(pe$(1)),xmargin+75,lyne*(addy+=1.1)+ymargin)
		if pe$(2)<>"" then let fnpa_txt(trim$(pe$(2)),xmargin+75,lyne*(addy+=1.1)+ymargin)
		if pe$(3)<>"" then let fnpa_txt(trim$(pe$(3)),xmargin+75,lyne*(addy+=1.1)+ymargin)
		if pe$(4)<>"" then let fnpa_txt(trim$(pe$(4)),xmargin+75,lyne*(addy+=1.1)+ymargin)
!
		if billcounter=1 then checkx=1.375 : checky=3.6875
		if billcounter=2 then checkx=6.75 : checky=3.6875
		if billcounter=3 then checkx=1.375 : checky=7.9375
		if billcounter=0 then checkx=6.75 : checky=7.9375
		bc$=""
		if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
		if billcounter=0 then
			fnpa_newpage
		end if
	fnend
!
def fn_bulksort ! bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #6: "Name=[Temp]\Temp."&session$&",Replace,RecL=31",internal,output
	L2790: !
	read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2820
	write #6,using "Form POS 1,C 12,n 2,n 7,c 10": z$,0,0,z$ ! just do it in account order
	! write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$ ! origional order
	goto L2790
	L2820: !
	close #1: ioerr ignore
	close #6: ioerr ignore
	execute "Index [Temp]\Temp."&session$&" [Temp]\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2860
	open #6: "Name=[Temp]\Temp."&session$&",KFName=[Temp]\TempIdx."&session$,internal,input,keyed
	L2860: !
fnend
include: Ertn
