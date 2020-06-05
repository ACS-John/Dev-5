! Replace S:\acsUB\ubprtbl1_ll
! pr bills for Town of Loma Linda
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(10)*80,txt$*45,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
 
	fncno(cno,cnam$) : _
	fnLastBillingDate(d1)
	addr_indent=8 : addr_down=3
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input  : _
	read #21,using "Form POS 41,2*C 40": at$(2),at$(3) : _
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
 
! fnTop("S:\acsUB\ubprtbl1",cap$="Print Bills")
	gosub BULKSORT ! want printed in alphabetic order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
!  open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
	open #8: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
 
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1") : _
	pf=33 : ll=30 : _
	respc=0
	fnLbl(1,1,"Message on Bill:",ll,1)
	fnTxt(1,pf,30,30) : _
	resp$(respc+=1)=mg$(1)
	fnLbl(3,1,"Date of Billing:",ll,1)
	fnTxt(3,pf,8,8,1,"1") : _
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(5,1,"Starting Account:",ll,1)
	fe$="ubm-act-nam" : _
	datafile$="[Q]\UBmstr\Customer.h[cno]" : _
	indexfile$="[Q]\UBmstr\ubindx5.h[cno]" : _
	kp=1741: kl=9 : dp=41 : dl=30 : _
	fncombof(fe$,5,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2) : _
	resp$(respc+=1)="[All]"
	fnLbl(7,1,"Route Number:",ll,1)
	fncmbrt2(7,pf) : _
	resp$(respc+=1)="[All]"
	fnChk(9,pf,"Select Accounts to Print:",1) : _
	resp$(respc+=1)="False"
	fnCmdSet(3) : _
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto ENDSCR
	d4=date(days(d1,'mmddyy')+30,'mmddyy')
	mg$(1) = resp$(1)
	d1 = val(resp$(2))
	if resp$(3)="[All]" then : _
		a$="" else : _
		a$ = lpad$(trim$(resp$(3)(1:9)),9)
	if resp$(4)="[All]" then : _
		prtbkno=0 else : _
		prtbkno = val(resp$(4))
	if resp$(5)="True" then sl1=1: z$="" else sl1=0
	if trim$(a$)<>"" then read #2,using L500,key=a$: z$,route,sequence nokey SCREEN1 : _
		holdz$=z$: begin=1 : _
		st1=1
L500: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed
	gosub BUD1
	gosub VBOPENPRINT
 
L600: if sl1=1 then goto SCREEN3
L610: read #6,using L640: z$ eof RELEASE_PRINT
	if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L610 ! start with
	begin=0 ! cancel starting account
L640: form pos 22,c 10
	read #1,using L680,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4 nokey L610
! read #1,using L680: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4 eof RELEASE_PRINT
	if d3=0 then d3=extra_3
	if d2=0 then d2=extra_4
L680: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1750,2*n 6
	if prtbkno=0 then goto L710
	if prtbkno><route then goto RELEASE_PRINT
L710: if f><d1 then goto L600
	if st1=0 then goto READALTADR
! If ST1$=Z$ Then sT1=0 Else Goto 560
READALTADR: !
! read alternate billing address
	read #3,using L770,key=z$: mat ba$ nokey L840
L770: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=ba$(j)
	next j
	goto L1000
 
L840: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=e$(j)
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto L1000
 
RELEASE_PRINT: !
	close #1: ioerr L930
L930: close #3: ioerr L940
L940: !
	fnpa_finis
	fnconsole(0)
	goto ENDSCR
 
L1000: !
	if bud1=1 then gosub BUD2
	pb=bal-g(11)
! If BAL<=0 Then g(10)=0 ! don't show penalty if balance 0 or less
! ______________print bill routine______________________________________
	gosub VBPRINT
! _____________end of pr routine______________________________________
	bct(2)=bct(2)+1 : _
	! accumulate totals
	goto L600
 
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
	fnCmdKey("&Next",1,1,0,"Accept this record for printing") : _
	fnCmdKey("&Complete",5,0,1,"Print all selected records")
	fnAcs2(mat resp$,ckey)
	a$ = lpad$(trim$(resp$(1)(1:10)),10) : _
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #8,using L680,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4 nokey SCREEN3
	if d3=0 then d3=extra_3
	if d2=0 then d2=extra_4
	goto READALTADR
 
SORT1: ! SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1510
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then routekey$="" else : _
		routekey$=cnvrt$("N 2",prtbkno)&"       " : _
		! key off first record in route (route # no longer part of customer #)
	restore #2,search>=routekey$:
L1310: read #2,using L1320: z$,f,route eof END5
L1320: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1350
	if prtbkno><route then goto END5
L1350: if f><d1 then goto L1310
	zip5$=cr$=""
	read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1380
L1380: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
	goto L1310
 
END5: close #6:
	open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output
L1430: form pos 1,c 128
	write #9,using L1430: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
	write #9,using L1430: "Mask 1,19,C,A"
	close #9:
	execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1480
L1480: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative
	open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative
L1510: return
 
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
	fnAcs2(mat resp$,ckey)
Xit: fnXit
 
ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto L1710
	execute "list -"&str$(line) : _
	pause  : _
	goto L1710
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L1710: execute act$
	goto ERTN
 
VBOPENPRINT: !
		fnPa_open("Landscape")
		lyne=3
return
 
VBPRINT: !
! -- Standard 4 Per Page Even Perferated Card Stock Bills
	checkcounter+=1
	if checkcounter=1 then xmargin=2 : ymargin=10
	if checkcounter=2 then xmargin=148 : ymargin=10
	if checkcounter=3 then xmargin=2 : ymargin=108
	if checkcounter=4 then xmargin=148 : ymargin=108 : checkcounter=0
 
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
! pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",extra_4)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",extra_3)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("Due upon receipt",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("'&e$(2)&'",'&str$(xmargin+2)&','&str$(lyne*9+ymargin)&')'
	pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
	pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+12)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+35)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+52)&','&str$(lyne*13+ymargin)&')'
 
PRINTGRID: !
	meter=14
	pr #20: 'Call Print.MyFontSize(10)'
	if g(1)<>0 then
		pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,7)&'",'&str$(xmargin+24)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(1)<>0
	if g(2)<>0 then
		pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,7)&'",'&str$(xmargin+24)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(2)<>0
	if g(3)<>0 or d(7)<>0 then
		pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("EL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(3)<>0 or d(7)<>0
! If A4=1 Then gCODE$="RSGS" Else If A4=2 Then gCODE$="CMGS" Else If A4=3 Then gCODE$="INGS" Else
	gcode$="GAS"
	if g(4)<>0 then
		pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(4)<>0
	if g(5)<>0 then
		pr #20: 'Call Print.AddText("SL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("SL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(5)<>0
	if g(6)<>0 then
		pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(6)<>0
	if g(7)<>0 then
		pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(7)=0
	if g(8)<>0 then
		pr #20: 'Call Print.AddText("Other",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("Other",'&str$(xmargin+01)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(8)<>0
	if g(9)<>0 then
		pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
! pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(9)<>0
! if pb><0 then
	pr #20: 'Call Print.AddLine('&str$(xmargin+49)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
! pr #20: 'Call Print.AddLine('&str$(xmargin+91+14)&','&str$(lyne*(meter)+ymargin+2)&',15,0)'
	pr #20: 'Call Print.AddText("   Net Bill",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')'
! pr #20: 'Call Print.AddText("Net",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin+2)&')'
! end if  ! pb><0
	pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
! If BUDGET>0 Then pB=PBUD ! owe old budget payment
	if pb then
		pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
! pr #20: 'Call Print.AddText("Prior",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin+2)&')'
! pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
	end if  ! pb
	pr #20: 'Call Print.MyFontSize(10)'
 
	if estimatedate=d1 then pr #20: 'Call Print.AddText("Bill estimated!",'&str$(xmargin+1)&','&str$(lyne*21+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*23+1+ymargin+10)&',63,0)'
! pr #20: 'Call Print.AddText("Budget Payment",'&STR$(XMARGIN+68)&','&STR$(LYNE*12+YMARGIN)&')'
! If BUDGET>0 Then bAL=BUDGET+PBUD ! IF BUDGET MAKE NET DUE = BUDGET PLUS ANY OLD BUDGET PAYMENTS NOT MADE
	if budget>0 then
		pr #20: 'Call Print.AddText("Actual Balance",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
		pr #20: 'Call Print.AddText("Budget Amount",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
!  if trim$(z$)='100100.00' then let fnpause
	else
		fnpa_txt('Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*24+ymargin+10)
		fnpa_txt(fnformnumb$(bal,2,9),xmargin+37,lyne*24+ymargin+10) ! 37 was 42
		fnpa_txt('Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',xmargin+1,lyne*25+ymargin+10)
		fnpa_txt(fnformnumb$(fn_pay_after_amt,2,9),xmargin+37,lyne*25+ymargin+10) ! 37 was 42
	end if
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
! pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
! pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
! pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
! pr #20: 'Call Print.AddText("  Permit No 4",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(9)'
! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
	pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("'&cnam$&'",'&str$(xmargin+68)&','&str$(lyne*9+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*11+ymargin)&')'
	if budget>0 then
		pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
	else
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*11+ymargin)&')'
		pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68)&','&str$(lyne*12+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+106)&','&str$(lyne*12+ymargin)&')'
	end if
	pr #20: 'Call Print.MyFontSize(9)'
	addy=12
	fnpa_txt(mg$(1),xmargin+68,(addy+=1)*lyne+ymargin)
! fnpa_txt(mg$(2),xmargin+68,(addy+=1)*lyne+ymargin)
	fnpa_txt(mg$(3),xmargin+68,(addy+=1)*lyne+ymargin)
	addy+=1
	pr #20: 'Call Print.MyFontSize(10)'
	if df$="Y" then : _
		pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
	if c4>0 then : _
		pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)&')'
	pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
	if pe$(1)<>"" then : _
		fnpa_txt(trim$(pe$(1)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
	if pe$(2)<>"" then : _
		fnpa_txt(trim$(pe$(2)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
	if pe$(3)<>"" then : _
		fnpa_txt(trim$(pe$(3)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
	if pe$(4)<>"" then : _
		fnpa_txt(trim$(pe$(4)),xmargin+68+addr_indent,lyne*(addy+=1)+ymargin+20+addr_down)
	pr #20: 'Call Print.AddText("Return Service Requested.",'&str$(xmargin+68)&','&str$(lyne*(addy+=2)+ymargin+20+addr_down)&')'
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
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output
L3040: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3070
	write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	goto L3040
L3070: close #1: ioerr L3080
L3080: close #6: ioerr L3090
L3090: execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L3110
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed
L3110: return
BUD1: !
	bud1=0
	dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L3200
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative
	bud1=1
	for j=1 to 5
		bd$(j)=str$(j+10)&",20,PIC(##/##/##),U,N"
	next j
L3200: return
BUD2: !
	budget=pbud=bd1=0
	mat bd1(5)
	mat bd1=(0)
	mat bd2=(0)
	if bud1=0 then goto L3360
	read #81,using L3280,key=z$: z$,mat ba,mat badr nokey L3360
L3280: form pos 1,c 10,pd 4,12*pd 5.2,2*pd 3
	ta1=badr(1)
L3300: if ta1=0 then goto L3360
	read #82,using L3320,rec=ta1: z$,mat bt1,nba noRec L3360
L3320: form pos 1,c 10,2*pd 4,24*pd 5.2,2*pd 4,pd 3
	if bt1(1,1)=d1 then budget=budget+bt1(12,1): goto L3350 ! budget for current month
! if bt1(14,1)=0 then pbud=pbud+bt1(12,1): goto L3350 ! budget for any previous months not paid
L3350: ta1=nba : goto L3300
L3360: return
def fn_pay_after_amt
		fn_pay_after_amt=round(bal*1.04,2)
fnend  ! fn_pay_after_amt
