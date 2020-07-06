! Replace S:\acsUB\ubprtfull_ashgrove
! pr bills for Ash Grove (full page)
autoLibrary
on error goto Ertn
 
dim resp$(20)*160,txt$*80,mg$(13)*160,rw(22,13),cap$*128
dim z$*10,e$(4)*30,f$*12,g(12)
dim _d(15)
dim _b(11)
dim extra1$*30
dim gb(10),pe$(4)*30,ba$(4)*30
dim datafile$*256,indexfile$*256
dim dueby$*30,prebal$*30,usage(3),billdate(3),ml$(2)*80,tg(11)
 
fnLastBillingDate(d1)
 
fnTop("S:\acsUB\ubprtbl1",cap$="Print Bills")
gosub BULKSORT
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
open #ubtransvb=15: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed
if exists("[Q]\UBmstr\message.h[cno]")=0 then goto L3250
L225: !
open #16: "Name=[Q]\UBmstr\message.h[cno]",internal,outIn,relative
for j=1 to 13
	read #16,using "form pos 1,c 60",rec=j: mg$(j) noRec L228
	L228: !
next j
prebal$="10:00 AM, xxxxxxx  xx"
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1")
	pf=26 : ll=24
	respc=0
	fnLbl(3,1,"Penalty Due Date:",ll,1)
	fnTxt(3,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(4,1,"Message on Bill:",ll,1)
	fnTxt(4,pf,60,60)
	resp$(respc+=1)=mg$(1)
	fnTxt(5,pf,60,60)
	resp$(respc+=1)=mg$(2)
	fnTxt(6,pf,60,60)
	resp$(respc+=1)=mg$(3)
	fnTxt(7,pf,60,60)
	resp$(respc+=1)=mg$(4)
	fnTxt(8,pf,60,60)
	resp$(respc+=1)=mg$(5)
	fnTxt(9,pf,60,60)
	resp$(respc+=1)=mg$(6)
	fnTxt(10,pf,60,60)
	resp$(respc+=1)=mg$(7)
	fnTxt(11,pf,60,60)
	resp$(respc+=1)=mg$(8)
	fnTxt(12,pf,60,60)
	resp$(respc+=1)=mg$(9)
	fnTxt(13,pf,60,60)
	resp$(respc+=1)=mg$(10)
	fnTxt(14,pf,60,60)
	resp$(respc+=1)=mg$(11)
	fnTxt(15,pf,60,60)
	resp$(respc+=1)=mg$(12)
	fnTxt(16,pf,60,60)
	resp$(respc+=1)=mg$(13)
	fnLbl(17,1,"Date of Billing:",ll,1)
	fnTxt(17,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(18,1,"Starting Account:",ll,1)
	fe$="ubm-act-nam"
	datafile$="[Q]\UBmstr\Customer.h[cno]"
	indexfile$="[Q]\UBmstr\ubindx5.h[cno]"
	kp=1741: kl=9 : dp=41 : dl=30
	fncombof(fe$,18,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2)
	resp$(respc+=1)="[All]"
	fnLbl(19,1,"Route Number:",ll,1)
	fncmbrt2(19,pf)
	resp$(respc+=1)="[All]"
	fnChk(20,pf,"Select Accounts to Print",1)
	resp$(respc+=1)="False"
! fnLbl(18,1,"Previous Balance Due By:",LL,1)
! fnTxt(18,PF,35,35,0,"",0,"Example: 10:00AM, August 30")
	! rESP$(RESPC+=1)=PREBAL$
! fnLbl(19,1,"If not paid by::",LL,1)
!  fnTxt(19,PF,25,25,0,"",0,"Example: September 4, 2007")
	!  rESP$(RESPC+=1)=DUEBY$
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto EndScreen
	d1 = val(resp$(15))
	d4 = val(resp$(1))
	mg$(1) = resp$(2)
	mg$(2) = resp$(3)
	mg$(3) = resp$(4)
	mg$(4) = resp$(5)
	mg$(5) = resp$(6)
	mg$(6) = resp$(7)
	mg$(7) = resp$(8)
	mg$(8) = resp$(9)
	mg$(9) = resp$(10)
	mg$(10) = resp$(11)
	mg$(11) = resp$(12)
	mg$(12) = resp$(13)
	mg$(13) = resp$(14)
	for j=1 to 13
		rewrite #16,using "form pos 1,c 60",rec=j: mg$(j) noRec L568 : lastj=j
	next j
	L568: !
	if lastj<13 then
		for j=lastj+1 to 13
			write #16,using "form pos 1,c 60": mg$(j)
		next j
	end if
	close #16:
	if resp$(16)="[All]" then
		a$=""
	else
		a$ = lpad$(trim$(resp$(16)(1:9)),9)
	end if
	if resp$(17)="[All]" then
		prtbkno=0
	else
		prtbkno = val(resp$(17))
	end if
	if resp$(18)="True" then sl1=1: z$="" else sl1=0
	prebal$=resp$(19)
	dueby$=resp$(20)
	goto L640 ! If TRIM$(PREBAL$)="" OR TRIM$(DUEBY$)="" Then Goto 550 Else Goto 560
	mat ml$(2)
	ml$(1)="You must answer the last two questions!"
	ml$(2)="Click OK to enter this informatio."
	fnmsgbox(mat ml$,resp$,'',0)
goto SCREEN1
L640: !
if trim$(a$)<>"" then
	read #2,using L650,key=a$: z$,route,sequence nokey SCREEN1
	L650: form pos 1,c 10,pos 1741,n 2,n 7
	holdz$=z$: begin=1
	st1=1
end if
if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed
		fnpa_open
		lyne=3
 
 
! on fkey 5 goto ReleasePrint
L740: !
if sl1=1 then goto SCREEN3
! Read #6,Using 780: Z$ Eof 1040
! If TRIM$(A$)<>"" AND BEGIN=1 AND Z$<>HOLDZ$ Then Goto 750 ! start with
! bEGIN=0 ! cancel starting account
L790: !
read #1,using L800: z$,mat e$,f$,a3,mat _b,final,mat _d,bal,_f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,seweravg eof ReleasePrint
L800: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1,pos 1712,c 1,pos 1822,n 9
if prtbkno=0 then goto L830
if prtbkno><route then goto L790
L830: !
if _f><d1 then goto L740
if st1=0 then goto READALTADR
READALTADR: ! r:
	! read alternate billing address
	read #3,using L880,key=z$: mat ba$ nokey L970
	L880: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then e1=e1+1 : pe$(e1)=ba$(j)
	next j
	if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
	if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
goto L1120 ! /r
L970: ! r:
	e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then e1=e1+1 : pe$(e1)=e$(j)
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
goto L1120 ! /r
ReleasePrint: ! r:
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
goto EndScreen ! /r
L1120: ! r:
	pb=bal-g(11)
	if bal<=0 then g(9)=g(10)=0 ! don't show penalty if balance 0 or less
	gosub VBPRINT
	bct(2)=bct(2)+1         ! accumulate totals
goto L740 ! /r
SCREEN3: ! r:
	fnTos
	fnLbl(1,1,"Account (blank to stop)",31,1)
	txt$=""
	if trim$(z$)<>"" then
		txt$="Last Account entered was "&z$
	end if
	fnLbl(3,1,txt$,44,1)
	fncmbact(1,17)
	resp$(1)=""
	fnCmdKey("&Print",1,1)
	fnCmdKey("&Finish",5,0,1)
	fnAcs(mat resp$,ckey)
	a$ = lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" or ckey=5 then goto ReleasePrint
	read #1,using L800,key=a$: z$,mat e$,f$,a3,mat _b,final,mat _d,bal,_f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final,df$,seweravg nokey SCREEN3
goto READALTADR ! /r
SORT1: ! r: SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1600
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
	restore #2,search>=routekey$:
L1400: read #2,using L1410: z$,_f,route eof END5
L1410: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1440
	if prtbkno><route then goto END5
L1440: if _f><d1 then goto L1400
	zip5$=cr$=""
	read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1470
L1470: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
goto L1400 ! /r
END5: ! r:
	close #6:
	open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output
	L1520: form pos 1,c 128
	write #9,using L1520: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
	write #9,using L1520: "Mask 1,19,C,A"
	close #9:
	execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr L1570
	L1570: execute "Sort "&env$('Temp')&"\Control."&session$&" -n"
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative
	open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative
	L1600: !
return ! /r
EndScreen: ! r: pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos
	mylen=23 : mypos=mylen+2
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1)
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
	fnCmdSet(52)
	fnAcs(mat resp$,ckey)
goto Xit !  /r
Xit: fnXit
 
VBPRINT: ! r:
! -- Printer Program for Laser 1-Per Page Utility Bills
 
! pr #20: 'Call Print.AddPicture("Ash Grove.jpg",20,1)'
	addy=20
	pr #20: 'Call Print.MyFontSize(12)'
	if pe$(1)<>"" then
		txt$=trim$(pe$(1))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(52)&')'
	end if
	if pe$(2)<>"" then
		txt$=trim$(pe$(2))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(57)&')'
	end if
 
	if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
	if pe$(3)<>"" then
		txt$=trim$(pe$(3))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(62)&')'
	end if
	if pe$(4)<>"" then
		txt$=trim$(pe$(4))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(18)&','&str$(67)&')'
	end if
	pr #20: 'Call Print.MyFontSize(24)'
	pr #20: 'Call Print.AddText("'&"City of Ash Grove"&'",'&str$(65)&','&str$(10)&')'
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.AddText("'&"P O Box 235"&'",'&str$(93)&','&str$(19)&')'
	pr #20: 'Call Print.AddText("'&"Ash Grove, Mo 65604"&'",'&str$(83)&','&str$(23)&')'
 
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.AddText("'&"417-751-2333"&'",'&str$(93)&','&str$(27)&')'
	pr #20: 'Call Print.MyFontSize(9)'
	pr #20: 'Call Print.AddLine('&str$(135)&','&str$(40)&',52,22,1)'
	pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(140)&','&str$(42)&')'
	txt$="       Account:"&trim$(z$)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(140)&','&str$(46)&')'
	pr #20: 'Call Print.AddText("Service From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d2)&'",'&str$(140)&','&str$(50)&')'
	pr #20: 'Call Print.AddText("Service To: '&cnvrt$("PIC(zzZZ/ZZ/ZZ)",d3)&'",'&str$(140)&','&str$(54)&')'
	pr #20: 'Call Print.AddText("Due Date: '&cnvrt$("PIC(zzzzZZ/ZZ/ZZ)",d4)&'",'&str$(140)&','&str$(58)&')'
	if final>0 then pr #20: 'Call Print.AddText("Final Bill'&cnvrt$("PIC(ZZzZZzZZ)",0)&'",'&str$(82)&','&str$(56)&')'
	pr #20: 'Call Print.MyFontSize(16)'
	pr #20: 'Call Print.AddLine('&str$(19)&','&str$(70)&',166,0)'
 
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.MyFontItalic(1)'
	if pb<0 then
		a$=cnvrt$("pic(--------.##)",pb)
		pr #20: 'Call Print.AddText("Previous Balance",'&str$(110)&','&str$(72)&')'
		goto L2280
	end if
	if pb=0 then
		goto L2300
	else
		a$=cnvrt$("pic(--------.##)",pb)
		pr #20: 'Call Print.AddText("Past Due Balance - Due Immediately",'&str$(80)&','&str$(72)&')'
	end if
	L2280: !
	pr #20: 'Call Print.MyFontItalic(0)'
	pr #20: 'Call Print.AddText("'&a$&'",'&str$(160)&','&str$(72)&')'
	L2300: ! pr #20: 'Call Print.AddLine('&STR$(109)&','&STR$(115)&',65,0)'
	pr #20: 'Call Print.MyFontItalic(1)'
	pr #20: 'Call Print.MyFontBold(1)'
	txt$=trim$(e$(1))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(135)&','&str$(65)&')'
	pr #20: 'Call Print.AddText("Current Charges",'&str$(110)&','&str$(78)&")"
	pr #20: 'Call Print.MyFontBold(0)'
 
	pr #20: 'Call Print.MyFontItalic(0)'
	pr #20: 'Call Print.AddText("Current Reading",'&str$(56)&','&str$(84)&")"
	pr #20: 'Call Print.AddText("Prior Reading",'&str$(93)&','&str$(84)&")"
	pr #20: 'Call Print.AddText("Usage",'&str$(128)&','&str$(84)&')'
	pr #20: 'Call Print.AddText("Charge",'&str$(170)&','&str$(84)&')'
	adder=5: lyne=85
	if g(1)=0 then
		goto L2450
	else
		a$=cnvrt$("pic(zzzzzzzz#)",_d(1))
		b$=cnvrt$("pic(zzzzzzzz#)",_d(3))
		c$=cnvrt$("pic(--------.##)",g(1))
		d$=cnvrt$("pic(zzzzzzzz#)",_d(2))
	end if
	pr #20: 'Call Print.AddText("Water",'&str$(26)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&a$&'",'&str$(70)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&d$&'",'&str$(102)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&b$&'",'&str$(121)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
	L2450: !
	if g(2)=0 then goto L2470 else           c$=cnvrt$("pic(--------.##)",g(2))
	pr #20: 'Call Print.AddText("Sewer",'&str$(26)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
	L2470: !
	if g(4)=0 then goto L2500 else           c$=cnvrt$("pic(--------.##)",g(4))
 
	pr #20: 'Call Print.AddText("Sewer Fee",'&str$(26)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
	L2500: !
	if g(5)=0 then goto L2520 else           c$=cnvrt$("pic(--------.##)",g(5))
	pr #20: 'Call Print.AddText("Primacy Fee",'&str$(26)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
	L2520: !
	if g(6)=0 then goto L2540 else           c$=cnvrt$("pic(--------.##)",g(6))
	pr #20: 'Call Print.AddText("Trash Charge",'&str$(26)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
	L2540: !
	if g(8)=0 then goto L2560 else           c$=cnvrt$("pic(--------.##)",g(8))
	pr #20: 'Call Print.AddText("Other Charge",'&str$(26)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
	L2560: !
	if g(9)=0 then goto L2580 else           c$=cnvrt$("pic(--------.##)",g(9))
	pr #20: 'Call Print.AddText("Sales Tax",'&str$(26)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(160)&','&str$(lyne)&')'
	L2580: !
	pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=adder)&',22,0)'
	a$=cnvrt$("pic(--------.##)",g(11))
	pr #20: 'Call Print.AddText("Total Current Charges",'&str$(110)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&a$&'",'&str$(160)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=adder)&',22,0)'
	pr #20: 'Call Print.MyFontSize(14)'
	pr #20: 'Call Print.MyFontBold(1)'
	c$=cnvrt$("pic(--------.##)",bal)
	pr #20: 'Call Print.AddText("Total Due",'&str$(120)&','&str$(lyne+=adder)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(150)&','&str$(lyne)&')'
	pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=adder)&',22,0)'
	pr #20: 'Call Print.AddLine('&str$(162)&','&str$(lyne+=1)&',22,0)'
	pr #20: 'Call Print.MyFontBold(1)'
	pr #20: 'Call Print.AddLine('&str$(26)&','&str$(125)&',160,55,1)'
	pr #20: 'Call Print.MyFontBold(0)'
	pr #20: 'Call Print.MyFontBold(0)'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.AddText("'&"Notes From City"&'",'&str$(90)&','&str$(123.5)&')'
	pr #20: 'Call Print.MyFontItalic(1)'
	lyne=124
	for j=1 to 13
		fnpa_txt(mg$(j),40,lyne+=4) ! pr #20: 'Call Print.AddText("'&mg$(j)&'",'&str$(40)&','&str$(lyne+=4)&')'
	next j
	pr #20: 'Call Print.MyFontItalic(0)'
	x=0
	! For J=1 To 38
	! pr #20: 'Call Print.AddLine('&STR$(X+=5)&','&STR$(208)&',3,0)'
	! Next J
	pr #20: 'Call Print.MyFontSize(7)'
	pr #20: 'Call Print.AddText("Please detach here and return with payment.",'&str$(70)&','&str$(191)&')'
	pr #20: 'Call Print.AddText("Make checks payable to City of Ash Grove.",'&str$(72)&','&str$(195)&')'
	pr #20: 'Call Print.AddText("A 10% penalty if not paid by the due date.",'&str$(72)&','&str$(199)&')'
	pr #20: 'Call Print.MyFontSize(10)'
	txt$="        Account:"&trim$(z$)
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(223)&')'
	pr #20: 'Call Print.AddText("Due Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&'",'&str$(130)&','&str$(227)&')'
	c$=cnvrt$("pic(--------.##)",bal)
	pr #20: 'Call Print.AddText("Total Due:",'&str$(130)&','&str$(231)&')'
	pr #20: 'Call Print.AddText("'&c$&'",'&str$(171)&','&str$(231)&')'
	txt$="After "&cnvrt$("pic(##/##/##)",d4)&" Pay: "&cnvrt$("pic(-------.##)",bal+g(10))
	pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(235)&')'
	if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(.82)&','&str$(2.75)&',"'&bc$&'")'
	pr #20: 'Call Print.AddText("'&"City of Ash Grove"&'",'&str$(30)&','&str$(246)&')'
	pr #20: 'Call Print.AddText("'&"P O Box 235"&'",'&str$(30)&','&str$(250)&')'
	pr #20: 'Call Print.AddText("'&"Ash Grove, Mo 65604"&'",'&str$(30)&','&str$(254)&')'
	if pe$(1)<>"" then
		txt$=trim$(pe$(1))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(248)&')'
	end if
	if pe$(2)<>"" then
		txt$=trim$(pe$(2))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(252)&')'
	end if
	if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
	if pe$(3)<>"" then
		txt$=trim$(pe$(3))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(256)&')'
	end if
	if pe$(4)<>"" then
		txt$=trim$(pe$(4))
		pr #20: 'Call Print.AddText("'&txt$&'",'&str$(130)&','&str$(260)&')'
	end if
	fnpa_newpage
return ! /r
BulkSort: ! r:bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output
	do
		read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3080
		write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	loop
	L3080: !
	close #1: ioerr ignore
	close #6: ioerr ignore
	if fnIndex('[temp]\Temp.[session]','[temp]\TempIdx.[session]','1,19') then
		open #6: 'Name=[temp]\Temp.[session],KFName=[temp]\TempIdx.[session]',internal,input,keyed
	end if
return ! /r
PRIOR_USAGES: ! r:
	mat usage=(0): mat billdate=(0)
	restore #15,key>=z$&"         ": nokey L3240 ! no average but active customer (use 0 usage)
L3160: read #ubtransvb,using L3170: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L3240
L3170: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
	if p$<>z$ then goto L3240
	if tcode<>1 then goto L3160 ! only charge transactions
	usage(3)=usage(2): billdate(3)=billdate(2)
	usage(2)=usage(1): billdate(2)=billdate(1)
	usage(1)=wu: billdate(1)=tdate
goto L3160
L3240: !
return ! /r
L3250: ! r:
	open #16: "Name=[Q]\UBmstr\message.h[cno],RecL=132,replace",internal,outIn,relative
	for j=1 to 10
		write #16,using "form pos 1,c 60": "" ! write 10 blank messages
	next j
	close #16:
goto L225 ! /r
include: Ertn
