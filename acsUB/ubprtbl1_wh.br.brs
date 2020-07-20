! Replace S:\acsUB\ubprtbl1_wh
! pr bills for Village of white hall

	autoLibrary
	on error goto Ertn

	dim resp$(10)*80,txt$*45,mg$(3)*30,rw(22,13)
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,datafile$*256,indexfile$*256

	fncno(cno)
	fnLastBillingDate(d1)
	addr_indent=8 : addr_down=3
	open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input
	read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
	close #21:
	z=21
	z=26
	for j=2 to udim(at$)
		at$(j)=trim$(at$(j))(1:z)
		x=len(at$(j)) : y=z-x
		at$(j)=rpt$(" ",int(y/2))&at$(j)
	next j
	linelength=62
	fnTop("S:\acsUB\ubprtbl1","Print Bills")
	gosub BULKSORT ! want printed in alphabetic order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.H[cno],Shr",internal,input,keyed  ! open in Account order
!  open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx2.H[cno],Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
	open #8: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndEX.H[cno],Shr",internal,input,keyed  ! open in alphabetic order  ! bethany special
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.H[cno],Shr",internal,input,keyed  ! open in route-sequence #

	mg$(1)=''
SCREEN1: !
	a$="" : prtbkno=0
	fnTos
	pf=33 : ll=30
	respc=0
	fnLbl(1,1,"Penalty Due Date:",ll,1)
	fnTxt(1,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
	fnLbl(2,1,"Message on Bill:",ll,1)
	fnTxt(2,pf,30,30)
	resp$(respc+=1)=mg$(1)

	fnLbl(3,1,"Date of Billing:",ll,1)
	fnTxt(3,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
	fnLbl(4,1,"Starting Route/Sequence:",ll,1)
	fncombof("ubm-act-nam",4,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
	resp$(respc+=1)=""
	fnLbl(5,1,"Route Number:",ll,1)
	fncmbrt2(5,pf)
	resp$(respc+=1)="[All]"
	fnChk(6,pf,"Select Accounts to Print:",1)
	resp$(respc+=1)="False"
	fnLbl(8,1,"Service From Date:",ll,1)
	fnTxt(8,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",0)
	fnLbl(9,1,"Service To Date:",ll,1)
	fnTxt(9,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",0)
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto ENDSCR
	d4 = val(resp$(1))
	mg$(1) = resp$(2)
! mg$(2) = resp$(3)
! mg$(3) = resp$(4)
	d1 = val(resp$(3))
	if resp$(4)="[All]" then
		a$=""
	else
		a$ = lpad$(trim$(resp$(4)(1:9)),9)
	end if
	if resp$(5)="[All]" then
		prtbkno=0
	else
		prtbkno = val(resp$(5))
	end if
	if resp$(6)="True" then sl1=1: z$="" else sl1=0
	pr service_from=val(resp$(7)) : service_to=val(resp$(8))
	if service_from<>0 and service_to<>0 then use_entered_dates=1 else use_entered_dates=0
	if trim$(a$)<>"" then 
		read #2,using L500,key=a$: z$,route,sequence nokey SCREEN1
		holdz$=z$: begin=1
		st1=1
	end if
L500: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account

	open #h_adrbil:=3: "Name=[Q]\UBmstr\UBAdrBil.H[cno],KFName=[Q]\UBmstr\adrIndex.H[cno],Shr",internal,input,keyed
	gosub BUD1
	gosub VBOPENPRINT

L600: if sl1=1 then goto SCREEN3
L610: read #6,using 'form pos 22,c 10': z$ eof RELEASE_PRINT
	if prtbkno>val(trim$(z$(1:2))) then goto L610 ! fixes select route for pr problem
	if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L610 ! start with
	begin=0 ! cancel starting account
	read #1,using F_CUSTOMER,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4,extra_22 nokey L610
! read #1,using F_CUSTOMER: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4 eof RELEASE_PRINT
	if d3=0 then d3=extra_3
	if d2=0 then d2=extra_4
F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1750,2*n 6,pos 1854,pd 5.2
	if prtbkno=0 then goto L710
	if prtbkno><route then goto L610
L710: if f><d1 then goto L600
	if st1=0 then goto READALTADR
! If ST1$=Z$ Then sT1=0 Else Goto 560
READALTADR: !
! read alternate billing address
	if extra_22<>1 then goto L840
	read #h_adrbil,using L770,key=z$: mat ba$ nokey L840
L770: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then e1=e1+1 : pe$(e1)=ba$(j)
	next j
	goto L1000

L840: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then e1=e1+1 : pe$(e1)=e$(j)
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto L1000

RELEASE_PRINT: ! r:
	close #6: ioerr ignore
	close #1: ioerr ignore
	close #h_adrbil: ioerr ignore

	fnpa_finis
	goto ENDSCR ! /r

L1000: !
	if bud1=1 then gosub BUD2
	pb=bal-g(11)
! If BAL<=0 Then g(10)=0 ! don't show penalty if balance 0 or less
! print bill routine
	gosub VBPRINT
! end of pr routine
	bct(2)=bct(2)+1	! accumulate totals
goto L600

SCREEN3: !
	fnTos
	fnLbl(1,1,"Account (blank to stop)",31,1)
	if trim$(z$)<>"" then
		fnLbl(3,1,"Last Account entered was "&z$,44,1)
	end if
	fncmbact(1,17)
	resp$(1)=a$
	fnCmdKey("&Next",1,1,0,"Accept this record for printing")
	fnCmdKey("&Complete",5,0,1,"Print all selected records")
	fnAcs(mat resp$,ckey)
	a$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #8,using F_CUSTOMER,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,extra_3,extra_4,extra_22 nokey SCREEN3
	if d3=0 then d3=extra_3
	if d2=0 then d2=extra_4
goto READALTADR

SORT1: ! r: SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1510
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then 
		routekey$="" 
	else 
		routekey$=cnvrt$("N 2",prtbkno)&"       " 		! key off first record in route (route # no longer part of customer #)
	end if
	restore #2,search>=routekey$:
	L1310: !
	read #2,using L1320: z$,f,route eof END5
	L1320: form pos 1,c 10,pos 296,pd 4,pos 1741
	if prtbkno=0 then goto L1350
	if prtbkno><route then goto END5
	L1350: if f><d1 then goto L1310
	zip5$=cr$=""
	read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1380
	L1380: !
	write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
goto L1310

END5: close #6:
	open #9: "Name="&env$('temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output
L1430: form pos 1,c 128
	write #9,using L1430: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
	write #9,using L1430: "Mask 1,19,C,A"
	close #9:
	execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr ignore
	execute "Sort "&env$('temp')&"\Control."&session$&" -n"
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative
	open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative
L1510: return  ! /r

ENDSCR: ! pr totals screen
	if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
	fnTos
	mylen=23 : mypos=mylen+2 
	respc=0
	fnLbl(1,1,"Total Bills Printed:",mylen,1)
	fnTxt(1,mypos,8,0,1,"",1)
	resp$(respc+=1)=cnvrt$("N 8",sum(bct))
	fnCmdSet(52)
	fnAcs(mat resp$,ckey)
Xit: fnXit

VBOPENPRINT: ! r:
	fnPa_open("Landscape")
	lyne=3
return  ! /r

VBPRINT: ! r:
	! -- Standard 4 Per Page Even Perferated Card Stock Bills
	checkcounter+=1
	if checkcounter=1 then xmargin=0 : ymargin=0
	if checkcounter=2 then xmargin=137 : ymargin=0
	if checkcounter=3 then xmargin=0 : ymargin=108
	if checkcounter=4 then xmargin=137 : ymargin=108 : checkcounter=0

	pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
	pr #20: "Call Print.MyFontBold(True)"
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.MyFont("Courier New")'
	pr #20: 'Call Print.AddText("'&env$('cnam')&'",'&str$(xmargin+8)&','&str$(lyne*1-1+ymargin)&')'
	pr #20: 'Call Print.MyFont("Lucida Console")'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.MyFontBold(False)'
	pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(xmargin+6)&','&str$(lyne*2+1+ymargin-.2)&')'
	pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(xmargin+6)&','&str$(lyne*3+1+ymargin)&')'
	pr #20: 'Call Print.AddText("#'&trim$(z$)&'  '&bulk$&'",'&str$(xmargin+4)&','&str$(lyne*5+ymargin)&')'
	pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
	if ~use_entered_dates then
		service_from=extra_4
		service_to=extra_3
	end if
	pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_from)&'  To: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",service_to)&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("Due upon receipt",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("'&e$(2)&'",'&str$(xmargin+2)&','&str$(lyne*9+ymargin)&')'
	pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
	pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
	pr #20: 'Call Print.MyFontSize(7)'
	pr #20: 'Call Print.AddText("Current",'&str$(xmargin+12+5)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+35+5)&','&str$(lyne*13+ymargin)&')'
	pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+52+5)&','&str$(lyne*13+ymargin)&')'

PRINTGRID: !
	meter=14 ! 02114   meter=20 ! lyne=2 ! 3 ! 2.15 !  started at 20 and 2.1
	pr #20: 'Call Print.MyFontSize(10)' ! 02116   pr #20: 'Call Print.MyFontSize(10)' ! line_top(1)
	if g(1)<>0 then
		pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,7)&'",'&str$(xmargin+24)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(1)<>0
	if g(2)<>0 then
		pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(2)<>0
	if g(3)<>0 or d(7)<>0 then
		pr #20: 'Call Print.AddText("EL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(5),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(7),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("EL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(3)<>0 or d(7)<>0
! If A4=1 Then gCODE$="RSGS" Else If A4=2 Then gCODE$="CMGS" Else If A4=3 Then gCODE$="INGS" Else
	gcode$="GAS"
	if g(4)<>0 then
		pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+6)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+25)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(4)<>0
	if g(5)<>0 then
		pr #20: 'Call Print.AddText("SL",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("SL",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(5),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(5)<>0
	if g(6)<>0 then
		pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("DEM",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(6)<>0
	if g(7)<>0 then
		pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("EL TAX",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(7),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(7)=0
	if g(8)<>0 then
		pr #20: 'Call Print.AddText("Other",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("Other",'&str$(xmargin+01)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(8)<>0
	if g(9)<>0 then
		pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin)&')'
		pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin)&')'
	end if  ! g(9)<>0
! if pb><0 then
	pr #20: 'Call Print.AddLine('&str$(xmargin+49)&','&str$(lyne*(meter+=1)+ymargin+2)&',15,0)'
	pr #20: 'Call Print.AddLine('&str$(xmargin+91+14)&','&str$(lyne*(meter)+ymargin+2)&',15,0)'
	pr #20: 'Call Print.AddText("   Net Bill",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin+2)&')'
	pr #20: 'Call Print.AddText("Net",'&str$(xmargin+92)&','&str$(lyne*(meter)+ymargin+2)&')'
! end if  ! pb><0
	pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
	pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
! If BUDGET>0 Then pB=PBUD ! owe old budget payment
	if pb then
		pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin+2)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+43)&','&str$(lyne*meter+ymargin+2)&')'
		pr #20: 'Call Print.AddText("Prior",'&str$(xmargin+91)&','&str$(lyne*(meter)+ymargin+2)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+91+8)&','&str$(lyne*meter+ymargin+2)&')'
	end if  ! pb
	pr #20: 'Call Print.MyFontSize(10)' : lyne=3

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
		pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*24+ymargin+10)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*24+ymargin+10)&')' ! 37 was 42
		pr #20: 'Call Print.AddText("Pay After '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+1)&','&str$(lyne*25+ymargin+10)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+37)&','&str$(lyne*25+ymargin+10)&')' ! 37 was 42
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
	pr #20: 'Call Print.AddText("First Class Mail",'&str$(xmargin+100)&','&str$(lyne*2-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  U.S. Postage  ",'&str$(xmargin+100)&','&str$(lyne*3-1+ymargin)&')'
	pr #20: 'Call Print.AddText("      Paid",'&str$(xmargin+100)&','&str$(lyne*4-1+ymargin)&')'
	pr #20: 'Call Print.AddText("  Permit No 4",'&str$(xmargin+100)&','&str$(lyne*5-1+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(9)'
! pr #20: 'Call Print.AddText("Address Service Requested",'&STR$(XMARGIN+68)&','&STR$(LYNE*7+YMARGIN-6)&')'
	pr #20: 'Call Print.AddText("Please return this",'&str$(xmargin+68+addr_indent)&','&str$(lyne*7+ymargin)&')'
	pr #20: 'Call Print.AddText("side with payment to:",'&str$(xmargin+68+addr_indent)&','&str$(lyne*8+ymargin)&')'
	pr #20: 'Call Print.AddText("'&env$('cnam')&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*9+ymargin)&')'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.AddText("Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+addr_indent)&','&str$(lyne*11+ymargin)&')'
	if budget>0 then
		pr #20: 'Call Print.AddText("'&fnformnumb$(budget+pbud,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*11+ymargin)&')'
	else
		pr #20: 'Call Print.AddText("'&fnformnumb$(bal,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*11+ymargin)&')'
		pr #20: 'Call Print.AddText("After  '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':",'&str$(xmargin+68+addr_indent)&','&str$(lyne*12+ymargin)&')'
		pr #20: 'Call Print.AddText("'&fnformnumb$(fn_pay_after_amt,2,9)&'",'&str$(xmargin+100+addr_indent)&','&str$(lyne*12+ymargin)&')'
	end if
	pr #20: 'Call Print.MyFontSize(9)'
	addy=12
	pr #20: 'Call Print.AddText("'&mg$(1)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
! pr #20: 'Call Print.AddText("'&mg$(2)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
	pr #20: 'Call Print.AddText("'&mg$(3)&'",'&str$(xmargin+68)&','&str$((addy+=1)*lyne+ymargin)&')'
	addy+=1
	pr #20: 'Call Print.MyFontSize(10)'
	if df$="Y" then : _
		pr #20: 'Call Print.AddText("Drafted",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)
	if c4>0 then : _
		pr #20: 'Call Print.AddText("Final Bill",'&str$(xmargin+1)&','&str$(lyne*(addy+=1)+ymargin)&')'
	pr #20: 'Call Print.AddText("#'&trim$(z$)&' '&bulk$&'",'&str$(xmargin+68+addr_indent+30)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
	if pe$(1)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(1))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
	if pe$(2)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(2))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
	if pe$(3)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(3))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
	if pe$(4)<>"" then : _
		pr #20: 'Call Print.AddText("'&trim$(pe$(4))&'",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=1)+ymargin+20+addr_down)&')'
	pr #20: 'Call Print.AddText("Return Service Requested.",'&str$(xmargin+68+addr_indent)&','&str$(lyne*(addy+=2)+ymargin+20+addr_down)&')'
	if checkcounter=1 then checkx=1.375 : checky=3.6875
	if checkcounter=2 then checkx=6.75 : checky=3.6875
	if checkcounter=3 then checkx=1.375 : checky=7.9375
	if checkcounter=0 then checkx=6.75 : checky=7.9375
	bc$=""
	if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(checkx)&','&str$(checky)&',"'&bc$&'")'
	if checkcounter=0 then : _
		fnpa_newpage
return  ! /r

BULKSORT: ! r: bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.H[cno],Shr",internal,input,keyed  ! open in Account order
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output
L3040: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L3070
	write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	goto L3040
L3070: close #1: ioerr ignore
	close #6: ioerr ignore
	execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\TempIdx."&session$&" 1,21,Replace,DupKeys -n" ioerr L3110 ! 1,19 was changed to 22,10 to switch sorting to account
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed
L3110: return  ! /r
BUD1: ! r:
	bud1=0
	dim ba(13),badr(2),bt1(14,2),bd1(5),bd2(5),bd3(5),bd$(5)*30
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed ioerr L3200
	open #82: "Name=[Q]\UBmstr\BudTrans.h[cno],Shr",internal,outIn,relative
	bud1=1
	for j=1 to 5
		bd$(j)=str$(j+10)&",20,PIC(##/##/##),U,N"
	next j
L3200: return  ! /r
BUD2: ! r:
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
	L3350: !
	ta1=nba
goto L3300
L3360: return  ! /r
def fn_pay_after_amt
	if bal<0 then let fn_pay_after_amt=round(bal,2) else let fn_pay_after_amt=round(bal*1.05,2)
fnend
include: Ertn
