! Replace S:\acsUB\ubpdreminder
! pr bills for Village of Thomasboro
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(10)*40,txt$*45,mg$(3)*30,rw(22,13),cap$*128
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11),extra1$*30
	dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40,datafile$*256,indexfile$*256
 
	fncno(cno,cnam$) : _
	fnLastBillingDate(d1)
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
 
	fnTop("S:\acsUB\ubprtbl1",cap$="Print Bills")
	gosub BULKSORT
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
 
SCREEN1: !
	a$="" : prtbkno=0
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
	resp$(respc+=1)="False"
	fnCmdSet(3) : _
	fnAcs(mat resp$,ckey)
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
	if resp$(8)="True" then sl1=1: z$="" else sl1=0
	if trim$(a$)<>"" then read #2,using L460,key=a$: z$,route,sequence nokey SCREEN1 : _
		holdz$=z$: begin=1 : _
		st1=1
L460: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed
	gosub VBOPENPRINT ! Open #20: "Name=[Q]\UBmstr\Bill"&WSID$&".txt,Replace,RecL=5000",Display,Output  : _
	! fnOPENPRN
 
	on fkey 5 goto RELEASE_PRINT
L550: if sl1=1 then goto SCREEN3
L560: read #6,using L590: z$ eof RELEASE_PRINT
	if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L560 ! start with
	begin=0 ! cancel starting account
L590: form pos 22,c 10
	read #1,using L610,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L560
L610: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9
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
		if rtrm$(ba$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=ba$(j)
	next j
	goto L920
 
L770: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then : _
			e1=e1+1 : pe$(e1)=e$(j)
	next j
	if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
	goto L920
IGNORE: continue
RELEASE_PRINT: !
	close #1: ioerr ignore
	close #3: ioerr ignore
	fnpa_finis
	goto ENDSCR
 
L920: !
	pb=bal-g(11)
	if bal<=0 then g(10)=0 ! don't show penalty if balance 0 or less
! print bill routine
	gosub VBPRINT
! end of pr routine
	bct(2)=bct(2)+1 : _
	! accumulate totals
	goto L550
 
SCREEN3: !
	fnTos
	fnLbl(1,1,"Account (blank to stop)",31,1)
! If TRIM$(A$)="" Then Goto 1030 Else Goto 1040 ! kj 7/12/05
	if trim$(z$)<>"" then 
		fnLbl(3,1,"Last Account entered was "&z$,44,1)
	end if
	fncmbact(1,17)
	resp$(1)=a$
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	a$ = lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" then goto RELEASE_PRINT
	if ckey=5 then goto RELEASE_PRINT
	read #1,using L610,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey SCREEN3
	goto READALTADR
 
SORT1: ! SELECT & SORT
	open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1390
	open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=19",internal,output
	s5=1
	if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
	restore #2,search>=routekey$:
	do
		read #2,using L1200: z$,f,route eof END5
		L1200: form pos 1,c 10,pos 296,pd 4,pos 1741
		if prtbkno and prtbkno><route then goto END5
		if f=d1 then
			zip5$=cr$=""
			read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey ignore
			write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
		end if
	loop
	END5: !
	close #6:
	open #9: "Name=[Temp]\Control.[session],Size=0,RecL=128,Replace",internal,output
	L1310: form pos 1,c 128
	write #9,using L1310: "File [Temp]\Temp.[session],,,[Temp]\Addr.[session],,,,,A,N"
	write #9,using L1310: "Mask 1,19,C,A"
	close #9:
	execute "Free [Temp]\Addr."&session$ ioerr ignore
	execute "Sort [Temp]\Control."&session$
	open #6: "Name="&env$('Temp')&"\Temp."&session$,internal,input,relative
	open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative
	L1390: !
return
 
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
	fnAcs(mat resp$,ckey)
Xit: fnXit
 
ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto L1590
	execute "list -"&str$(line) : _
	pause  : _
	goto L1590
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L1590: execute act$
	goto ERTN
 
VBOPENPRINT: !
	fnpa_open
	lyne=3
	character=1.5
	spacer=0
return
 
VBPRINT: !
	pr #20: "Call Print.MyFontBold(True)"
	pr #20: 'Call Print.MyFontSize(16)'
	pr #20: 'Call Print.MyFont("Courier New")'
	pr #20: 'Call Print.AddText("'&at$(1)&'",'&str$(10)&','&str$(lyne*4+spacer)&')'
	pr #20: 'Call Print.MyFont("Lucida Console")'
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.MyFontBold(False)'
	pr #20: 'Call Print.AddText("'&at$(2)&'",'&str$(10)&','&str$(lyne*6.5+spacer)&')'
	pr #20: 'Call Print.AddText("'&at$(3)&'",'&str$(10)&','&str$(lyne*8+spacer)&')'
	pr #20: "Call Print.MyFontBold(True)"
	pr #20: 'Call Print.MyFontSize(12)'
	pr #20: 'Call Print.AddLine('&str$(115)&','&str$(lyne*12+spacer)&',75,'&str$(30)&',True)'
	pr #20: 'Call Print.AddText("A Friendly Reminder....",'&str$(100)&','&str$(lyne+spacer)&')'
	pr #20: 'Call Print.MyFontSize(10)'
	pr #20: 'Call Print.MyFontBold(False)'
	pr #20: 'Call Print.AddText("If your check has already been mailed,please ",'&str$(100)&','&str$(lyne*3+spacer)&')'
	pr #20: 'Call Print.AddText("disregard this notice.  If not, your remittance by mail ",'&str$(100)&','&str$(lyne*4+spacer)&')'
	pr #20: 'Call Print.AddText("will be greatly appreciated.",'&str$(100)&','&str$(lyne*5+spacer)&')'
	pr #20: 'Call Print.AddText("Thank You!",'&str$(150)&','&str$(lyne*7+spacer)&')'
	pr #20: 'Call Print.AddText("Customer #:  '&z$&'",'&str$(125)&','&str$(lyne*14+spacer)&')'
	pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(zZZ/ZZ/ZZ)",d1)&'",'&str$(125)&','&str$(lyne*16+spacer)&')'
	pr #20: 'Call Print.AddText("Balance Due: '&cnvrt$("pic(---,---.##)",bal)&'",'&str$(125)&","&str$(lyne*18+spacer)&')'
	pr #20: 'Call Print.MyFontSize(13)'
	pr #20: 'Call Print.AddText("'&pe$(1)&'",'&str$(20)&','&str$(lyne*16+spacer)&')'
	pr #20: 'Call Print.AddText("'&pe$(2)&'",'&str$(20)&','&str$(lyne*17.5+spacer)&')'
	pr #20: 'Call Print.AddText("'&pe$(3)&'",'&str$(20)&','&str$(lyne*19+spacer)&')'
	pr #20: 'Call Print.AddText("'&pe$(4)&'",'&str$(20)&','&str$(lyne*20.5+spacer)&')'
	checkcounter+=1 : _
	spacer+=90
	if checkcounter=3 then : _
		fnpa_newpage : _
		checkcounter=0 : _
		spacer=0
return
 
BULKSORT: ! bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=31",internal,output
L2070: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2100
	write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	goto L2070
L2100: close #1: ioerr L2110
L2110: close #6: ioerr L2120
L2120: execute "Index "&env$('Temp')&"\Temp."&session$&" "&env$('Temp')&"\TempIdx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2140
	open #6: "Name="&env$('Temp')&"\Temp."&session$&",KFName="&env$('Temp')&"\TempIdx."&session$,internal,input,keyed
L2140: return
