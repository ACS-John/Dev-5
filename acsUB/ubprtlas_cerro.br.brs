! Replace S:\acsUB\ubPrtLas_Cerro
! pr bills (new format)
 
	autoLibrary
	on error goto Ertn
 
	dim resp$(10)*80,txt$*40,mg$(3)*30,rw(22,13),indexfile$*256
	dim cap$*128,datafile$*256
	dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
	dim gb(10),pe$(4)*30,ba$(4)*30,ba(12)
 
! fnTop - set by another calling program
	fnLastBillingDate(d1)
!
	dim serviceName$(10)*20,service$(10)*2,tax_code$(10)*1,penalty$(10)*1
	fnget_services(mat serviceName$, mat service$, mat tax_code$,mat penalty$,mat subjectto)
	linelength=62
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
	open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
	open #81: "Name=[Q]\UBmstr\BudMstr.h[cno],KFName=[Q]\UBmstr\BudIdx1.h[cno],Shr",internal,outIn,keyed
 
SCREEN1: !
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1")
	pf=26 : ll=24
	respc=0
	a$="" : prtbkno=0
	fnTos(sn$="UBPrtBl1-1")
	pf=26 : ll=24
	respc=0
	fnLbl(1,1,"Service From:",ll,1)
	fnTxt(1,pf,8,8,1,"1",0,tt$)
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
	fnLbl(2,1,"Service To:",ll,1)
	fnTxt(2,pf,8,8,1,"1")
	resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
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
	fncombof(fe$,8,pf,40,datafile$,kp,kl,dp,dl,indexfile$,2)
	resp$(respc+=1)="[All]"
	fnLbl(9,1,"Route Number:",ll,1)
	fncmbrt2(9,pf)
	resp$(respc+=1)="[All]"
	fnChk(10,pf,"Select Accounts to Print",1)
	resp$(respc+=1)="False"
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto ENDSCR
	d1 = val(resp$(7))
	d2x= val(resp$(1))
	d3x= val(resp$(2))
	d4 = val(resp$(3))
	mg$(1) = resp$(4)
	mg$(2) = resp$(5)
	mg$(3) = resp$(6)
	if resp$(8)="[All]" then
		a$=""
	else
		a$ = lpad$(trim$(resp$(8)(1:10)),10)
	end if
	if resp$(9)="[All]" then
		prtbkno=0
	else
		prtbkno = val(resp$(9))
	end if
	if resp$(10)="True" then sl1=1 else sl1=0
	if trim$(a$)<>"" then
		read #1,using L580,key=a$: z$,route,sequence nokey SCREEN1
		st1=1
	end if
L580: form pos 1,c 10,pos 1741,n 2,n 7
	if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
	if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
	if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
 
	open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed
	fnopenprn
 
	on fkey 5 goto RELEASE_PRINT
	gosub BULKSORT
L680: if sl1=1 then goto SCREEN3 ! select accounts
L690: read #7,using L700: r6 eof RELEASE_PRINT
L700: form pos 1,pd 3
	read #1,using L740,rec=r6: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est nokey L690
	goto L740
	read #2,using L740: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est eof RELEASE_PRINT
L740: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1859,pd 5.2,pos 1750,2*n 6,pos 1831,n 9
	if d2=0 and d2x>0 then d2=d2x ! set date to screen if no date in record
	if d3=0 and d3x>0 then d3=d3x ! set date to screen if no date in record
	mat ba=(0): budget=0
	read #81,using L790,key=z$: x$,mat ba nokey L810
L790: form pos 1,c 10,pd 4,12*pd 5.2
	for j=2 to 12: budget=budget+ba(j): next j ! get total budget amount
L810: if prtbkno=0 then goto L830
	if prtbkno><route then goto RELEASE_PRINT
L830: if f><d1 then goto L680
	if st1=0 then goto HERE
	if st1$=z$ then st1=0 else goto L680
HERE: !
! read alternate billing address
	read #3,using L890,key=z$: mat ba$ nokey L960
L890: form pos 11,4*c 30
	e1=0 : mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then
			e1=e1+1 : pe$(e1)=ba$(j)
		end if
	next j
	goto L1080
 
L960: e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then
			e1=e1+1 : pe$(e1)=e$(j)
		end if
	next j
	goto L1080
 
RELEASE_PRINT: !
	close #1: ioerr L1040
L1040: close #3: ioerr L1050
L1050: fncloseprn
	goto ENDSCR
 
L1080: !
	pb=bal-g(11)
! ______________print bill routine______________________________________
	gosub PRINTBILL
! _____________end of pr routine______________________________________
	bct(2)=bct(2)+1
	goto L680
 
SCREEN3: !
	fnTos
	fnLbl(1,1,"Account (blank to stop)" ,31,1)
	if trim$(a$)="" then goto L1200 else goto L1210
L1200: !
	if z$<>"" then
		fnLbl(3,1,"Last Account entered was "&z$ ,44,1)
	end if
L1210: fncmbact(1,17) !
	resp$(1)=a$
	fnCmdSet(3): fnAcs(mat resp$,ckey)
	a$ = lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(a$)="" or ckey=5 then goto RELEASE_PRINT
	read #1,using L740,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,escrow,d2,d3,est nokey SCREEN3
	goto HERE
 
BULKSORT: ! sort in bulk sort code sequence
	open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output
L1290: form pos 1,c 128
	write #9,using L1290: "FILE customer.H[cno],[Q]\UBmstr,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
	if prtbkno>0 then write #9,using L1290: 'RECORD I,1,2,N,"'&str$(prtbkno)&'","'&str$(prtbkno)&'"'
	write #9,using L1290: "MASK 1942,12,C,A,1,10,C,A"
	close #9:
	execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1350
L1350: execute "Sort "&env$('Temp')&"\Control."&session$
	open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative
	return
 
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
Xit: !
fnXit
PRINTBILL: ! r:
	if final=2 then
		g(8)-=b(8): g(11)=g(12)+g(8): bal+=g(8)
	end if
	penalty=0
	for j=1 to 10
		if penalty$(j)="Y" then penalty+=g(j) : g(j)=0 ! accumulate all penalties and set charge to zero
	next j
	pb=bal-g(11)
	pr #255: ""
	pr #255: ""
	pr #255,using L1550: "FROM",int(d2x*.01),"TO",int(d3x*.01),d1
L1550: form pos 1,c 5,pic(##/##),x 2,c 3,pic(##/##),pos 22,pic(##/##/##),skip 4
	if pb<>0 then pb$="   PRIOR BALANCE" else pb$=""
	pr #255: ""
L1580: form pos 3,c 17,nz 10.2,pos 38,c 10,skip 1
	if g(1)=0 then t$="" else t$=service$(1)
	pr #255,using L1610: t$,0,d(1),d(3),g(1)
L1610: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 3,nz 10.2,nz 12.2,skip 1
L1620: form pos 1,c 3,nz 1,nz 8,nz 8,nz 9.2,x 5,pic(zz/zz/zz),skip 1
	if g(2)=0 then t$="" else t$=service$(2)
	if bal<=0 then pr #255,using L1610: t$,0,0,0,g(2),0,bal : goto L1660
	pr #255,using L1610: t$,0,0,0,g(2),bal+penalty,bal
L1660: if g(3)=0 then t$="" else t$=service$(3)
	pr #255,using L1620: t$,0,0,0,g(3),d4
	if g(4)=0 then t$="" else t$=service$(4)
	pr #255,using L1610: t$,0,0,0,g(4)
	if g(5)=0 then t$="" else t$=service$(5)
	pr #255,using L1610: t$,0,0,0,g(5)
	if g(6)=0 then t$="" else t$=service$(6)
	pr #255,using L1580: pb$,pb,z$
	form pos 1,c 3,2*nz 6,nz 5,nz 10.2,x 1,c 10,skip 1
	if g(8)=0 then t$="" else t$=service$(8)
	pr #255,using L1610: t$,0,0,0,g(8)
	if est=1 then est$="BILL ESTIMATED" else est$=""
	if c4>0 then final$="FINAL BILL" else final$=""
	if df$="Y" then final$="DRAFTED"
	if bal<=0 then penalty=0
	if env$('client')="Cerro Gordo V" and bal<0 then g(5)=0
	pr #255: ""
	pr #255,using 'Form POS 7,C 20,POS 38,C 25': est$,pe$(1)(1:25)
	pr #255,using 'Form POS 1,CR 7,X 1,PIC(ZZ/ZZ/ZZ),NZ 13.2,POS 38,C 25': 'DUE BY:',d4,bal,pe$(2)(1:25)
	pr #255,using 'Form POS 13,C 18,POS 38,C 25': e$(1)(1:18),pe$(3)(1:25)
	pr #255,using 'Form POS 2,C 10,X 5,C 10,POS 38,C 25': z$,final$,pe$(4)(1:25)
	bills+=1
	pr #255,using L1910: mg$(1)
	pr #255,using L1910: mg$(2)
	pr #255,using L1910: mg$(3)
L1910: form pos 2,c 30,skip 1
	if int(bills/3)<>bills/3 then pr #255,using L1910: " "," "           ! space extra if 1st or 2nd bill
	if int(bills/3)=bills/3 then pr #255: newpage ! BOTTOM OF PAGE
	return ! /r
include: Ertn
