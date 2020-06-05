 
	autoLibrary
	on error goto Ertn
 
	dim rate(18,20),usage(18,20),cde(20),d(12),t(18,2),a(4),message$*40
	dim usagtot(18,20),ratetot(18,20),customer(18,20)
	dim fa$(5),sa$(4),fb$(1),fc$(1),sb$(1)*38,fd$(1),z$(4)*11,srvc$*11
	dim io2$(38),cnam$*40,code$(4),a(7),d(15),g(10),e$(4)*30,f$(3)*12
 
	fncno(cno,cnam$)
	fnLastBillingDate(bdate)
	fnTop(program$,"Analyze Charges")
MAIN: !
	fnTos
	mylen=20
	mypos=mylen+2
	fnLbl(1,1,"Billing Date:",mylen,1)
	fnTxt(1,mypos,8,8,0,"1")
	resp$(1)=str$(bdate)
	fnLbl(2,1,"Type of Service:",mylen,1)
	code$(1)="Water"
	code$(2)="Sewer"
	code$(3)="Electric"
	code$(4)="Gas"
	fncomboa("Service",2,mylen+3,mat code$,"",16)
	fnLbl(3,1,"Rate Code",mylen,1)
	fnTxt(3,mypos,3,3,0,"30")
	resp$(3)=""
	fnCmdSet(3): fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	bdate= val(resp$(1))
	if resp$(2)="Water"    then srvc=1 : srvc$=resp$(2)
	if resp$(2)="Sewer"    then srvc=2 : srvc$=resp$(2)
	if resp$(2)="Electric" then srvc=3 : srvc$=resp$(2)
	if resp$(2)="Gas"      then srvc=4 : srvc$=resp$(2)
	rcode=val(resp$(3))
	fnopenprn
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
 
	on fkey 5 goto DONE
	fnopenprn(cp,58,220,process)
! Read #1,Using 370: MAT A,MAT D,F Eof 400
! Form POS 143,4*PD 2,POS 217,12*PD 5,POS 296,PD 4
! If F<>BDATE Then Goto 360
! If A(SVCE)<>RATCODE Then Goto 360
	gosub PRINTIT
DONE: close #1: ioerr L420
L420: fncloseprn
Xit: fnXit
 
ERTN: fnerror(program$,err,line,act$,"Xit")
	if uprc$(act$)<>"PAUSE" then goto L490
	execute "list -"&str$(line) : _
	pause  : _
	goto L490
	pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause
L490: execute act$
	goto ERTN
 
PRINTIT: !
	gosub HDR
L540: read #1,using L610: z$,mat e$,mat a,mat f$,mat d,mat g eof L810 ! READ MASTER RECORD
	if a(srvc)=0 then goto L540 ! no service
	usage=0
	if srvc=1 then usage=d(3): amount=g(1): meter$=f$(1) ! water
	if srvc=2 then usage=d(3): amount=g(2): meter$="" ! sewer
	if srvc=3 then usage=d(7): amount=g(3): meter$=f$(2) ! electric
	if srvc=4 then usage=d(11): amount=g(4): meter$=f$(3) ! gas
L610: form pos 1,c 10,pos 11,4*c 30,pos 143,7*pd 2,pos 131,c 12,pos 361,c 12,pos 373,c 12,pos 217,15*pd 5,pos 300,10*pd 4.2
	if a(srvc)=tc or tc=0 then goto L630 else goto L540
L630: pr #255,using L660: z$,e$(2),e$(1),meter$,usage,amount pageoflow PGOF
	totusage=totusage+usage
	totamount=totamount+amount
L660: form x 5,c 10,x 5,c 30,x 7,c 30,x 2,c 12,x 2,pic(zzzzzzzzz),x 2,n 12.2,skip 2
	goto L540
 
PGOF: pr #255: newpage
	gosub HDR
	continue
 
HDR: !
	p2=p2+1
	pr #255,using "Form POS 1,CC 80": cnam$ : _
	pr #255,using "Form POS 1,CC 80": "Consumption List - "&srvc$ ! "                       pr #255,Using " Form POS 1,CC 80": "Rate Code "&STR$(SRVC) : _
	pr #255,using "Form POS 90,C 5,PIC(ZZZ)": "Page ",p2 : _
	pr #255: ""
	if tc<>0 then pr #255,using L770: srvc$&" Code ",tc
L770: form pos 41,c 9,n 2,skip 2
	pr #255: tab(7);"Customer #";tab(21);"Name";tab(58);"Meter Address";tab(90);"   Merer #    Consumption  Dollar Amt"
	pr #255: tab(7);"__________";tab(21);"________________________________";tab(58);"______________________________  ____________  ___________  __________"
return
L810: pr #255,using "Form POS 101,C 28": "____________  ____________" : _
	pr #255,using "Form POS 101,N 12,X 2,N 12.2": totusage,totamount : _
	pr #255,using "Form POS 101,C 28": "============  ============"
	close #1:
return
