! Replace S:\acsTM\CliLabel
! pr client labels
 
	autoLibrary
	fnTop(program$,"Client Labels")
	on error goto L850
 
	dim label_text$(5)*40
	dim z$*5,a$(3)*30,prg$*20,bk$(20)*30,nam$*30,a1$*30
	dim ma(20),app(20),totalapp(20),totalma(20)
	dim a2$*30,a3$*30
 
	on fkey 5 goto L710
	! needs something like Let FNPRG$
	open #1: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndex.h[cno],Shr",internal,input,keyed
	open #32: "Name=S:\Core\Data\acsllc\CLmstr.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr",internal,input,keyed
	L190: !
	pr newpage
	if prtall=1 then let fnopenprn
	close #101: ioerr ignore
	open #101: "SROW=10,SCOL=20,EROW=12,ECOL=60,BORDER=DR,CAPTION=PRINT LABELS FOR SELECTED CLIENTS",display,outIn
	pr f "11,21,C 14,N": "Client Number:"
	pr f "13,22,C 32,B,5": "Cancel (Esc)"
	pr f "14,22,C 32,B,6": "Search (F6)"
	pr f "15,22,C 32,B,10": "Print all (F10)"
	L250: !
	input fields "11,46,C 5,UE,N": k$
	if cmdkey=99 or cmdkey=5 then goto Xit
	if cmdkey=10 then prtall=1 : goto PRINT_EM
	if cmdkey=6 then goto SRCH1
	z$=rpad$(trim$(k$),5)
	read #1,using F_CLIENT,key=z$: z$,mat a$,mat app,mat ma nokey L250
	gosub PRINT_LABEL
	goto L190
 
	dim st2$(20)*24,ap2(20),ma2(20)
 
PRINT_EM: !
	pr newpage
	pr f "10,25,c 50,n": "PRINT CLIENT LABELS IN PROCESS"
	pr f "12,34,C 11,B,5": "Cancel (F5)"
	j=1
	READ_CLIENT: !
	read #1,using F_CLIENT: z$,mat a$,mat app,mat ma,mat ap2,mat ma2 eof L710 ioerr L850
	if trim$(a$(3))='' then goto READ_CLIENT ! skip clients with no CSZ
	for j=1 to min(20, udim(app))
		if app(j)<>0 then totalapp(j)+=1
		if ma(j)<>0 then totalma(j)+=1
	next j
	F_CLIENT: form pos 1,c 5,3*c 30,pos 375,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
	L470: !
	p1=pos(a$(3),",",1)
	if p1=0 then goto L520
	st$=uprc$(ltrm$(rtrm$(a$(3)(p1+1:p1+3))))
	if st$="MO" then goto L520
	L520: !
	gosub PRINT_LABEL
goto READ_CLIENT
 
PRINT_LABEL: ! r:
	a$(1)=rtrm$(a$(1))
	l2=len(a$(1))
	l1=l2-3
	c$=a$(1)(l1:l2)
	if c$="CPA." or c$=" CPA" then goto L640
	if c$="INC." or c$=" INC" then goto L640
	p1=pos(a$(1),",",1)
	if p1=0 then goto L640
	if a$(1)(p1+1:p1+1)=" " then p2=2 else p2=1
	a$(1)=rtrm$(a$(1)(p1+p2:30))&" "&a$(1)(1:p1-1)
	L640: !
	mat label_text$=("")
	for j=1 to 3 : label_text$(j)=a$(j) : next j
	fnaddlabel(mat label_text$)
	labelcount+=1
return  ! /r
 
L710: !
	close #1: ioerr ignore
	fnlabel(mat linestyle$)
	labelcount=0
	if uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" or uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" then chain prg$ ! XXX
goto Xit
Xit: !
	if labelcount>0 then
		fnlabel(mat linestyle$)
	end if
fnXit
 
L850: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L870
	goto L910
L870: pr newpage
	if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L900
	goto L910
L900: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
L910: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
	input fields "24,60,C 1,N": quitcode$
	if rtrm$(uprc$(quitcode$))="Q" then goto Xit
	pr f "23,3,C 78,N": ""
	pr f "24,3,C 78,N": ""
	retry
SRCH1: s1=1 ! NAME SEARCH
	open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outIn  ! SAVE SCREEN
L990: pr #127: newpage
	close #101: ioerr L1010
L1010: open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=BUSINESS NAME SEARCH",display,outIn
	prtall=0
	pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
	pr f "9,32,C 16,R,N": "Press F5 to stop"
L1050: input fields "7,50,C 30,UE,N": nam$
	if cmdkey=5 then goto SRCHEND
	nam$=rtrm$(nam$)
	l1=len(nam$)
	restore #32,search>=nam$: nokey L1050
	close #101: ioerr ignore
	L1110: !
	pr newpage
	pr f "1,10,C 5,R,N": "ACCT#"
	pr f "1,17,C 30,R,N": "COMPANY NAME"
	cde=0
	for j=1 to 20
		read #32,using F_CLIENT,release: k$,a1$,a2$,a3$ eof L1280
		form pos 1,c 5,c 30
! IF UPRC$(A3$)(1:3)><"RIC" THEN 940
		if a1$(1:l1)=nam$ or prtall=1 then goto L1200 else goto L1280
L1200: cde=1
		pr f str$(j+1)&",10,C 5,U,N": k$
		pr f str$(j+1)&",17,C 30,U,N": a1$
		if j>1 then goto L1270
		bk=bk+1
		if bk>20 then bk=1
		bk$(bk)=a1$
L1270: next j
L1280: if j>1 then j=j-1
	mat in2$(j)
L1300: pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
L1310: input fields "24,58,C 5,RE,N": k$
	alp=0
	if cmdkey=5 then goto SRCHEND
	if rtrm$(k$)="" then goto L1380
	z$=rpad$(trim$(k$),5)
	read #1,using F_CLIENT,key=z$: z$,mat a$,mat app,mat ma nokey L1310
	gosub PRINT_LABEL : goto L1300
L1380: if cmdkey><2 then goto L1430
	bk=bk-1
	if bk<1 then goto L1450
	restore #32,key>=bk$(bk): nokey L1450
	bk=bk-1
L1430: !
	selclp=1
goto L1110
L1450: !
	selclp=0
goto L990
SRCHEND: !
	close #101: ioerr ignore
	close #127: ioerr ignore
goto L250

